package llvm

import llvm.core._
import llvm.Util._
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import java.io.FileReader

// TODO oh, decide formatting >_<

// TODO make sure all the stuff we lazily created gets filled out eventually

object Parser extends JavaTokenParsers {
  // TODO make an implicit def for the named instruction pattern

  // TODO other things from http://llvm.org/docs/LangRef.html

  def parse(fn: String): Module = parseAll(module, new FileReader(fn)) match {
    case Success(mod, rest) => mod
    case err                => throw new Exception("Parsing issue: " + err)
  }

  // for local values
  private val symbol_table = new HashMap[String, Value]()
  // for global values
  private val global_table = new HashMap[String, GlobalVariable]()
  // for local BBs
  private val bb_table = new HashMap[String, BasicBlock]()
  // for named types
  private val type_table = new HashMap[String, Later[Type]]()

  // TODO how's this? :P
  //type InstFactory = (Function) => Instruction
  // TODO actually, save on typing and get rid of the 'name => inst' bit?

  def lookup_bb(key: String): BasicBlock = {
    if (bb_table.contains(key)) {
      return bb_table(key)
    } else {
      val b = new BasicBlock(Some(key))
      bb_table(key) = b
      return b
    }
  }

  def module = comment ~ rep(target) ~ rep(type_decl) ~ rep(global_string | global_decl) ~
               rep(function) ~ rep(fxn_declares) ^^
               { case j1~j2~_~globals~fxns~declares => new Module(
                   fxns ++ declares, globals, j1 :: j2
                 )
               }
  def comment = """;.*""".r
  def target  = """target.*""".r
  def llvm_id = ("""[a-zA-Z$._][a-zA-Z$._0-9]*""".r | wholeNumber)
  // TODO this throws away params
  def fxn_declares = "declare"~>ir_type~function_name~function_sig ^^
                     { case t~n~_ => (m: Module) => new Function(
                         parent = m,
                         name = Some(n),
                         ret_type = t,
                         params = Nil
                       ) }

  def linkage = opt("external" | "internal")
  def global_decl = global_id ~ "=" ~ linkage ~ "global" ~ ir_type ~
                    opt(constant ~ alignment) ^^
                    { case n~"="~_~"global"~t~Some(c~a) => {
                        val g = new GlobalVariable(Some(n), t.ptr_to, c, a)
                        global_table(n) = g
                        g
                      }
                      case n~"="~_~"global"~t~None => {
                        // TODO no default val...
                        val g = new GlobalVariable(Some(n), t.ptr_to, null)
                        global_table(n) = g
                        g
                      }
                    }

  def global_string = global_id ~ "= private unnamed_addr constant" ~ array_type ~
                      constant_string ~ alignment ^^
                      {
                        case n~_~t~s~_ => {
                          // TODO not sure of type
                          // TODO giving it the name twice?
                          val g = new GlobalVariable(
                            Some(n), t.ptr_to, ConstantString(s, t.size, Some(n))
                          )
                          // add to symbol table
                          global_table(n) = g
                          g
                        }
                      }
  def constant_string = "c\"" ~> """[^"]*""".r <~ "\""
            
  def global_id = "@" ~> llvm_id
  def global_id_lookup = global_id ^^ { case id => global_table(id) }

  def type_decl = local_id ~ "=" ~ lazy_ir_type ^^
                  { case n~"="~t => {
                      type_table(n) = t
                      t
                    }
                  }

  def function = "define" ~> ir_type ~ function_name ~ param_list ~
                 function_attribs ~ "{" ~ rep(bb) <~ "}" ^^
                 { case t~n~p~a~"{"~b => {
                     val f = (m: Module) => new Function(
                       parent = m,
                       name = Some(n),
                       ret_type = t,
                       params = p,
                       junk = a
                     ).setup_blocks(b)
                     // Clear the symbol table for the next function's locals
                     symbol_table.clear
                     bb_table.clear
                     f
                   }
                 }
  def function_name = "@" ~> ident
  def param_list = "(" ~> repsep(param_pair, ",") <~ ")"
  def param_pair = (ir_type ~ local_id) ^^
                   { case t~n => {
                       val p = new Parameter(Some(n), t)
                       symbol_table(n) = p
                       p
                     }
                   }
  def function_attribs = opt("nounwind" | "noalias") ^^ {
                           case Some(a) => " " + a
                           case None    => ""
                         }

  def ir_type = lazy_ir_type ^^ { case l => l() }  // evaluate now
  def lazy_ir_type: Parser[Later[Type]] = ptr_type ~
                    opt("(" ~> repsep(lazy_ir_type, ",") ~ ")" ~ rep("*")) ^^
                { case b~None => b
                  case b~Some(ls~")"~Nil)   => FunctionType(b, ls, false)
                  case b~Some(ls~")"~stars) => PointerType(FunctionType(b, ls, false), stars.size)
                }
  def ptr_type: Parser[Later[Type]] = prim_type ~ rep("*") ^^
                 { case p~Nil   => p
                   case p~stars => PointerType(p, stars.size)
                 }
  val int_type = """i(\d+)""".r
  def prim_type: Parser[Later[Type]] = ("float" | "double" | "void" | int_type |
                                        array_type | struct_type | local_id) ^^
                  { case "float"       => FloatType()
                    case "double"      => DoubleType()
                    case "void"        => VoidType()
                    case int_type(bw)  => IntegerType(bw.toInt)
                    case a: ArrayType  => a
                    case s: StructType => s
                    case name          => later { type_table(name.toString) }
                  }
  def array_type: Parser[ArrayType] = "["~>wholeNumber~"x"~ir_type<~"]" ^^
                   { case num~"x"~t => ArrayType(t, num.toInt) }
  def struct_type: Parser[StructType] = "type {" ~> repsep(lazy_ir_type, ",") <~ "}" ^^
                                        { case ls => StructType(ls) }

  def bb_header = "; <label>:" ~> wholeNumber ~ "; preds =" ~
                  repsep(label, ",") ^^
                  { case lbl~junk~preds => lbl :: preds }
  def bb = opt(bb_header) ~ rep(named_inst|unnamed_inst) ~ term_inst ^^
           {
             case Some(lbl :: preds)~i~t => (lookup_bb(lbl)).setup(
               i, t, preds.map(lookup_bb)
             )
             case None~i~t => (lookup_bb("0")).setup(i, t, Nil)
             case _ => throw new Exception("Impossible match parsing bb")
           }

  def term_inst = void_ret_inst | ret_inst | uncond_br_inst | br_inst
  // note: splitting up disjunctions explicitly somehow helps type inference
  def void_ret_inst = "ret void" ^^ { case _ => new VoidReturnInst() }
  def ret_inst = "ret"~>typed_value ^^ { case t~v => new ReturnInst(v, t) }
  def uncond_br_inst = "br label"~>label ^^
                       { case to => new UnconditionalBranchInst(lookup_bb(to)) }
  def br_inst = "br" ~> typed_value ~ ", label" ~ label ~ ", label" ~ label ^^
                {
                  case t~v~_~t1~_~t2 => new BranchInst(
                    t, v, lookup_bb(t1), lookup_bb(t2)
                  )
                }

  def named_inst = local_id ~ "=" ~ inst ^^
                 {
                   case name~"="~factory => {
                     val i = factory(Some(name))
                     symbol_table(name) = i
                     i
                   }
                 }
  def unnamed_inst = inst ^^ { case i => i(None) }

  def inst = alloca_inst | store_inst | load_inst | icmp_inst | phi_inst |
             call_inst | bitcast_inst | add_inst | gep_inst | sext_inst
  def local_id = "%" ~> llvm_id
  def local_id_lookup = local_id ^^ { case id => symbol_table(id) }
  def label = "%" ~> wholeNumber  // for BBs
  def constant = constant_int | constant_array
  def constant_int = wholeNumber ^^ { case n => ConstantInt(n.toInt, 32) }
  def constant_array: Parser[Constant] = "[" ~> repsep(typed_value, ",") <~ "]" ^^
                     {
                       case ls => ConstantArray(
                         // throw away the type in the list of (type, val) pairs
                         ls.head._2.ltype, ls.size, ls.map(_._2), None
                       )
                     }

  def value = local_id_lookup | global_id_lookup | constant
  def typed_value = typed_val1 | typed_val2
  def typed_val1 = ir_type ~ value ^^ { case t~v => recast(t, v) }
  def typed_val2 = ir_type~gep_inst ^^ {
                     case t~factory => {
                       val g = factory(None)  // nameless
                       assert_eq(t, g.ltype)
                       recast(t, g)
                     } }
  
  // sometimes we get more info about a value...
  def recast(t: Type, v: Value): ~[Type, Value] = (v, t) match {
    case (ConstantInt(n, _), IntegerType(bw)) => new ~[Type, Value](t, ConstantInt(n, bw))
    case (v, t) => new ~[Type, Value](t, v)
    // TODO assert types eq in second case?
  }

  def alloca_inst = "alloca" ~> ir_type ~ alignment ^^
                    { case t~junk => (n: Option[String]) => new AllocaInst(n, t.ptr_to, junk)
                    }
  def alignment   = opt(", align" ~ wholeNumber) ^^ {
                      case Some(text~n) => text + " " + n
                      case None         => ""
                    }
  def store_inst  = "store" ~> typed_value ~ "," ~ ir_type ~ value ~ alignment ^^
                    { case st~sv~","~dt~dv~junk =>
                        (n: Option[String]) => new StoreInst(n, sv, st, dv, dt, junk)
                    }
  def load_inst   = "load" ~> typed_value ~ alignment ^^
                    { case t~sv~junk => (n: Option[String]) => new LoadInst(n, sv, t, junk)
                    }
  def icmp_inst   = "icmp" ~> icmp_op ~ ir_type ~ value ~ "," ~ value ^^
                    { case op~t~v1~","~v2 =>
                      // TODO use typed_value and recast v2.
                      (n: Option[String]) => new IcmpInst(n, op, t, v1, v2)
                    }
  def icmp_op     = ("eq" | "ne" | "ugt" | "uge" | "ult" | "ule" |
                     "sgt" | "sge" | "slt" | "sle")
  def phi_inst    = "phi" ~> ir_type ~ repsep(phi_case, ",") ^^
                    { case t~cases => (n: Option[String]) => new PHIInst(n, t, cases) }
  def phi_case    = "[" ~> value ~ "," ~ label <~ "]" ^^
                    { case v~","~l => (v, lookup_bb(l)) }
  def call_inst   = "call" ~> function_attribs ~> ir_type ~ opt(function_sig) ~
                    function_name ~ arg_list <~ function_attribs ^^
                    { case t~_~c~a => (n: Option[String]) => new CallInst(n, c, t, a) }
  // mostly shows up for var-arg stuff. TODO refactor with new fxn_type
  def function_sig = "(" ~> repsep(ir_type, ",") <~ ", ...)"<~opt("*")
  def bare_arg_list = repsep(arg, ",")
  def arg_list    = "(" ~> bare_arg_list <~ ")"
  def arg: Parser[Value] = typed_value ^^ { case t~v => v }
  
  def bitcast_inst = "bitcast"~>ir_type~value~"to"~ir_type ^^
                     { case t1~v~"to"~t2 =>
                       (n: Option[String]) => new BitcastInst(n, t2, v, t1)
                     }

  def add_inst = "add"~>add_specs~>ir_type~value~","~value ^^
                 { case t~v1~_~v2 =>
                    (n: Option[String]) => new AddInst(n, t, v1, v2)
                 }
  def add_specs = opt("nuw")~opt("nsw")

  // the ()'s are for when this is embedded in an argument list.. generalize?
  def gep_inst = "getelementptr"~>opt("inbounds")~>(arg_list|bare_arg_list) ^^
                 { case ls => (n: Option[String]) => new GEPInst(n, ls) }
  def sext_inst = "sext"~>typed_value~"to"~ir_type ^^
                  { case t1~v~"to"~t2 => (n: Option[String]) => new SextInst(n, t1, v, t2) }
}
