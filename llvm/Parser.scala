package llvm

import llvm.core._
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import java.io.FileReader

// TODO oh, decide formatting >_<

// TODO make sure all the stuff we lazily created gets filled out eventually

// TODO do the assertions in the actual values, not parsing

object Parser extends JavaTokenParsers {
  private def assert_eq(a: Any, b: Any) = assert(a == b, a + " != " + b)

  // TODO can't use <~ and ~> to get rid of every junk token, unfortunately
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

  // TODO how's this? :P
  type InstFactory = (Function) => Instruction

  def lookup_bb(key: String): BasicBlock = {
    if (bb_table.contains(key)) {
      return bb_table(key)
    } else {
      val b = new BasicBlock()
      b.name = Some(key)
      bb_table(key) = b
      return b
    }
  }

  def module = (comment ~> rep(target) ~> rep(type_decl) ~> rep(global_decl |
                global_string) ~ rep(function)) ^^
               { case globals~fxns => new Module(fxns, globals) }
  def comment = """;.*""".r
  def target  = """target.*""".r
  def llvm_id = ("""[a-zA-Z$._][a-zA-Z$._0-9]*""".r | wholeNumber)

  def global_decl = global_id ~ "= global" ~ ir_type ~ constant ~ "," ~ alignment ^^
                    { case n~_~t~c~","~a => {
                        val g = new GlobalVariable()
                        g.ltype = t.ptr_to
                        g.name = Some(n)
                        g.default_val = c
                        // add it to the symbol table
                        global_table(n) = g
                        g
                      }
                    }
  def global_string = global_id ~ "= private unnamed_addr constant" ~ array_type ~
                      constant_string ~ ", " ~ alignment ^^
                      {
                        case name~_~t~s~_~_ => {
                          val g = new GlobalVariable()
                          g.ltype = t.ptr_to  // TODO not sure
                          g.name = Some(name)
                          g.default_val = new ConstantString(s, t.size)
                          // add to symbol table
                          global_table(name) = g
                          g
                        }
                      }
  def constant_string = "c\"" ~> """[^"]*""".r <~ "\""
            
  def global_id = "@" ~> llvm_id
  def global_id_lookup = global_id ^^ { case id => global_table(id) }

  def type_decl = local_id ~ "= type {" ~ repsep(ir_type, ",") <~ "}"
  // TODO process above.

  def function = "define" ~> ir_type ~ function_name ~ param_list ~
                 function_attribs ~ "{" ~ rep(bb) <~ "}" ^^
                 { case t~n~p~a~"{"~b => {
                     val f = (m: Module) => new Function(
                       parent = m,
                       name_tmp = Some(n),
                       ret_type = t,
                       params = p,
                       blocks = b
                     )
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
                       val p = new Parameter()
                       p.name = Some(n)
                       p.ltype = t
                       symbol_table(n) = p
                       p
                     }
                   }
  def function_attribs = opt("nounwind" | "noalias")

  def ir_type: Parser[Type] = prim_type ~ rep("*") ^^
                { case p~Nil   => p
                  case p~stars => PointerType(p, stars.size)
                }
  val int_type = """i(\d+)""".r
  def prim_type: Parser[Type] = ("float" | "double" | "void" | int_type | array_type | local_id) ^^
                  { case "float"      => FloatType()
                    case "double"     => DoubleType()
                    case "void"       => VoidType()
                    case int_type(bw) => IntegerType(bw.toInt)
                    case a: ArrayType => a
                    case name         => NamedType(name.toString)
                  }
  def array_type: Parser[ArrayType] = "["~>wholeNumber~"x"~ir_type<~"]" ^^
                   {
                     case num~"x"~t => ArrayType(t, num.toInt)
                   }

  def bb_header = "; <label>:" ~> wholeNumber ~ "; preds =" ~
                  repsep(label, ",") ^^
                  { case lbl~junk~preds => lbl :: preds }
  def bb = opt(bb_header) ~ rep(named_inst) ~ term_inst ^^
           {
             case Some(lbl :: preds)~i~t => {
               val b = lookup_bb(lbl)
               b.preds = preds.map(lookup_bb)
               b.inst_ls = i
               b.term_inst = t
               (t :: i).foreach(_.parent = b)
               b
             }
             case None~i~t => {
               val b = lookup_bb("0")
               b.preds = Nil
               b.inst_ls = i
               b.term_inst = t
               (t :: i).foreach(_.parent = b)
               b
             }
             case _ => throw new Exception("Impossible match parsing bb")
           }

  // (the right type isn't inferred)
  def term_inst: Parser[TerminatorInst] = (
                  ("ret" ~ ir_type ~ value)
                | ("ret void")
                | ("br label" ~ label)
                | ("br" ~ ir_type ~ value ~ ", label" ~ label ~ ", label" ~ label)) ^^
                {
                  case "ret void" => new VoidReturnInst()
                  case "ret"~t~raw_v => {
                    val v = cast_value(raw_v)
                    // check well-formedness
                    assert_eq(t, v.ltype)
                    val r = new ReturnInst()
                    r.ret_val = v
                    r
                  }
                  case "br label"~to => {
                    val b = new UnconditionalBranchInst()
                    b.target = lookup_bb(to.toString)
                    b
                  }
                  case "br"~t~raw_v~_~t1~_~t2 => {
                    val b = new BranchInst()
                    val v = cast_value(raw_v)
                    assert_eq(t, v.ltype)
                    b.test_val = v
                    b.true_target = lookup_bb(t1.toString)
                    b.false_target = lookup_bb(t2.toString)
                    b
                  }
                }

  // TODO why does the type get erased when involved with disjunctions?
  def cast_inst(x: Any): Instruction = x match {
    case i: Instruction => i
    case _              => throw new Exception("or " + x.getClass)
  }
  def cast_value(x: Any): Value = x match {
    case v: Value => v
    case _        => throw new Exception("or " + x.getClass)
  }

  // TODO if we split it up, can we avoid the type loss?
  def named_inst = ((local_id ~ "=" ~ inst) | inst) ^^
                 {
                   case name~"="~raw_i => {
                     val i = cast_inst(raw_i)
                     i.name = Some(name.toString)
                     // add it to the symbol table
                     symbol_table(name.toString) = i
                     i
                   }
                   case raw_i => {
                     val i = cast_inst(raw_i)
                     i.name = None
                     i
                   }
                 }
  def inst = (alloca_inst | store_inst | load_inst | icmp_inst | phi_inst |
              call_inst | bitcast_inst | add_inst)
  def local_id = "%" ~> llvm_id
  def local_id_lookup = local_id ^^ { case id => symbol_table(id) }
  def label = "%" ~> wholeNumber  // for BBs
  def constant = constant_int
  def constant_int = wholeNumber ^^ { case n => new ConstantInt(n.toInt) }
  def value = local_id_lookup | global_id_lookup | constant

  def alloca_inst = "alloca" ~> ir_type ~ "," ~ alignment ^^
                    { case t~","~a => {
                        val a = new AllocaInst()
                        a.ltype = t.ptr_to
                        a
                      }
                    }
  def alignment   = "align" ~ wholeNumber
  def store_inst  = "store" ~> ir_type ~ value ~ "," ~ ir_type ~ value <~
                    opt("," ~ alignment) ^^
                    { case st~sv~","~dt~dv => {
                        val s = new StoreInst()
                        assert_eq(st, sv.ltype)
                        assert_eq(dt, dv.ltype)
                        s.src = sv
                        s.dst = dv
                        s
                      }
                    }
  def load_inst   = "load" ~> ir_type ~ value <~ opt("," ~ alignment) ^^
                    { case t~sv => {
                        val l = new LoadInst()
                        assert_eq(t, sv.ltype)  // TODO ptr?
                        l.src = sv
                        l.ltype = t.deref
                        l
                      }
                    }
  def icmp_inst   = "icmp" ~> icmp_op ~ ir_type ~ value ~ "," ~ value ^^
                    { case o~t~v1~","~v2 => {
                        val i = new IcmpInst()
                        i.op = o
                        assert_eq(t, v1.ltype)
                        assert_eq(t, v2.ltype)
                        i.val1 = v1
                        i.val2 = v2
                        i
                      }
                    }
  def icmp_op     = ("eq" | "ne" | "ugt" | "uge" | "ult" | "ule" |
                     "sgt" | "sge" | "slt" | "sle")
  def phi_inst    = "phi" ~> ir_type ~ repsep(phi_case, ",") ^^
                    { case t~cases => {
                        val p = new PHIInst()
                        cases.foreach(c => assert_eq(c._1.ltype, t))
                        p.cases = cases.map(c => (c._1, lookup_bb(c._2)))
                        p.ltype = t
                        p
                      }
                    }
  def phi_case    = "[" ~> value ~ "," ~ label <~ "]" ^^
                    { case v~","~l => (v, l) }
  def call_inst   = "call" ~> function_attribs ~> ir_type ~ opt(function_sig) ~
                    function_name ~ arg_list <~ function_attribs ^^
                    { case t~_~n~a => new CallInst(n, t, a) }
  // mostly shows up for var-arg stuff
  def function_sig = "(" ~> repsep(ir_type, ",") <~ ", ...)*"
  def arg_list    = "(" ~> repsep(arg, ",") <~ ")"
  def arg: Parser[Value] = ir_type ~ (value|gep_inst) ^^
                    { case t~v => {
                        assert_eq(t, v.ltype)
                        v
                      }
                    }
  
  def bitcast_inst = "bitcast"~>ir_type~value~"to"~ir_type ^^
                     {
                       case t1~v~"to"~t2 => {
                         val b = new BitcastInst()
                         b.value = v
                         b.ltype = t2
                         assert(v.ltype == t1)
                         b
                       }
                     }

  def add_inst = "add"~>add_specs~>ir_type~value~","~value ^^
                 {
                   case t~v1~_~v2 => {
                     assert(v1.ltype == v2.ltype)
                     val a = new AddInst()
                     a.ltype = t
                     a.v1 = v1
                     a.v2 = v2
                     a
                   }
                 }
  def add_specs = opt("nuw")~opt("nsw")

  // the ()'s are for when this is embedded in an argument list.. generalize?
  def gep_inst = "getelementptr"~>opt("inbounds")~>arg_list ^^
                 {
                   case ls => {
                     // TODO i actually think the first is always a pointer. gep
                     // is hard.
                     val g = new GEPInst()
                     g.fields = ls
                     // TODO this is _probably_ wrong.
                     g.ltype = ls.head.asInstanceOf[GlobalVariable].default_val.ltype.asInstanceOf[SequentialType].ptr_to_member
                     g
                   }
                 }
}
