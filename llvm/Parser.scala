package llvm

import llvm.core._
import llvm.Util._
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import java.io.FileReader

// TODO oh, decide formatting >_<

// based off http://llvm.org/docs/LangRef.html
object Parser extends JavaTokenParsers {
  // TODO deal with the named instruction nonsense

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

  def lookup_bb(key: String): BasicBlock = {
    if (bb_table.contains(key)) {
      return bb_table(key)
    } else {
      val b = new BasicBlock(Some(key))
      bb_table(key) = b
      return b
    }
  }

  def lookup_global(key: String, ltype: Type): GlobalVariable = {
    if (global_table.contains(key)) {
      val g = global_table(key)
      assert_eq(g.ltype, ltype)
      return g
    } else {
      val g = new GlobalVariable(Some(key), ltype)
      global_table(key) = g
      return g
    }
  }

  def module = comment ~ rep(target) ~ rep(type_decl) ~ rep(global_def | global_decl) ~
               rep(function | fxn_declare) ^^
               { case j1~j2~_~globals~fxns => new Module(
                   fxns, globals, j1 :: j2
                 )
               }
  def comment = """;.*""".r
  def target  = """target.*""".r
  def llvm_id = ("""[a-zA-Z$._][a-zA-Z$._0-9]*""".r | wholeNumber)
  // TODO this throws away params
  def fxn_declare = "declare"~>param_attrib_ls~>ir_type~function_name~function_sig ^^
                     { case t~n~_ => (m: Module) => new Function(
                         parent = m,
                         name = Some(n),
                         ret_type = t,
                         params = Nil
                       ) }

  def linkage = rep(linkage_attribs)
  def linkage_attribs = ("external" | "internal" | "private" | "unnamed_addr" |
                         "constant" | "global" | "common")
  def global_decl = global_id ~ "=" ~ linkage ~ ir_type ^^
                    { case n~"="~_~t => lookup_global(n, t.ptr_to) }
  def global_def = global_id ~ "=" ~ linkage ~ const_value ~ alignment ^^
                   { case n~"="~_~v~a => {
                       val g = lookup_global(n, v.ltype.ptr_to)
                        g.default_val = v
                        g.junk = a
                        g
                      }
                    }
            
  def global_id = "@" ~> llvm_id

  def type_decl = local_id ~ "=" ~ lazy_ir_type ^^
                  { case n~"="~t => {
                      t.name = Some(n)
                      type_table(n) = t
                      t
                    }
                  }

  def function = "define" ~> function_pre_attribs~>ir_type ~ function_name ~ param_list ~
                 function_post_attribs ~ "{" ~ rep(bb) <~ "}" ^^
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
  def function_name = "@" ~> llvm_id
  def param_list = "(" ~> repsep(param_pair, ",") <~ ")"
  def param_attrib_ls = rep(param_attrib)
  def param_attrib = ("zeroext" | "signext" | "inreg" | "byval" | "sret" |
                      "noalias" | "nocapture" | "nest")
  def param_pair = (ir_type ~ param_attrib_ls ~ local_id) ^^
                   { case t~_~n => {
                       val p = new Parameter(Some(n), t)
                       symbol_table(n) = p
                       p
                     }
                   }
  // TODO pre is linkage? post is what?
  def function_pre_attribs = opt("internal") ~ param_attrib_ls
  def function_post_attribs = rep(fxn_attrib) ^^ {
    case ls => ls.mkString(" ")
  }
  def fxn_attrib = ("address_safety" | """alignstack\(\d+\)""".r | "alwaysinline" |
                    "nonlazybind" | "inlinehint" | "naked" | "noimplicitfloat" |
                    "noinline" | "noredzone" | "noreturn" | "nounwind" |
                    "optsize" | "readnone" | "readonly" | "returns_twice" |
                    "ssp" | "sspreq" | "uwtable")

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
  def array_type: Parser[ArrayType] = "["~>wholeNumber~"x"~lazy_ir_type<~"]" ^^
                   { case num~"x"~t => ArrayType(t, num.toInt) }
  def struct_type: Parser[StructType] = "type {" ~> repsep(lazy_ir_type, ",") <~ "}" ^^
                                        { case ls => StructType(ls) }

  def bb_header = "; <label>:" ~> wholeNumber ~ "; preds =" ~
                  repsep(label, ",") ^^
                  { case lbl~junk~preds => lbl :: preds }
  def bb_lack_header = "; No predecessors!" ^^ { case _ => List("no_preds") }
  def bb = opt(bb_header | bb_lack_header) ~ rep(named_inst|unnamed_inst) ~ term_inst ^^
           {
             case Some(lbl :: preds)~i~t => (lookup_bb(lbl)).setup(
               i, t, preds.map(lookup_bb)
             )
             case None~i~t => (lookup_bb("0")).setup(i, t, Nil)
             case _ => throw new Exception("Impossible match parsing bb")
           }

  def term_inst = void_ret_inst | ret_inst | uncond_br_inst | br_inst |
                  switch_inst | unreachable_inst
  // note: splitting up disjunctions explicitly somehow helps type inference
  def void_ret_inst = "ret void" ^^ { case _ => new VoidReturnInst() }
  def ret_inst = "ret"~>single_value ^^ { case v => new ReturnInst(v) }
  def uncond_br_inst = "br label"~>label ^^
                       { case to => new UnconditionalBranchInst(lookup_bb(to)) }
  def br_inst = "br" ~> single_value ~ ", label" ~ label ~ ", label" ~ label ^^
                {
                  case v~_~t1~_~t2 => new BranchInst(
                    v, lookup_bb(t1), lookup_bb(t2)
                  )
                }
  def unreachable_inst = "unreachable" ^^ { case _ => new UnreachableInst() }
  def switch_inst = "switch"~>single_value~", label"~label~"["~rep(switch_target)~"]" ^^
  {
    case v~_~default~"["~ls~"]" => new SwitchInst(v, lookup_bb(default), ls)
  }
  def switch_target = single_value ~ ", label" ~ label ^^ { case v~_~l => (v, lookup_bb(l)) }
  def select_inst = "select"~>single_value~","~single_value~","~single_value ^^
            {
              case sel_v~","~v1~","~v2 => (n: Option[String]) => new SelectInst(
                n, sel_v, v1, v2
              )
            }

  // TODO i'm deprecating you next, bro.
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
             call_inst | indirect_call_inst | math_inst | cast_inst | gep_inst |
             select_inst
  def local_id = "%" ~> llvm_id
  def local_id_lookup = local_id ^^ { case id => symbol_table(id) }
  // TODO make it slurp the "label" part too, and change preds?
  def label = "%" ~> wholeNumber  // for BBs

  ///////////////////////////////////////////////////////////////////

  ////// the glue
  def single_value = ir_type ~ param_attrib_ls ~ bare_value ^^ {
    case t~_~bare => make_value(t, bare)
  }
  def const_value = ir_type ~ param_attrib_ls ~ bare_const_value ^^ {
    case t~_~bare => make_const_value(t, bare)
  }
  def pair_value = ir_type ~ param_attrib_ls ~ bare_value ~ "," ~ bare_value ^^ {
    case t~_~b1~","~b2 => (make_value(t, b1), make_value(t, b2))
  }

  def make_value(t: Type, pair: (String, Any)) = pair match {
    case ("local", v)   => make_local(t, v.asInstanceOf[Value])
    case ("global", g)  => make_global(t, g.asInstanceOf[String])
    case ("gep", ls)    => make_gep(t, ls.asInstanceOf[List[Value]])
    case _              => make_const_value(t, pair)
  }
  def make_const_value(t: Type, pair: (String, Any)) = pair match {
    case ("hex", n)     => make_const_hex_int(t, n.asInstanceOf[String])
    case ("num", n)     => make_const_num(t, n.asInstanceOf[String])
    case ("bool", b)    => make_const_bool(t, b.asInstanceOf[String])
    case ("zero", _)    => make_const_zeros(t)
    case ("null", _)    => make_const_null(t)
    case ("array", ls)  => make_const_array(t.asInstanceOf[ArrayType], ls.asInstanceOf[List[Value]])
    case ("struct", ls) => make_const_struct(t, ls.asInstanceOf[List[Value]])
    case ("string", s)  => make_const_string(t, s.asInstanceOf[String])
  }

  def bare_value: Parser[(String, Any)] = bare_local | bare_global | bare_const_value |
                                          bare_gep
  // be sure to attempt hex int before the others
  def bare_const_value: Parser[(String, Any)] = bare_const_hex_int | bare_const_num |
                                                bare_const_bool | bare_const_zeros |
                                                bare_const_null | bare_const_array |
                                                bare_const_struct | bare_const_string

  ///// actual constructors
  def make_local(t: Type, v: Value): Value = {
    assert_eq(v.ltype, t)
    return v
  }
  def make_global(t: Type, n: String) = lookup_global(n, t)
  def make_gep(t: Type, ls: List[Value]): Value = {
    val gep = new GEPInst(None, ls)
    assert_eq(gep.ltype, t)
    return gep
  }
  // TODO, yes, it throws away the number, but for now...
  def make_const_hex_int(t: Type, n: String) = t match {
    case IntegerType(bw) => ConstantInt(
      java.lang.Long.parseLong(n.drop(2), 16).toInt, bw
    )
    case FloatType()  => ConstantFP(java.lang.Long.parseLong(n.drop(2), 16).toDouble, t)
    case DoubleType() => ConstantFP(java.lang.Long.parseLong(n.drop(2), 16).toDouble, t)
    // TODO we could even parse more specifically!
    case _ => throw new Exception("weird type " + t + " on int " + n)
  }
  def make_const_num(t: Type, n: String) = t match {
    case IntegerType(bw) => ConstantInt(n.toInt, bw)
    case FloatType()     => ConstantFP(n.toDouble, t)
    case DoubleType()    => ConstantFP(n.toDouble, t)
  }
  def make_const_bool(t: Type, b: String) = t match {
    case IntegerType(bw) => ConstantInt(
      if (b == "true") 1 else 0, bw
    )
    case _ => throw new Exception("weird type " + t + " on a bool")
  }
  def make_const_zeros(t: Type) = ConstantZeros(t)
  def make_const_null(t: Type) = ConstantNull(t)
  def make_const_array(t: ArrayType, ls: List[Value]) = ConstantArray(t, ls, None)
  def make_const_struct(t: Type, ls: List[Value]) = ConstantStruct(t, ls)
  def make_const_string(t: Type, s: String) = ConstantString(t, s, None)

  ////////// regex matchers
  def bare_local = local_id_lookup ^^ { case l => ("local", l) }
  def bare_global = global_id ^^ { case g => ("global", g) }
  def bare_const_hex_int = """0x[0-9A-F]+""".r ^^ { case n => ("hex", n) }
  def bare_const_num = floatingPointNumber ^^ { case n => ("num", n) }
  def bare_const_bool = ("true" | "false") ^^ { case b => ("bool", b) }
  def bare_const_zeros = "zeroinitializer" ^^ { case z => ("zero", z) }
  def bare_const_null = "null" ^^ { case n => ("null", n) }
  def bare_const_array = "[" ~> repsep(single_value, ",") <~ "]" ^^ { case ls => ("array", ls) }
  def bare_const_struct = "{" ~> repsep(single_value, ",") <~ "}" ^^ { case ls => ("struct", ls) }
  def bare_const_string = "c\"" ~> """[^"]*""".r <~ "\"" ^^ { case s => ("string", s) }
  def bare_gep = "getelementptr"~>opt("inbounds")~>arg_list ^^ { case ls => ("gep", ls) }

  ///////////////////////////////////////////////////////////////////

  // and now the bestiary
  def alloca_inst = "alloca" ~> ir_type ~ alignment ^^
                    { case t~junk => (n: Option[String]) => new AllocaInst(n, t.ptr_to, junk)
                    }
  def alignment   = opt(", align" ~ wholeNumber) ^^ {
                      case Some(text~n) => text + " " + n
                      case None         => ""
                    }
  def store_inst  = "store" ~> single_value ~ "," ~ single_value ~ alignment ^^
                    { case sv~","~dv~junk =>
                        (n: Option[String]) => new StoreInst(n, sv, dv, junk)
                    }
  def load_inst   = "load" ~> single_value ~ alignment ^^
                    { case sv~junk => (n: Option[String]) => new LoadInst(n, sv, junk) }
  def icmp_inst   = "icmp" ~> icmp_op ~ pair_value ^^
                    { case op~vals => (n: Option[String]) => new IcmpInst(
                        n, op, vals._1, vals._2
                      )
                    }
  def icmp_op     = ("eq" | "ne" | "ugt" | "uge" | "ult" | "ule" |
                     "sgt" | "sge" | "slt" | "sle")
  def phi_inst    = "phi" ~> ir_type ~ repsep(phi_case, ",") ^^
                    { case t~cases => (n: Option[String]) => new PHIInst(
                        // cases are a list of ((string, any), bb)
                        n, t, cases.map(c => (make_value(t, c._1), c._2))
                      )
                    }
  def phi_case    = "[" ~> bare_value ~ "," ~ label <~ "]" ^^
                    { case b~","~l => (b, lookup_bb(l)) }
  def call_inst   = "call" ~> function_post_attribs ~> param_attrib_ls ~> ir_type ~
                    opt(function_sig) ~ function_name ~ arg_list <~ function_post_attribs ^^
                    { case t~_~c~a => (n: Option[String]) => new CallInst(n, c, t, a) }
  def indirect_call_inst = "call" ~> function_post_attribs ~> param_attrib_ls ~> ir_type ~
                            opt(function_sig) ~ local_id_lookup ~ arg_list <~
                            function_post_attribs ^^
              { case t~_~v~a => (n: Option[String]) => new IndirectCallInst(n, v, t, a) }
  // mostly shows up for var-arg stuff. TODO refactor with new fxn_type
  def function_sig = "(" ~> repsep(ir_type~param_attrib_ls, ",") <~ opt(", ...") <~ ")" <~
                     opt("*") <~ function_post_attribs
  def bare_arg_list = repsep(single_value, ",")
  def arg_list    = "(" ~> bare_arg_list <~ ")"
  
  def math_inst = math_op~math_specs~pair_value ^^
                 { case op~_~vals =>
                    (n: Option[String]) => new MathInst(
                      n, op, vals._1, vals._2
                    )
                 }
  def math_specs = opt("nuw")~opt("nsw")
  def math_op = ("add" | "fadd" | "sub" | "fsub" | "mul" | "fmul" | "udiv" |
                 "sdiv" | "fdiv" | "urem" | "srem" | "frem" | "shl" | "lshr" |
                 "ashr" | "and" | "or" | "xor")
  def cast_inst = cast_op~single_value~"to"~ir_type ^^
                  { case op~v~"to"~t =>
                    (n: Option[String]) => new CastInst(n, op, t, v)
                  }
  def cast_op = ("trunc" | "zext" | "sext" | "fptrunc" | "fpext" | "fptoui" |
                 "fptosi" | "uitofp" | "sitofp" | "ptrtoint" | "inttoptr" |
                 "bitcast")

  def gep_inst = "getelementptr"~>opt("inbounds")~>bare_arg_list ^^
                 { case ls => (n: Option[String]) => new GEPInst(n, ls) }
}
