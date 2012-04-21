package llvm

import llvm.core._
import scala.collection.mutable.HashMap
import scala.util.parsing.combinator._
import java.io.FileReader

// TODO oh, decide formatting >_<

// TODO make sure all the stuff we lazily created gets filled out eventually

// TODO do the assertions in the actual values, not parsing

object Parser extends JavaTokenParsers {
  // TODO util class
  //private def assert_eq(a: Any, b: Any) = assert(a == b, a + " != " + b)
  // TODO enable this once types work
  private def assert_eq(a: Any, b: Any) = {}

  // TODO can't use <~ and ~> to get rid of every junk token, unfortunately
  // TODO globals and other things from http://llvm.org/docs/LangRef.html

  def parse(fn: String): Module = parseAll(module, new FileReader(fn)) match {
    case Success(mod, rest) => mod
    case err                => throw new Exception("Parsing issue: " + err)
  }

  // for local values
  private val symbol_table = new HashMap[String, Value]()
  // for local BBs
  private val bb_table = new HashMap[String, BasicBlock]()
  // for all functions
  private val fxn_table = new HashMap[String, Function]()

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

  def module = comment ~> rep(target) ~> rep(function) ^^
               { case fxns => {
                   val m = new Module()
                   m.fxn_table = fxns.map(f => (f.name.get, f)).toMap
                   // TODO set up globals too
                   m
                 }
               }
  def comment = """;.*""".r
  def target  = """target.*""".r

  def function = "define" ~> ir_type ~ function_name ~ param_list ~
                 function_attribs ~ "{" ~ rep(bb) <~ "}" ^^
                 { case t~n~p~a~"{"~b => {
                     val f = new Function()
                     f.name = Some(n)
                     f.ret_type = t
                     f.params = p
                     p.foreach(_.parent = f)
                     f.blocks = b
                     b.foreach(_.parent = f)
                     // Clear the symbol table for the next function's locals
                     symbol_table.clear
                     bb_table.clear
                     fxn_table(n) = f
                     f
                   }
                 }
  def function_name = "@" ~> ident
  def param_list = "(" ~> repsep(param_pair, ",") <~ ")"
  def param_pair = (ir_type ~ "%" ~ ident) ^^
                   { case t~"%"~n => {
                       val p = new Parameter()
                       p.name = Some(n)
                       p.ltype = t
                       symbol_table(n) = p
                       p
                     }
                   }
  def function_attribs = opt("nounwind")

  def ir_type = prim_type ~ rep("*") ^^
                { case p~Nil   => p
                  case p~stars => PointerType(p, stars.size)
                }
  def prim_type = ("float" | "double" | """i\d+""".r) ^^
                  { case "float"  => FloatType()
                    case "double" => DoubleType()
                    case int      => IntegerType(int.tail.toInt)
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
  // TODO distinguish exactly what values are
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
  def inst = (alloca_inst | store_inst | load_inst | icmp_inst | phi_inst | call_inst)
  // TODO deal with constants too
  def local_id = "%" ~> """[\d\w]+""".r
  def local_id_lookup = local_id ^^ { case id => symbol_table(id) }
  def label = "%" ~> wholeNumber  // for BBs
  def constant = constant_int
  def constant_int = wholeNumber ^^ { case n => new ConstantInt(n.toInt) }
  def value = (local_id_lookup | constant)

  def alloca_inst = ("alloca" ~> ir_type ~ "," ~ alignment) ^^
                    { case t~","~a => {
                        val a = new AllocaInst()
                        a.ltype = t
                        a
                      }
                    }
  def alignment   = "align" ~ "4" // TODO generalize me
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
                        assert_eq(t, sv.ltype)  // TODO ptr
                        l.src = sv
                        l.ltype = t
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
  def call_inst   = "call" ~> ir_type ~ function_name ~ arg_list ^^
                    { case t~n~a => {
                        val c = new CallInst()
                        c.fxn = fxn_table(n)  // TODO might not be there yet
                        assert_eq(t, c.fxn.ret_type)
                        c.ltype = t
                        c.args = a
                        c
                      }
                    }
  def arg_list    = "(" ~> repsep(arg, ",") <~ ")"
  def arg         = ir_type ~ "%" ~ value ^^
                    { case t~"%"~v => {
                        assert_eq(t, v.ltype)
                        v
                      }
                    }
}
