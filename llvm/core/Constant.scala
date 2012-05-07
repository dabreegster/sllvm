package llvm.core

import llvm.Util._

abstract class Constant(name: Option[String], ltype: Type) extends Value(name, ltype) {}

case class ConstantInt(val n: Int, bw: Int) extends Constant(None, IntegerType(bw)) {
  // TODO ever have a name?
  // TODO some kind of AP int knockoff?

  def ir_form = n.toString
  override def id = n.toString
  override def full_name = ltype + " " + n
}

case class ConstantFP(val n: Double) extends Constant(None, DoubleType()) {
  // TODO or FloatType?
  // TODO ever have a name?

  def ir_form = n.toString
  override def id = n.toString
  override def full_name = ltype + " " + n
}

case class ConstantString(s: String, size: Int, opt_name: Option[String]) extends Constant(
  opt_name, ArrayType(IntegerType(8), size)
) {
  // TODO this should really be a case of ConstantArray

  def ir_form = "c\"" + s + "\""
}

case class ConstantArray(base_type: Type, size: Int, contents: List[Value],
                         opt_name: Option[String], junk: String = "")
      extends Constant(opt_name, ArrayType(base_type, size))
{
  def ir_form = "[" + contents.map(_.full_name).mkString(", ") + "]" + junk
}

//class ConstantArray() extends Constant() {
  // TODO constructor
//}

// TODO constant arrays and structs

// TODO in llvm, Function subclasses GlobalValue, which subclasses this
