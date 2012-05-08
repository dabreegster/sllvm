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

case class ConstantFP(val n: Double, t: Type) extends Constant(None, t) {
  // TODO ever have a name?

  def ir_form = n.toString
  override def id = n.toString
  override def full_name = ltype + " " + n
}

case class ConstantString(t: Type, s: String, opt_name: Option[String])
    extends Constant(opt_name, t)
{
  // TODO this should really be a case of ConstantArray
  // TODO gotta unescape the string first
  //assert_eq(ltype, ArrayType(IntegerType(8), s.size))

  def ir_form = "c\"" + s + "\""
}

// TODO Array, not List
case class ConstantArray(array_ltype: ArrayType, contents: List[Value],
                         opt_name: Option[String], junk: String = "")
      extends Constant(opt_name, array_ltype)
{
  contents.foreach(v => assert_eq(v.ltype, base_type))

  def base_type = array_ltype.base
  def size = array_ltype.size

  def ir_form = "[" + contents.map(_.full_name).mkString(", ") + "]" + junk
}

// LLVM's way of not specifying tons of 0's
case class ConstantZeros(t: Type) extends Constant(None, t) {
  def ir_form = "zeroinitializer"
}

case class ConstantNull(t: Type) extends Constant(None, t) {
  def ir_form = "null"
}

case class ConstantStruct(t: Type, contents: List[Value]) extends Constant(None, t)
{
  assert_eq(ltype, StructType(contents.map(v => later { v.ltype })))

  def ir_form = "{ " + contents.map(_.full_name).mkString(", ") + " }"
}

// TODO in llvm, Function subclasses GlobalValue, which subclasses this
