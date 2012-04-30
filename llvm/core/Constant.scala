package llvm.core

abstract class Constant(name: Option[String], ltype: Type) extends Value(name, ltype) {}

class ConstantInt(val n: Int) extends Constant(None, IntegerType(32)) {
  // TODO what bitwidth?
  // TODO ever have a name?
  // TODO some kind of AP int knockoff?

  def ir_form = n.toString
  override def id = n.toString
  override def full_name = ltype + " " + n
}

class ConstantFP(val n: Double) extends Constant(None, DoubleType()) {
  // TODO or FloatType?
  // TODO ever have a name?

  def ir_form = n.toString
  override def id = n.toString
  override def full_name = ltype + " " + n
}

class ConstantString(s: String, size: Int, name: Option[String]) extends Constant(
  name, ArrayType(IntegerType(8), size)
) {
  // TODO this should really be a case of ConstantArray

  def ir_form = "c\"" + s + "\""
}

//class ConstantArray() extends Constant() {
  // TODO constructor
//}

// TODO constant arrays and structs

// TODO in llvm, Function subclasses GlobalValue, which subclasses this
