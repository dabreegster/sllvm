package llvm.core

abstract class Constant() extends Value() {}

class ConstantInt(val n: Int) extends Constant() {
  // TODo some kind of AP int knockoff?
  ltype = IntegerType(32) // TODO what bitwidth?

  def ir_form = n.toString
  override def full_name = ltype + " " + n
}

class ConstantFP() extends Constant() {
  // TODO constructor
  var n: Double = -1
  ltype = DoubleType()  // TODO or FloatType?

  def ir_form = n.toString
  override def full_name = ltype + " " + n
}

// TODO constant arrays and structs

// TODO in llvm, Function subclasses GlobalValue, which subclasses this
