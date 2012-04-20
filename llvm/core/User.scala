package llvm.core

abstract class User() extends Value() {
  // TODO better constructor
  private var operands: List[Value] = Nil

  def add_operand(v: Value) = {
    operands :+= v
    v.users :+= this
  }
}
