package llvm.core

abstract class User(name: Option[String], ltype: Type) extends Value(name, ltype) {
  // TODO better constructor
  private var operands: List[Value] = Nil

  def add_operand(v: Value) = {
    operands :+= v
    v.users :+= this
  }
}
