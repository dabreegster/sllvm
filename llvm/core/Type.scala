package llvm.core

// TODO how to represent these? singletons could be cool..
// TODO are case classes even a good idea?

class Type() {}

case class IntegerType(bitwidth: Int) extends Type() {
  override def toString = "i" + bitwidth
}
case class VoidType() extends Type() {
  override def toString = "void"
}
case class FloatType() extends Type() {
  override def toString = "float"
}
case class DoubleType() extends Type() {
  override def toString = "double"
}
case class LabelType() extends Type() {
  // for BBs
  override def toString = "label"
}
class SequentialType(base: Type) extends Type() {}
case class ArrayType(base: Type, size: Int) extends SequentialType(base) {
  override def toString = "array[" + base + " ](" + size + ")"
}
case class PointerType(base: Type, levels: Int) extends SequentialType(base) {
  override def toString = base + ("*" * levels)
  }
//case class StructType(fields: Map[String, Type]) extends Type
case class FunctionType(ret_type: Type, param_types: List[Type],
                        var_arg: Boolean) extends Type()
{
  override def toString = "f :: " + param_types + " -> " + ret_type
}

//class SequentialType[T <: Type]()                extends Type
// TODO hard to parametrize that way
