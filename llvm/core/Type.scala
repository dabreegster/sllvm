package llvm.core

// TODO how to represent these? singletons could be cool..
// TODO are case classes even a good idea?

class Type() {
  def deref: Type = throw new Exception("Can't dereference a primitive type " + this)
  def ptr_to: PointerType = new PointerType(this, 1)
}

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
class SequentialType(base: Type) extends Type() {
  // TODO dubious
  def ptr_to_member = base.ptr_to
}
case class ArrayType(base: Type, size: Int) extends SequentialType(base) {
  override def toString = "array[" + base + " ](" + size + ")"
}
case class PointerType(base: Type, levels: Int) extends SequentialType(base) {
  override def toString = base + ("*" * levels)
  override def deref = if (levels == 1)
                         base
                       else
                         new PointerType(base, levels - 1)
  override def ptr_to = new PointerType(base, levels + 1)
}
//case class StructType(fields: Map[String, Type]) extends Type
case class FunctionType(ret_type: Type, param_types: List[Type],
                        var_arg: Boolean) extends Type()
{
  override def toString = "f :: " + param_types + " -> " + ret_type
}

case class NamedType(name: String) extends Type() {} // TODO tmp.

//class SequentialType[T <: Type]()                extends Type
// TODO hard to parametrize that way
