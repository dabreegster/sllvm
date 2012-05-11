package llvm.core

import llvm.Util._

// TODO how to represent these? singletons could be cool..
// TODO are case classes even a good idea?

class Type() {
  var name: Option[String] = None

  // TODO or make them cast to PointerType?
  def deref: Type = throw new Exception("Can't dereference a primitive type " + this)
  def ptr_to: PointerType = new PointerType(this, 1)
  def ptrs_to(lvls: Int): Type = if (lvls == 0)
                                   this
                                 else
                                   ptr_to.ptrs_to(lvls - 1)
  def derefs(lvls: Int): Type = if (lvls == 0)
                                  this
                                else
                                  deref.derefs(lvls - 1)
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
case class ArrayType(base_factory: Later[Type], size: Int) extends Type() {
  lazy val base = base_factory()

  override def equals(other: Any) = other match {
    case x: ArrayType => (base == x.base && size == x.size)
    case _ => false
  }
  override def toString = "[" + base + " x " + size + "]"

  // TODO dubious
  def ptr_to_member = base.ptr_to
}
case class PointerType(base_factory: Later[Type], levels: Int) extends Type() {
  lazy val base = base_factory()

  override def toString = base + ("*" * levels)
  override def equals(other: Any) = other match {
    case x: PointerType => (base == x.base && levels == x.levels)
    case _ => false
  }
  override def deref = if (levels == 1)
                         base
                       else
                         new PointerType(base, levels - 1)
  override def ptr_to = new PointerType(base, levels + 1)
}
case class StructType(fields_factory: List[Later[Type]]) extends Type() {
  lazy val fields = fields_factory.map(_())

  // TODO a diff extractor here?
  override def equals(other: Any) = other match {
    case x: StructType => fields == x.fields
    case _ => false
  }

  // TODO recursive types break things, this is a temporary workaround
  override def toString = name match {
    case Some(n) => "%" + n   // TODO always a local?
    case None    => "type { " + fields.map(_.toString).mkString(", ") + " }"
  }
  //override def toString = "type { " + fields.map(_.toString).mkString(", ") + " }"
}
case class FunctionType(ret_factory: Later[Type], params_factory: List[Later[Type]],
                        var_arg: Boolean) extends Type()
{
  lazy val ret_type = ret_factory()
  lazy val param_types = params_factory.map(_())

  override def equals(other: Any) = other match {
    case x: FunctionType => (ret_type == x.ret_type && param_types == x.param_types)
    case _ => false
  }

  override def toString = "f :: " + param_types + " -> " + ret_type
}
