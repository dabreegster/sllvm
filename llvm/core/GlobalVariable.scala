package llvm.core

class GlobalVariable(name: Option[String], ltype: Type) extends Constant(name, ltype)
{
  // TODO this should be Constant, but due to const_expr instructions... we'd
  // have to mod those to spit out constants, which may be fine
  var default_val: Value = null
  var junk: String = ""

  // just like alloca, ltype is a pointer to whatever was allocated
  def alloced_type = ltype.deref
  def ir_form = "global " + alloced_type + " " + default_val + junk
  override def id = "@" + name.get
}
