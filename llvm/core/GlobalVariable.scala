package llvm.core

class GlobalVariable(name: Option[String], ltype: Type) extends Constant(name, ltype)
{
  var default_val: Constant = null
  var junk: String = ""

  // just like alloca, ltype is a pointer to whatever was allocated
  def alloced_type = ltype.deref
  def ir_form = "global " + alloced_type + " " + default_val + junk
  override def id = "@" + name.get
}
