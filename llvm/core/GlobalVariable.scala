package llvm.core

class GlobalVariable(name: Option[String], ltype: Type,
                     val default_val: Constant, junk: String = ""
                    ) extends Constant(name, ltype)
{
  // just like alloca, ltype is a pointer to whatever was allocated

  def alloced_type = ltype.deref
  def ir_form = "global " + alloced_type + " " + default_val + junk
  override def id = "@" + name.get
}
