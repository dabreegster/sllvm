package llvm.core

class GlobalVariable() extends Constant() {
  // just like alloca, ltype is a pointer to whatever was allocated
  var default_val: Constant = null  // TODO assert the types all match up

  def alloced_type = ltype.deref
  def ir_form = "global " + alloced_type + " " + default_val // TODO alignment
  override def id = "@" + name.get
}
