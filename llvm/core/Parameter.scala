package llvm.core

class Parameter(name: Option[String], ltype: Type) extends Value(name, ltype) {
  // TODO constructor
  var parent: Function = null

  // it should have just one, hopefully
  def get_user(): Value = {
    llvm.Util.assert_eq(users.size, 1)
    return users.head match {
      case StoreInst(_, dst) => dst
      case _ => throw new Exception("Unexpected user of parameter")
    }
  }

  def ir_form = full_name // TODO not sure this is ever called..
  override def uniq_name = parent.name.get + "/" + name.get
}
