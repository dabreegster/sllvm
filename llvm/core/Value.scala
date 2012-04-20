package llvm.core

abstract class Value() {
  // TODO constructor
  var name: String = null
  var ltype: Type = null
  // TODO make up a name if we don't have one

  var users: List[User] = Nil

  // TODO do we want distinction between printing just name and whole line?

  def id = "%" + name
  def full_name = ltype + " " + id
  def ir_form(): String
  override def toString = "%%%s = %s".format(name, ir_form)
}
