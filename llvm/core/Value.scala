package llvm.core

abstract class Value() {
  // TODO constructor
  var name: Option[String] = None
  var ltype: Type = null
  // TODO make up a name if we don't have one

  var users: List[User] = Nil

  // TODO do we want distinction between printing just name and whole line?

  // TODO the difference between these 3 is slightly annoying
  def id = "%" + name.get
  def full_name = ltype + " " + id
  def ir_form(): String
  override def toString = name match {
    case Some(n) => "%%%s = %s".format(n, ir_form)
    case None    => ir_form
  }
}
