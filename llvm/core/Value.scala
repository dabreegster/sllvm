package llvm.core

abstract class Value(val name: Option[String], val ltype: Type) {
  var users: List[User] = Nil

  // TODO do we want distinction between printing just name and whole line?

  // TODO the difference between these 3 is slightly annoying
  def id = "%" + name.get
  def full_name = ltype + " " + id
  def ir_form(): String   // abstract
  override def toString = name match {
    case Some(n) => id + " = " + ir_form
    case None    => ir_form
  }
}
