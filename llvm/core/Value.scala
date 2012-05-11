package llvm.core

import scala.collection.mutable.MutableList

abstract class Value(val name: Option[String], val ltype: Type) {
  val users = new MutableList[User]()

  def add_use(u: User) = {
    users += u
  }

  // TODO do we want distinction between printing just name and whole line?

  // TODO the difference between these is slightly annoying
  def id = name match {
    // case 1 is normal
    case Some(n) => "%" + n
    // case 2 is when getelementptr inst's are embedded in a call
    case None    => ir_form
  }
  def full_name = ltype + " " + id
  def ir_form(): String   // abstract
  override def toString = name match {
    case Some(n) => id + " = " + ir_form
    case None    => ir_form
  }
  // inclues the parent function
  def uniq_name = name.get
}
