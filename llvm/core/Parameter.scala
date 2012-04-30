package llvm.core

class Parameter(name: Option[String], ltype: Type) extends Value(name, ltype) {
  // TODO constructor
  var parent: Function = null

  def ir_form = full_name // TODO not sure this is ever called..
}
