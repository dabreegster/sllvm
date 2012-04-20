package llvm.core

class Parameter() extends Value() {
  // TODO constructor
  var parent: Function = null

  def ir_form = full_name // TODO not sure this is ever called..
}
