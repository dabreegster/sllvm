package llvm.core

class Module() {
  // TODO constructor
  var fxn_table: Map[String, Function] = Map()
  //var globals: Map[String, GlobalVariable] = Map()

  def functions = fxn_table.values
}
