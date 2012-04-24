package llvm.core

class Module() {
  // TODO constructor
  var fxn_table: Map[String, Function] = Map()
  var global_table: Map[String, GlobalVariable] = Map()

  def functions = fxn_table.values
  def globals = global_table.values
  def ir_form = globals.map(_.toString).mkString("\n") +
                (if (globals.isEmpty) "" else "\n\n") +
                functions.map(_.ir_form).mkString("\n")
}
