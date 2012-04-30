package llvm.core

class Module(val functions: List[Function], val globals: List[GlobalVariable]) {
  val fxn_table = functions.map(f => (f.name.get, f)).toMap
  val global_table = globals.map(g => (g.name.get, g)).toMap

  def ir_form = globals.map(_.toString).mkString("\n") +
                (if (globals.isEmpty) "" else "\n\n") +
                functions.map(_.ir_form).mkString("\n")
}
