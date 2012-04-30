package llvm.core

class Module(val make_functions: List[(Module) => Function],
             val globals: List[GlobalVariable])
{
  val fxn_table = make_functions.map(f => f(this)).map(f => (f.name.get, f)).toMap
  val global_table = globals.map(g => (g.name.get, g)).toMap

  def functions = fxn_table.values
  def ir_form = globals.map(_.toString).mkString("\n") +
                (if (globals.isEmpty) "" else "\n\n") +
                functions.map(_.ir_form).mkString("\n")
}
