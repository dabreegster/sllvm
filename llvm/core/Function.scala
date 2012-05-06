package llvm.core

class Function(name: Option[String], val ret_type: Type,
               val params: List[Parameter], val parent: Module,
               junk: String = ""
              ) extends Value(name, ret_type) // TODO FunctionType(...)?
{
  var blocks: List[BasicBlock] = Nil

  // TODO
  params.foreach(p => p.parent = this)

  // builder pattern
  def setup_blocks(ls: List[BasicBlock]): Function = {
    blocks = ls
    blocks.foreach(b => b.parent = this)
    return this
  }

  var var_arg: Boolean = false

  def is_declaration = blocks.isEmpty

  // TODO user means somebody calls it
  // TODO iterate through flat list of instructions
  override def id = "@" + name.get
  override def full_name = ret_type + " " + id
  private def param_ir_form = "(" + params.map(_.full_name).mkString(", ") + ")"
  def ir_form = if (is_declaration)
                  "declare " + full_name + param_ir_form + "\n"
                else
                  "define " + full_name + param_ir_form + junk +
                  " {\n" + blocks.map(_.ir_form).mkString("\n") + "\n}\n"
}
