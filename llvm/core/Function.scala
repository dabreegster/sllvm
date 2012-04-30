package llvm.core

class Function(name: Option[String], val ret_type: Type,
               val params: List[Parameter], val blocks: List[BasicBlock],
               val parent: Module
              ) extends Value(name, ret_type) // TODO FunctionType(...)?
{
  // TODO
  params.foreach(p => p.parent = this)
  blocks.foreach(b => b.parent = this)

  var var_arg: Boolean = false

  // TODO user means somebody calls it
  // TODO iterate through flat list of instructions
  override def id = "@" + name.get
  override def full_name = ret_type + " " + id
  def ir_form = "define " + full_name + "(" +
                params.map(_.full_name).mkString(", ") + ") {\n" +
                blocks.map(_.ir_form).mkString("\n") + "\n}\n"
}
