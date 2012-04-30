package llvm.core

class Function(name_tmp: Option[String], val ret_type: Type,
               val params: List[Parameter], val blocks: List[BasicBlock],
               val parent: Module
              ) extends Value()
{

  // TODO
  params.foreach(p => p.parent = this)
  blocks.foreach(b => b.parent = this)
  name = name_tmp   // TODO fix value

  var var_arg: Boolean = false
  //ltype = FunctionType(ret_type, params.map(_.ltype), var_arg)

  // TODO user means somebody calls it
  // TODO iterate through flat list of instructions
  override def id = "@" + name.get
  override def full_name = ret_type + " " + id
  def ir_form = "define " + full_name + "(" +
                params.map(_.full_name).mkString(", ") + ") {\n" +
                blocks.map(_.ir_form).mkString("\n") + "\n}\n"
}
