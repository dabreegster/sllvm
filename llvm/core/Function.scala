package llvm.core

class Function() extends Value() {
  // TODO constructor
  var ret_type: Type = null
  var params: List[Parameter] = Nil
  var var_arg: Boolean = false
  var blocks: List[BasicBlock] = Nil
  //ltype = FunctionType(ret_type, params.map(_.ltype), var_arg)

  // TODO user means somebody calls it
  // TODO iterate through flat list of instructions
  override def id = "@" + name
  def ir_form = "define " + full_name + "(" +
                params.map(_.full_name).mkString(", ") + ") {\n" +
                blocks.map(_.ir_form).mkString("\n") + "\n}\n"
}
