package llvm.core

class BasicBlock() extends Value() {
  // TODO someday, all constructor params
  var preds: List[BasicBlock] = Nil
  var inst_ls: List[Instruction] = Nil
  var term_inst: TerminatorInst = null
  var parent: Function = null

  def instructions = inst_ls ++ List(term_inst) // TODO more efficiently
  def label = name
  def succs = term_inst.succs
  def is_entry = preds.isEmpty  // TODO should only happen when name == "0"

  def ir_form = ir_header + instructions.map("  " + _.toString).mkString("\n")
  def ir_header = if (is_entry)
                    ""
                  else
                    "; <label>:" + name + "\t\t\t; preds = " +
                    preds.map(_.id).mkString(", ") + "\n"
}
