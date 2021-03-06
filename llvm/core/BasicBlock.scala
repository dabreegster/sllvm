package llvm.core

class BasicBlock(name: Option[String]) extends Value(name, LabelType()) {
  // All of these things may change, so...
  var preds: List[BasicBlock] = Nil
  var inst_ls: List[Instruction] = Nil
  var term_inst: TerminatorInst = null
  var parent: Function = null

  // builder pattern suffices
  def setup(inst: List[Instruction], term: TerminatorInst,
            pred_ls: List[BasicBlock]): BasicBlock =
  {
    inst_ls = inst
    term_inst = term
    preds = pred_ls
    instructions.foreach(i => i.set_parent(this))
    return this
  }

  // the predicate must return true at instances of TerminatorInst
  // caller's responsability to stitch things back up
  // split the instruction list into 2 by the first thing satisfying predicate
  def split(by: (Instruction) => Boolean): List[BasicBlock] = inst_ls.span(
    i => !by(i)
  ) match {
    // even if header or rest is Nil, that's fine
    case (header, (end: TerminatorInst) :: rest) => {
      // split this BB into two
      // TODO make sure we stitch up ALL the references?
      val bb1 = new BasicBlock(name).setup(header, end, preds)
      // stitch up the succs of our preds
      for (pred <- preds) {
        pred.term_inst.succs -= this
        pred.term_inst.succs += bb1
      }

      val bb2 = new BasicBlock(Some(name.get + "'")).setup(
        rest, term_inst, List(bb1)
      )
      val new_bb2 = bb2.split(by)
      end.succs = Set(new_bb2.head)
      bb1 :: new_bb2
    }
    
    // No matching instructions, do nothing
    case (_, Nil) => List(this)
  }

  def instructions = inst_ls ++ List(term_inst) // TODO more efficiently
  def label = name.get
  def succs = term_inst.succs
  def is_entry = preds.isEmpty  // TODO should only happen when name == "0"

  def ir_form = ir_header + instructions.map("  " + _.toString).mkString("\n")
  def ir_header = if (is_entry)
                    ""
                  else
                    "\n%-50s%s\n".format("; <label>:" + name.get, "; preds = " +
                    preds.map(_.id).mkString(", "))
  override def toString = "BB " + parent.name.get + "/" + name.get
  def gv_name = "\"bb_" + parent.name.get + "/" + name.get + "\""
  def outline_graphviz = "    %s [label=\"%s\"];\n%s\n".format(
    gv_name, this,
    succs.map(s => "    %s -> %s;".format(gv_name, s.gv_name)).mkString("\n")
  )
  def gv_form = this + "\\n" + instructions.map(_.gv_form).mkString("\\l") + "\\l"
  def detailed_graphviz = "    %s [shape=box, label=\"%s\"];\n%s\n".format(
    gv_name, gv_form,
    succs.map(s => "    %s -> %s;".format(gv_name, s.gv_name)).mkString("\n")
  )

}
