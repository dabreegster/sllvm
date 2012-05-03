package llvm

import scala.collection.mutable.ListBuffer
import llvm.core._

// A transformation pass to start a new BB after any function call
object SplitAtCall {
  def main(args: Array[String]) = {
    val module = Parser.parse(args(0))

    // TODO maybe add this manip at the Function level
    for (fxn <- module.functions) {
      fxn.setup_blocks(fxn.blocks.flatMap(split_bb(_)))
    }
    
    println(module.ir_form)
  }

  def split_bb(bb: BasicBlock): List[BasicBlock] = {
    // TODO er... at least generalize by a predicate
    def is_call_inst(i: Instruction) = i match {
      case _: CallInst => true
      case _ => false
    }

    // split the instruction list into 2 by the first Call instruction
    return bb.inst_ls.span(i => !is_call_inst(i)) match {
      // even if header or rest is Nil, that's fine
      // we want to make every CallInst wind up as the term_inst
      case (header, (call: CallInst) :: rest) => {
        // split this BB into two
        // TODO make sure we stitch up ALL the references?
        val bb1 = new BasicBlock(bb.name)
        bb1.preds = bb.preds
        bb1.inst_ls = header
        bb1.term_inst = call
        bb1.parent = bb.parent

        val bb2 = new BasicBlock(Some(bb.name.get + "'"))
        bb2.preds = List(bb1)
        bb2.inst_ls = rest
        bb2.term_inst = bb.term_inst
        bb2.parent = bb.parent

        // TODO how to do more cleanly?
        call.succs = List(bb2)

        bb1 :: split_bb(bb2)
      }

      // No call instructions, do nothing
      case (_, Nil) => List(bb)
    }
  }
}
