package llvm

import scala.collection.mutable.ListBuffer
import llvm.core._

// A transformation pass to start a new BB after any function call
object SplitAtCall {
  def main(args: Array[String]) = {
    val module = Parser.parse(args(0))

    // TODO better way to write?
    def is_call_inst(i: Instruction) = i match {
      case _: CallInst => true
      case _ => false
    }

    for (fxn <- module.functions) {
      fxn.setup_blocks(fxn.blocks.flatMap(_.split(is_call_inst)))
    }
    
    println(module.ir_form)
  }
}
