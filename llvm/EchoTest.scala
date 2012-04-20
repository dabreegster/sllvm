package llvm

// Parse IR and then emit again.
object EchoTest {
  def main(args: Array[String]) = {
    val module = Parser.parse(args(0))
    for (fxn <- module.functions) {
      println(fxn.ir_form)
    }
  }
}
