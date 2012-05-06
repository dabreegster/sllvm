package llvm

// Parse IR and then emit again.
object EchoTest {
  def main(args: Array[String]) = {
    val module = Parser.parse(args(0))
    print(module.ir_form)
  }
}
