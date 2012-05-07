package llvm

// Visualizes control flow graph
object VizTest {
  def main(args: Array[String]) = {
    val module = Parser.parse(args(0))
    llvm.SplitAtCall.split(module)
    llvm.Util.show_gv(module.graphviz)
  }
}
