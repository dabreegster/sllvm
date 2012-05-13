package llvm

import java.io.{File, FileWriter}
import scala.sys.process._

object Util {
  // Syntactic sugar for deferring lazy computations
  type Later[T] = () => T
  implicit def now2later[T](n: T): Later[T] = () => n
  implicit def later2now[T](l: Later[T]): T = l()
  def later[T](thunk: => T): () => T = () => thunk

  // Pretty debugging
  // TODO implicitly take ir_form of the offending instruction?
  def assert_eq(a: Any, b: Any) = assert(a == b, a + " != " + b)
  private var indent_log = 0
  def log_push = { indent_log += 1 }
  def log_pop =  { indent_log -= 1 }
  def indent = "  " * indent_log
  def log(msg: String) = println(indent + msg)
  def log_indent(thunk: => Unit): Unit = {
    log_push
    thunk
    log_pop
  }

  // Interactive graphvizualization
  def graphviz(contents: String) = "digraph sllvm {\n" + contents + "}"
  def show_gv(contents: String) = {
    val out = new FileWriter("/tmp/sllvm.gv")
    out.write(graphviz(contents) + "\n")
    out.close
    val png_fn = "/tmp/sllvm.png"
    "dot -Tpng /tmp/sllvm.gv" #> new File(png_fn) !;
    "geeqie " + png_fn !
  }
}
