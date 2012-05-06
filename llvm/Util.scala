package llvm

object Util {
  // Syntactic sugar for deferring lazy computations
  type Later[T] = () => T
  implicit def now2later[T](n: T): Later[T] = () => n
  implicit def later2now[T](l: Later[T]): T = l()
  def later[T](thunk: => T): () => T = () => thunk

  // Pretty debugging
  // TODO implicitly take ir_form of the offending instruction?
  def assert_eq(a: Any, b: Any) = assert(a == b, a + " != " + b)
}
