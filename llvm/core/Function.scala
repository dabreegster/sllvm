package llvm.core

import scala.collection.mutable.{HashSet => MutableSet}

import llvm.Util._

class Function(name: Option[String], val ret_type: Type,
               val params: List[Parameter], val parent: Module,
               junk: String = ""
              )
  extends Value(name, FunctionType(ret_type, params.map(p => later { p.ltype }), false))
{
  var blocks: List[BasicBlock] = Nil

  // TODO
  params.foreach(p => p.parent = this)

  // builder pattern
  def setup_blocks(ls: List[BasicBlock]): Function = {
    blocks = ls
    blocks.foreach(b => b.parent = this)
    return this
  }

  var var_arg: Boolean = false

  def is_declaration = blocks.isEmpty

  // TODO user means somebody calls it
  // TODO iterate through flat list of instructions
  override def id = "@" + name.get
  override def full_name = ret_type + " " + id
  private def param_ir_form = "(" + params.map(_.full_name).mkString(", ") + ")"
  def ir_form = if (is_declaration)
                  "declare " + full_name + param_ir_form + "\n"
                else
                  "define " + full_name + param_ir_form + junk +
                  " {\n" + blocks.map(_.ir_form).mkString("\n") + "\n}\n"
  override def toString = "Function " + name.get
  def graphviz = if (is_declaration)
                   "  cluster_%s [label=\"Stub %s\"];\n".format(name.get, this)
                 else
                   "  subgraph cluster_%s {\n    label=\"%s\";\n%s  }\n".format(
                    name.get, this, blocks.map(bb => bb.outline_graphviz).mkString("")
                  )
  def instructions = blocks.flatMap(b => b.instructions)
  def is_root() = name.get == "main"
  // what do we call directly?
  def direct_calls: Set[Function] = instructions.flatMap(i => i match {
    case CallInst(callee, _) => Some(callee)
    case _ => None
  }).toSet

  // cache it once we've requested it
  lazy val rpo_order: List[BasicBlock] = {
    // TODO check correctness...
    val visited = new MutableSet[BasicBlock]()

    def dfs(node: BasicBlock): List[BasicBlock] = {
      visited += node
      return node.succs.toList.flatMap(s => if (visited(s)) Nil else dfs(s)) ++ List(node)
    }

    val order = dfs(blocks.head).reverse
    assert(visited.size == blocks.size)   // did we reach it all?
    order
  }
  // TODO lazily give dom frontiers
}
