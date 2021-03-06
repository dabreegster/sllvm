package llvm.core

import llvm.Util._

// TODO case classes? or these extractors?
// TODO apply and unapply could almost parse/unparse...
  // *** extractors.

// note: the junk String that some instructions take is for extra alignment info
// we don't care to encode that in sllvm, but when we re-print the IR, it's nice
// to make the diffs smaller

abstract class Instruction(name: Option[String], ltype: Type) extends User(name, ltype)
{
  // TODO constructor
  var parent: BasicBlock = null

  def set_parent(bb: BasicBlock) = {
    parent = bb
  }

  protected def propagate_parent(bb: BasicBlock, values: Traversable[Value]) = {
    values.foreach(v => v match {
      case i: Instruction => i.set_parent(bb)
      case _ =>
    })
    parent = bb
  }

  def function = parent.parent
  def module = function.parent
  def gv_form = toString
  override def uniq_name = function.name.get + "/" + name.get
}

abstract class TerminatorInst(ltype: Type, name: Option[String],
                              var succs: Set[BasicBlock])
                extends Instruction(name, ltype)
{
  // successor list is 'var' due to CallInst
  // TODO perhaps making TerminatorInst be a trait is cleaner?
  // then normal/terminator versions of call could be possible
}

class ReturnInst(val ret_val: Value) extends TerminatorInst(ret_val.ltype, None, Set())
{
  override def set_parent(bb: BasicBlock) = propagate_parent(bb, List(ret_val))

  def ir_form = "ret " + ret_val.full_name
}
object ReturnInst {
  def unapply(i: ReturnInst) = Some(i.ret_val)
}

class VoidReturnInst() extends TerminatorInst(VoidType(), None, Set()) {
  // TODO related to above how?

  def ir_form = "ret void"
}

class UnconditionalBranchInst(val target: BasicBlock)
      extends TerminatorInst(VoidType(), None, Set(target))
{
  def ir_form = "br label " + target.id
}
object UnconditionalBranchInst {
  def unapply(i: UnconditionalBranchInst) = Some(i.target)
}

class BranchInst(val test_val: Value, val true_target: BasicBlock,
                 val false_target: BasicBlock)
      extends TerminatorInst(VoidType(), None, Set(true_target, false_target))
{
  def ir_form = "br %s, label %s, label %s".format(
    test_val.full_name, true_target.id, false_target.id
  )
}

class UnreachableInst() extends TerminatorInst(VoidType(), None, Set())
{
  def ir_form = "unreachable"
}

class SwitchInst(val test_val: Value, val default_target: BasicBlock,
                 val targets: List[(Value, BasicBlock)])
  extends TerminatorInst(VoidType(), None, (default_target :: targets.map(_._2)).toSet)
{
  def ir_form = "switch " + test_val.full_name + ", label %" +
      default_target.name.get + "[\n" +
      targets.map(t => t._1.full_name + ", label %" + t._2.name.get).mkString("\n") + "\n]"
}

class AllocaInst(name: Option[String], ltype: Type, junk: String = "")
      extends Instruction(name, ltype)
{
  // ltype is a pointer to whatever was allocated

  def alloced_type = ltype.deref
  def ir_form = "alloca " + alloced_type + junk
  override def gv_form = name.get + " = alloca " + alloced_type
}

class StoreInst(name: Option[String], val src: Value,
                val dst: Value, junk: String = "")
      extends Instruction(name, VoidType())
{
  assert_eq(src.ltype.ptr_to, dst.ltype)
  src.add_use(this)
  override def set_parent(bb: BasicBlock) = propagate_parent(bb, List(src, dst))

  def ir_form = "store %s, %s".format(src.full_name, dst.full_name) + junk
  override def gv_form = "store %s, %s".format(src.full_name, dst.full_name)
}
object StoreInst {
  def unapply(i: StoreInst) = Some(i.src, i.dst)
}

class LoadInst(name: Option[String], val src: Value, junk: String = "")
      extends Instruction(name, src.ltype.deref)
{
  def ir_form = "load " + src.full_name + junk
  override def gv_form = name.get + " = load " + src.full_name
}
object LoadInst {
  def unapply(i: LoadInst) = Some(i.src)
}

class IcmpInst(name: Option[String], val op: String,
               val val1: Value, val val2: Value
              ) extends Instruction(name, IntegerType(1))
{
  assert_eq(val1.ltype, val2.ltype)

  def val_type = val1.ltype
  def ir_form = "icmp %s %s %s, %s".format(op, val_type, val1.id, val2.id)
}

class FcmpInst(name: Option[String], val op: String,
               val val1: Value, val val2: Value
              ) extends Instruction(name, IntegerType(1))
{
  assert_eq(val1.ltype, val2.ltype)

  def val_type = val1.ltype
  def ir_form = "fcmp %s %s %s, %s".format(op, val_type, val1.id, val2.id)
}

class PHIInst(name: Option[String], ltype: Type,
              val cases: List[(Value, BasicBlock)]
             ) extends Instruction(name, ltype)
{
  cases.foreach(c => assert_eq(c._1.ltype, ltype))

  def ir_form = "phi " + ltype + " " + cases.map(
    c => "[ %s, %s ]".format(c._1.id, c._2.id)
  ).mkString(", ")
}
object PHIInst {
  def unapply(i: PHIInst) = Some(i.cases)
}

class CallInst(name: Option[String], call: String, ltype: Type,
               val args: List[Value]
              ) extends TerminatorInst(ltype, name, Set())
{
  lazy val callee: Function = {
    val f = module.fxn_table(call)
    assert_eq(ltype, f.ret_type)
    f
  }

  override def set_parent(bb: BasicBlock) = propagate_parent(bb, args)

  def ir_form = "call " + callee.full_name + "(" +
                args.map(_.full_name).mkString(", ") + ")"
}
object CallInst {
  def unapply(i: CallInst) = Some(i.callee, i.args)
}

class IndirectCallInst(name: Option[String], val call: Value, ltype: Type,
                       val args: List[Value]
                      ) extends TerminatorInst(ltype, name, Set())
{
  // TODO how to share this better?
  override def set_parent(bb: BasicBlock) = {
    args.foreach(a => a match {
      case i: Instruction => i.set_parent(bb)
      case _ =>
    })
    super.set_parent(bb)
  }

  def ir_form = "call " + call.id + "(" +
                args.map(_.full_name).mkString(", ") + ")"
}
object IndirectCallInst {
  def unapply(i: IndirectCallInst) = Some(i.call, i.args)
}

class AsmCallInst(name: Option[String], call: (String, String), ltype: Type,
                  val args: List[Value]
                 ) extends TerminatorInst(ltype, name, Set())
{
  // TODO how to share this better?
  override def set_parent(bb: BasicBlock) = {
    args.foreach(a => a match {
      case i: Instruction => i.set_parent(bb)
      case _ =>
    })
    super.set_parent(bb)
  }

  def ir_form = "call \"" + call._1 + "\", \"" + call._2 + "\"(" +
                args.map(_.full_name).mkString(", ") + ")"
}

// The many instances are boilerplate and boring. Could make op an enum at
// least?
class MathInst(name: Option[String], val op: String,
               val v1: Value, val v2: Value
              ) extends Instruction(name, v1.ltype)
{
  assert_eq(v1.ltype, v2.ltype)

  // TODO record the spec too for add/sub
  def ir_form = op + " " + ltype + " " + v1.id + ", " + v2.id
}

// Likewise, many boring instances
class CastInst(name: Option[String], op: String, val target_type: Type,
               val value: Value)
    extends Instruction(name, target_type)
{
  def ir_form = op + " " + value.full_name + " to " + ltype
}
object CastInst {
  def unapply(i: CastInst) = Some(i.value, i.target_type)
}

class GEPInst(name: Option[String], val fields: List[Value]) extends Instruction(
  name, GEPInst.chase_indexed_type(fields)
) {
  // TODO all the fields?
  override def set_parent(bb: BasicBlock) = propagate_parent(bb, fields)

  // TODO first is a pointer, rest are ints?
  def base = fields.head
  def indices = fields.tail

  def ir_form = "getelementptr " + fields.map(_.toString).mkString(",")
}

object GEPInst {
  def chase_indexed_type(fields: List[Value]): Type = {
    val indices = fields.tail
    // treat the first index as a pointer dereference, always. usually this
    // index is '0', but when it's not, it may have something to do with a SIMD
    // vector being passed in, or a struct containing a pointer to something?
    val derefed = fields.head.ltype.deref

    // proceed with indices.tail
    var cur = derefed
    for (idx <- indices.tail) {
      cur = cur match {
        case ArrayType(base, sz) => base
        case s: StructType => s.fields(idx.asInstanceOf[ConstantInt].n)
      }
    }

    return cur.ptr_to
  }

  def unapply(i: GEPInst) = Some(i.base, i.indices)
}

class SelectInst(name: Option[String], select_val: Value, v1: Value, v2: Value)
    extends Instruction(name, v1.ltype)
{
  assert_eq(v1.ltype, v2.ltype)

  def ir_form = "select " + select_val.full_name + ", " + v1.full_name + ", " + v2.full_name
}
