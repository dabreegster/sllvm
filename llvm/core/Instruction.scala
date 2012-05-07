package llvm.core

import llvm.Util._

// TODO case classes?
// TODO apply and unapply could almost parse/unparse...
  // *** extractors.

// note: the junk String that some instructions take is for extra alignment info
// we don't care to encode that in sllvm, but when we re-print the IR, it's nice
// to make the diffs smaller

abstract class Instruction(name: Option[String], ltype: Type) extends User(name, ltype)
{
  // TODO constructor
  var parent: BasicBlock = null

  def function = parent.parent
  def module = function.parent
  def gv_form = toString
}

abstract class TerminatorInst(ltype: Type, name: Option[String],
                              var succs: List[BasicBlock])
                extends Instruction(name, ltype)
{
  // successor list is 'var' due to CallInst
  // TODO perhaps making TerminatorInst be a trait is cleaner?
  // then normal/terminator versions of call could be possible
}

class ReturnInst(ret_val: Value, ret_type: Type)
      extends TerminatorInst(ret_val.ltype, None, Nil)
{
  assert_eq(ltype, ret_type)
  // TODO type should involve the ret_val type, right?

  def ir_form = "ret " + ret_val.full_name
}

class VoidReturnInst() extends TerminatorInst(VoidType(), None, Nil) {
  // TODO related to above how?

  def ir_form = "ret void"
}

class UnconditionalBranchInst(val target: BasicBlock)
      extends TerminatorInst(VoidType(), None, List(target))
{
  def ir_form = "br label " + target.id
}

class BranchInst(ltype: Type, val test_val: Value, val true_target: BasicBlock,
                 val false_target: BasicBlock)
      extends TerminatorInst(VoidType(), None, List(true_target, false_target))
{
  assert_eq(ltype, test_val.ltype)

  def ir_form = "br %s, label %s, label %s".format(
    test_val.full_name, true_target.id, false_target.id
  )
}

class AllocaInst(name: Option[String], ltype: Type, junk: String = "")
      extends Instruction(name, ltype)
{
  // ltype is a pointer to whatever was allocated

  def alloced_type = ltype.deref
  def ir_form = "alloca " + alloced_type + junk
  override def gv_form = name.get + " = alloca " + alloced_type
}

class StoreInst(name: Option[String], val src: Value, src_type: Type,
                val dst: Value, dst_type: Type, junk: String = "")
      extends Instruction(name, VoidType())
{
  assert_eq(src.ltype, src_type)
  assert_eq(dst.ltype, dst_type)

  def ir_form = "store %s, %s".format(src.full_name, dst.full_name) + junk
  override def gv_form = "store %s, %s".format(src.full_name, dst.full_name)
}

class LoadInst(name: Option[String], val src: Value, src_type: Type, junk: String = "")
      extends Instruction(name, src_type.deref)
{
  assert_eq(src.ltype, src_type)

  def ir_form = "load " + src.full_name + junk
  override def gv_form = name.get + " = load " + src.full_name
}

class IcmpInst(name: Option[String], val op: String, cmp_type: Type,
               val val1: Value, val val2: Value
              ) extends Instruction(name, IntegerType(1))
{
  assert_eq(cmp_type, val1.ltype)
  assert_eq(cmp_type, val2.ltype)

  def val_type = val1.ltype
  def ir_form = "icmp %s %s %s, %s".format(op, val_type, val1.id, val2.id)
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

class CallInst(name: Option[String], call: String, ltype: Type,
               val args: List[Value]
              ) extends TerminatorInst(ltype, name, Nil)
{
  lazy val callee: Function = {
    val f = module.fxn_table(call)
    assert_eq(ltype, f.ret_type)
    f
  }

  def ir_form = "call " + callee.full_name + "(" +
                args.map(_.full_name).mkString(", ") + ")"
}

// The many instances are boilerplate and boring. Could make op an enum at
// least?
class MathInst(name: Option[String], val op: String, ltype: Type,
               val v1: Value, val v2: Value
              ) extends Instruction(name, ltype)
{
  assert_eq(ltype, v1.ltype)
  assert_eq(ltype, v2.ltype)

  // TODO record the spec too for add/sub
  def ir_form = op + " " + ltype + " " + v1 + ", " + v2
}

// Likewise, many boring instances
class CastInst(name: Option[String], op: String, target_type: Type,
               value: Value, val_type: Type
              ) extends Instruction(name, target_type)
{
  assert_eq(value.ltype, val_type)

  def ir_form = op + " " + value.full_name + " to " + ltype
}

class GEPInst(name: Option[String], val fields: List[Value]) extends Instruction(
  name, GEPInst.chase_indexed_type(fields)
) {
  // TODO first is a pointer, rest are ints?

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
}
