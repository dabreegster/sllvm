package llvm.core

import llvm.Util._

// TODO case classes?
// TODO apply and unapply could almost parse/unparse...
  // *** extractors.

abstract class Instruction(name: Option[String], ltype: Type) extends User(name, ltype)
{
  // TODO constructor
  var parent: BasicBlock = null

  def function = parent.parent
  def module = function.parent
}

abstract class TerminatorInst(ltype: Type, name: Option[String] = None
                             ) extends Instruction(name, ltype)
{
  // TODO constructor
  var succs: List[BasicBlock] = Nil
}

class ReturnInst(ret_val: Value, ret_type: Type) extends TerminatorInst(ret_val.ltype) {
  assert_eq(ltype, ret_type)
  // TODO type should involve the ret_val type, right?

  def ir_form = "ret " + ret_val.full_name
}

class VoidReturnInst() extends TerminatorInst(VoidType()) {
  // TODO related to above how?

  def ir_form = "ret void"
}

class UnconditionalBranchInst(val target: BasicBlock) extends TerminatorInst(VoidType())
{
  def ir_form = "br label " + target.id
}

class BranchInst(ltype: Type, val test_val: Value, val true_target: BasicBlock,
                 val false_target: BasicBlock) extends TerminatorInst(VoidType())
{
  assert_eq(ltype, test_val.ltype)

  def ir_form = "br %s, label %s, label %s".format(
    test_val.full_name, true_target.id, false_target.id
  )
}

class AllocaInst(name: Option[String], ltype: Type) extends Instruction(name, ltype)
{
  // ltype is a pointer to whatever was allocated

  def alloced_type = ltype.deref
  def ir_form = "alloca " + alloced_type   // TODO alignment
}

class StoreInst(name: Option[String], val src: Value, src_type: Type,
                val dst: Value, dst_type: Type) extends Instruction(name, VoidType())
{
  assert_eq(src.ltype, src_type)
  assert_eq(dst.ltype, dst_type)

  def ir_form = "store %s, %s".format(src.full_name, dst.full_name)  // TODO alignment
}

class LoadInst(name: Option[String], val src: Value, src_type: Type)
      extends Instruction(name, src_type.deref)
{
  assert_eq(src.ltype, src_type)

  def ir_form = "load " + src.full_name // TODO alignment
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

  def ir_form = "phi " + ltype + cases.map(
    c => "[ %s, %s ]".format(c._1.id, c._2.id)
  ).mkString(", ")
}

class CallInst(name: Option[String], call: String, ltype: Type,
               val args: List[Value]
              ) extends TerminatorInst(ltype, name)
{
  lazy val callee: Function = {
    val f = module.fxn_table(call)
    assert_eq(ltype, f.ret_type)
    f
  }

  def ir_form = "call " + callee.full_name + "(" +
                args.map(_.full_name).mkString(", ") + ")"
}

class BitcastInst(name: Option[String], target_type: Type,
                  value: Value, val_type: Type
                 ) extends Instruction(name, target_type)
{
  assert_eq(value.ltype, val_type)

  def ir_form = "bitcast " + value.full_name + " to " + ltype
}

class AddInst(name: Option[String], ltype: Type, val v1: Value,
              val v2: Value
             ) extends Instruction(name, ltype)
{
  assert_eq(ltype, v1.ltype)
  assert_eq(ltype, v2.ltype)

  // TODO record the spec too
  def ir_form = "add " + ltype + " " + v1 + ", " + v2
}

// TODO the first index is always bogus? always 0 to deref the ptr?
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

class SextInst(name: Option[String], orig_type: Type, val v: Value,
               new_type: Type
             ) extends Instruction(name, new_type)
{
  assert_eq(orig_type, v.ltype)

  def ir_form = "sext " + orig_type + " " + v + " to " + new_type
}
