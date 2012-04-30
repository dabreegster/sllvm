package llvm.core

// TODO case classes?
// TODO it's annoying to make everything take all these constructor params...
// but probably not a huge deal.
// TODO apply and unapply could almost parse/unparse...

abstract class Instruction(name: Option[String], ltype: Type) extends User(name, ltype)
{
  // TODO constructor
  var parent: BasicBlock = null

  def function = parent.parent
  def module = function.parent
}

abstract class TerminatorInst(ltype: Type) extends Instruction(None, ltype) {
  // TODO constructor
  var succs: List[BasicBlock] = Nil
}

class ReturnInst(ret_val: Value) extends TerminatorInst(ret_val.ltype) {
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
  assert(ltype == test_val.ltype)

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
  assert(src.ltype == src_type)
  assert(dst.ltype == dst_type)

  def ir_form = "store %s, %s".format(src.full_name, dst.full_name)  // TODO alignment
}

class LoadInst(name: Option[String], val src: Value, src_type: Type) extends Instruction(
  name, src_type.deref
) {
  assert(src.ltype == src_type)

  def ir_form = "load " + src.full_name // TODO alignment
}

class IcmpInst(name: Option[String], val op: String, cmp_type: Type,
               val val1: Value, val val2: Value
              ) extends Instruction(name, IntegerType(1))
{
  assert(cmp_type == val1.ltype)
  assert(cmp_type == val2.ltype)

  def val_type = val1.ltype
  def ir_form = "icmp %s %s %s, %s".format(op, val_type, val1.id, val2.id)
}

class PHIInst(name: Option[String], ltype: Type,
              val cases: List[(Value, BasicBlock)]
             ) extends Instruction(name, ltype)
{
  cases.foreach(c => assert(c._1.ltype == ltype)) // TODO assert_eq

  def ir_form = "phi " + ltype + cases.map(
    c => "[ %s, %s ]".format(c._1.id, c._2.id)
  ).mkString(", ")
}

class CallInst(name: Option[String], call: String, ltype: Type,
               val args: List[Value]
              ) extends Instruction(name, ltype)
{
  lazy val callee: Function = {
    val f = module.fxn_table(call)
    assert(ltype == f.ret_type)
    f
  }

  def ir_form = "call " + callee.full_name + "(" +
                args.map(_.full_name).mkString(", ") + ")"
}

class BitcastInst(name: Option[String], target_type: Type,
                  value: Value, val_type: Type
                 ) extends Instruction(name, target_type)
{
  assert(value.ltype == val_type)

  def ir_form = "bitcast " + value.full_name + " to " + ltype
}

class AddInst(name: Option[String], ltype: Type, val v1: Value,
              val v2: Value
             ) extends Instruction(name, ltype)
{
  assert(ltype == v1.ltype)
  assert(ltype == v2.ltype)

  // TODO record the spec too
  def ir_form = "add " + ltype + " " + v1 + ", " + v2
}

class GEPInst(name: Option[String], val fields: List[Value]) extends Instruction(
  name, fields.head.asInstanceOf[GlobalVariable].default_val.ltype.asInstanceOf[SequentialType].ptr_to_member
  // TODO ^ that's probably how we shouldnt do things.
) {
  // TODO first is a pointer, rest are ints?

  def ir_form = "getelementptr " + fields.map(_.toString).mkString(",")
}
