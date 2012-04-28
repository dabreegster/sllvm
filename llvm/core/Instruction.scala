package llvm.core

// TODO case classes?
// TODO it's annoying to make everything take all these constructor params...
// but probably not a huge deal.
// TODO apply and unapply could almost parse/unparse...

abstract class Instruction() extends User() {
  // TODO constructor
  var parent: BasicBlock = null

  def function = parent.parent
}

abstract class TerminatorInst() extends Instruction() {
  // TODO constructor
  var succs: List[BasicBlock] = Nil
  name = None
}

class ReturnInst() extends TerminatorInst() {
  // TODO constructor
  var ret_val: Value = null
  // TODO type should involve the ret_val type?

  def ir_form = "ret " + ret_val.full_name
}

class VoidReturnInst() extends TerminatorInst() {
  // TODO related to above how?
  ltype = VoidType()

  def ir_form = "ret void"
}

class UnconditionalBranchInst() extends TerminatorInst() {
  // TODO constructor
  var target: BasicBlock = null

  def ir_form = "br label " + target.id
}

class BranchInst() extends TerminatorInst() {
  // TODO constructor
  var test_val: Value = null
  var true_target: BasicBlock = null
  var false_target: BasicBlock = null

  def ir_form = "br %s, label %s, label %s".format(
    test_val.full_name, true_target.id, false_target.id
  )
}
class AllocaInst() extends Instruction() {
  // ltype is a pointer to whatever was allocated

  def alloced_type = ltype.deref
  def ir_form = "alloca " + alloced_type   // TODO alignment
}

class StoreInst() extends Instruction() {
  // TODO constructor
  var src: Value = null
  var dst: Value = null
  // TODO make sure types matches up
  ltype = VoidType()  // TODO not sure what type this has

  def ir_form = "store %s, %s".format(src.full_name, dst.full_name)  // TODO alignment
}

class LoadInst() extends Instruction() {
  // TODO constructor
  var src: Value = null

  def ir_form = "load " + src.full_name // TODO alignment
}

class IcmpInst() extends Instruction() {
  // TODO constructor
  var op: String = null // TODO enum
  var val1: Value = null
  var val2: Value = null
  ltype = IntegerType(1)

  def val_type = val1.ltype
  def ir_form = "icmp %s %s %s, %s".format(op, val_type, val1.id, val2.id)
}

class PHIInst() extends Instruction() {
  // TODO constructor
  var cases: List[(Value, BasicBlock)] = Nil

  def ir_form = "phi " + ltype + cases.map(
    c => "[ %s, %s ]".format(c._1.id, c._2.id)
  ).mkString(", ")
}

class CallInst() extends Instruction() {
  // TODO constructor
  var fxn: Function = null
  var args: List[Value] = Nil // TODO Parameter class?

  def ir_form = "call " + fxn.full_name + "(" + args.map(_.full_name).mkString(", ") + ")"
}

class BitcastInst() extends Instruction() {
  // TODO constructor
  var value: Value = null
  // ltype is target_type

  def ir_form = "bitcast " + value.full_name + " to " + ltype
}

class AddInst() extends Instruction() {
  // TODO constructor
  var v1: Value = null
  var v2: Value = null

  // TODO record the spec too
  def ir_form = "add " + ltype + " " + v1 + ", " + v2
}

class GEPInst() extends Instruction() {
  // TODO constructor
  var fields: List[Value] = Nil
  // TODO this is so wrong. first is a pointer, rest are ints?

  def ir_form = "getelementptr " + fields.map(_.toString).mkString(",")
}
