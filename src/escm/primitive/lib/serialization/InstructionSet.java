// Author: Jordan Randleman - escm.primitive.lib.serialization.InstructionSet
// Purpose:
//    Special Datum used internally to store an instruction set during the 
//    CPS execution of <serialize>.

package escm.primitive.lib.serialization;
import java.util.Objects;
import java.util.ArrayList;
import escm.type.Datum;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;

public class InstructionSet extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Value
  ArrayList<Instruction> value = new ArrayList<Instruction>();

  public ArrayList<Instruction> value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public InstructionSet() {}
  
  public InstructionSet(ArrayList<Instruction> value) {
    this.value = value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "instruction-set";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof InstructionSet && ((InstructionSet)o).value.equals(value);
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    return "Instruction set "+value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    return String.format("#<instruction-set %s>", value);
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public InstructionSet quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter (effectively "get")
  public InstructionSet loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public InstructionSet loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public InstructionSet shallowCopy() {
    return this;
  }
}