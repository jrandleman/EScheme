// Author: Jordan Randleman - escm.vm.type.ExecutionState
// Purpose:
//    Wrapper class containing a state of the EScheme bytecode interpreter.

package escm.vm.type;
import java.util.ArrayList;
import java.util.Stack;
import escm.type.Datum;
import escm.type.Void;

public class ExecutionState {
  ////////////////////////////////////////////////////////////////////////////
  // State fields
  public Environment env;
  public ArrayList<Instruction> instructions;
  public Stack<Datum> stack;
  public Datum cvr; // current value register
  public int cii; // current instruction index


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public ExecutionState(Environment env, ArrayList<Instruction> instructions) {
    this.env = env;
    this.instructions = instructions;
    this.stack = new Stack<Datum>();
    this.cvr = Void.VALUE;
    this.cii = 0;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Continuation State Copy
  public ExecutionState getContinuationState(Datum value) {
    ExecutionState instanceState = new ExecutionState(env,instructions);
    instanceState.stack.addAll(stack);
    instanceState.cii = cii;
    instanceState.cvr = value;
    return instanceState;
  }
}