// Author: Jordan Randleman - escm.vm.util.ExecutionState
// Purpose:
//    Wrapper class containing a state of the EScheme bytecode interpreter.

package escm.vm.util;
import java.util.ArrayList;
import java.util.ArrayDeque;
import escm.type.Datum;
import escm.type.Void;

public class ExecutionState {
  ////////////////////////////////////////////////////////////////////////////
  // State fields
  public Environment env;
  public ArrayList<Instruction> instructions;
  public ArrayDeque<Datum> stack;
  public Datum cvr; // current value register
  public int cii;   // current instruction index


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public ExecutionState(Environment env, ArrayList<Instruction> instructions) {
    this.env = env;
    this.instructions = instructions;
    this.stack = new ArrayDeque<Datum>();
    this.cvr = Void.VALUE;
    this.cii = 0;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Continuation State Copy
  private ExecutionState(Environment env, ArrayList<Instruction> instructions, ArrayDeque<Datum> stack, Datum cvr, int cii) {
    this.env = env;
    this.instructions = instructions;
    this.stack = stack;
    this.cvr = cvr;
    this.cii = cii;
  }

  public ExecutionState getContinuationState(Datum value) {
    return new ExecutionState(env,instructions,new ArrayDeque<Datum>(stack),value,cii);
  }
}