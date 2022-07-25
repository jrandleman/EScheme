// Author: Jordan Randleman - escm.type.PrimitiveProcedure
// Purpose:
//    Java primitive procedure specialization of "escm.type.Procedure".
//    Wraps a "escm.vm.type.Callable" object under the hood.

package escm.type;
import java.util.ArrayList;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.ExecutionState;
import escm.vm.runtime.EscmCallStack;

public class PrimitiveProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal primitive procedure field
  private Callable prm;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public PrimitiveProcedure(java.lang.String name, Callable prm) {
    this.name = name;
    this.prm = prm;
  }

  public PrimitiveProcedure(java.lang.String name, Primitive prm) {
    this.name = name;
    this.prm = new Callable() {
      public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
        return continuation.run(prm.callWith(parameters));
      }
    };
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public PrimitiveProcedure loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.type.Environment.java)
  public PrimitiveProcedure loadWithName(java.lang.String name) throws Exception {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new PrimitiveProcedure(name,prm);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof PrimitiveProcedure && ((PrimitiveProcedure)o).prm == prm;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> {
      EscmCallStack.push(name);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        EscmCallStack.pop(name);
        return continuation.run(value);
      };
      return prm.callWith(arguments,popContinuation);
    };
  }
}