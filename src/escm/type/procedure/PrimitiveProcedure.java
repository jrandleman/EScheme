// Author: Jordan Randleman - escm.type.procedure.PrimitiveProcedure
// Purpose:
//    Java primitive procedure specialization of "escm.type.procedure.Procedure".
//    Wraps a "escm.vm.type.Callable" object under the hood.

package escm.type.procedure;
import java.util.ArrayList;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;
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
  public PrimitiveProcedure loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  private PrimitiveProcedure(java.lang.String name, SourceInformation invocationSource, Callable prm) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.prm = prm;
  }


  public PrimitiveProcedure loadWithName(java.lang.String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new PrimitiveProcedure(name,invocationSource,prm);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public PrimitiveProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new PrimitiveProcedure(name,invocationSource,prm);
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
      EscmCallStack.Entry originalCallStack = EscmCallStack.currentCallStack();
      EscmCallStack.push(name,invocationSource);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        EscmCallStack.restore(originalCallStack);
        return continuation.run(value);
      };
      return prm.callWith(arguments,popContinuation);
    };
  }
}