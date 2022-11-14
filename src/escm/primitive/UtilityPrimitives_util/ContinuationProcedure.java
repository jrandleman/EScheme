// Author: Jordan Randleman - escm.primitive.UtilityPrimitives_util.ContinuationProcedure
// Purpose:
//    Java continuation procedure specialization of "escm.type.procedure.Procedure".
//    Wraps a "escm.util.Trampoline.Continuation" object under the hood.

package escm.primitive.UtilityPrimitives_util;
import java.util.ArrayList;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.procedure.Procedure;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;

public class ContinuationProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal continuation field
  private Trampoline.Continuation continuation;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public ContinuationProcedure(Trampoline.Continuation continuation) {
    this.continuation = continuation;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public ContinuationProcedure loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  private ContinuationProcedure(String name, SourceInformation invocationSource, Trampoline.Continuation continuation) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.continuation = continuation;
  }


  public ContinuationProcedure loadWithName(String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new ContinuationProcedure(name,invocationSource,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public ContinuationProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new ContinuationProcedure(name,invocationSource,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof ContinuationProcedure && ((ContinuationProcedure)o).continuation == continuation;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation ignored) throws Exception {
    return () -> {
      if(arguments.size() != 1)
        throw new Exceptionf("ContinuationProcedure %s wasn't given exactly 1 argument!", name);
      return continuation.run(arguments.get(0));
    };
  }
}