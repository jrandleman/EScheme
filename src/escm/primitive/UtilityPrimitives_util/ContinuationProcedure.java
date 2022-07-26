// Author: Jordan Randleman - escm.primtiive.UtilityPrimitives_util.ContinuationProcedure
// Purpose:
//    Java continuation procedure specialization of "escm.type.Procedure".
//    Wraps a "escm.util.Trampoline.Continuation" object under the hood.

package escm.primitive.UtilityPrimitives_util;
import java.util.ArrayList;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Procedure;
import escm.vm.type.ExecutionState;

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
  public ContinuationProcedure loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.type.Environment.java)
  private ContinuationProcedure(String name, Trampoline.Continuation continuation) {
    this.name = name;
    this.continuation = continuation;
  }


  public ContinuationProcedure loadWithName(String name) throws Exception {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new ContinuationProcedure(name,continuation);
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
        throw new Exceptionf("escm.primitive.UtilityPrimitives_util.ContinuationProcedure %s wasn't given exactly 1 argument!", name);
      return continuation.run(arguments.get(0));
    };
  }
}