// Author: Jordan Randleman - escm.type.procedure.Procedure
// Purpose:
//    Abstract Base Class for EScheme procedures -- the contract all EScheme procedure
//    types must implement to be used applied by the core interpreter. Note that
//    EScheme programmers will only ever see one "procedure" type in the runtime, and
//    that this base class primarily serves to support multiple internal procedure types,
//    the differences between which are abstracted away from the EScheme programmer.
//
//    For example, Java primitive procedures (defined by the interpreter's implementation) 
//    and EScheme compound procedures (defined by EScheme programmers) must be applied 
//    differently under the hood, but to the EScheme programmer, they are indistinguishable.

package escm.type.procedure;
import java.util.ArrayList;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.vm.type.callable.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;

public abstract class Procedure extends Datum implements Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Static Default Function Name (for anonymous lambdas)
  public static final java.lang.String DEFAULT_NAME = "";


  ////////////////////////////////////////////////////////////////////////////
  // Procedure Name
  protected java.lang.String name = DEFAULT_NAME;

  public java.lang.String name() {
    return name;
  }

  public java.lang.String readableName() {
    if(name.equals(DEFAULT_NAME)) return "lambda:"+hashCode();
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation Source (to be printed by escm.vm.runtime.EscmCallStack)
  protected SourceInformation invocationSource = null;


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public abstract String docstring();
  public abstract Datum signature(); // <#f> on failure
  public abstract Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public abstract Procedure quote(ExecutionState state);


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter Abstraction
  public abstract Procedure loadWithState(ExecutionState state);


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  public abstract Procedure loadWithName(java.lang.String name);


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public abstract Procedure loadWithInvocationSource(SourceInformation invocationSource);


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public abstract int hashCode();


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "procedure";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<procedure " + readableName() + '>';
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Procedure shallowCopy() {
    return this;
  }
}