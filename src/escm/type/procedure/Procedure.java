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
import java.util.Objects;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.vm.type.ExecutionState;
import escm.vm.type.Callable;

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


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public abstract Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter Abstraction
  public abstract Procedure loadWithState(ExecutionState state) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.type.Environment.java)
  public abstract Procedure loadWithName(java.lang.String name) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public boolean equals(Object o) {
    return eq(o);
  }


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
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),this);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(name.equals(DEFAULT_NAME)) return "#<procedure>";
    return "#<procedure " + name + '>';
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Procedure copy() {
    return this;
  }
}