// Author: Jordan Randleman - escm.type.Datum
// Purpose:
//    Abstract Base Class for Scheme data -- a contract all Scheme data 
//    types must implement to be used by the core interpreter. This bakes in 
//    extensibility for our interpreter, adding in a new primitive type only
//    requires the extension of this contract!

package escm.type;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;

public abstract class Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract java.lang.String type();


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public abstract boolean isTruthy();


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);
  public abstract boolean equals(Object o);


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public abstract int hashCode();


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract java.lang.String display();
  public abstract java.lang.String write();
  public abstract java.lang.String pprint();

  // Define <toString> as an alias of <write>
  public java.lang.String toString() {
    return write();
  }

  // Profiler to help print datum details in error messages
  public java.lang.String profile() {
    return java.lang.String.format("%s of type \"%s\"", write(), type());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public abstract Datum loadWithState(ExecutionState state) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public abstract Datum loadWithName(java.lang.String name) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public abstract Datum copy();
}