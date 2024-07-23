// Author: Jordan Randleman - escm.type.Datum
// Purpose:
//    Abstract Base Class for Scheme data -- a contract all Scheme data 
//    types must implement to be used by the core interpreter. This bakes in 
//    extensibility for our interpreter, adding in a new primitive type only
//    requires the extension of this contract!

package escm.type;
import escm.vm.util.ExecutionState;
import escm.vm.type.callable.DocString;

public abstract class Datum implements DocString {
  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract java.lang.String type();


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public abstract boolean isTruthy();


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);
  public abstract boolean equal(Object o);

  public final boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public abstract java.lang.String docstring();


  ////////////////////////////////////////////////////////////////////////////
  // HashCode => MUST BE OVERRIDEN FOR VALUE TYPES TO HASH PROPERLY!
  public int hashCode() {
    return super.hashCode();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract java.lang.String display();
  public abstract java.lang.String write();
  public abstract java.lang.String pprint();

  // Define <toString> as an alias of <write>
  public final java.lang.String toString() {
    return write();
  }

  // Profiler to help print datum details in error messages
  public final java.lang.String profile() {
    return java.lang.String.format("%s of type \"%s\"", write(), type());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  // => WARNING: THIS WILL INFINITELY RECURSE ON CYCLIC VECTORS/HASHMAPS!
  public abstract Datum quote(ExecutionState state);


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public abstract Datum loadWithState(ExecutionState state) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public abstract Datum loadWithName(java.lang.String name);


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public abstract Datum shallowCopy();
}