// Author: Jordan Randleman - escm.type.port.Port
// Purpose:
//    Abstract Base Class for Scheme input and output ports.
//
//    Guarentees:
//      - String sourceName()
//
//      - void close()
//      - boolean isClosed()
//      - boolean isOpen()

package escm.type.port;
import escm.type.Datum;
import escm.vm.type.ExecutionState;

public abstract class Port extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract String type();


  ////////////////////////////////////////////////////////////////////////////
  // Name
  public abstract String sourceName();


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public abstract void close() throws Exception;

  public abstract boolean isClosed();

  public boolean isOpen() {
    return isClosed() == false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public abstract int hashCode();


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract String display();

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Datum loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Datum loadWithName(String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}