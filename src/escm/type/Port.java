// Author: Jordan Randleman - escm.type.Port
// Purpose:
//    Abstract Base Class for Scheme input and output ports.
//
//    Provides:
//      - static STDIN  [System.in]
//      - static STDOUT [System.out]
//      - static STDERR [System.err]
//
//    Guarentees:
//      - String sourceName()
//
//      - void close()
//      - boolean isClosed()
//      - boolean isOpen()

package escm.type;
import escm.vm.type.ExecutionState;

public abstract class Port extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static Fields
  public static final InputPort STDIN = InputPort.STDIN;
  public static final OutputPort STDOUT = OutputPort.STDOUT;
  public static final OutputPort STDERR = OutputPort.STDERR;


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract java.lang.String type();


  ////////////////////////////////////////////////////////////////////////////
  // Name
  public abstract java.lang.String sourceName();


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
  public abstract java.lang.String display();

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Datum loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Datum loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}