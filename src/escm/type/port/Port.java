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
//
//      - boolean isTemporary()

package escm.type.port;
import escm.type.Datum;
import escm.vm.util.ExecutionState;

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
  // Temporary File Predicate
  public static final String TEMP_FILE_PREFIX = "escm-tmp-pfx-";

  public static final String TEMP_FILE_SUFFIX = "-escm-tmp-sfx";

  public boolean isTemporary() {
    String path = sourceName();
    return path.endsWith(TEMP_FILE_SUFFIX) && path.contains(TEMP_FILE_PREFIX);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public abstract String docstring();


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
  public Port loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Port loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Port shallowCopy() {
    return this;
  }
}