// Author: Jordan Randleman - escm.type.port.Eof
// Purpose:
//    Single value to represent EOF.

package escm.type.port;
import java.util.Objects;
import escm.type.Datum;
import escm.vm.type.ExecutionState;

public class Eof extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static Value
  public static final Eof VALUE = new Eof();


  ////////////////////////////////////////////////////////////////////////////
  // Private Constructor (use <VALUE>)
  private Eof(){}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "eof";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Eof;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    return "#eof";
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Eof loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Eof loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Eof copy() {
    return this;
  }
}