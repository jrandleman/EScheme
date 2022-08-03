// Author: Jordan Randleman - escm.type.Void
// Purpose:
//    Void primitive type, the "value" returned by special forms like "define" and "set!".

package escm.type;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Void extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static Value
  public static final Void VALUE = new Void();


  ////////////////////////////////////////////////////////////////////////////
  // Private Constructor (use <VALUE>)
  private Void(){}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "void";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Void;
  }

  public boolean eqv(Object o) {
    return eq(o);
  }

  public boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "";
  }

  public java.lang.String write() {
    return "#void";
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Void loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Void loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}