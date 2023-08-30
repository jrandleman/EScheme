// Author: Jordan Randleman - escm.type.Void
// Purpose:
//    Void primitive type, the "value" returned by special forms like "define" and "set!".

package escm.type;
import java.util.Objects;
import escm.vm.util.ExecutionState;

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

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public java.lang.String docstring() {
    return "The \"nothing\" type: #void";
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
  public Void loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Void loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Void copy() {
    return this;
  }
}