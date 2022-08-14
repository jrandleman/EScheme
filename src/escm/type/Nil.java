// Author: Jordan Randleman - escm.type.Nil
// Purpose:
//    Nil primitive type, also known as the "empty list".

package escm.type;
import java.util.Objects;
import escm.vm.util.ExecutionState;

public class Nil extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static Value
  public static final Nil VALUE = new Nil();


  ////////////////////////////////////////////////////////////////////////////
  // Private Constructor (use <VALUE>)
  private Nil(){}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "nil";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Nil;
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
  public java.lang.String display() {
    return "()";
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Nil loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Nil loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Nil copy() {
    return this;
  }
}