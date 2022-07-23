// Author: Jordan Randleman - escm.type.Nil
// Purpose:
//    Nil primitive type, also known as the "empty list".

package escm.type;
import java.util.Objects;
import escm.vm.type.ExecutionState;

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
    return "()";
  }

  public java.lang.String write() {
    return display();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Nil loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Nil loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}