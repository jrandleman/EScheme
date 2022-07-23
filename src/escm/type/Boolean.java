// Author: Jordan Randleman - escm.type.Boolean
// Purpose:
//    Boolean primitive type.

package escm.type;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Boolean extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static T/F Constants
  public static final Boolean TRUE = new Boolean();
  
  public static final Boolean FALSE = new Boolean();


  ////////////////////////////////////////////////////////////////////////////
  // Private Constructor (use <valueOf()>)
  private Boolean() {}


  ////////////////////////////////////////////////////////////////////////////
  // Factory Function
  public static Boolean valueOf(boolean b) {
    if(b) return TRUE;
    return FALSE;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "boolean";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return this == TRUE;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Boolean && ((Boolean)o) == this;
  }

  public boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),this == TRUE);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(this == TRUE) return "#t";
    return "#f";
  }

  public java.lang.String write() {
    return display();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Boolean loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Boolean loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}