// Author: Jordan Randleman - escm.type.bool.Boolean
// Purpose:
//    Abstract boolean primitive type.

package escm.type.bool;
import java.util.Objects;
import escm.type.Datum;
import escm.vm.util.ExecutionState;

public abstract class Boolean extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static T/F Constants
  public static final TrueBoolean TRUE = new TrueBoolean();
  
  public static final FalseBoolean FALSE = new FalseBoolean();


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
    return this instanceof TrueBoolean;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(isTruthy()) return o instanceof TrueBoolean;
    return o instanceof FalseBoolean;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),isTruthy());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(isTruthy()) return "#t";
    return "#f";
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Boolean loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Boolean loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Boolean copy() {
    return this;
  }
}