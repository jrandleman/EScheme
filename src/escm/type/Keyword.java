// Author: Jordan Randleman - escm.type.Keyword
// Purpose:
//    Keyword primitive type, effectively symbols that evaluate to themselves.

package escm.type;
import java.util.Objects;
import escm.vm.util.ExecutionState;

public class Keyword extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private java.lang.String value = "";


  ////////////////////////////////////////////////////////////////////////////
  // Value Getter
  public java.lang.String value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public Keyword(java.lang.String s) {
    value = ':' + s;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "keyword";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Keyword && ((Keyword)o).value.equals(value);
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public java.lang.String docstring() {
    return "Keyword value: "+value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return value;
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public Keyword quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Keyword loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Keyword loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Keyword shallowCopy() {
    return this;
  }
}