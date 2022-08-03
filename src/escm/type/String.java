// Author: Jordan Randleman - escm.type.String
// Purpose:
//    String primitive type.

package escm.type;
import java.util.Objects;
import escm.util.StringParser;
import escm.vm.type.ExecutionState;

public class String extends Datum {
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
  public String(java.lang.String s) {
    value = s;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "string";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof String && ((String)o).value.equals(value);
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
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return value;
  }

  public java.lang.String write() {
    return '"' + StringParser.escape(value) + '"';
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public String loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public String loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying semantics
  public Datum copy() {
    return this;
  }
}