// Author: Jordan Randleman - escm.type.Symbol
// Purpose:
//    Symbol primitive type, used extensively in metaprogramming.

package escm.type;
import java.util.Objects;
import java.util.ArrayList;
import escm.vm.type.ExecutionState;

public class Symbol extends Datum {
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
  public Symbol(java.lang.String s) {
    value = s;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "symbol";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Symbol && ((Symbol)o).value.equals(value);
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
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Datum loadWithState(ExecutionState state) throws Exception {
    return state.env.get(value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Symbol loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}