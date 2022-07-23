// Author: Jordan Randleman - escm.type.Number
// Purpose:
//    Number primitive type.

package escm.type;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Number extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private double value = 0.0;


  ////////////////////////////////////////////////////////////////////////////
  // Value Getters
  public double doubleValue() {
    return value;
  }

  public int intValue() {
    return (int)value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public Number(double d) {
    value = d;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "number";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Number && ((Number)o).value == value;
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
    if(Math.round(value) == value) {
      return java.lang.String.format("%.0f", value);
    } else {
      return Double.toString(value);
    }
  }

  public java.lang.String write() {
    return display();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Number loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Number loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}