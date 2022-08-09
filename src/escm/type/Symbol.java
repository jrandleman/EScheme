// Author: Jordan Randleman - escm.type.Symbol
// Purpose:
//    Symbol primitive type, used extensively in metaprogramming.

package escm.type;
import java.util.Objects;
import java.util.ArrayList;
import escm.util.Exceptionf;
import escm.vm.type.ExecutionState;
import escm.vm.util.SourceInformation;

public class Symbol extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Source Information 
  //   * Used to print unknown variable locations by <escm.vm.type.Environment>
  private SourceInformation source = null;

  public boolean hasSourceInformation() {
    return source != null;
  }

  public SourceInformation source() throws Exception {
    if(source == null)
      throw new Exceptionf("Can't get fileName/line/column source information for Datum %s", profile());
    return source;
  }


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
  public Symbol(java.lang.String val) {
    value = val;
  }

  public Symbol(java.lang.String val, SourceInformation src) {
    value = val;
    source = src.clone();
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

  public boolean equal(Object o) {
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
    return state.env.get(this);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Symbol loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Symbol copy() {
    return this;
  }
}