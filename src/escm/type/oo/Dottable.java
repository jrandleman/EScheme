// Author: Jordan Randleman - escm.type.oo.Dottable
// Purpose:
//    Dottable abstract class, super of meta-objects and modules.
//    Denotes a namespace container.
//
// Includes:
//    - Datum get(String name)
//    - void set(String name, Datum newValue)
//    - void define(String name, Datum newValue)
//    - boolean has(String name)


package escm.type.oo;
import escm.type.Datum;
import escm.type.Symbol;
import escm.vm.util.ExecutionState;

public abstract class Dottable extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Get Operations
  public abstract Datum get(String name) throws Exception;


  public abstract Datum get(Symbol name) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Set! Operations
  public abstract void set(String name, Datum newValue) throws Exception;


  public abstract void set(Symbol name, Datum newValue) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Define Operations
  public abstract void define(String name, Datum newValue) throws Exception;


  public abstract void define(Symbol name, Datum newValue) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Property Querying Operations
  public abstract boolean has(String name);


  public abstract boolean has(Symbol name);


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract String type();


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public abstract boolean equal(Object o);


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public abstract String docstring();


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract String display();

  public abstract String write();

  public abstract String pprint();


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Dottable loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public abstract Dottable loadWithName(String name);


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public abstract Dottable copy();
}