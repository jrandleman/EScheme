// Author: Jordan Randleman - escm.type.oo.EscmModule
// Purpose:
//    Escm Module class, wrapping an Environment Java object internally.

package escm.type.oo;
import escm.type.Datum;
import escm.type.Symbol;
import escm.vm.util.Environment;

public class EscmModule extends Dottable {
  ////////////////////////////////////////////////////////////////////////////
  // Private Fields
  private java.lang.String name = null;
  private java.lang.String absoluteFilePath = null;
  private Environment bindings = null;


  ////////////////////////////////////////////////////////////////////////////
  // Public Accessors
  public java.lang.String name() {
    return name;
  }

  public java.lang.String absoluteFilePath() {
    return absoluteFilePath;
  }

  public Datum bindingsAsAssocList() {
    return bindings.bindingsAsAssocList();
  }

  public Datum bindingsAsList() {
    return bindings.bindingsAsList();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public EscmModule(java.lang.String name, java.lang.String absoluteFilePath, Environment bindings) {
    this.name = name;
    this.absoluteFilePath = absoluteFilePath;
    this.bindings = bindings;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get Operations
  public Datum get(String name) throws Exception {
    return bindings.get(new Symbol(name));
  }

  public Datum get(Symbol name) throws Exception {
    return bindings.get(name);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set! Operations
  public void set(String name, Datum newValue) throws Exception {
    bindings.set(new Symbol(name),newValue);
  }

  public void set(Symbol name, Datum newValue) throws Exception {
    bindings.set(name,newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define Operations
  public void define(String name, Datum newValue) throws Exception {
    bindings.define(new Symbol(name),newValue);
  }

  public void define(Symbol name, Datum newValue) throws Exception {
    bindings.define(name,newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Querying Operations
  public boolean has(String name) {
    return bindings.has(new Symbol(name));
  }


  public boolean has(Symbol name) {
    return bindings.has(name);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "module";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof EscmModule && ((EscmModule)o).bindings == this.bindings;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    return "Module named \""+name+"\" imported from \""+absoluteFilePath+"\"";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    return "#<module " + name  + " (" + absoluteFilePath + ")>";
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public EscmModule loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmModule copy() {
    return this;
  }
}