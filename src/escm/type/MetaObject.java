// Author: Jordan Randleman - escm.type.MetaObject
// Purpose:
//    MetaObject abstract class, super of classes, interfaces, & objects.
//
// Includes:
//    - MetaObject getSuper()
//    - ArrayList<EscmInterface> getEscmInterfaces()
//
//    - Datum get(java.lang.String name)
//    - void  set(java.lang.String name, Datum newValue)
//    - void  define(java.lang.String name, Datum newValue)
//
//    - boolean has(java.lang.String name)
//
//    - ArrayList<String> props() // returns list of prop names
//
//    - static BindingsMap loadPropsWithState(BindingsMap props, ExecutionState state)
//    - static BindingsMap copyProps(BindingsMap props)
//      * BindingsMap ::= ConcurrentHashMap<java.lang.String,Datum>


package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.vm.type.ExecutionState;

public abstract class MetaObject extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Super Object Abstraction
  public abstract MetaObject getSuper();


  ////////////////////////////////////////////////////////////////////////////
  // Interfaces Abstraction
  public abstract ArrayList<EscmInterface> getEscmInterfaces();


  ////////////////////////////////////////////////////////////////////////////
  // Property Fields: { propName: propValue, ... }
  protected ConcurrentHashMap<java.lang.String,Datum> props = new ConcurrentHashMap<java.lang.String,Datum>();


  ////////////////////////////////////////////////////////////////////////////
  // Property Managing Helper Operations
  private Datum get_recur(java.lang.String name) {
    Datum val = props.get(name);
    if(val != null) return val;
    MetaObject superObj = getSuper();
    if(superObj == null) return null;
    return superObj.get_recur(name);
  }


  private boolean set_recur(java.lang.String name, Datum newValue) {
    if(props.containsKey(name)) {
      props.put(name,newValue);
      return true;
    }
    MetaObject superObj = getSuper();
    if(superObj == null) return false;
    return superObj.set_recur(name,newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Managing Operations
  // NOTE: Binds <self> to methods.
  public Datum get(java.lang.String name) throws Exception {
    Datum val = props.get(name);
    if(val != null) {
      if(val instanceof CompoundProcedure) {
        return ((CompoundProcedure)val).loadWithSelf(this);
      }
      return val;
    }
    MetaObject superObj = getSuper();
    if(superObj == null) 
      throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s", name, write());
    val = superObj.get_recur(name);
    if(val == null)
      throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s", name, write());
    if(val instanceof CompoundProcedure) {
      return ((CompoundProcedure)val).loadWithSelf(this);
    }
    return val;
  }


  public void set(java.lang.String name, Datum newValue) throws Exception {
    if(set_recur(name,newValue) == false)
      throw new Exceptionf("'MetaObject [SET! TO %s] \"%s\" isn't a property of object %s", newValue.write(), name, write());
  }


  public void define(java.lang.String name, Datum newValue) {
    props.put(name,newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Querying Operations
  public boolean has(java.lang.String name) {
    if(props.containsKey(name)) return true;
    MetaObject superObj = getSuper();
    if(superObj == null) return false;
    return superObj.has(name);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Serialization Operations
  public ArrayList<java.lang.String> props() {
    ArrayList<java.lang.String> propsList = new ArrayList<java.lang.String>();
    for(java.lang.String key : props.keySet())
      propsList.add(key);
    return propsList;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <super>-binding helper methods
  protected void bindImmediateMethodsWithSuper() throws Exception {
    for(java.lang.String s : props.keySet()) {
      Datum val = props.get(s);
      if(val instanceof CompoundProcedure) {
        props.put(s,((CompoundProcedure)val).loadWithSuper(getSuper()));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract java.lang.String type();


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public abstract boolean equals(Object o);


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),props);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract java.lang.String display();

  public abstract java.lang.String write();

  public abstract java.lang.String pprint();


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public abstract MetaObject loadWithState(ExecutionState state) throws Exception;


  // Support function create new <props> loaded in the current state
  public static ConcurrentHashMap<java.lang.String,Datum> loadPropsWithState(ConcurrentHashMap<java.lang.String,Datum> props, ExecutionState state) throws Exception {
    ConcurrentHashMap<java.lang.String,Datum> loaded = new ConcurrentHashMap<java.lang.String,Datum>(props.size());
    for(ConcurrentHashMap.Entry<java.lang.String,Datum> e : props.entrySet())
      loaded.put(e.getKey(),e.getValue().loadWithState(state));
    return loaded;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public abstract MetaObject loadWithName(java.lang.String name) throws Exception;


  // [ONLY FOR USE UPON INITIAL CONSTRUCTION] Support function to bind names to methods.
  //   => NOTE: MUTATES THE GIVEN <props> ARGUMENT !!!
  public static void bindMethodsWithName(java.lang.String namePrefix, ConcurrentHashMap<java.lang.String,Datum> props) {
    for(ConcurrentHashMap.Entry<java.lang.String,Datum> e : props.entrySet()) {
      Datum propValue = e.getValue();
      if(propValue instanceof CompoundProcedure) {
        java.lang.String propName = e.getKey();
        props.put(propName,((CompoundProcedure)propValue).loadWithForcedName(namePrefix+propName));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public abstract MetaObject copy();


  // Support function to generate a copy <props>
  public static ConcurrentHashMap<java.lang.String,Datum> copyProps(ConcurrentHashMap<java.lang.String,Datum> props) {
    ConcurrentHashMap<java.lang.String,Datum> copied = new ConcurrentHashMap<java.lang.String,Datum>(props.size());
    for(ConcurrentHashMap.Entry<java.lang.String,Datum> e : props.entrySet())
      copied.put(e.getKey(),e.getValue().copy());
    return copied;
  }
}