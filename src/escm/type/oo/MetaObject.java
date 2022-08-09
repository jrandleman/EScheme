// Author: Jordan Randleman - escm.type.oo.MetaObject
// Purpose:
//    MetaObject abstract class, super of classes, interfaces, & objects.
//
// Includes:
//    - MetaObject getSuper()
//    - ArrayList<EscmInterface> getEscmInterfaces()
//
//    - Datum get(String name)
//    - void  set(String name, Datum newValue)
//    - void  define(String name, Datum newValue)
//
//    - boolean has(String name)
//
//    - ArrayList<String> props() // returns list of prop names
//
//    - static BindingsMap loadPropsWithState(BindingsMap props, ExecutionState state)
//    - static BindingsMap copyProps(BindingsMap props)
//      * BindingsMap ::= ConcurrentHashMap<String,Datum>


package escm.type.oo;
import java.util.ArrayList;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.procedure.CompoundProcedure;
import escm.vm.type.ExecutionState;
import escm.vm.util.SourceInformation;

public abstract class MetaObject extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Super Object Abstraction
  public abstract MetaObject getSuper();


  ////////////////////////////////////////////////////////////////////////////
  // Interfaces Abstraction
  public abstract ArrayList<EscmInterface> getEscmInterfaces();


  ////////////////////////////////////////////////////////////////////////////
  // Property Fields: { propName: propValue, ... }
  protected ConcurrentHashMap<String,Datum> props = new ConcurrentHashMap<String,Datum>();


  ////////////////////////////////////////////////////////////////////////////
  // Property Managing Helper Operations
  private Datum get_recur(String name) {
    Datum val = props.get(name);
    if(val != null) return val;
    MetaObject superObj = getSuper();
    if(superObj == null) return null;
    return superObj.get_recur(name);
  }


  private boolean set_recur(String name, Datum newValue) {
    if(props.containsKey(name)) {
      props.put(name,newValue);
      return true;
    }
    MetaObject superObj = getSuper();
    if(superObj == null) return false;
    return superObj.set_recur(name,newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get Operations
  private Datum get(String name, SourceInformation source) throws Exception {
    Datum val = props.get(name);
    if(val != null) {
      if(val instanceof CompoundProcedure) {
        return ((CompoundProcedure)val).loadWithSelf(this);
      }
      return val;
    }
    MetaObject superObj = getSuper();
    if(superObj == null) {
      if(source == null) {
        throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s", name, write());
      } else {
        throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s\n>> Location: %s", name, write(), source);
      }
    }
    val = superObj.get_recur(name);
    if(val == null) {
      if(source == null) {
        throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s", name, write());
      } else {
        throw new Exceptionf("'MetaObject [GET] \"%s\" isn't a property of object %s\n>> Location: %s", name, write(), source);
      }
    }
    if(val instanceof CompoundProcedure) {
      return ((CompoundProcedure)val).loadWithSelf(this);
    }
    return val;
  }


  public Datum get(String name) throws Exception {
    return get(name,null);
  }


  public Datum get(Symbol name) throws Exception {
    if(name.hasSourceInformation())
      return get(name.value(),name.source());
    return get(name.value(),null);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set! Operations
  private void set(String name, Datum newValue, SourceInformation source) throws Exception {
    if(set_recur(name,newValue) == false) {
      if(source == null) {
        throw new Exceptionf("'MetaObject [SET! TO %s] \"%s\" isn't a property of object %s", newValue.write(), name, write());
      } else {
        throw new Exceptionf("'MetaObject [SET! TO %s] \"%s\" isn't a property of object %s\n>> Location: %s", newValue.write(), name, write(), source);
      }
    }
  }


  public void set(String name, Datum newValue) throws Exception {
    set(name,newValue,null);
  }


  public void set(Symbol name, Datum newValue) throws Exception {
    if(name.hasSourceInformation()) {
      set(name.value(),newValue,name.source());
    } else {
      set(name.value(),newValue,null);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define Operations
  public void define(String name, Datum newValue) {
    props.put(name,newValue);
  }


  public void define(Symbol name, Datum newValue) {
    props.put(name.value(),newValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Querying Operations
  public boolean has(String name) {
    if(props.containsKey(name)) return true;
    MetaObject superObj = getSuper();
    if(superObj == null) return false;
    return superObj.has(name);
  }


  public boolean has(Symbol name) {
    return has(name.value());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Property Serialization Operations
  public ArrayList<String> props() {
    ArrayList<String> propsList = new ArrayList<String>();
    for(String key : props.keySet())
      propsList.add(key);
    return propsList;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <super>-binding helper methods
  protected void bindImmediateMethodsWithSuper() throws Exception {
    for(String s : props.keySet()) {
      Datum val = props.get(s);
      if(val instanceof CompoundProcedure) {
        props.put(s,((CompoundProcedure)val).loadWithSuper(getSuper()));
      }
    }
  }


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
  // Serialization
  public abstract String display();

  public abstract String write();

  public abstract String pprint();


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public abstract MetaObject loadWithState(ExecutionState state) throws Exception;


  // Support function create new <props> loaded in the current state
  public static ConcurrentHashMap<String,Datum> loadPropsWithState(ConcurrentHashMap<String,Datum> props, ExecutionState state) throws Exception {
    ConcurrentHashMap<String,Datum> loaded = new ConcurrentHashMap<String,Datum>(props.size());
    for(ConcurrentHashMap.Entry<String,Datum> e : props.entrySet())
      loaded.put(e.getKey(),e.getValue().loadWithState(state));
    return loaded;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public abstract MetaObject loadWithName(String name) throws Exception;


  // [ONLY FOR USE UPON INITIAL CONSTRUCTION] Support function to bind names to methods.
  //   => NOTE: MUTATES THE GIVEN <props> ARGUMENT !!!
  public static void bindMethodsWithName(String namePrefix, ConcurrentHashMap<String,Datum> props) {
    for(ConcurrentHashMap.Entry<String,Datum> e : props.entrySet()) {
      Datum propValue = e.getValue();
      if(propValue instanceof CompoundProcedure) {
        String propName = e.getKey();
        props.put(propName,((CompoundProcedure)propValue).loadWithForcedName(namePrefix+propName));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public abstract MetaObject copy();


  // Support function to generate a copy <props>
  public static ConcurrentHashMap<String,Datum> copyProps(ConcurrentHashMap<String,Datum> props) {
    ConcurrentHashMap<String,Datum> copied = new ConcurrentHashMap<String,Datum>(props.size());
    for(ConcurrentHashMap.Entry<String,Datum> e : props.entrySet())
      copied.put(e.getKey(),e.getValue().copy());
    return copied;
  }
}