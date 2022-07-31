// Author: Jordan Randleman - escm.type.oo.EscmObject
// Purpose:
//    Escm Object class, implements "escm.vm.type.Callable" to allow application overloading!
//
// Includes:
//    - boolean isFunctor() // object has a "->procedure" method
//
//    - boolean instanceOf(<EscmClass>-or<EscmInterface>) // determine if an instance of the class or interface
//
//    - callWith(...) // overloads application via a "->procedure" method!

package escm.type.oo;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Trampoline;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.procedure.CompoundProcedure;
import escm.vm.type.Callable;
import escm.vm.type.ExecutionState;

public class EscmObject extends MetaObject implements Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Class
  private EscmClass prototypeClass = null;

  public EscmClass getEscmClass() {
    return prototypeClass;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Interfaces Implemented
  public ArrayList<EscmInterface> getEscmInterfaces() {
    return prototypeClass.getEscmInterfaces();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Super Object
  private EscmObject superObject = null;

  public EscmObject getSuper() {
    return superObject;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  // @PRECONDITION: <superObject> MUST ALREADY BE INITIALIZED!
  // @PRECONDITION: <props> MUST HAVE <self> AND <super> BOUND TO METHODS EXTERNALLY
  public EscmObject(EscmClass prototypeClass, EscmObject superObject, ConcurrentHashMap<String,Datum> props) {
    this.prototypeClass = prototypeClass;
    this.superObject = superObject;
    this.props = props;
    this.props.put("class",this.prototypeClass);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Functor Logic (Application Overload)
  public Trampoline.Bounce callWith(ArrayList<Datum> args, Trampoline.Continuation continuation) throws Exception {
    if(!has("->procedure"))
      throw new Exceptionf("Object of class %s error: can't be applied without an \"->procedure\" method!", prototypeClass.readableName());
    Datum procedure = get("->procedure");
    if(!(procedure instanceof CompoundProcedure))
      throw new Exceptionf("Object of class %s error: \"->procedure\" property %s isn't a method!", prototypeClass.readableName(),procedure.profile());
    return ((CompoundProcedure)procedure).loadWithSelf(this).callWith(args,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Functor Predicate
  public boolean isFunctor() {
    return props.containsKey("->procedure");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Update the Super Object (& return the updated super) (equivalent to the "super!" macro)
  public Trampoline.Bounce updateSuper(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
    if(superObject == null)
      throw new Exceptionf("'super! class %s doesn't have a <super> to initialize!", prototypeClass.readableName());
    return superObject.prototypeClass.callWith(parameters,(newSuperObj) -> () -> {

      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // @NOTE: This is ONLY thread safe if the user ONLY uses <super!> as the 
      //        1st statement in the constructor (as directed by the README)
      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      this.superObject = (EscmObject)newSuperObj;
      
      this.bindImmediateMethodsWithSuper();
      return continuation.run(this.superObject);
    });
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance of Interface or Class
  private boolean interfaceImplementsInterface(EscmInterface i1, EscmInterface i2) {
    for(EscmInterface iface : i1.getEscmInterfaces()) {
      if(iface == i2 || interfaceImplementsInterface(iface,i2)) return true;
    }
    return false;
  }


  public boolean instanceOf(EscmInterface i) {
    EscmObject obj = this;
    while(obj != null) {
      ArrayList<EscmInterface> interfaces = obj.prototypeClass.getEscmInterfaces();
      if(interfaces.contains(i)) return true;
      for(EscmInterface iface : interfaces) {
        if(interfaceImplementsInterface(iface,i)) return true;
      }
      obj = obj.superObject;
    }
    return false;
  }


  public boolean instanceOf(EscmClass c) {
    EscmObject obj = this;
    while(obj != null) {
      if(obj.prototypeClass == c) return true;
      obj = obj.superObject;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "object";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof EscmObject && ((EscmObject)o).props == this.props;
  }

  public boolean equals(Object o) {
    if(!(o instanceof EscmObject) || ((EscmObject)o).prototypeClass != prototypeClass) return false;
    EscmObject that = (EscmObject)o;
    // Check Equality of Props
    if(this.props.size() != that.props.size()) return false;
    for(ConcurrentHashMap.Entry<String,Datum> e : this.props.entrySet()) {
      Datum thisProp = e.getValue();
      Datum thatProp = that.props.get(e.getKey());
      if(thatProp == null) {
        return false;
      }
      if(thisProp instanceof CompoundProcedure && thatProp instanceof CompoundProcedure && 
         !((CompoundProcedure)thisProp).name().equals(((CompoundProcedure)thatProp).name())) {
        return false;
      }
      if(!thisProp.equals(thatProp)) {
        return false;
      }
    }
    // Check Equality of Super (as needed)
    return this.superObject == null || this.superObject.equals(that.superObject);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  private String getPropertiesAsString() {
    HashSet<String> propNames = new HashSet<String>();
    EscmObject obj = this;
    while(obj != null) {
      propNames.addAll(obj.props());
      obj = obj.getSuper();
    }
    StringBuilder sb = new StringBuilder("{");
    for(String propName : propNames) {
      sb.append(propName);
      sb.append("=");
      try {
        sb.append(get(propName).write());
      } catch(Exception e) {
        // We know this won't trigger since we're only querying 
        // for properties that we know the object already has.
      }
      sb.append(", ");
    }
    int sbLength = sb.length();
    sb.delete(sbLength-2,sbLength);
    sb.append("}");
    return sb.toString();
  }


  public String display() {
    return "#<object " + getPropertiesAsString() + '>';
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public EscmObject loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public EscmObject loadWithName(String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  // <ignore> is used to distinguish this ctor from the above public one
  private EscmObject(int ignore, EscmClass prototypeClass, EscmObject superObject, ConcurrentHashMap<String,Datum> props) {
    this.prototypeClass = prototypeClass;
    if(superObject == null) {
      this.superObject = null;
    } else {
      this.superObject = superObject.copy();
    }
    this.props = props;
    try {
      bindImmediateMethodsWithSuper();
    } catch (Exception e) {
      // Exception for "null defn envs" in method fcns would've already happend by now,
      // so we know there's nothing to bother catching here! Still putting in a block
      // though to remove <throws Exception> from this method signature.
    } 
  }


  // PRECONDITION: <this> IS THE RESULT OF A CALL TO <loadWithState()>
  public EscmObject copy() {
    // NOTE: EscmClass.copy() reflects <this>, so the ".class" member is unnaffected!
    return new EscmObject(0,prototypeClass,superObject,MetaObject.copyProps(props));
  }
}