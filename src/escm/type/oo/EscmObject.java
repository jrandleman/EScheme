// Author: Jordan Randleman - escm.type.oo.EscmObject
// Purpose:
//    Escm Object class, implements "escm.vm.type.Callable" to allow application overloading!
//    Create an object instance by invoking <callWith()> on an <EscmClass>.
//
// Includes:
//    - boolean isFunctor() // object has a "->procedure" method
//
//    - boolean instanceOf(<EscmClass>-or<EscmInterface>) // determine if an instance of the class or interface
//
//    - callWith(...) // overloads application via a "->procedure" method!

package escm.type.oo;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Trampoline;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.procedure.MethodProcedure;
import escm.vm.type.Callable;

public class EscmObject extends MetaObject implements Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Class
  private EscmClass escmClass = null;

  public EscmClass getEscmClass() {
    return escmClass;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Interfaces Implemented
  public ArrayList<EscmInterface> getEscmInterfaces() {
    return escmClass.getEscmInterfaces();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Super Object
  private EscmObject superObject = null;

  public EscmObject getSuper() {
    return superObject;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Internal Constructor
  // @PRECONDITION: <superObject> MUST ALREADY BE INITIALIZED!
  // @PRECONDITION: <props> MUST HAVE <self> AND <super> BOUND TO METHODS EXTERNALLY
  EscmObject(EscmClass escmClass, EscmObject superObject, ConcurrentHashMap<String,Datum> props) {
    this.escmClass = escmClass;
    this.superObject = superObject;
    this.props = props;
    this.props.put("class",this.escmClass);
    this.bindMethodsWithSuper();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Functor Logic (Application Overload)
  public Trampoline.Bounce callWith(ArrayList<Datum> args, Trampoline.Continuation continuation) throws Exception {
    if(!has("->procedure"))
      throw new Exceptionf("Object of class %s error: can't be applied without a \"->procedure\" method!", escmClass.readableName());
    Datum procedure = get("->procedure");
    if(!(procedure instanceof MethodProcedure))
      throw new Exceptionf("Object of class %s error: \"->procedure\" property %s isn't a method!", escmClass.readableName(),procedure.profile());
    return ((MethodProcedure)procedure).loadWithSelf(this).callWith(args,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Immediate Method Name Generation
  protected String generateMethodName(String propertyName) {
    StringBuilder sb = new StringBuilder();
    sb.append(escmClass.name());
    sb.append(".");
    sb.append(propertyName);
    sb.append(" [class::instance]");
    return sb.toString();
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
      throw new Exceptionf("'super! class %s doesn't have a <super> to initialize!", escmClass.readableName());
    return superObject.escmClass.callWith(parameters,(newSuperObj) -> () -> {

      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      // @NOTE: This is ONLY thread safe if the user ONLY uses <super!> as the 
      //        1st statement in the constructor (as directed by the README)
      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
      this.superObject = (EscmObject)newSuperObj;
      
      this.bindMethodsWithSuper();
      return continuation.run(this.superObject);
    });
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance of Interface or Class
  private boolean interfaceImplementsInterface(EscmInterface i1, EscmInterface i2) {
    for(EscmInterface iface : i1.getEscmInterfaces()) {
      if(iface.eq(i2) || interfaceImplementsInterface(iface,i2)) return true;
    }
    return false;
  }


  public boolean instanceOf(EscmInterface i) {
    EscmObject obj = this;
    while(obj != null) {
      ArrayList<EscmInterface> interfaces = obj.escmClass.getEscmInterfaces();
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
      if(obj.escmClass == c) return true;
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

  public boolean equal(Object o) {
    if(!(o instanceof EscmObject) || ((EscmObject)o).escmClass != escmClass) return false;
    EscmObject that = (EscmObject)o;
    // Check Equality of Props
    if(this.props.size() != that.props.size()) return false;
    for(ConcurrentHashMap.Entry<String,Datum> e : this.props.entrySet()) {
      Datum thisProp = e.getValue();
      Datum thatProp = that.props.get(e.getKey());
      if(thatProp == null) {
        return false;
      }
      if(thisProp instanceof MethodProcedure && thatProp instanceof MethodProcedure) {
        if(!((MethodProcedure)thisProp).name().equals(((MethodProcedure)thatProp).name()))
          return false;
      } else if(!thisProp.equal(thatProp)) {
        return false;
      }
    }
    // Check Equality of Super (as needed)
    return this.superObject == null || this.superObject.equal(that.superObject);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    String className = escmClass.name();
    if(className.length() == 0) return "#<object>";
    return "#<object (class=" + className + ")>";
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public EscmObject loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  // <ignore> is used to distinguish this ctor from the above public one
  private EscmObject(int ignore, EscmClass escmClass, EscmObject superObject, ConcurrentHashMap<String,Datum> props) {
    this.escmClass = escmClass;
    if(superObject == null) {
      this.superObject = null;
    } else {
      this.superObject = superObject.copy();
    }
    this.props = props;
    this.bindMethodsWithSuper();
  }


  public EscmObject copy() {
    return new EscmObject(0,escmClass,superObject,new ConcurrentHashMap<String,Datum>(props));
  }
}