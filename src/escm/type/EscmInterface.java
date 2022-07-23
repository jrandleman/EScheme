// Author: Jordan Randleman - escm.type.EscmInterface
// Purpose:
//    Escm Interface class, supporting a list of instance property names 
//    that implementing classes MUST provide.
//
// Includes:
//    - java.lang.String name()         // returns <""> if an anonymous interface
//    - java.lang.String readableName() // returns <"#<anonymous>"> if an anonymous interface
//
//    - ArrayList<java.lang.String> instanceProps() // returns instance property names
//
//    - void confirmSatisfiedBy(EscmClass class) // throws an error if the given class doesn't satisfy <this>

package escm.type;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.vm.type.ExecutionState;

public class EscmInterface extends MetaObject {
  ////////////////////////////////////////////////////////////////////////////
  // Name
  public java.lang.String name() {
    Datum val = props.get("name");
    if(val == null || !(val instanceof Symbol)) return "";
    return ((Symbol)val).value();
  }


  public java.lang.String readableName() {
    java.lang.String name = name();
    if(name.length() == 0) return "#<anonymous>";
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Interfaces Implemented
  private ArrayList<EscmInterface> interfaces = null;

  public ArrayList<EscmInterface> getEscmInterfaces() {
    return new ArrayList<EscmInterface>(interfaces);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Super Object Abstraction (<null> for interfaces)
  public EscmInterface getSuper() {
    return null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Object-Construction Template
  private ArrayList<java.lang.String> requiredProps = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  private void bindImmediateMethodsWithName() {
    MetaObject.bindMethodsWithName("[interface::static] "+name()+".",props);
    // NOTE: We don't need to bind the super interface method names, since 
    //       they've already been bound when creating them prior to being 
    //       passed to this interface's ctor.
  }


  public EscmInterface(ArrayList<EscmInterface> interfaces, ConcurrentHashMap<java.lang.String,Datum> props, ArrayList<java.lang.String> requiredProps) throws Exception {
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
    bindImmediateMethodsWithSuper();
    bindImmediateMethodsWithName();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance Property Serialization Operations
  public ArrayList<java.lang.String> instanceProps() {
    return new ArrayList<java.lang.String>(requiredProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Class Satisfaction Verification
  private java.lang.String stringifyArrayListString(ArrayList<java.lang.String> al) {
    StringBuilder sb = new StringBuilder("{");
    for(int i = 0, n = al.size(); i < n; ++i) {
      sb.append(al.get(i));
      if(i+1 < n) sb.append(", ");
    }
    return sb.toString() + '}';
  }


  private java.lang.String generateSatisfactionError(EscmClass topmostClass, ArrayList<java.lang.String> inheritanceChain, java.lang.String missedReqProp) {
    if(inheritanceChain.size() > 0) inheritanceChain.remove(0);
    return java.lang.String.format("Class with instance props %s inheriting %s doesn't have required prop \"%s\" for interface \"%s\"", 
                                   stringifyArrayListString(topmostClass.instanceProps()), 
                                   stringifyArrayListString(inheritanceChain), 
                                   missedReqProp, 
                                   readableName());
  }


  private void confirmSatisfiedBy(EscmClass cls, ArrayList<java.lang.String> inheritanceChain, EscmClass topmostClass, java.lang.String missedReqProp) throws Exception {
    if(cls == null) {
      if(missedReqProp != null) {
        throw new Exception(generateSatisfactionError(topmostClass,inheritanceChain,missedReqProp));
      }
      return;
    }
    inheritanceChain.add(cls.readableName());
    ArrayList<java.lang.String> objProps = cls.instanceProps();
    for(java.lang.String prop : requiredProps) {
      if(!objProps.contains(prop)) {
        confirmSatisfiedBy(cls.getSuper(),inheritanceChain,topmostClass,prop);
      }
    }
  }


  public void confirmSatisfiedBy(EscmClass cls) throws Exception {
    confirmSatisfiedBy(cls,new ArrayList<java.lang.String>(),cls,null);
    for(EscmInterface superIface : interfaces) {
      superIface.confirmSatisfiedBy(cls);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "interface";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof EscmInterface && ((EscmInterface)o).props == this.props;
  }

  public boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    java.lang.String name = name();
    if(name.length() == 0) return "#<interface>";
    return "#<interface " + name  + '>';
  }

  public java.lang.String write() {
    return display();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public EscmInterface loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter

  // NOTE: <ignore> here just distinguishes this private ctor from the public one
  private EscmInterface(int ignore, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<java.lang.String,Datum> props, ArrayList<java.lang.String> requiredProps) throws Exception {
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
    bindImmediateMethodsWithSuper();
  }


  public EscmInterface loadWithName(java.lang.String name) throws Exception {
    java.lang.String currentName = name();
    if(currentName.length() > 0) return this;
    EscmInterface i = new EscmInterface(0,this.interfaces,MetaObject.copyProps(this.props),this.requiredProps);
    i.props.put("name",new Symbol(name));
    i.bindImmediateMethodsWithName();
    return i;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmInterface copy() {
    return this;
  }
}