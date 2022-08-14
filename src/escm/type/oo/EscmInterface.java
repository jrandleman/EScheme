// Author: Jordan Randleman - escm.type.oo.EscmInterface
// Purpose:
//    Escm Interface class, supporting a list of instance property names 
//    that implementing classes MUST provide.
//
// Includes:
//    - String name()         // returns <""> if an anonymous interface
//    - String readableName() // returns <"#<anonymous>"> if an anonymous interface
//
//    - ArrayList<String> instanceProps() // returns instance property names
//
//    - void confirmSatisfiedBy(EscmClass class) // throws an error if the given class doesn't satisfy <this>

package escm.type.oo;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.Symbol;

public class EscmInterface extends MetaObject {
  ////////////////////////////////////////////////////////////////////////////
  // Name
  public String name() {
    Datum val = props.get("name");
    if(val == null || !(val instanceof Symbol)) return "";
    return ((Symbol)val).value();
  }


  public String readableName() {
    String name = name();
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
  private ArrayList<String> requiredProps = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public EscmInterface(ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ArrayList<String> requiredProps) throws Exception {
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
    this.convertProceduresToMethodsAndBindSuper();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Immediate Method Name Generation
  protected String generateMethodName(String propertyName) {
    StringBuilder sb = new StringBuilder();
    sb.append(name());
    sb.append(".");
    sb.append(propertyName);
    sb.append(" [interface::static]");
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance Property Serialization Operations
  public ArrayList<String> instanceProps() {
    return new ArrayList<String>(requiredProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Class Satisfaction Verification
  private String stringifyArrayListString(ArrayList<String> al) {
    StringBuilder sb = new StringBuilder("{");
    for(int i = 0, n = al.size(); i < n; ++i) {
      sb.append(al.get(i));
      if(i+1 < n) sb.append(", ");
    }
    return sb.toString() + '}';
  }


  private String generateSatisfactionError(EscmClass topmostClass, ArrayList<String> inheritanceChain, String missedReqProp) {
    if(inheritanceChain.size() > 0) inheritanceChain.remove(0);
    return String.format("Class with instance props %s inheriting %s doesn't have required prop \"%s\" for interface \"%s\"", 
                                   stringifyArrayListString(topmostClass.instanceProps()), 
                                   stringifyArrayListString(inheritanceChain), 
                                   missedReqProp, 
                                   readableName());
  }


  private void confirmSatisfiedBy(EscmClass cls, ArrayList<String> inheritanceChain, EscmClass topmostClass, String missedReqProp) throws Exception {
    if(cls == null) {
      if(missedReqProp != null) {
        throw new Exception(generateSatisfactionError(topmostClass,inheritanceChain,missedReqProp));
      }
      return;
    }
    inheritanceChain.add(cls.readableName());
    ArrayList<String> objProps = cls.instanceProps();
    for(String prop : requiredProps) {
      if(!objProps.contains(prop)) {
        confirmSatisfiedBy(cls.getSuper(),inheritanceChain,topmostClass,prop);
      }
    }
  }


  public void confirmSatisfiedBy(EscmClass cls) throws Exception {
    confirmSatisfiedBy(cls,new ArrayList<String>(),cls,null);
    for(EscmInterface superIface : interfaces) {
      superIface.confirmSatisfiedBy(cls);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "interface";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof EscmInterface && ((EscmInterface)o).props == this.props;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    String name = name();
    if(name.length() == 0) return "#<interface>";
    return "#<interface " + name  + '>';
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  private EscmInterface(String name, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ArrayList<String> requiredProps) {
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
    this.props.put("name",new Symbol(name));
    this.bindMethodsWithName();
    // @NOTE: Don't need to bind <super> since it's #f for interfaces & already bound as such in <props>.
  }


  public EscmInterface loadWithName(String name) {
    String currentName = name();
    if(currentName.length() > 0) return this;
    return new EscmInterface(name,this.interfaces,MetaObject.copyProps(this.props),this.requiredProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmInterface copy() {
    return this;
  }
}