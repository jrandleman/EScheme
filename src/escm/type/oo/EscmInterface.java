// Author: Jordan Randleman - escm.type.oo.EscmInterface
// Purpose:
//    Escm Interface class, supporting a list of instance property names 
//    that implementing classes MUST provide.
//
// Includes:
//    - String name()         // returns <""> if an anonymous interface
//    - String readableName() // returns <"#<anonymous>"> if an anonymous interface
//
//    - boolean hasInstanceProp(String name)
//    - void forEachInstanceProperty(InstancePropertyIterationProcedure ipp) // iterates over prop names
//
//    - void confirmSatisfiedBy(EscmClass class) // throws an error if the given class doesn't satisfy <this>

package escm.type.oo;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.procedure.CompoundProcedure;
import escm.util.error.Exceptionf;
import escm.vm.type.callable.DocString;

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

  public boolean hasInterface(EscmInterface iface) {
    return interfaces.contains(iface);
  }

  public void forEachInterface(InterfaceIterationProcedure ip) {
    for(EscmInterface iface : interfaces) {
      if(!ip.exec(iface)) return;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Super Object Abstraction (<null> for interfaces)
  public EscmInterface getSuper() {
    return null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Object-Construction Template
  private ArrayList<String> requiredProps = null;
  private ConcurrentHashMap<String,CompoundProcedure> methodsTypeSignatures = null;


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  private String docstring = null;

  // Print class name + super + interfaces
  private void accumulateInterfaceSignature(StringBuilder sb) {
    sb.append("Interface "+readableName());
    if(interfaces.size() > 0) {
      sb.append(" (:extends");
      for(EscmInterface iface : interfaces) {
        sb.append(" "+iface.readableName());
      }
      sb.append(")");
    }
    sb.append("\n");
  }

  // Print user docstring
  private boolean accumulateUserDocstring(StringBuilder sb) {
    if(docstring.length() > 0) {
      sb.append("  User Description:\n    ");
      sb.append(docstring.replaceAll("\n","\n    "));
      sb.append("\n\n");
      return true;
    }
    return false;
  }

  // Print required props
  private boolean accumulateRequiredProperties(boolean printed, StringBuilder sb) {
    if(requiredProps.size() == 0) return printed;
    if(printed) sb.append("  ----------------------------------------------------------------------\n");
    sb.append("  Required Instance Properties:");
    for(String required : requiredProps) {
      sb.append("\n    "+required);
    }
    sb.append("\n\n");
    printed = true;
    return printed;
  }

  // Print method signature + user docstring (if given)
  private void accumulateMethod(StringBuilder sb, String name, CompoundProcedure val) {
    Datum signatures = EscmClass.tagMethodWithName(new Symbol(name),val.signature());
    if(!(signatures instanceof Pair)) return;
    EscmClass.accumulateMethodSignatureAndDocstring(sb,signatures,val.docstring());
  }

  // Print required type signatures
  private boolean accumulateTypeSignatures(boolean printed, StringBuilder sb) {
    if(props.size() == 0) return false;
    if(methodsTypeSignatures.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  Required Instance Methods:");
      TreeMap<String,CompoundProcedure> sortedSigs = new TreeMap<String,CompoundProcedure>(methodsTypeSignatures);
      for(Map.Entry<String,CompoundProcedure> sig : sortedSigs.entrySet()) {
        accumulateMethod(sb,sig.getKey(),sig.getValue());
      }
      sb.append("\n");
      printed = true;
    }
    return printed;
  }

  // Print static fields & methods
  private boolean accumulateStaticProperties(boolean printed, StringBuilder sb) {
    if(props.size() == 0) return false;
    ConcurrentHashMap<String,Datum> fields = new ConcurrentHashMap<String,Datum>();
    ConcurrentHashMap<String,CompoundProcedure> methods = new ConcurrentHashMap<String,CompoundProcedure>();
    for(ConcurrentHashMap.Entry<String,Datum> prop : props.entrySet()) {
      Datum val = prop.getValue();
      if(val instanceof CompoundProcedure) {
        methods.put(prop.getKey(),(CompoundProcedure)val);
      } else {
        fields.put(prop.getKey(),val);
      }
    }
    if(fields.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  Static Fields:");
      for(ConcurrentHashMap.Entry<String,Datum> field : fields.entrySet()) {
        Datum val = field.getValue();
        sb.append("\n    "+field.getKey()+" [type: <"+val.type()+">, hashcode: "+val.hashCode()+"]\n");
      }
      sb.append("\n");
      printed = true;
    }
    if(methods.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  Static Methods:");
      TreeMap<String,CompoundProcedure> sortedMethods = new TreeMap<String,CompoundProcedure>(methods);
      for(Map.Entry<String,CompoundProcedure> method : sortedMethods.entrySet()) {
        accumulateMethod(sb,method.getKey(),method.getValue());
      }
      sb.append("\n");
      printed = true;
    }
    return printed;
  }

  public String docstring() {
    StringBuilder sb = new StringBuilder();
    // Print class name + super + interfaces
    accumulateInterfaceSignature(sb);
    // Print user docstring
    boolean printed = accumulateUserDocstring(sb);
    // Print instance properties
    printed = accumulateRequiredProperties(printed,sb);
    // Print type signatures
    printed = accumulateTypeSignatures(printed,sb);
    // Print static properties
    printed = accumulateStaticProperties(printed,sb);
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public EscmInterface(String name, String docstring, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ArrayList<String> requiredProps, ConcurrentHashMap<String,CompoundProcedure> methodsTypeSignatures) throws Exception {
    this.docstring = DocString.format(docstring);
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
    this.methodsTypeSignatures = methodsTypeSignatures;
    if(name == null) {
      this.convertProceduresToMethods();
    } else {
      this.props.put("name",new Symbol(name));
      this.convertProceduresToNamedMethods();
    }
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
  public static interface InstancePropertyIterationProcedure {
    public boolean exec(String prop); // returns whether to continue
  }

  public boolean hasInstanceProp(String name) {
    return requiredProps.contains(name);
  }

  public void forEachInstanceProperty(InstancePropertyIterationProcedure ipp) {
    for(String prop : requiredProps) {
      if(!ipp.exec(prop)) return;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Class Satisfaction Verification
  private void confirmHasProp(EscmClass cls, EscmClass topmostClass, String targetProp) throws Exception {
    if(cls == null) {
      throw new Exceptionf(
        "Class %s missing required property \"%s\" for interface \"%s\"",
        topmostClass.readableName(),
        targetProp,
        readableName()
      );
    }
    if(!cls.hasInstanceProp(targetProp)) {
      confirmHasProp(cls.getSuper(),topmostClass,targetProp);
    }
  }


  private void confirmHasMethod(EscmClass cls, EscmClass topmostClass, String methodName, CompoundProcedure method) throws Exception {
    if(cls == null) {
      throw new Exceptionf(
        "Class %s missing required method \"%s\" %s for interface %s", 
        topmostClass.readableName(),
        methodName, 
        method.signature(),
        readableName()
      );
    }
    if(!cls.hasInstanceMethod(methodName,method)) {
      confirmHasMethod(cls.getSuper(),topmostClass,methodName,method);
    }
  }


  private void confirmSatisfiedBy(EscmClass cls, EscmClass topmostClass) throws Exception {
    EscmClass superClass = cls.getSuper();
    for(String prop : requiredProps) {
      if(!cls.hasInstanceProp(prop)) {
        confirmHasProp(superClass,topmostClass,prop);
      }
    }
    for(ConcurrentHashMap.Entry<String,CompoundProcedure> typeSig : methodsTypeSignatures.entrySet()) {
      String name = typeSig.getKey();
      CompoundProcedure method = typeSig.getValue();
      if(!cls.hasInstanceMethod(name,method)) {
        confirmHasMethod(superClass,topmostClass,name,method);
      }
    }
  }


  public void confirmSatisfiedBy(EscmClass cls) throws Exception {
    confirmSatisfiedBy(cls,cls);
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
  // We use <ignore> here to distinguish from the <public> ctor
  private EscmInterface(int ignore, String name, String docstring, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ArrayList<String> requiredProps) {
    this.docstring = docstring;
    this.interfaces = interfaces;
    this.props = props;
    this.props.put("name",new Symbol(name));
    this.requiredProps = requiredProps;
    this.bindMethodsWithName();
  }


  public EscmInterface loadWithName(String name) {
    if(name.equals("self") || name.equals("super") || name().length() > 0) return this;
    return new EscmInterface(0,name,docstring,this.interfaces,this.props,this.requiredProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmInterface shallowCopy() {
    return this;
  }
}