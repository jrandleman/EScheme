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
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.bool.Boolean;
import escm.type.procedure.CompoundProcedure;

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
    if(printed) sb.append("  ----------------------------------------------------------------------\n\n");
    sb.append("  Required Instance Properties:");
    for(String required : requiredProps) {
      sb.append("\n    "+required);
    }
    sb.append("\n\n");
    printed = true;
    return printed;
  }

  // Replaces names of <signature> with <newName>.
  private Datum tagMethodWithName(Datum newName, Datum signature) {
    if(!(signature instanceof Pair)) return Pair.List(newName);
    Pair ctorSignaturePair = (Pair)signature;
    Datum fst = ctorSignaturePair.car();
    if(!(fst instanceof Pair)) return new Pair(newName,ctorSignaturePair.cdr());
    Datum sigs = Nil.VALUE;
    while(signature instanceof Pair) {
      Pair p = (Pair)signature;
      if(p.car() instanceof Pair) {
        sigs = new Pair(new Pair(newName,((Pair)p.car()).cdr()),sigs);
      }
      signature = p.cdr();
    }
    if(sigs instanceof Nil) return sigs;
    return (Datum)((Pair)sigs).reverse();
  }

  // Print method signature + user docstring (if given)
  private void accumulateMethod(StringBuilder sb, String name, CompoundProcedure val) {
    Datum signatures = tagMethodWithName(new Symbol(name),val.signature());
    if(!(signatures instanceof Pair)) return;
    Pair psig = (Pair)signatures;
    // print multiple signatures
    if(psig.car() instanceof Pair) {
      sb.append("\n   ");
      while(signatures instanceof Pair) {
        psig = (Pair)signatures;
        sb.append(" "+psig.car().write());
        signatures = psig.cdr();
      }
    // print single signatures
    } else {
      sb.append("\n    "+psig.write());
    }
    // print user docstring (if given)
    String docs = val.docstring();
    if(docs.length() > 0) {
      sb.append("\n      "+docs.replaceAll("\n","\n      "));
    }
    sb.append("\n");
  }

  // Print static fields & methods
  private boolean accumulateProperties(boolean printed, StringBuilder sb) {
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
      if(printed) sb.append("  ----------------------------------------------------------------------\n\n");
      sb.append("  Static Fields:");
      for(ConcurrentHashMap.Entry<String,Datum> field : fields.entrySet()) {
        Datum val = field.getValue();
        sb.append("\n    "+field.getKey()+" [type: <"+val.type()+">, hashcode: "+val.hashCode()+"]\n");
      }
      sb.append("\n");
      printed = true;
    }
    if(methods.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n\n");
      sb.append("  Static Methods:");
      for(ConcurrentHashMap.Entry<String,CompoundProcedure> method : methods.entrySet()) {
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
    // Print static properties
    printed = accumulateProperties(printed,sb);
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public EscmInterface(String name, String docstring, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ArrayList<String> requiredProps) throws Exception {
    this.docstring = docstring.trim();
    this.interfaces = interfaces;
    this.props = props;
    this.requiredProps = requiredProps;
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
  private static class StringArrayListBox {
    public ArrayList<String> value = new ArrayList<String>();
  }


  private String stringifyArrayListString(ArrayList<String> al) {
    StringBuilder sb = new StringBuilder("{");
    for(int i = 0, n = al.size(); i < n; ++i) {
      sb.append(al.get(i));
      if(i+1 < n) sb.append(", ");
    }
    return sb.toString() + '}';
  }


  private ArrayList<String> getClassInstanceProperties(EscmClass topmostClass) {
    StringArrayListBox sb = new StringArrayListBox();
    topmostClass.forEachInstanceProperty((prop) -> {
      sb.value.add(prop);
      return true;
    });
    return sb.value;
  }


  private String generateSatisfactionError(EscmClass topmostClass, ArrayList<String> inheritanceChain, String missedReqProp) {
    if(inheritanceChain.size() > 0) inheritanceChain.remove(0);
    return String.format("Class with instance props %s inheriting %s doesn't have required prop \"%s\" for interface \"%s\"", 
                                   stringifyArrayListString(getClassInstanceProperties(topmostClass)), 
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
    for(String prop : requiredProps) {
      if(!cls.hasInstanceProp(prop)) {
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
  public EscmInterface copy() {
    return this;
  }
}