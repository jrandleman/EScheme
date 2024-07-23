// Author: Jordan Randleman - escm.type.oo.EscmClass
// Purpose:
//    Escm Class class, implements "escm.vm.type.callable.Callable" to double as a ctor!
//
// Includes:
//    - String name()         // returns <""> if an anonymous class
//    - String readableName() // returns <"#<anonymous>"> if an anonymous class
//
//    - boolean hasInstanceProp(String name)
//    - void forEachInstanceProperty(InstancePropertyIterationProcedure ipp) // iterates over prop names
//
//    - callWith(...) // overloads constructing an object instance of this class!

package escm.type.oo;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.procedure.CompoundProcedure;
import escm.type.procedure.MethodProcedure;
import escm.vm.type.callable.Callable;
import escm.vm.type.callable.DocString;

public class EscmClass extends MetaObject implements Callable {
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
  // Super Class
  private EscmClass superClass = null;

  public EscmClass getSuper() {
    return superClass;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance Properties
  private ConcurrentHashMap<String,Datum> objectProps = null;


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  private String docstring = null;

  // Print class name + super + interfaces
  private void accumulateClassSignature(StringBuilder sb) {
    sb.append("Class "+readableName());
    if(superClass != null) {
      sb.append(" (:extends "+superClass.readableName()+")");
    }
    if(interfaces.size() > 0) {
      sb.append(" (:implements");
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

  // Replaces names of <signature> with <newName>.
  // Also used by <EscmObject>.
  static Datum tagMethodWithName(Datum newName, Datum signature) {
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

  // @PRECONDITION: <signatures instanceof Pair>
  private void accumulateMethodSignatureAndDocstring(StringBuilder sb, Datum signatures, String docstring) {
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
    if(docstring.length() > 0) {
      sb.append("\n      "+docstring.replaceAll("\n","\n      "));
    }
    sb.append("\n");
  }

  // Print constructor (if given)
  private boolean accumulateConstructor(boolean printed, StringBuilder sb) {
    Datum ctor = objectProps.get("new");
    if(ctor != null && ctor instanceof CompoundProcedure) {
      CompoundProcedure val = (CompoundProcedure)ctor;
      Datum signatures = tagMethodWithName(new Symbol("new"),val.signature());
      if(!(signatures instanceof Pair)) return printed;
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  Constructor:");
      accumulateMethodSignatureAndDocstring(sb,signatures,val.docstring());
      sb.append("\n");
      printed = true;
    }
    return printed;
  }

  // Print method signature + user docstring (if given)
  private void accumulateMethod(StringBuilder sb, boolean isStaticProp, String name, CompoundProcedure val) {
    if(!isStaticProp && name.equals("new")) return;
    Datum signatures = tagMethodWithName(new Symbol(name),val.signature());
    if(!(signatures instanceof Pair)) return;
    accumulateMethodSignatureAndDocstring(sb,signatures,val.docstring());
  }

  // Print fields & methods
  private boolean accumulateProperties(boolean printed, StringBuilder sb, boolean isStaticProps, ConcurrentHashMap<String,Datum> baseProps) {
    if(baseProps.size() == 0) return false;
    String propType = isStaticProps ? "Static" : "Instance";
    ConcurrentHashMap<String,Datum> fields = new ConcurrentHashMap<String,Datum>();
    ConcurrentHashMap<String,CompoundProcedure> methods = new ConcurrentHashMap<String,CompoundProcedure>();
    for(ConcurrentHashMap.Entry<String,Datum> prop : baseProps.entrySet()) {
      Datum val = prop.getValue();
      if(val instanceof CompoundProcedure) {
        methods.put(prop.getKey(),(CompoundProcedure)val);
      } else {
        fields.put(prop.getKey(),val);
      }
    }
    if(fields.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  "+propType+" Fields:");
      for(ConcurrentHashMap.Entry<String,Datum> field : fields.entrySet()) {
        Datum val = field.getValue();
        sb.append("\n    "+field.getKey()+" [type: <"+val.type()+">, hashcode: "+val.hashCode()+"]\n");
      }
      sb.append("\n");
      printed = true;
    }
    if(methods.size() > 0) {
      if(printed) sb.append("  ----------------------------------------------------------------------\n");
      sb.append("  "+propType+" Methods:");
      for(ConcurrentHashMap.Entry<String,CompoundProcedure> method : methods.entrySet()) {
        accumulateMethod(sb,isStaticProps,method.getKey(),method.getValue());
      }
      sb.append("\n");
      printed = true;
    }
    return printed;
  }

  public String docstring() {
    StringBuilder sb = new StringBuilder();
    // Print class name + super + interfaces
    accumulateClassSignature(sb);
    // Print user docstring
    boolean printed = accumulateUserDocstring(sb);
    // Print constructor
    printed = accumulateConstructor(printed,sb);
    // Print static properties
    printed = accumulateProperties(printed,sb,true,props);
    // Print instance properties
    printed = accumulateProperties(printed,sb,false,objectProps);
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public EscmClass(String name, String docstring, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) throws Exception {
    this.docstring = DocString.format(docstring);
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    for(EscmInterface iface : interfaces) {
      iface.confirmSatisfiedBy(this);
    }
    if(name == null) {
      this.convertProceduresToMethods();
    } else {
      this.props.put("name",new Symbol(name));
      this.convertProceduresToNamedMethods();
      this.bindInstanceMethodsWithName();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Immediate Method Name Generation
  protected String generateMethodName(String propertyName) {
    StringBuilder sb = new StringBuilder();
    sb.append(name());
    sb.append(".");
    sb.append(propertyName);
    sb.append(" [class::static]");
    return sb.toString();
  }


  protected String generateInstanceMethodName(String propertyName) {
    StringBuilder sb = new StringBuilder();
    sb.append(name());
    sb.append(".");
    sb.append(propertyName);
    sb.append(" [class::instance]");
    return sb.toString();
  }


  private void bindInstanceMethodsWithName() {
    for(ConcurrentHashMap.Entry<String,Datum> e : this.objectProps.entrySet()) {
      Datum val = e.getValue();
      if(val instanceof CompoundProcedure) {
        String key = e.getKey();
        this.objectProps.put(key,((CompoundProcedure)val).loadWithName(generateInstanceMethodName(key)));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instance Property Operations
  public static interface InstancePropertyIterationProcedure {
    public boolean exec(String prop); // returns whether to continue
  }

  public boolean hasInstanceProp(String name) throws Exception {
    return objectProps.containsKey(name);
  }

  public void forEachInstanceProperty(InstancePropertyIterationProcedure ipp) {
    for(String prop : objectProps.keySet()) {
      if(!ipp.exec(prop)) return;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor Signature
  public Datum signature() {
    String name = name();
    Datum signatureHead = name.length() == 0 ? this : new Symbol(name);
    Datum ctor = objectProps.get("new");
    if(ctor != null && ctor instanceof CompoundProcedure) {
      return tagMethodWithName(signatureHead,((CompoundProcedure)ctor).signature());
    }
    return Pair.List(signatureHead);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructing Objects
  private Trampoline.Bounce constructSuperObject(EscmObject superObj, Trampoline.Continuation continuation) throws Exception {
    EscmObject obj = new EscmObject(this,superObj,MetaObject.copyProps(objectProps));
    Datum ctor = obj.props.get("new");
    // Invoke a "new" ctors in super objects (non-nullary ctors must be explicitely called by user via "(super! <arg> ...)")
    if(ctor == null || !(ctor instanceof MethodProcedure) || !((MethodProcedure)ctor).isThunk()) {
      return continuation.run(obj);
    }
    obj.props.remove("new");
    return ((MethodProcedure)ctor).callWith(new ArrayList<Datum>(),(ignored) -> () -> continuation.run(obj));
  }


  private Trampoline.Bounce constructInheritanceObjectChain(Trampoline.Continuation continuation) throws Exception {
    if(superClass == null)
      return constructSuperObject(null,continuation);
    return superClass.constructInheritanceObjectChain((superObj) -> () -> constructSuperObject((EscmObject)superObj,continuation));
  }


  private Trampoline.Bounce constructObject(EscmObject superObj, ArrayList<Datum> args, Trampoline.Continuation continuation) throws Exception {
    EscmObject obj = new EscmObject(this,superObj,MetaObject.copyProps(objectProps));
    Datum ctor = obj.props.get("new");
    if(ctor != null && ctor instanceof MethodProcedure) {
      obj.props.remove("new");
      return ((MethodProcedure)ctor).callWith(args,(ignored) -> () -> continuation.run(obj));
    } else if(args.size() > 0) {
      throw new Exceptionf("Class %s construction error: no custom ctor defined for args: %s", readableName(), Exceptionf.profileArgs(args));
    }
    return continuation.run(obj);
  }


  public Trampoline.Bounce callWith(ArrayList<Datum> args, Trampoline.Continuation continuation) throws Exception {
    if(superClass == null) return constructObject(null,args,continuation);
    return superClass.constructInheritanceObjectChain((superObj) -> () -> constructObject((EscmObject)superObj,args,continuation));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "class";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof EscmClass && ((EscmClass)o).props == this.props;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    String name = name();
    if(name.length() == 0) return "#<class>";
    return "#<class " + name + '>';
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
  private EscmClass(int ignore, String name, String docstring, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) {
    this.docstring = docstring;
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    this.props.put("name",new Symbol(name));
    this.bindMethodsWithName();
    this.bindInstanceMethodsWithName();
  }


  public EscmClass loadWithName(String name) {
    if(name.equals("self") || name.equals("super") || name().length() > 0) return this;
    return new EscmClass(0,name,this.docstring,this.superClass,this.interfaces,this.props,this.objectProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmClass shallowCopy() {
    return this;
  }
}