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
import escm.type.bool.Boolean;
import escm.type.procedure.CompoundProcedure;
import escm.type.procedure.MethodProcedure;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.callable.Callable;

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
  // Constructor
  public EscmClass(String name, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) throws Exception {
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
  public Datum tagContructorWithClassName(Datum signatureHead, Datum ctorSignature) {
    if(!(ctorSignature instanceof Pair)) return Pair.List(signatureHead);
    Pair ctorSignaturePair = (Pair)ctorSignature;
    Datum fst = ctorSignaturePair.car();
    if(!(fst instanceof Pair)) return new Pair(signatureHead,ctorSignaturePair.cdr());
    Datum sigs = Nil.VALUE;
    while(ctorSignature instanceof Pair) {
      Pair p = (Pair)ctorSignature;
      if(p.car() instanceof Pair) {
        sigs = new Pair(new Pair(signatureHead,((Pair)p.car()).cdr()),sigs);
      }
      ctorSignature = p.cdr();
    }
    if(sigs instanceof Nil) return sigs;
    return (Datum)((Pair)sigs).reverse();
  }

  public Datum signature() {
    String name = name();
    Datum signatureHead = name.length() == 0 ? this : new Symbol(name);
    Datum ctor = objectProps.get("new");
    if(ctor != null && ctor instanceof CompoundProcedure) {
      return tagContructorWithClassName(signatureHead,((CompoundProcedure)ctor).signature());
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
  private EscmClass(int ignore, String name, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) {
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
    return new EscmClass(0,name,this.superClass,this.interfaces,this.props,this.objectProps);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmClass copy() {
    return this;
  }
}