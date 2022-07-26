// Author: Jordan Randleman - escm.type.oo.EscmClass
// Purpose:
//    Escm Class class, implements "escm.vm.type.Callable" to double as a ctor!
//
// Includes:
//    - String name()         // returns <""> if an anonymous class
//    - String readableName() // returns <"#<anonymous>"> if an anonymous class
//
//    - ArrayList<String> instanceProps() // returns instance property names
//
//    - callWith(...) // overloads constructing an object instance of this class!

package escm.type.oo;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.procedure.CompoundProcedure;
import escm.type.procedure.MethodProcedure;
import escm.vm.type.Callable;

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

  public ArrayList<EscmInterface> getEscmInterfaces() {
    return new ArrayList<EscmInterface>(interfaces);
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
  private void convertInstanceProceduresToMethods() {
    for(ConcurrentHashMap.Entry<String,Datum> e : this.objectProps.entrySet()) {
      Datum val = e.getValue();
      if(val instanceof CompoundProcedure) {
        this.objectProps.put(e.getKey(),new MethodProcedure((CompoundProcedure)val));
      }
    }
  }


  public EscmClass(EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) throws Exception {
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    for(EscmInterface iface : interfaces) {
      iface.confirmSatisfiedBy(this);
    }
    this.convertProceduresToMethodsAndBindSuper();
    this.convertInstanceProceduresToMethods();
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


  ////////////////////////////////////////////////////////////////////////////
  // Instance Property Serialization Operations
  public ArrayList<String> instanceProps() {
    return new ArrayList<String>(objectProps.keySet());
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
    return ((MethodProcedure)ctor).loadWithSelf(obj).callWith(new ArrayList<Datum>(),(ignored) -> () -> continuation.run(obj));
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
      return ((MethodProcedure)ctor).loadWithSelf(obj).callWith(args,(ignored) -> () -> continuation.run(obj));
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
  private void bindInstanceMethodsWithName() {
    for(ConcurrentHashMap.Entry<String,Datum> e : this.objectProps.entrySet()) {
      Datum val = e.getValue();
      if(val instanceof MethodProcedure) {
        String key = e.getKey();
        this.objectProps.put(key,((MethodProcedure)val).loadWithForcedName(generateInstanceMethodName(key)));
      }
    }
  }


  private EscmClass(String name, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) {
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    this.props.put("name",new Symbol(name));
    this.bindMethodsWithName();
    this.bindInstanceMethodsWithName();
    // @NOTE: No need to re-bind super, since <props> already points to <superClass> as <super>
  }


  public EscmClass loadWithName(String name) {
    String currentName = name();
    if(currentName.length() > 0) return this;
    return new EscmClass(name,this.superClass,this.interfaces,MetaObject.copyProps(this.props),MetaObject.copyProps(this.objectProps));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmClass copy() {
    return this;
  }
}