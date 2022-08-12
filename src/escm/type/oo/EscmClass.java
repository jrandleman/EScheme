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
import java.util.Arrays;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.procedure.CompoundProcedure;
import escm.vm.type.Callable;
import escm.vm.type.ExecutionState;

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
  private void bindImmediateMethodsWithName() {
    String currentName = name();
    MetaObject.bindMethodsWithName(currentName+"."," [class::static]",props);
    MetaObject.bindMethodsWithName(currentName+"."," [class::instance]",objectProps);
    // NOTE: We don't need to bind the super class's method names, since 
    //       they've already been bound when creating them prior to being 
    //       passed to this class's ctor.
  }


  public EscmClass(EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) throws Exception {
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    for(EscmInterface iface : interfaces) {
      iface.confirmSatisfiedBy(this);
    }
    bindImmediateMethodsWithSuper();
    bindImmediateMethodsWithName();
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
    obj.bindImmediateMethodsWithSuper();
    Datum ctor = obj.props.get("new");
    // Invoke a "new" ctors in super objects (non-nullary ctors must be explicitely called by user via "(super! <arg> ...)")
    if(ctor == null || !(ctor instanceof CompoundProcedure) || !((CompoundProcedure)ctor).isThunk()) {
      return continuation.run(obj);
    }
    obj.props.remove("new");
    return ((CompoundProcedure)ctor).loadWithSelf(obj).callWith(new ArrayList<Datum>(),(ignored) -> () -> continuation.run(obj));
  }


  private Trampoline.Bounce constructInheritanceObjectChain(Trampoline.Continuation continuation) throws Exception {
    if(superClass == null)
      return constructSuperObject(null,continuation);
    return superClass.constructInheritanceObjectChain((superObj) -> () -> constructSuperObject((EscmObject)superObj,continuation));
  }


  private Trampoline.Bounce constructObject(EscmObject superObj, ArrayList<Datum> args, Trampoline.Continuation continuation) throws Exception {
    EscmObject obj = new EscmObject(this,superObj,MetaObject.copyProps(objectProps));
    obj.bindImmediateMethodsWithSuper();
    Datum ctor = obj.props.get("new");
    if(ctor != null && ctor instanceof CompoundProcedure) {
      obj.props.remove("new");
      return ((CompoundProcedure)ctor).loadWithSelf(obj).callWith(args,(ignored) -> () -> continuation.run(obj));
    } else if(args.size() > 0) {
      throw new Exceptionf("Class %s construction error: no custom constructor defined!", readableName());
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
  // Loading-into-memory semantics for the VM's interpreter
  public EscmClass loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter

  // NOTE: <ignore> here just distinguishes this private ctor from the public one
  private EscmClass(int ignore, EscmClass superClass, ArrayList<EscmInterface> interfaces, ConcurrentHashMap<String,Datum> props, ConcurrentHashMap<String,Datum> objectProps) throws Exception {
    this.superClass = superClass;
    this.interfaces = interfaces;
    this.props = props;
    this.objectProps = objectProps;
    bindImmediateMethodsWithSuper();
  }


  public EscmClass loadWithName(String name) throws Exception {
    String currentName = name();
    if(currentName.length() > 0) return this;
    EscmClass c = new EscmClass(0,this.superClass,this.interfaces,MetaObject.copyProps(this.props),this.objectProps);
    c.props.put("name",new Symbol(name));
    c.bindImmediateMethodsWithName();
    return c;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public EscmClass copy() {
    return this;
  }
}