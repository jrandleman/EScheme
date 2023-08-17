// Author: Jordan Randleman - escm.primitive.OOPrimitives
// Purpose:
//    Java primitives for Object Oriented procedures.

package escm.primitive;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.oo.MetaObject;
import escm.type.oo.EscmInterface;
import escm.type.oo.EscmClass;
import escm.type.oo.EscmObject;
import escm.type.Symbol;
import escm.type.Keyword;
import escm.type.bool.Boolean;
import escm.type.procedure.CompoundProcedure;
import escm.type.Void;
import escm.type.Nil;
import escm.util.Exceptionf;
import escm.util.Pair;
import escm.util.Trampoline;
import escm.vm.type.Primitive;
import escm.vm.type.Callable;
import escm.vm.type.PrimitiveCallable;
import escm.vm.util.ObjectAccessChain;

public class OOPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // escm-oo-class
  public static class EscmOoClass extends Primitive {
    public java.lang.String escmName() {
      return "escm-oo-class";
    }

    // Returns <null> if <super> DNE
    private static EscmClass parseSuper(ArrayList<Datum> parameters) throws Exception {
      Datum supr = parameters.get(0);
      if(supr.eq(Boolean.FALSE)) return null;
      if(!(supr instanceof EscmClass))
        throw new Exceptionf("'class <super> parameter isn't a class (given %s): %s", supr.profile(), Exceptionf.profileArgs(parameters));
      return (EscmClass)supr;
    }

    private static ArrayList<EscmInterface> parseInterfaces(ArrayList<Datum> parameters) throws Exception {
      Datum interfaceList = parameters.get(1);
      ArrayList<EscmInterface> interfaces = new ArrayList<EscmInterface>();
      while(interfaceList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)interfaceList;
        Datum iface = par.car();
        if(!(iface instanceof EscmInterface))
          throw new Exceptionf("'class :implements value %s isn't an interface: %s", iface.profile(), Exceptionf.profileArgs(parameters));
        interfaces.add((EscmInterface)iface);
        interfaceList = par.cdr();
      }
      return interfaces;
    }

    // Returns <null> if constructor DNE
    private static CompoundProcedure parseConstructor(ArrayList<Datum> parameters) throws Exception {
      Datum ctor = parameters.get(2);
      if(ctor.eq(Boolean.FALSE)) return null;
      if(!(ctor instanceof CompoundProcedure))
        throw new Exceptionf("'class <new> constructor must be a custom procedure (given %s): %s", ctor.profile(), Exceptionf.profileArgs(parameters));
      return (CompoundProcedure)ctor;
    }

    private static ConcurrentHashMap<String,Datum> parsePropertyNamesAndValues(ArrayList<Datum> parameters, String propType, int nameIndex, int valIndex) throws Exception {
      Datum nameList = parameters.get(nameIndex);
      Datum valueList = parameters.get(valIndex);
      ConcurrentHashMap<String,Datum> namesAndValues = new ConcurrentHashMap<String,Datum>();
      while(nameList instanceof escm.type.Pair && valueList instanceof escm.type.Pair) {
        escm.type.Pair np = (escm.type.Pair)nameList;
        escm.type.Pair vp = (escm.type.Pair)valueList;
        Datum name = np.car();
        if(!(name instanceof Symbol))
          throw new Exceptionf("'class %s property name %s isn't a symbol: %s", propType, name.profile(), Exceptionf.profileArgs(parameters));
        namesAndValues.put(((Symbol)name).value(),vp.car());
        nameList = np.cdr();
        valueList = vp.cdr();
      }
      if(nameList instanceof escm.type.Pair || valueList instanceof escm.type.Pair)
        throw new Exceptionf("'class given different number of %s property names and values: %s", propType, Exceptionf.profileArgs(parameters));
      return namesAndValues;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 7) 
        throw new Exceptionf("'(escm-oo-class <super> <interface-list> <ctor> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list> <instance-prop-value-list>) expects exactly 7 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      EscmClass supr = parseSuper(parameters);
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      CompoundProcedure ctor = parseConstructor(parameters);
      ConcurrentHashMap<String,Datum> staticProps = parsePropertyNamesAndValues(parameters,"static",3,4);
      ConcurrentHashMap<String,Datum> instanceProps = parsePropertyNamesAndValues(parameters,"instance",5,6);
      if(ctor != null) instanceProps.put("new",ctor);
      // Create the class!
      return new EscmClass(supr,interfaces,staticProps,instanceProps);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-oo-interface
  public static class EscmOoInterface extends Primitive {
    public java.lang.String escmName() {
      return "escm-oo-interface";
    }

    // Returns <null> if <super> DNE
    private static ArrayList<EscmInterface> parseInterfaces(ArrayList<Datum> parameters) throws Exception {
      Datum interfaceList = parameters.get(0);
      ArrayList<EscmInterface> interfaces = new ArrayList<EscmInterface>();
      while(interfaceList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)interfaceList;
        Datum iface = par.car();
        if(!(iface instanceof EscmInterface))
          throw new Exceptionf("'interface :implements value %s isn't an interface: %s", iface.profile(), Exceptionf.profileArgs(parameters));
        interfaces.add((EscmInterface)iface);
        interfaceList = par.cdr();
      }
      return interfaces;
    }

    private static ArrayList<String> parseInstancePropertyNames(ArrayList<Datum> parameters) throws Exception {
      Datum nameList = parameters.get(3);
      ArrayList<String> names = new ArrayList<String>();
      while(nameList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)nameList;
        Datum name = par.car();
        if(!(name instanceof Symbol))
          throw new Exceptionf("'interface instance property name %s isn't a symbol: %s", name.profile(), Exceptionf.profileArgs(parameters));
        names.add(((Symbol)name).value());
        nameList = par.cdr();
      }
      return names;
    }

    private static ConcurrentHashMap<String,Datum> parsePropertyNamesAndValues(ArrayList<Datum> parameters) throws Exception {
      Datum nameList = parameters.get(1);
      Datum valueList = parameters.get(2);
      ConcurrentHashMap<String,Datum> namesAndValues = new ConcurrentHashMap<String,Datum>();
      while(nameList instanceof escm.type.Pair && valueList instanceof escm.type.Pair) {
        escm.type.Pair np = (escm.type.Pair)nameList;
        escm.type.Pair vp = (escm.type.Pair)valueList;
        Datum name = np.car();
        if(!(name instanceof Symbol))
          throw new Exceptionf("'interface static property name %s isn't a symbol: %s", name.profile(), Exceptionf.profileArgs(parameters));
        namesAndValues.put(((Symbol)name).value(),vp.car());
        nameList = np.cdr();
        valueList = vp.cdr();
      }
      if(nameList instanceof escm.type.Pair || valueList instanceof escm.type.Pair)
        throw new Exceptionf("'interface given different number of static property names and values: %s", Exceptionf.profileArgs(parameters));
      return namesAndValues;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(escm-oo-interface <interface-list> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list>) expects exactly 4 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      ConcurrentHashMap<String,Datum> staticProps = parsePropertyNamesAndValues(parameters);
      ArrayList<String> instanceNames = parseInstancePropertyNames(parameters);
      // Create the interface!
      return new EscmInterface(interfaces,staticProps,instanceNames);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // meta-object?
  public static class IsMetaObject extends Primitive {
    public java.lang.String escmName() {
      return "meta-object?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(meta-object? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.get(0) instanceof escm.type.oo.MetaObject) return Boolean.TRUE;
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // interface?
  public static class IsInterface extends Primitive {
    public java.lang.String escmName() {
      return "interface?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(interface? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.get(0) instanceof EscmInterface) return Boolean.TRUE;
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // class?
  public static class IsClass extends Primitive {
    public java.lang.String escmName() {
      return "class?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(class? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.get(0) instanceof EscmClass) return Boolean.TRUE;
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // object?
  public static class IsObject extends Primitive {
    public java.lang.String escmName() {
      return "object?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(object? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.get(0) instanceof EscmObject) return Boolean.TRUE;
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // functor?
  public static class IsFunctor extends Primitive {
    public java.lang.String escmName() {
      return "functor?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(functor? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(obj instanceof EscmObject && ((EscmObject)obj).isFunctor()) return Boolean.TRUE;
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-is?
  public static class OoIs extends Primitive {
    public java.lang.String escmName() {
      return "oo-is?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum arg1 = parameters.get(0), arg2 = parameters.get(1);
      if(!(arg1 instanceof EscmObject))
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) 1st arg isn't a object: %s", Exceptionf.profileArgs(parameters));
      // confirm object is of the class
      if(arg2 instanceof EscmClass) {
        return Boolean.valueOf(((EscmObject)arg1).instanceOf((EscmClass)arg2));
      // confirm object implements the interface
      } else if(arg2 instanceof EscmInterface) {
        return Boolean.valueOf(((EscmObject)arg1).instanceOf((EscmInterface)arg2));
      // invalid 2nd <oo-is> arg
      } else {
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) 2nd arg isn't a class or interface: %s", Exceptionf.profileArgs(parameters));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-has?
  public static class OoHas extends Primitive {
    public java.lang.String escmName() {
      return "oo-has?";
    }

    public static ArrayList<Symbol> parseAccessChain(String primitiveSignature, int totalAccessChainParams, ArrayList<Datum> parameters) throws Exception {
      ArrayList<Symbol> accessChain = new ArrayList<Symbol>();
      for(int i = 1; i < totalAccessChainParams; ++i) {
        Datum d = parameters.get(i);
        if(!(d instanceof Symbol))
          throw new Exceptionf("'%s arg #%d isn't a symbol: %s", primitiveSignature, i+1, Exceptionf.profileArgs(parameters));
        Symbol prop = (Symbol)d;
        if(ObjectAccessChain.is(prop)) {
          for(Symbol p : ObjectAccessChain.parse(prop)) {
            accessChain.add(p);
          }
        } else {
          accessChain.add(prop);
        }
      }
      return accessChain;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      int totalParameters = parameters.size();
      if(totalParameters < 2) 
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = parseAccessChain("(oo-has? <meta-object> <property-symbol-name> ...)",totalParameters,parameters);
      // "has?" Logic
      MetaObject obj = (MetaObject)mo;
      for(int i = 0, n = accessChain.size(); i < n; ++i) {
        Symbol prop = accessChain.get(i);
        if(!obj.has(prop)) return Boolean.FALSE;
        Datum val = obj.get(prop);
        if(i+1 < n) {
          if(!(val instanceof MetaObject)) return Boolean.FALSE;
          obj = (MetaObject)val;
        }
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-get
  public static class OoGet extends Primitive {
    public java.lang.String escmName() {
      return "oo-get";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      int totalParameters = parameters.size();
      if(totalParameters < 2) 
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = OoHas.parseAccessChain("(oo-get <meta-object> <property-symbol-name> ...)",totalParameters,parameters);
      // "get" Logic
      for(int i = 0, n = accessChain.size(); i < n; ++i) {
        MetaObject obj = (MetaObject)mo;
        Symbol prop = accessChain.get(i);
        if(!obj.has(prop))
          throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) object property '%s doesn't exist: %s", prop, Exceptionf.profileArgs(parameters));
        mo = obj.get(prop);
        if(i+1 < n && !(mo instanceof MetaObject))
          throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) object property '%s isn't a meta-object: %s", prop, Exceptionf.profileArgs(parameters));
      }
      return mo;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-set!
  public static class OoSetBang extends Primitive {
    public java.lang.String escmName() {
      return "oo-set!";
    }

    public static Symbol getLastProp(String primitiveName, Datum mo, ArrayList<Symbol> accessChain, ArrayList<Datum> parameters) throws Exception {
      int n = accessChain.size()-1;
      for(int i = 0; i < n; ++i) {
        MetaObject obj = (MetaObject)mo;
        Symbol prop = accessChain.get(i);
        if(!obj.has(prop))
          throw new Exceptionf("'(%s <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", primitiveName, prop, Exceptionf.profileArgs(parameters));
        mo = obj.get(prop);
        if(!(mo instanceof MetaObject))
          throw new Exceptionf("'(%s <meta-object> <property-symbol-name> ... <value>) object property '%s isn't a meta-object: %s", primitiveName, prop, Exceptionf.profileArgs(parameters));
      }
      return accessChain.get(n);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      int totalParameters = parameters.size();
      if(totalParameters < 3) 
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = OoHas.parseAccessChain("(oo-set! <meta-object> <property-symbol-name> ... <value>)",totalParameters-1,parameters);
      // "set!" Logic
      Symbol lastProp = getLastProp("oo-set!",mo,accessChain,parameters);
      MetaObject obj = (MetaObject)mo;
      if(!obj.has(lastProp))
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", lastProp, Exceptionf.profileArgs(parameters));
      obj.set(lastProp,parameters.get(totalParameters-1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-define
  public static class OoDefine extends Primitive {
    public java.lang.String escmName() {
      return "oo-define";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      int totalParameters = parameters.size();
      if(totalParameters < 3) 
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = OoHas.parseAccessChain("(oo-define <meta-object> <property-symbol-name> ... <value>)",totalParameters-1,parameters);
      // "define" Logic
      Symbol definedProp = OoSetBang.getLastProp("oo-define",mo,accessChain,parameters);
      ((MetaObject)mo).define(definedProp,parameters.get(totalParameters-1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-super
  public static class OoSuper extends Primitive {
    public java.lang.String escmName() {
      return "oo-super";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oo-super <class-or-object>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof EscmObject) && !(obj instanceof EscmClass))
        throw new Exceptionf("'(oo-super <class-or-object>) arg isn't a class or object: %s", Exceptionf.profileArgs(parameters));
      Datum supr = ((MetaObject)obj).getSuper();
      if(supr == null) return Boolean.FALSE;
      return supr;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-interfaces
  private static class DatumBox {
    public Datum value = Nil.VALUE;
    public DatumBox(Datum val) {
      value = val;
    }
  }


  public static class OoInterfaces extends Primitive {
    public java.lang.String escmName() {
      return "oo-interfaces";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oo-interfaces <meta-object>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof MetaObject))
        throw new Exceptionf("'(oo-interfaces <meta-object>) arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      DatumBox db = new DatumBox(Nil.VALUE);
      ((MetaObject)obj).forEachInterface((iface) -> {
        db.value = new escm.type.Pair(iface,db.value);
        return true;
      });
      return db.value;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-properties
  public static class OoProperties extends Primitive {
    public java.lang.String escmName() {
      return "oo-properties";
    }

    private Datum getObjectProperties(EscmObject obj) {
      HashSet<String> instanceNames = new HashSet<String>();
      Datum propList = Nil.VALUE;
      while(obj != null) {
        for(String name : obj.props()) {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList = new escm.type.Pair(new Symbol(name),propList);
          }
        }
        obj = obj.getSuper();
      }
      return propList;
    }

    private Datum getClassProperties(EscmClass obj) {
      HashSet<String> staticNames = new HashSet<String>();
      HashSet<String> instanceNames = new HashSet<String>();
      Datum propList = Nil.VALUE;
      while(obj != null) {
        for(String name : obj.props()) {
          if(!staticNames.contains(name)) {
            staticNames.add(name);
            propList = new escm.type.Pair(escm.type.Pair.List(new Keyword("static"),new Symbol(name)),propList);
          }
        }
        for(String name : obj.instanceProps()) {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList = new escm.type.Pair(new Symbol(name),propList);
          }
        }
        obj = obj.getSuper();
      }
      return propList;
    }

    private Datum getInterfaceProperties(HashSet<String> staticNames, HashSet<String> instanceNames, EscmInterface obj, Datum propList) {
      EscmInterface iter = obj;
      while(iter != null) {
        for(String name : iter.props()) {
          if(!staticNames.contains(name)) {
            staticNames.add(name);
            propList = new escm.type.Pair(escm.type.Pair.List(new Keyword("static"),new Symbol(name)),propList);
          }
        }
        for(String name : iter.instanceProps()) {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList = new escm.type.Pair(new Symbol(name),propList);
          }
        }
        iter = iter.getSuper();
      }
      DatumBox db = new DatumBox(propList);
      obj.forEachInterface((implemented) -> {
        db.value = getInterfaceProperties(staticNames,instanceNames,implemented,db.value);
        return true;
      });
      return db.value;
    }

    private Datum getInterfaceProperties(EscmInterface obj) {
      HashSet<String> staticNames = new HashSet<String>();
      HashSet<String> instanceNames = new HashSet<String>();
      Datum propList = Nil.VALUE;
      return getInterfaceProperties(staticNames,instanceNames,obj,propList);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oo-properties <meta-object>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof MetaObject))
        throw new Exceptionf("'(oo-properties <meta-object>) arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      if(obj instanceof EscmObject) {
        return getObjectProperties((EscmObject)obj);
      } else if(obj instanceof EscmClass) {
        return getClassProperties((EscmClass)obj);
      } else { // if(obj instanceof EscmInterface) {
        return getInterfaceProperties((EscmInterface)obj);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-oo-super!
  public static class EscmOoSuperBang extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "escm-oo-super!";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(escm-oo-super! <object> <param> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof EscmObject))
        throw new Exceptionf("'(escm-oo-super! <object> <param> ...) 1st arg isn't an object: %s", Exceptionf.profileArgs(parameters));
      parameters.remove(0);
      return ((EscmObject)obj).updateSuper(parameters,continuation);
    }
  }
}