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
import escm.type.Boolean;
import escm.type.procedure.CompoundProcedure;
import escm.type.Void;
import escm.type.Nil;
import escm.util.Exceptionf;
import escm.util.Pair;
import escm.util.Trampoline;
import escm.vm.type.Primitive;
import escm.vm.type.Callable;
import escm.vm.type.PrimitiveCallable;
import escm.vm.type.ObjectAccessChain;

public class OOPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // escm-oo-class
  public static class EscmOoClass implements Primitive {
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
        throw new Exceptionf("'class <new> constructor must be a procedure (given %s): %s", ctor.profile(), Exceptionf.profileArgs(parameters));
      return (CompoundProcedure)ctor;
    }

    private static ArrayList<String> parsePropertyNames(ArrayList<Datum> parameters, String propType, int parameterIndex) throws Exception {
      Datum nameList = parameters.get(parameterIndex);
      ArrayList<String> names = new ArrayList<String>();
      while(nameList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)nameList;
        Datum name = par.car();
        if(!(name instanceof Symbol))
          throw new Exceptionf("'class %s property name %s isn't a symbol: %s", propType, name.profile(), Exceptionf.profileArgs(parameters));
        names.add(((Symbol)name).value());
        nameList = par.cdr();
      }
      return names;
    }

    private static ArrayList<Datum> parsePropertyValues(ArrayList<Datum> parameters, String propType, int parameterIndex, int totalPropNames) throws Exception {
      Datum valueList = parameters.get(parameterIndex);
      ArrayList<Datum> values = new ArrayList<Datum>();
      while(valueList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)valueList;
        values.add((Datum)par.car());
        valueList = par.cdr();
      }
      if(values.size() != totalPropNames)
        throw new Exceptionf("'class given different number of %s property names and values (%d & %d respectively): %s", propType, totalPropNames, values.size(), Exceptionf.profileArgs(parameters));
      return values;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 7) 
        throw new Exceptionf("'(escm-oo-class <super> <interface-list> <ctor> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list> <instance-prop-value-list>) expects exactly 7 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      EscmClass supr = parseSuper(parameters);
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      CompoundProcedure ctor = parseConstructor(parameters);
      ArrayList<String> staticNames = parsePropertyNames(parameters,"static",3);
      ArrayList<Datum> staticValues = parsePropertyValues(parameters,"static",4,staticNames.size());
      ArrayList<String> instanceNames = parsePropertyNames(parameters,"instance",5);
      ArrayList<Datum> instanceValues = parsePropertyValues(parameters,"instance",6,instanceNames.size());
      // Bind property names to values
      ConcurrentHashMap<String,Datum> staticProps = new ConcurrentHashMap<String,Datum>();
      for(int i = 0, n = staticNames.size(); i < n; ++i) {
        staticProps.put(staticNames.get(i),staticValues.get(i));
      }
      ConcurrentHashMap<String,Datum> instanceProps = new ConcurrentHashMap<String,Datum>();
      for(int i = 0, n = instanceNames.size(); i < n; ++i) {
        instanceProps.put(instanceNames.get(i),instanceValues.get(i));
      }
      if(ctor != null) instanceProps.put("new",ctor);
      // Create the class!
      return new EscmClass(supr,interfaces,staticProps,instanceProps);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-oo-interface
  public static class EscmOoInterface implements Primitive {
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

    private static ArrayList<String> parsePropertyNames(ArrayList<Datum> parameters, String propType, int parameterIndex) throws Exception {
      Datum nameList = parameters.get(parameterIndex);
      ArrayList<String> names = new ArrayList<String>();
      while(nameList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)nameList;
        Datum name = par.car();
        if(!(name instanceof Symbol))
          throw new Exceptionf("'interface %s property name %s isn't a symbol: %s", propType, name.profile(), Exceptionf.profileArgs(parameters));
        names.add(((Symbol)name).value());
        nameList = par.cdr();
      }
      return names;
    }

    private static ArrayList<Datum> parsePropertyValues(ArrayList<Datum> parameters, String propType, int parameterIndex, int totalPropNames) throws Exception {
      Datum valueList = parameters.get(parameterIndex);
      ArrayList<Datum> values = new ArrayList<Datum>();
      while(valueList instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)valueList;
        values.add((Datum)par.car());
        valueList = par.cdr();
      }
      if(values.size() != totalPropNames)
        throw new Exceptionf("'interface given different number of %s property names and values (%d & %d respectively): %s", propType, totalPropNames, values.size(), Exceptionf.profileArgs(parameters));
      return values;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(escm-oo-interface <interface-list> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list>) expects exactly 4 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      ArrayList<String> staticNames = parsePropertyNames(parameters,"static",1);
      ArrayList<Datum> staticValues = parsePropertyValues(parameters,"static",2,staticNames.size());
      ArrayList<String> instanceNames = parsePropertyNames(parameters,"instance",3);
      // Bind property names to values
      ConcurrentHashMap<String,Datum> staticProps = new ConcurrentHashMap<String,Datum>();
      for(int i = 0, n = staticNames.size(); i < n; ++i) {
        staticProps.put(staticNames.get(i),staticValues.get(i));
      }
      // Create the interface!
      return new EscmInterface(interfaces,staticProps,instanceNames);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // meta-object?
  public static class IsMetaObject implements Primitive {
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
  public static class IsInterface implements Primitive {
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
  public static class IsClass implements Primitive {
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
  public static class IsObject implements Primitive {
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
  public static class IsFunctor implements Primitive {
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
  public static class OoIs implements Primitive {
    public java.lang.String escmName() {
      return "oo-is?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum arg1 = parameters.get(0), arg2 = parameters.get(1);
      if(!(arg1 instanceof EscmObject))
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) 1st arg isn't a object: %s", Exceptionf.profileArgs(parameters));
      if(!(arg2 instanceof EscmClass) && !(arg2 instanceof EscmInterface))
        throw new Exceptionf("'(oo-is? <object> <class-or-interface>) 2nd arg isn't a class or interface: %s", Exceptionf.profileArgs(parameters));
      // confirm object is of the class
      if(arg2 instanceof EscmClass) {
        if(((EscmObject)arg1).instanceOf((EscmClass)arg2)) return Boolean.TRUE;
        return Boolean.FALSE;
      // confirm object implements the interface
      } else {
        if(((EscmObject)arg1).instanceOf((EscmInterface)arg2)) return Boolean.TRUE;
        return Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-has?
  public static class OoHas implements Primitive {
    public java.lang.String escmName() {
      return "oo-has?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() < 2) 
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = new ArrayList<Symbol>();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        if(!(parameters.get(i) instanceof Symbol))
          throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) arg #%d isn't a symbol: %s", i+1, Exceptionf.profileArgs(parameters));
        Symbol prop = (Symbol)parameters.get(i);
        if(ObjectAccessChain.is(prop)) {
          accessChain.addAll(ObjectAccessChain.parse(prop));
        } else {
          accessChain.add(prop);
        }
      }
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
  public static class OoGet implements Primitive {
    public java.lang.String escmName() {
      return "oo-get";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() < 2) 
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = new ArrayList<Symbol>();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        if(!(parameters.get(i) instanceof Symbol))
          throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) arg #%d isn't a symbol: %s", i+1, Exceptionf.profileArgs(parameters));
        Symbol prop = (Symbol)parameters.get(i);
        if(ObjectAccessChain.is(prop)) {
          accessChain.addAll(ObjectAccessChain.parse(prop));
        } else {
          accessChain.add(prop);
        }
      }
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
  public static class OoSetBang implements Primitive {
    public java.lang.String escmName() {
      return "oo-set!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() < 3) 
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = new ArrayList<Symbol>();
      for(int i = 1, n = parameters.size(); i < n-1; ++i) {
        if(!(parameters.get(i) instanceof Symbol))
          throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) arg #%d isn't a symbol: %s", i+1, Exceptionf.profileArgs(parameters));
        Symbol prop = (Symbol)parameters.get(i);
        if(ObjectAccessChain.is(prop)) {
          accessChain.addAll(ObjectAccessChain.parse(prop));
        } else {
          accessChain.add(prop);
        }
      }
      Datum newValue = parameters.get(parameters.size()-1);
      // "set!" Logic
      for(int i = 0, n = accessChain.size(); i < n-1; ++i) {
        MetaObject obj = (MetaObject)mo;
        Symbol prop = accessChain.get(i);
        if(!obj.has(prop))
          throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", prop, Exceptionf.profileArgs(parameters));
        mo = obj.get(prop);
        if(!(mo instanceof MetaObject))
          throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s isn't a meta-object: %s", prop, Exceptionf.profileArgs(parameters));
      }
      Symbol lastProp = accessChain.get(accessChain.size()-1);
      MetaObject obj = (MetaObject)mo;
      if(!obj.has(lastProp))
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", lastProp, Exceptionf.profileArgs(parameters));
      obj.set(lastProp,newValue);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-define
  public static class OoDefine implements Primitive {
    public java.lang.String escmName() {
      return "oo-define";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() < 3) 
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum mo = parameters.get(0);
      if(!(mo instanceof MetaObject))
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Symbol> accessChain = new ArrayList<Symbol>();
      for(int i = 1, n = parameters.size(); i < n-1; ++i) {
        if(!(parameters.get(i) instanceof Symbol))
          throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) arg #%d isn't a symbol: %s", i+1, Exceptionf.profileArgs(parameters));
        Symbol prop = (Symbol)parameters.get(i);
        if(ObjectAccessChain.is(prop)) {
          accessChain.addAll(ObjectAccessChain.parse(prop));
        } else {
          accessChain.add(prop);
        }
      }
      Datum newValue = parameters.get(parameters.size()-1);
      // "define" Logic
      for(int i = 0, n = accessChain.size(); i < n-1; ++i) {
        MetaObject obj = (MetaObject)mo;
        Symbol prop = accessChain.get(i);
        if(!obj.has(prop))
          throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", prop, Exceptionf.profileArgs(parameters));
        mo = obj.get(prop);
        if(!(mo instanceof MetaObject))
          throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) object property '%s isn't a meta-object: %s", prop, Exceptionf.profileArgs(parameters));
      }
      Symbol definedProp = accessChain.get(accessChain.size()-1);
      ((MetaObject)mo).define(definedProp,newValue);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-super
  public static class OoSuper implements Primitive {
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
  public static class OoInterfaces implements Primitive {
    public java.lang.String escmName() {
      return "oo-interfaces";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oo-interfaces <meta-object>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof MetaObject))
        throw new Exceptionf("'(oo-interfaces <meta-object>) arg isn't a meta-object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<EscmInterface> interfaces = ((MetaObject)obj).getEscmInterfaces();
      Datum interfaceList = Nil.VALUE;
      for(EscmInterface iface : interfaces) {
        interfaceList = new escm.type.Pair(iface,interfaceList);
      }
      return interfaceList;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-properties
  public static class OoProperties implements Primitive {
    public java.lang.String escmName() {
      return "oo-properties";
    }

    private Datum getObjectProperties(EscmObject obj) {
      HashSet<String> propNames = new HashSet<String>();
      while(obj != null) {
        propNames.addAll(obj.props());
        obj = obj.getSuper();
      }
      Datum propList = Nil.VALUE;
      for(String propName : propNames) {
        propList = new escm.type.Pair(new Symbol(propName),propList);
      }
      return propList;
    }

    private Datum getClassProperties(EscmClass obj) {
      HashSet<String> instancePropNames = new HashSet<String>();
      HashSet<String> staticPropNames = new HashSet<String>();
      while(obj != null) {
        staticPropNames.addAll(obj.props());
        instancePropNames.addAll(obj.instanceProps());
        obj = obj.getSuper();
      }
      Datum propList = Nil.VALUE;
      for(String instancePropName : instancePropNames) {
        propList = new escm.type.Pair(new Symbol(instancePropName),propList);
      }
      for(String staticPropName : staticPropNames) {
        propList = new escm.type.Pair(escm.type.Pair.List(new Keyword("static"),new Symbol(staticPropName)),propList);
      }
      return propList;
    }

    private void getInterfaceProperties_recur(HashSet<String> instancePropNames, HashSet<String> staticPropNames, EscmInterface iface) {
      instancePropNames.addAll(iface.instanceProps());
      staticPropNames.addAll(iface.props());
      for(EscmInterface implemented : iface.getEscmInterfaces())
        getInterfaceProperties_recur(instancePropNames,staticPropNames,implemented);
    }

    private Datum getInterfaceProperties(EscmInterface obj) {
      HashSet<String> instancePropNames = new HashSet<String>();
      HashSet<String> staticPropNames = new HashSet<String>();
      getInterfaceProperties_recur(instancePropNames,staticPropNames,obj);
      Datum propList = Nil.VALUE;
      for(String instancePropName : instancePropNames) {
        propList = new escm.type.Pair(new Symbol(instancePropName),propList);
      }
      for(String staticPropName : staticPropNames) {
        propList = new escm.type.Pair(escm.type.Pair.List(new Keyword("static"),new Symbol(staticPropName)),propList);
      }
      return propList;
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
  public static class EscmOoSuperBang implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "escm-oo-super!";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(escm-oo-super! <object> <param> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof EscmObject))
        throw new Exceptionf("'(escm-oo-super! <object> <param> ...) 1st arg isn't an object: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Datum> args = new ArrayList<Datum>(parameters.subList(1,parameters.size()));
      return ((EscmObject)obj).updateSuper(args,continuation);
    }
  }
}