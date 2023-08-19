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

    // Returns <null> if <name-keyword> DNE
    public static String parseOptionalClassOrInterfaceName(ArrayList<Datum> parameters) {
      Datum nameString = parameters.get(0);
      if(nameString instanceof Keyword) return ((Keyword)nameString).value().substring(1);
      return null;
    }

    // Returns <null> if <super> DNE
    private static EscmClass parseSuper(ArrayList<Datum> parameters) throws Exception {
      Datum supr = parameters.get(1);
      if(supr.eq(Boolean.FALSE)) return null;
      if(!(supr instanceof EscmClass))
        throw new Exceptionf("'class <super> parameter isn't a class (given %s): %s", supr.profile(), Exceptionf.profileArgs(parameters));
      return (EscmClass)supr;
    }

    private static ArrayList<EscmInterface> parseInterfaces(ArrayList<Datum> parameters) throws Exception {
      Datum interfaceList = parameters.get(2);
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
      Datum ctor = parameters.get(3);
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
      if(parameters.size() != 8) 
        throw new Exceptionf("'(escm-oo-class <name-keyword> <super> <interface-list> <ctor> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list> <instance-prop-value-list>) expects exactly 8 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      String className = parseOptionalClassOrInterfaceName(parameters);
      EscmClass supr = parseSuper(parameters);
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      CompoundProcedure ctor = parseConstructor(parameters);
      ConcurrentHashMap<String,Datum> staticProps = parsePropertyNamesAndValues(parameters,"static",4,5);
      ConcurrentHashMap<String,Datum> instanceProps = parsePropertyNamesAndValues(parameters,"instance",6,7);
      if(ctor != null) instanceProps.put("new",ctor);
      // Create the class!
      return new EscmClass(className,supr,interfaces,staticProps,instanceProps);
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
      Datum interfaceList = parameters.get(1);
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

    private static ConcurrentHashMap<String,Datum> parsePropertyNamesAndValues(ArrayList<Datum> parameters) throws Exception {
      Datum nameList = parameters.get(2);
      Datum valueList = parameters.get(3);
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

    private static ArrayList<String> parseInstancePropertyNames(ArrayList<Datum> parameters) throws Exception {
      Datum nameList = parameters.get(4);
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

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 5) 
        throw new Exceptionf("'(escm-oo-interface <name-keyword> <interface-list> <static-prop-name-list> <static-prop-value-list> <instance-prop-name-list>) expects exactly 5 args: %s", Exceptionf.profileArgs(parameters));
      // Parse components
      String interfaceName = EscmOoClass.parseOptionalClassOrInterfaceName(parameters);
      ArrayList<EscmInterface> interfaces = parseInterfaces(parameters);
      ConcurrentHashMap<String,Datum> staticProps = parsePropertyNamesAndValues(parameters);
      ArrayList<String> instanceNames = parseInstancePropertyNames(parameters);
      // Create the interface!
      return new EscmInterface(interfaceName,interfaces,staticProps,instanceNames);
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
      if(parameters.get(0) instanceof MetaObject) return Boolean.TRUE;
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
  // Helper class to abstract <oo-has?> & <oo-get> operations
  private static class OoGetLastItemInAccessChain {
    public static interface MissingLogic {
      public Datum exec(Symbol prop) throws Exception;
    }

    public static interface NonMetaObjectLogic {
      public Datum exec(Symbol prop, Datum propValue) throws Exception;
    }

     public static interface SuccessLogic {
      public Datum exec(Datum finalValue) throws Exception;
    }

    private static Symbol parsePropertySymbol(String primitiveSignature, int i, ArrayList<Datum> parameters) throws Exception {
      Datum d = parameters.get(i);
      if(!(d instanceof Symbol))
        throw new Exceptionf("'%s arg #%d isn't a symbol: %s", primitiveSignature, i+1, Exceptionf.profileArgs(parameters));
      return (Symbol)d;
    }

    public static Datum logic(String primitiveSignature, MetaObject rootObj, ArrayList<Datum> parameters, MissingLogic missing, NonMetaObjectLogic nonMetaObject, SuccessLogic win) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Symbol prop = parsePropertySymbol(primitiveSignature,i,parameters);
        // Process symbols in object-access-chain
        if(ObjectAccessChain.is(prop)) {
          Symbol[] parsedProperties = ObjectAccessChain.parse(prop);
          for(int j = 0; j < parsedProperties.length; ++j) {
            if(!rootObj.has(parsedProperties[j])) return missing.exec(parsedProperties[j]);
            Datum value = rootObj.get(parsedProperties[j]);
            if(i+1 < n || j+1 < parsedProperties.length) {
              if(!(value instanceof MetaObject)) return nonMetaObject.exec(parsedProperties[j],value);
              rootObj = (MetaObject)value;
            } else {
              return win.exec(value);
            }
          }
        // Process immediate symbol
        } else {
          if(!rootObj.has(prop)) return missing.exec(prop);
          Datum value = rootObj.get(prop);
          if(i+1 < n) {
            if(!(value instanceof MetaObject)) return nonMetaObject.exec(prop,value);
            rootObj = (MetaObject)value;
          } else {
            return win.exec(value);
          }
        }
      }
      // unreachable
      throw new Exceptionf("INTERNAL ESCM BUG: '%s couldn't execute properly: %s", primitiveSignature, Exceptionf.profileArgs(parameters));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Helper class to abstract <oo-set!> & <oo-define> operations
  private static class OoMutateLastPropertyInAccessChain {
    public static interface MissingLogic {
      public Datum exec(Symbol prop) throws Exception;
    }

    public static interface NonMetaObjectLogic {
      public Datum exec(Symbol prop, Datum propValue) throws Exception;
    }

    public static interface SuccessLogic {
      public Datum exec(MetaObject obj, Symbol lastProp, Datum newValue) throws Exception;
    }

    private static Symbol parsePropertySymbol(String primitiveSignature, int i, ArrayList<Datum> parameters) throws Exception {
      Datum d = parameters.get(i);
      if(!(d instanceof Symbol))
        throw new Exceptionf("'%s arg #%d isn't a symbol: %s", primitiveSignature, i+1, Exceptionf.profileArgs(parameters));
      return (Symbol)d;
    }

    public static Datum logic(String primitiveSignature, MetaObject rootObj, ArrayList<Datum> parameters, MissingLogic missing, NonMetaObjectLogic nonMetaObject, SuccessLogic win) throws Exception {
      int totalParameters = parameters.size();
      Datum lastValue = parameters.get(totalParameters-1);
      for(int i = 1, n = totalParameters-1; i < n; ++i) {
        Symbol prop = parsePropertySymbol(primitiveSignature,i,parameters);
        // Process symbols in object-access-chain
        if(ObjectAccessChain.is(prop)) {
          Symbol[] parsedProperties = ObjectAccessChain.parse(prop);
          for(int j = 0; j < parsedProperties.length; ++j) {
            if(i+1 < n || j+1 < parsedProperties.length) {
              if(!rootObj.has(parsedProperties[j])) return missing.exec(parsedProperties[j]);
              Datum value = rootObj.get(parsedProperties[j]);
              if(!(value instanceof MetaObject)) return nonMetaObject.exec(parsedProperties[j],value);
              rootObj = (MetaObject)value;
            } else {
              return win.exec(rootObj,parsedProperties[j],lastValue);
            }
          }
        // Process immediate symbol
        } else {
          if(i+1 < n) {
            if(!rootObj.has(prop)) return missing.exec(prop);
            Datum value = rootObj.get(prop);
            if(!(value instanceof MetaObject)) return nonMetaObject.exec(prop,value);
            rootObj = (MetaObject)value;
          } else {
            return win.exec(rootObj,prop,lastValue);
          }
        }
      }
      // unreachable
      throw new Exceptionf("INTERNAL ESCM BUG: '%s couldn't execute properly: %s", primitiveSignature, Exceptionf.profileArgs(parameters));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-has?
  public static class OoHas extends Primitive {
    public java.lang.String escmName() {
      return "oo-has?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum rootObj = parameters.get(0);
      if(!(rootObj instanceof MetaObject))
        throw new Exceptionf("'(oo-has? <meta-object> <property-symbol-name> ...) 1st arg isn't a <meta-object>: %s", Exceptionf.profileArgs(parameters));
      OoGetLastItemInAccessChain.MissingLogic missingCase = (badProperty) -> Boolean.FALSE;
      OoGetLastItemInAccessChain.NonMetaObjectLogic nonDotCase = (badProperty,badValue) -> Boolean.FALSE;
      OoGetLastItemInAccessChain.SuccessLogic winCase = (finalValue) -> Boolean.TRUE;
      return OoGetLastItemInAccessChain.logic("(oo-has? <meta-object> <property-symbol-name> ...)",(MetaObject)rootObj,parameters,missingCase,nonDotCase,winCase);
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
      if(parameters.size() < 2) 
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum rootObj = parameters.get(0);
      if(!(rootObj instanceof MetaObject))
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) 1st arg isn't a <meta-object>: %s", Exceptionf.profileArgs(parameters));
      OoGetLastItemInAccessChain.MissingLogic missingCase = (badProperty) -> {
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) object property '%s doesn't exist: %s", badProperty, Exceptionf.profileArgs(parameters));
      };
      OoGetLastItemInAccessChain.NonMetaObjectLogic nonDotCase = (badProperty,badValue) -> {
        throw new Exceptionf("'(oo-get <meta-object> <property-symbol-name> ...) object property '%s (%s) isn't a <meta-object>: %s", badProperty, badValue.profile(), Exceptionf.profileArgs(parameters));
      };
      OoGetLastItemInAccessChain.SuccessLogic winCase = (finalValue) -> finalValue;
      return OoGetLastItemInAccessChain.logic("(oo-get <meta-object> <property-symbol-name> ...)",(MetaObject)rootObj,parameters,missingCase,nonDotCase,winCase);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-set!
  public static class OoSetBang extends Primitive {
    public java.lang.String escmName() {
      return "oo-set!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // Validate Params
      if(parameters.size() < 3) 
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum rootObj = parameters.get(0);
      if(!(rootObj instanceof MetaObject))
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a <meta-object>: %s", Exceptionf.profileArgs(parameters));
      OoMutateLastPropertyInAccessChain.MissingLogic missingCase = (badProperty) -> {
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", badProperty, Exceptionf.profileArgs(parameters));
      };
      OoMutateLastPropertyInAccessChain.NonMetaObjectLogic nonDotCase = (badProperty,badValue) -> {
        throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s (%s) isn't a <meta-object>: %s", badProperty, badValue.profile(), Exceptionf.profileArgs(parameters));
      };
      OoMutateLastPropertyInAccessChain.SuccessLogic winCase = (obj,lastProp,newValue) -> {
        if(!obj.has(lastProp))
          throw new Exceptionf("'(oo-set! <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", lastProp, Exceptionf.profileArgs(parameters));
        obj.set(lastProp,newValue);
        return Void.VALUE;
      };
      return OoMutateLastPropertyInAccessChain.logic("(oo-set! <meta-object> <property-symbol-name> ... <value>)",(MetaObject)rootObj,parameters,missingCase,nonDotCase,winCase);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oo-define
  public static class OoDefine extends Primitive {
    public java.lang.String escmName() {
      return "oo-define";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 3) 
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum rootObj = parameters.get(0);
      if(!(rootObj instanceof MetaObject))
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) 1st arg isn't a <meta-object>: %s", Exceptionf.profileArgs(parameters));
      OoMutateLastPropertyInAccessChain.MissingLogic missingCase = (badProperty) -> {
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) object property '%s doesn't exist: %s", badProperty, Exceptionf.profileArgs(parameters));
      };
      OoMutateLastPropertyInAccessChain.NonMetaObjectLogic nonDotCase = (badProperty,badValue) -> {
        throw new Exceptionf("'(oo-define <meta-object> <property-symbol-name> ... <value>) object property '%s (%s) isn't a <meta-object>: %s", badProperty, badValue.profile(), Exceptionf.profileArgs(parameters));
      };
      OoMutateLastPropertyInAccessChain.SuccessLogic winCase = (obj,lastProp,newValue) -> {
        obj.define(lastProp,newValue);
        return Void.VALUE;
      };
      return OoMutateLastPropertyInAccessChain.logic("(oo-define <meta-object> <property-symbol-name> ... <value>)",(MetaObject)rootObj,parameters,missingCase,nonDotCase,winCase);
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

    private static Keyword STATIC_KEYWORD = new Keyword("static");

    private Datum getObjectProperties(EscmObject obj) {
      HashSet<String> instanceNames = new HashSet<String>();
      DatumBox propList = new DatumBox(Nil.VALUE);
      while(obj != null) {
        obj.forEachProperty((name) -> {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList.value = new escm.type.Pair(new Symbol(name),propList.value);
          }
          return true;
        });
        obj = obj.getSuper();
      }
      return propList.value;
    }

    private Datum getClassProperties(EscmClass obj) {
      HashSet<String> staticNames = new HashSet<String>();
      HashSet<String> instanceNames = new HashSet<String>();
      DatumBox propList = new DatumBox(Nil.VALUE);
      while(obj != null) {
        obj.forEachProperty((name) -> {
          if(!staticNames.contains(name)) {
            staticNames.add(name);
            propList.value = new escm.type.Pair(escm.type.Pair.List(STATIC_KEYWORD,new Symbol(name)),propList.value);
          }
          return true;
        });
        obj.forEachInstanceProperty((name) -> {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList.value = new escm.type.Pair(new Symbol(name),propList.value);
          }
          return true;
        });
        obj = obj.getSuper();
      }
      return propList.value;
    }

    private Datum getInterfaceProperties(HashSet<String> staticNames, HashSet<String> instanceNames, EscmInterface obj, DatumBox propList) {
      EscmInterface iter = obj;
      while(iter != null) {
        iter.forEachProperty((name) -> {
          if(!staticNames.contains(name)) {
            staticNames.add(name);
            propList.value = new escm.type.Pair(escm.type.Pair.List(STATIC_KEYWORD,new Symbol(name)),propList.value);
          }
          return true;
        });
        iter.forEachInstanceProperty((name) -> {
          if(!instanceNames.contains(name)) {
            instanceNames.add(name);
            propList.value = new escm.type.Pair(new Symbol(name),propList.value);
          }
          return true;
        });
        iter = iter.getSuper();
      }
      obj.forEachInterface((implemented) -> {
        propList.value = getInterfaceProperties(staticNames,instanceNames,implemented,propList);
        return true;
      });
      return propList.value;
    }

    private Datum getInterfaceProperties(EscmInterface obj) {
      HashSet<String> staticNames = new HashSet<String>();
      HashSet<String> instanceNames = new HashSet<String>();
      return getInterfaceProperties(staticNames,instanceNames,obj,new DatumBox(Nil.VALUE));
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