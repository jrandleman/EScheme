// Author: Jordan Randleman - escm.type.procedure.types.TypeChecker
// Purpose:
//    EScheme <callable> type annotation checking utility functions.
//    See doc/types.md for more information on using EScheme's types.
//    Provides:
//      1. <Predicate>: Type checking functional interface.
//      2. <getPredicate(key)>: Keyword-to-Predicate type compiler.

package escm.type.procedure.types;
import java.util.ArrayList;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.oo.EscmObject;
import escm.type.procedure.types.TypeTree.ValueGenerator;
import escm.type.oo.EscmClass;
import escm.type.oo.EscmInterface;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.util.Environment;
import escm.vm.util.ObjectAccessChain;
import escm.primitive.FunctionalPrimitives.IsCallable;

public class TypeChecker {
  ////////////////////////////////////////////////////////////////////////////
  // Type Checking Predicate Functional Interface
  public static interface Predicate {
    public boolean check(Environment env, Datum value) throws Exception;
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Parser Predicate Value Generator
  private static class PredicateGenerator implements ValueGenerator<Predicate> {
    /////////////////////////////////////
    // Primitive Predicates
    public Predicate primitiveAny() {
      return (env, value) -> { 
        return true; 
      };
    }

    public Predicate primitiveNum() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Number; 
      };
    }
    public Predicate primitiveInt() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Real && ((escm.type.number.Real)value).isInteger();
      };
    }
    public Predicate primitiveFlo() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Inexact;
      };
    }
    public Predicate primitiveReal() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Real;
      };
    }
    public Predicate primitiveExact() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isExact();
      };
    }
    public Predicate primitiveInexact() {
      return (env, value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isInexact();
      };
    }

    public Predicate primitiveStr() {
      return (env, value) -> { 
        return value instanceof escm.type.String;
      };
    }
    public Predicate primitiveChar() {
      return (env, value) -> { 
        return value instanceof escm.type.Character;
      };
    }
    public Predicate primitiveKey() {
      return (env, value) -> { 
        return value instanceof escm.type.Keyword;
      };
    }
    public Predicate primitiveBool() {
      return (env, value) -> { 
        return value instanceof escm.type.bool.Boolean;
      };
    }
    public Predicate primitiveSym() {
      return (env, value) -> { 
        return value instanceof escm.type.Symbol;
      };
    }
    public Predicate primitiveVoid() {
      return (env, value) -> { 
        return value instanceof escm.type.Void;
      };
    }

    public Predicate primitiveNil() {
      return (env, value) -> { 
        return value instanceof escm.type.Nil;
      };
    }
    public Predicate primitiveAtom() {
      return (env, value) -> { 
        return !(value instanceof escm.type.Pair);
      };
    }

    public Predicate primitiveThread() {
      return (env, value) -> { 
        return value instanceof escm.type.concurrent.Thread;
      };
    }
    public Predicate primitiveMutex() {
      return (env, value) -> { 
        return value instanceof escm.type.concurrent.Mutex;
      };
    }

    public Predicate primitiveFn() {
      return (env, value) -> { 
        return IsCallable.logic(value);
      };
    }
    public Predicate primitiveProcedure() {
      return (env, value) -> { 
        return value instanceof escm.type.procedure.Procedure;
      };
    }
    public Predicate primitiveSyntax() {
      return (env, value) -> { 
        return value instanceof escm.type.procedure.SyntaxProcedure;
      };
    }

    public Predicate primitiveMetaobj() {
      return (env, value) -> { 
        return value instanceof escm.type.oo.MetaObject;
      };
    }
    public Predicate primitiveObject() {
      return (env, value) -> { 
        return value instanceof EscmObject;
      };
    }
    public Predicate primitiveClass() {
      return (env, value) -> { 
        return value instanceof EscmClass;
      };
    }
    public Predicate primitiveInterface() {
      return (env, value) -> { 
        return value instanceof EscmInterface;
      };
    }

    public Predicate primitiveDottable() {
      return (env, value) -> { 
        return value instanceof escm.type.oo.Dottable;
      };
    }
    public Predicate primitiveModule() {
      return (env, value) -> { 
        return value instanceof escm.type.oo.EscmModule;
      };
    }

    public Predicate primitivePort() {
      return (env, value) -> { 
        return value instanceof escm.type.port.Port;
      };
    }
    public Predicate primitiveIport() {
      return (env, value) -> { 
        return value instanceof escm.type.port.InputPort;
      };
    }
    public Predicate primitiveOport() {
      return (env, value) -> { 
        return value instanceof escm.type.port.OutputPort;
      };
    }

    public Predicate primitiveTypeAlias() {
      return (env, value) -> { 
        return value instanceof escm.type.procedure.types.TypeAlias;
      };
    }

    /////////////////////////////////////
    // Container Predicates
    public Predicate containerVec(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { return value instanceof escm.type.Vector; };
      }
      return (env, value) -> { 
        if((value instanceof escm.type.Vector) == false) return false;
        return ((AssociativeCollection)value).containsType(env,valType);
      };
    }
    public Predicate containerMap(Predicate keyType, Predicate valType) {
      if(keyType != null) {
        if(valType != null) {
          return (env, value) -> { 
            if((value instanceof escm.type.Hashmap) == false) return false;
            return ((escm.type.Hashmap)value).containsTypes(env,keyType,valType);
          };
        }
        return (env, value) -> { 
          if((value instanceof escm.type.Hashmap) == false) return false;
          return ((escm.type.Hashmap)value).containsKeyType(env,keyType);
        };
      } else if(valType != null) {
        return (env, value) -> { 
          if((value instanceof escm.type.Hashmap) == false) return false;
          return ((escm.type.Hashmap)value).containsValueType(env,valType);
        };
      } else {
        return (env, value) -> { return value instanceof escm.type.Hashmap; };
      }
    }
    public Predicate containerMap(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { return value instanceof escm.type.Hashmap; };
      }
      return (env, value) -> { 
        if((value instanceof escm.type.Hashmap) == false) return false;
        return ((AssociativeCollection)value).containsType(env,valType);
      };
    }
    public Predicate containerPair(Predicate keyType, Predicate valType) {
      if(keyType != null) {
        if(valType != null) {
          return (env, value) -> { 
            if((value instanceof escm.type.Pair) == false) return false;
            return ((escm.type.Pair)value).containsTypes(env,keyType,valType);
          };
        }
        return (env, value) -> { 
          if((value instanceof escm.type.Pair) == false) return false;
          return ((escm.type.Pair)value).containsKeyType(env,keyType);
        };
      } else if(valType != null) {
        return (env, value) -> {
          if((value instanceof escm.type.Pair) == false) return false;
          return ((escm.type.Pair)value).containsValueType(env,valType);
        };
      } else {
        return (env, value) -> { return value instanceof escm.type.Pair; };
      }
    }
    public Predicate containerPair(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { return value instanceof escm.type.Pair; };
      }
      return (env, value) -> { 
        if((value instanceof escm.type.Pair) == false) return false;
        return ((AssociativeCollection)value).containsType(env,valType);
      };
    }
    public Predicate containerList(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { return escm.type.Pair.isList(value); };
      }
      return (env, value) -> { 
        if(escm.type.Pair.isList(value) == false) return false;
        return ((AssociativeCollection)value).containsType(env,valType);
      };
    }
    public Predicate containerAC(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { 
          return value instanceof AssociativeCollection && !escm.type.Pair.isDottedList(value);
        };
      }
      return (env, value) -> { 
        if((value instanceof AssociativeCollection) == false || escm.type.Pair.isDottedList(value)) return false;
        return ((AssociativeCollection)value).containsType(env,valType);
      };
    }
    public Predicate containerOC(Predicate valType) {
      if(valType == null) {
        return (env, value) -> { 
          return value instanceof OrderedCollection && !escm.type.Pair.isDottedList(value);
        };
      }
      return (env, value) -> { 
        if((value instanceof OrderedCollection) == false || escm.type.Pair.isDottedList(value)) return false;
        return ((OrderedCollection)value).containsType(env,valType);
      };
    }


    /////////////////////////////////////
    // User Type (Class, Interface, Alias) Predicates
    public Predicate userType(String type, String name, Environment defEnv) {
      return (env, value) -> {
        Datum classOrInterfaceOrAlias = env.nullableGet(name);
        if(classOrInterfaceOrAlias == null) {
          throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s",name,type);
        }
        if(classOrInterfaceOrAlias instanceof TypeAlias) {
          return ((TypeAlias)classOrInterfaceOrAlias).check(value);
        } else if((value instanceof EscmObject) == false) {
          return false;
        }
        EscmObject obj = (EscmObject)value;
        if(classOrInterfaceOrAlias instanceof EscmClass) {
          return obj.instanceOf((EscmClass)classOrInterfaceOrAlias);
        } else if(classOrInterfaceOrAlias instanceof EscmInterface) {
          return obj.instanceOf((EscmInterface)classOrInterfaceOrAlias);
        } else {
          throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s",name,type);
        }
      };
    }
    public Predicate moduleUserType(String type, String name, ObjectAccessChain chain, Environment defEnv) {
      return (env, value) -> {
        Datum classOrInterfaceOrAlias = chain.nullableLoadWithState(env);
        if(classOrInterfaceOrAlias == null) {
          throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s",name,type);
        }
        if(classOrInterfaceOrAlias instanceof TypeAlias) {
          return ((TypeAlias)classOrInterfaceOrAlias).check(value);
        } else if((value instanceof EscmObject) == false) {
          return false;
        }
        EscmObject obj = (EscmObject)value;
        if(classOrInterfaceOrAlias instanceof EscmClass) {
          return obj.instanceOf((EscmClass)classOrInterfaceOrAlias);
        } else if(classOrInterfaceOrAlias instanceof EscmInterface) {
          return obj.instanceOf((EscmInterface)classOrInterfaceOrAlias);
        } else {
          throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s",name,type);
        }
      };
    }


    /////////////////////////////////////
    // Compound Type
    public Predicate compound(ArrayList<Predicate> orTypes) {
      return (env, value) -> {
        for(Predicate ot : orTypes) {
          if(ot.check(env,value) == true) return true;
        }
        return false;
      };
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main Predicate Generator
  private static PredicateGenerator PREDICATE_GENERATOR = new PredicateGenerator();

  public static Predicate getPredicate(Keyword type) throws Exception {
    return TypeTree.walk(type,null,PREDICATE_GENERATOR);
  }
}