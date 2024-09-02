// Author: Jordan Randleman - escm.type.procedure.TypeChecker
// Purpose:
//    EScheme <callable> type annotation utility functions.

package escm.type.procedure;
import java.util.ArrayList;
import escm.primitive.FunctionalPrimitives.IsCallable;
import escm.type.Datum;
import escm.util.Pair;
import escm.util.error.Exceptionf;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.util.Environment;

public class TypeChecker {
  ////////////////////////////////////////////////////////////////////////////
  // Type checking predicate functional interface.
  public static interface Predicate {
    public boolean check(Datum value) throws Exception;
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Parse Primitive Types
  private static Pair<Predicate,Integer> parsePrimitive(String type, int i) {
    // Any type
    if(type.startsWith("any",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return true; 
      },i+3);
    }
    
    // Numeric types
    if(type.startsWith("number",i) || type.startsWith("complex",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Number;
      },i+(type.charAt(i) == 'n' ? 6 : 7));
    }
    if(type.startsWith("int",i)) { // no exactness guarentee!
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Real && ((escm.type.number.Real)value).isInteger();
      },i+3);
    }
    if(type.startsWith("flo",i)) { // Flonum
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Inexact;
      },i+3);
    }
    if(type.startsWith("real",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Real;
      },i+4);
    }
    if(type.startsWith("exact",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isExact();
      },i+5);
    }
    if(type.startsWith("inexact",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isInexact();
      },i+7);
    }

    // Common atomic types
    if(type.startsWith("string",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.String;
      },i+6);
    }
    if(type.startsWith("char",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.Character;
      },i+4);
    }
    if(type.startsWith("key",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.Keyword;
      },i+3);
    }
    if(type.startsWith("bool",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.bool.Boolean;
      },i+4);
    }
    if(type.startsWith("symbol",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.Symbol;
      },i+6);
    }
    if(type.startsWith("void",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.Void;
      },i+4);
    }

    // List-base types
    if(type.startsWith("nil",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.Nil;
      },i+3);
    }
    if(type.startsWith("atom",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return !(value instanceof escm.type.Pair);
      },i+4);
    }

    // Concurrency types
    if(type.startsWith("thread",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.concurrent.Thread;
      },i+6);
    }
    if(type.startsWith("mutex",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.concurrent.Mutex;
      },i+5);
    }

    // Functional types
    if(type.startsWith("fn",i)) { // Callable
      return new Pair<Predicate,Integer>((value) -> { 
        return IsCallable.logic(value);
      },i+2);
    }
    if(type.startsWith("procedure",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.procedure.Procedure;
      },i+9);
    }
    if(type.startsWith("syntax",i)) {
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.procedure.SyntaxProcedure;
      },i+6);
    }

    // Object-oriented types
    if(type.startsWith("metaobj",i)) { // includes modules!
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.oo.Dottable;
      },i+7);
    }
    if(type.startsWith("object",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.oo.EscmObject;
      },i+6);
    }
    if(type.startsWith("class",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.oo.EscmClass;
      },i+5);
    }
    if(type.startsWith("interface",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.oo.EscmInterface;
      },i+9);
    }

    // I/O types
    if(type.startsWith("port",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.port.Port;
      },i+4);
    }
    if(type.startsWith("inport",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.port.InputPort;
      },i+6);
    }
    if(type.startsWith("outport",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.port.OutputPort;
      },i+7);
    }

    // Module types
    if(type.startsWith("module",i)) { 
      return new Pair<Predicate,Integer>((value) -> { 
        return value instanceof escm.type.oo.EscmModule;
      },i+6);
    }

    // Non-primitive types
    return null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Container Types
  private static boolean parsedAnyParameter(String type, int startIndex, int endIndex) {
    return startIndex+3 == endIndex && type.startsWith("any",startIndex);
  }

  private static Pair<Predicate,Integer> parseContainer(Environment env, String type, int typeLength, int i) throws Exception{
    // Common compound types
    // - Vector
    if(type.startsWith("vector",i)) { 
      int next = i+6;
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"vector<\" (index %d): %s",next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return value instanceof escm.type.Vector;
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if((value instanceof escm.type.Vector) == false) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return value instanceof escm.type.Vector;
        },next);
      }
    // - Map
    } else if(type.startsWith("map",i)) { 
      int next = i+3;
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength) {
          throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
        }
        // Double Parameters
        if(type.charAt(parameterType.second) == ',') {
          Pair<Predicate,Integer> valueParameterType = parseType(env,type,typeLength,parameterType.second+1);
          if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",parameterType.second+1,type);
          }
          // <any,any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second) && parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) {
            return new Pair<Predicate,Integer>((value) -> { 
              return value instanceof escm.type.Hashmap;
            },valueParameterType.second+1);
          }
          return new Pair<Predicate,Integer>((value) -> { 
            if((value instanceof escm.type.Hashmap) == false) {
              return false;
            }
            return ((escm.type.Hashmap)value).containsTypes(parameterType.first,valueParameterType.first);
          },valueParameterType.second+1);
        } 
        // Single Parameter
        if(type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return value instanceof escm.type.Hashmap;
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if((value instanceof escm.type.Hashmap) == false) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return value instanceof escm.type.Hashmap;
        },next);
      }

    // List-base types
    // - Pair
    } else if(type.startsWith("pair",i)) { 
      int next = i+4;
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength) {
          throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
        }
        // Double Parameters
        if(type.charAt(parameterType.second) == ',') {
          Pair<Predicate,Integer> valueParameterType = parseType(env,type,typeLength,parameterType.second+1);
          if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",parameterType.second+1,type);
          }
          // <any,any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second) && parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) {
            return new Pair<Predicate,Integer>((value) -> { 
              return value instanceof escm.type.Pair;
            },valueParameterType.second+1);
          }
          return new Pair<Predicate,Integer>((value) -> { 
            if((value instanceof escm.type.Pair) == false) {
              return false;
            }
            return ((escm.type.Pair)value).containsTypes(parameterType.first,valueParameterType.first);
          },valueParameterType.second+1);
        } 
        // Single Parameter
        if(type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return value instanceof escm.type.Pair;
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if((value instanceof escm.type.Pair) == false) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return value instanceof escm.type.Pair;
        },next);
      }
    // - List
    } else if(type.startsWith("list",i)) { 
      int next = i+4;
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"list<\" (index %d): %s",next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return escm.type.Pair.isList(value);
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if(escm.type.Pair.isList(value) == false) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return escm.type.Pair.isList(value);
        },next);
      }

    // Generic compound types
    // - Associative Collection
    } else if(type.startsWith("associative-collection",i) || type.startsWith("ac",i)) {
      int next = i+(type.charAt(i+1) == 'c' ? 2 : 22);
      String name = type.charAt(i+1) == 'c' ? "ac" : "associative-collection";
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return value instanceof AssociativeCollection && !escm.type.Pair.isDottedList(value);
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if((value instanceof AssociativeCollection) == false || escm.type.Pair.isDottedList(value)) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return value instanceof AssociativeCollection && !escm.type.Pair.isDottedList(value);
        },next);
      }
    // - Ordered Collection
    } else if(type.startsWith("ordered-collection",i) || type.startsWith("oc",i)) {
      int next = i+(type.charAt(i+1) == 'c' ? 2 : 18);
      String name = type.charAt(i+1) == 'c' ? "oc" : "ordered-collection";
      // Parameterized
      if(next < typeLength && type.charAt(next) == '<') {
        Pair<Predicate,Integer> parameterType = parseType(env,type,typeLength,next+1);
        if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
          throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
        }
        // <any> parameter
        if(parsedAnyParameter(type,next+1,parameterType.second)) {
          return new Pair<Predicate,Integer>((value) -> { 
            return value instanceof escm.vm.type.collection.OrderedCollection && !escm.type.Pair.isDottedList(value);
          },parameterType.second+1);
        }
        return new Pair<Predicate,Integer>((value) -> { 
          if((value instanceof escm.vm.type.collection.OrderedCollection) == false || escm.type.Pair.isDottedList(value)) {
            return false;
          }
          return ((AssociativeCollection)value).containsType(parameterType.first);
        },parameterType.second+1);
      // No Parameter
      } else {
        return new Pair<Predicate,Integer>((value) -> { 
          return value instanceof escm.vm.type.collection.OrderedCollection && !escm.type.Pair.isDottedList(value);
        },next);
      }
    
    // Non-container types
    } else {
      return null;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Class/Interface Types
  private static boolean isTypeDelimiter(char c) {
    return c == '|' || c == '<' || c == '>' || c == ',';
  }

  private static Pair<Predicate,Integer> parseClassOrInterface(Environment env, String type, int typeLength, int i) {
    int idx = i;
    while(idx < typeLength && !isTypeDelimiter(type.charAt(idx))) {
      ++idx;
    }
    String className = type.substring(i,idx);
    return new Pair<Predicate,Integer>((value) -> {
      Datum classOrInterface = env.nullableGet(className);
      if(classOrInterface == null) {
        throw new Exceptionf("Invalid Class or Interface Type \"%s\" (index %d): %s",className,i,type);
      }
      if((value instanceof escm.type.oo.EscmObject) == false) return false;
      escm.type.oo.EscmObject obj = (escm.type.oo.EscmObject)value;
      if(classOrInterface instanceof escm.type.oo.EscmClass) {
        return obj.instanceOf((escm.type.oo.EscmClass)classOrInterface);
      }
      if(classOrInterface instanceof escm.type.oo.EscmInterface) {
        return obj.instanceOf((escm.type.oo.EscmInterface)classOrInterface);
      }
      return false;
    }, idx);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Compound "|" Types
  private static Pair<Predicate,Integer> parseRest(Environment env, String type, int typeLength, int i, Pair<Predicate,Integer> firstType) throws Exception {
    if(firstType.second >= typeLength) return firstType;
    char delimiter = type.charAt(firstType.second);
    if(delimiter == '>' || delimiter == ',') return firstType;
    if(delimiter == '|') {
      ArrayList<Predicate> orTypes = new ArrayList<Predicate>();
      orTypes.add(firstType.first);
      int idx = firstType.second;
      while(idx < typeLength && type.charAt(idx) == '|') {
        Pair<Predicate,Integer> orType = parseType(env,type,typeLength,idx+1);
        orTypes.add(orType.first);
        idx = orType.second;
      }
      return new Pair<Predicate,Integer>((value) -> {
        for(Predicate ot : orTypes) {
          if(ot.check(value) == true) return true;
        }
        return false;
      }, idx);
    }
    throw new Exceptionf("Invalid Compound Type (index %d): %s",i,type);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type Parsing Handle
  // TYPE ::=
  //   | primitiveREST
  //   | container<TYPE>REST
  //   | container<TYPE,TYPE>REST
  //   | classREST
  //   | interfaceREST
  // REST ::=
  //   | ε
  //   | >
  //   | ,
  //   | |TYPE
  private static Pair<Predicate,Integer> parseType(Environment env, String type, int typeLength, int index) throws Exception {
    if(index >= typeLength) {
      throw new Exceptionf("Invalid Empty Type (index %d): %s",index,type);
    }
    Pair<Predicate,Integer> p = parsePrimitive(type,index);
    if(p == null) {
      p = parseContainer(env,type,typeLength,index);
      if(p == null) {
        p = parseClassOrInterface(env,type,typeLength,index);
      }
    }
    if(p == null) {
      throw new Exceptionf("Invalid Type (index %d): %s",index,type);
    }
    return parseRest(env,type,typeLength,index,p);
  }

  public static Predicate getTypePredicate(Environment env, String type) throws Exception {
    return parseType(env,type,type.length(),0).first;
  }
}