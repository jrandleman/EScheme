// Author: Jordan Randleman - escm.type.procedure.TypeChecker
// Purpose:
//    EScheme <callable> type annotation utility functions.
//    Provides:
//      1. <Predicate>: Type checking functional interface.
//      2. <getPredicate(key)>: Keyword-to-Predicate type compiler.


//////////////////////////////////////////////////////////////////////////////
// SYNTAX OVERVIEW
//  EScheme denotes types with keywords, and "compound types" via "|" syntax.
//    * EX: ":string|number" represents either a string or a number.
//
//  EScheme types are typically either a "primitive" or "container" type.
//  If a type is neither a primitive nor a container, it is presumed to
//  represent some class or interface: if the type doesn't resolve to a valid
//  class or interface during a runtime type-check, an error is thrown.
//
//  EScheme types are parsed and converted to predicates during compilation, 
//  with the predicates being applied at runtime.
//
//  EScheme's primitive types are as follows:
/**
:any

:number ; aliased by ":complex"
:int
:flo
:real
:exact
:inexact

:string
:char
:key
:bool
:symbol
:void

:thread
:mutex

:nil
:atom

:fn ; all callables
:procedure
:syntax

:metaobj ; includes modules
:object
:class
:interface

:port
:inport
:outport

:module
 */


// EScheme types support the following containers:
//   * Note: All containers may be parameterized by adding the "<type>" suffix.
//     - EX: ":list<string|symbol>" is a list where each element is either a
//       string or symbol.
//       * For either a list that only has strings OR a list that only has
//         symbols, use ":list<string>|list<symbol>".
//     - Furthermore, ":pair" and ":map" may also be parameterized with the
//       "<type,type>" suffix in order to type-check their keys and values.
 /**
:vector
:map

:pair
:list

:associative-collection ; aliased by ":ac"
:ordered-collection ; aliased by ":oc"
  */


package escm.type.procedure;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Pair;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.oo.EscmObject;
import escm.type.oo.EscmClass;
import escm.type.oo.EscmInterface;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.util.Environment;
import escm.primitive.FunctionalPrimitives.IsCallable;

public class TypeChecker {
  ////////////////////////////////////////////////////////////////////////////
  // Type Checking Predicate Functional Interface
  @FunctionalInterface
  public static interface Predicate {
    public boolean check(Environment env, Datum value) throws Exception;
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Parse Primitive Types
  private static ConcurrentHashMap<String,Predicate> PRIMITIVE_TYPES = new ConcurrentHashMap<String,Predicate>();

  static {
    // Any type
    PRIMITIVE_TYPES.put("any",(env, value) -> { 
      return true; 
    });

    // Numeric types
    PRIMITIVE_TYPES.put("number",(env, value) -> { 
      return value instanceof escm.type.number.Number;
    });
    PRIMITIVE_TYPES.put("complex",(env, value) -> { 
      return value instanceof escm.type.number.Number;
    });
    PRIMITIVE_TYPES.put("int",(env, value) -> { 
      return value instanceof escm.type.number.Real && ((escm.type.number.Real)value).isInteger();
    });
    PRIMITIVE_TYPES.put("flo",(env, value) -> { 
      return value instanceof escm.type.number.Inexact;
    });
    PRIMITIVE_TYPES.put("real",(env, value) -> { 
      return value instanceof escm.type.number.Real;
    });
    PRIMITIVE_TYPES.put("exact",(env, value) -> { 
      return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isExact();
    });
    PRIMITIVE_TYPES.put("inexact",(env, value) -> { 
      return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isInexact();
    });

    // Common atomic types
    PRIMITIVE_TYPES.put("string",(env, value) -> { 
      return value instanceof escm.type.String;
    });
    PRIMITIVE_TYPES.put("char",(env, value) -> { 
      return value instanceof escm.type.Character;
    });
    PRIMITIVE_TYPES.put("key",(env, value) -> { 
      return value instanceof escm.type.Keyword;
    });
    PRIMITIVE_TYPES.put("bool",(env, value) -> { 
      return value instanceof escm.type.bool.Boolean;
    });
    PRIMITIVE_TYPES.put("symbol",(env, value) -> { 
      return value instanceof escm.type.Symbol;
    });
    PRIMITIVE_TYPES.put("void",(env, value) -> { 
      return value instanceof escm.type.Void;
    });

    // List-base types
    PRIMITIVE_TYPES.put("nil",(env, value) -> { 
      return value instanceof escm.type.Nil;
    });
    PRIMITIVE_TYPES.put("atom",(env, value) -> { 
      return !(value instanceof escm.type.Pair);
    });

    // Concurrency types
    PRIMITIVE_TYPES.put("thread",(env, value) -> { 
      return value instanceof escm.type.concurrent.Thread;
    });
    PRIMITIVE_TYPES.put("mutex",(env, value) -> { 
      return value instanceof escm.type.concurrent.Mutex;
    });

    // Functional types
    PRIMITIVE_TYPES.put("fn",(env, value) -> { 
      return IsCallable.logic(value);
    });
    PRIMITIVE_TYPES.put("procedure",(env, value) -> { 
      return value instanceof escm.type.procedure.Procedure;
    });
    PRIMITIVE_TYPES.put("syntax",(env, value) -> { 
      return value instanceof escm.type.procedure.SyntaxProcedure;
    });

    // Object-oriented types
    PRIMITIVE_TYPES.put("metaobj",(env, value) -> { 
      return value instanceof escm.type.oo.Dottable;
    });
    PRIMITIVE_TYPES.put("object",(env, value) -> { 
      return value instanceof EscmObject;
    });
    PRIMITIVE_TYPES.put("class",(env, value) -> { 
      return value instanceof EscmClass;
    });
    PRIMITIVE_TYPES.put("interface",(env, value) -> { 
      return value instanceof EscmInterface;
    });

    // I/O types
    PRIMITIVE_TYPES.put("port",(env, value) -> { 
      return value instanceof escm.type.port.Port;
    });
    PRIMITIVE_TYPES.put("inport",(env, value) -> { 
      return value instanceof escm.type.port.InputPort;
    });
    PRIMITIVE_TYPES.put("outport",(env, value) -> { 
      return value instanceof escm.type.port.OutputPort;
    });

    // Module types
    PRIMITIVE_TYPES.put("module",(env, value) -> { 
      return value instanceof escm.type.oo.EscmModule;
    });
  }

  private static Pair<Predicate,Integer> parsePrimitive(String type, String name, int next) {
    Predicate pred = PRIMITIVE_TYPES.get(name);
    if(pred != null) {
      return new Pair<Predicate,Integer>(pred,next);
    }
    return null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Container Types
  private static boolean parsedAnyParameter(String type, int startIndex, int endIndex) {
    return startIndex+3 == endIndex && type.startsWith("any",startIndex);
  }

  private static Pair<Predicate,Integer> parseContainer(String type, int typeLength, String name, int next) throws Exception{
    switch(name) {
      // Common compound types
      // - Vector
      case "vector": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"vector<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return value instanceof escm.type.Vector;
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if((value instanceof escm.type.Vector) == false) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return value instanceof escm.type.Vector;
          },next);
        }
      }
      // - Map
      case "map": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength) {
            throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
          }
          // Double Parameters
          if(type.charAt(parameterType.second) == ',') {
            Pair<Predicate,Integer> valueParameterType = parseType(type,typeLength,parameterType.second+1);
            if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
              throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",parameterType.second+1,type);
            }
            // parsed <any,*> or <*,any> parameter
            if(parsedAnyParameter(type,next+1,parameterType.second)) {
              if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <any,any>
                return new Pair<Predicate,Integer>((env, value) -> { 
                  return value instanceof escm.type.Hashmap;
                },valueParameterType.second+1);
              } else { // <any,*>
                return new Pair<Predicate,Integer>((env, value) -> { 
                  if((value instanceof escm.type.Hashmap) == false) {
                    return false;
                  }
                  return ((escm.type.Hashmap)value).containsValueType(env,valueParameterType.first);
                },valueParameterType.second+1);
              }
            } else if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <*,any>
              return new Pair<Predicate,Integer>((env, value) -> { 
                if((value instanceof escm.type.Hashmap) == false) {
                  return false;
                }
                return ((escm.type.Hashmap)value).containsKeyType(env,parameterType.first);
              },valueParameterType.second+1);
            }
            return new Pair<Predicate,Integer>((env, value) -> { 
              if((value instanceof escm.type.Hashmap) == false) {
                return false;
              }
              return ((escm.type.Hashmap)value).containsTypes(env,parameterType.first,valueParameterType.first);
            },valueParameterType.second+1);
          } 
          // Single Parameter
          if(type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return value instanceof escm.type.Hashmap;
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if((value instanceof escm.type.Hashmap) == false) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return value instanceof escm.type.Hashmap;
          },next);
        }
      }

      // List-base types
      // - Pair
      case "pair": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength) {
            throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
          }
          // Double Parameters
          if(type.charAt(parameterType.second) == ',') {
            Pair<Predicate,Integer> valueParameterType = parseType(type,typeLength,parameterType.second+1);
            if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
              throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",parameterType.second+1,type);
            }
            // <any,any> parameter
            if(parsedAnyParameter(type,next+1,parameterType.second) && parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) {
              return new Pair<Predicate,Integer>((env, value) -> { 
                return value instanceof escm.type.Pair;
              },valueParameterType.second+1);
            }
            return new Pair<Predicate,Integer>((env, value) -> { 
              if((value instanceof escm.type.Pair) == false) {
                return false;
              }
              return ((escm.type.Pair)value).containsTypes(env,parameterType.first,valueParameterType.first);
            },valueParameterType.second+1);
          } 
          // Single Parameter
          if(type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return value instanceof escm.type.Pair;
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if((value instanceof escm.type.Pair) == false) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return value instanceof escm.type.Pair;
          },next);
        }
      }
      // - List
      case "list": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"list<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return escm.type.Pair.isList(value);
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if(escm.type.Pair.isList(value) == false) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return escm.type.Pair.isList(value);
          },next);
        }
      }

      // Generic compound types
      // - Associative Collection
      case "associative-collection": case "ac": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return value instanceof AssociativeCollection && !escm.type.Pair.isDottedList(value);
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if((value instanceof AssociativeCollection) == false || escm.type.Pair.isDottedList(value)) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return value instanceof AssociativeCollection && !escm.type.Pair.isDottedList(value);
          },next);
        }
      }
      // - Ordered Collection
      case "ordered-collection": case "oc": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<Predicate,Integer>((env, value) -> { 
              return value instanceof OrderedCollection && !escm.type.Pair.isDottedList(value);
            },parameterType.second+1);
          }
          return new Pair<Predicate,Integer>((env, value) -> { 
            if((value instanceof OrderedCollection) == false || escm.type.Pair.isDottedList(value)) {
              return false;
            }
            return ((AssociativeCollection)value).containsType(env,parameterType.first);
          },parameterType.second+1);
        // No Parameter
        } else {
          return new Pair<Predicate,Integer>((env, value) -> { 
            return value instanceof OrderedCollection && !escm.type.Pair.isDottedList(value);
          },next);
        }
      }

      // Non-container types
      default: {
        return null;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Class/Interface Types
  private static Pair<Predicate,Integer> parseClassOrInterface(String type, int typeLength, String name, int index, int next) {
    return new Pair<Predicate,Integer>((env, value) -> {
      Datum classOrInterface = env.nullableGet(name);
      if(classOrInterface == null) {
        throw new Exceptionf("Invalid Class or Interface Type \"%s\" (index %d): %s",name,index,type);
      }
      if((value instanceof EscmObject) == false) return false;
      EscmObject obj = (EscmObject)value;
      if(classOrInterface instanceof EscmClass) {
        return obj.instanceOf((EscmClass)classOrInterface);
      } else if(classOrInterface instanceof EscmInterface) {
        return obj.instanceOf((EscmInterface)classOrInterface);
      } else {
        throw new Exceptionf("Invalid Class or Interface Type \"%s\" (index %d): %s",name,index,type);
      }
    }, next);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Compound "|" Types
  private static Pair<Predicate,Integer> parseRest(String type, int typeLength, int index, Pair<Predicate,Integer> firstType) throws Exception {
    if(firstType.second >= typeLength) return firstType;
    char delimiter = type.charAt(firstType.second);
    if(delimiter == '>' || delimiter == ',') return firstType;
    if(delimiter == '|') {
      ArrayList<Predicate> orTypes = new ArrayList<Predicate>();
      orTypes.add(firstType.first);
      int idx = firstType.second;
      while(idx < typeLength && type.charAt(idx) == '|') {
        Pair<Predicate,Integer> orType = parseType(type,typeLength,idx+1);
        orTypes.add(orType.first);
        idx = orType.second;
      }
      return new Pair<Predicate,Integer>((env, value) -> {
        for(Predicate ot : orTypes) {
          if(ot.check(env,value) == true) return true;
        }
        return false;
      }, idx);
    }
    throw new Exceptionf("Invalid Compound Type (index %d): %s",index,type);
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
  private static boolean isTypeDelimiter(char c) {
    return c == '|' || c == '<' || c == '>' || c == ',';
  }

  private static Pair<Predicate,Integer> parseType(String type, int typeLength, int index) throws Exception {
    if(index >= typeLength) {
      throw new Exceptionf("Invalid Empty Type (index %d): %s",index,type);
    }
    int next = index;
    while(next < typeLength && !isTypeDelimiter(type.charAt(next))) {
      ++next;
    }
    String name = type.substring(index,next);
    Pair<Predicate,Integer> p = parsePrimitive(type,name,next);
    if(p == null) {
      p = parseContainer(type,typeLength,name,next);
      if(p == null) {
        p = parseClassOrInterface(type,typeLength,name,index,next);
      }
    }
    if(p == null) {
      throw new Exceptionf("Invalid Type (index %d): %s",index,type);
    }
    return parseRest(type,typeLength,index,p);
  }

  public static Predicate getPredicate(Keyword type) throws Exception {
    String name = type.value();
    return parseType(name,name.length(),1).first;
  }
}