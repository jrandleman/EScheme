// Author: Jordan Randleman - escm.type.procedure.types.TypeChecker
// Purpose:
//    EScheme <callable> type annotation utility functions.
//    See doc/types.md for more information on using EScheme's types.
//    Provides:
//      1. <Predicate>: Type checking functional interface.
//      2. <getPredicate(key)>: Keyword-to-Predicate type compiler.

package escm.type.procedure.types;
import java.util.ArrayList;
import escm.util.Pair;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Symbol;
import escm.type.oo.EscmObject;
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
  // Parse Primitive Types
  private static Pair<Predicate,Integer> parsePrimitive(String type, String name, int next) {
    switch(name) {
      // Any type
      case "any": return new Pair<Predicate,Integer>((env, value) -> { 
        return true; 
      },next);

      // Numeric types
      case "num": case "complex": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Number;
      },next);
      case "int": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Real && ((escm.type.number.Real)value).isInteger();
      },next);
      case "flo": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Inexact;
      },next);
      case "real": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Real;
      },next);
      case "exact": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isExact();
      },next);
      case "inexact": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.number.Number && ((escm.type.number.Number)value).isInexact();
      },next);

      // Common atomic types
      case "str": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.String;
      },next);
      case "char": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.Character;
      },next);
      case "key": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.Keyword;
      },next);
      case "bool": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.bool.Boolean;
      },next);
      case "sym": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.Symbol;
      },next);
      case "void": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.Void;
      },next);

      // List-base types
      case "nil": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.Nil;
      },next);
      case "atom": return new Pair<Predicate,Integer>((env, value) -> { 
        return !(value instanceof escm.type.Pair);
      },next);

      // Concurrency types
      case "thread": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.concurrent.Thread;
      },next);
      case "mutex": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.concurrent.Mutex;
      },next);

      // Functional types
      case "fn": return new Pair<Predicate,Integer>((env, value) -> { 
        return IsCallable.logic(value);
      },next);
      case "procedure": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.procedure.Procedure;
      },next);
      case "syntax": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.procedure.SyntaxProcedure;
      },next);

      // Object-oriented types
      case "metaobj": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.oo.MetaObject;
      },next);
      case "object": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof EscmObject;
      },next);
      case "class": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof EscmClass;
      },next);
      case "interface": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof EscmInterface;
      },next);

      // Dottable types
      case "dottable": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.oo.Dottable;
      },next);

      // Module types
      case "module": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.oo.EscmModule;
      },next);

      // I/O types
      case "port": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.port.Port;
      },next);
      case "inport": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.port.InputPort;
      },next);
      case "outport": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.port.OutputPort;
      },next);

      // Type Alias types
      case "type-alias": return new Pair<Predicate,Integer>((env, value) -> { 
        return value instanceof escm.type.procedure.types.TypeAlias;
      },next);

      // Non-primitive type
      default: return null;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Container Types
  private static boolean parsedAnyParameter(String type, int startIndex, int endIndex) {
    return startIndex+3 == endIndex && type.startsWith("any",startIndex);
  }

  private static Pair<Predicate,Integer> parseContainer(String type, int typeLength, String name, int next) throws Exception{
    switch(name) {
      // Common collection types
      // - Vector
      case "vec": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<Predicate,Integer> parameterType = parseType(type,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"vec<\" (index %d): %s",next+1,type);
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

      // List-base collection types
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
            // parsed <any,*> or <*,any> parameter
            if(parsedAnyParameter(type,next+1,parameterType.second)) {
              if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <any,any>
                return new Pair<Predicate,Integer>((env, value) -> { 
                  return value instanceof escm.type.Pair;
                },valueParameterType.second+1);
              } else { // <any,*>
                return new Pair<Predicate,Integer>((env, value) -> { 
                  if((value instanceof escm.type.Pair) == false) {
                    return false;
                  }
                  return ((escm.type.Pair)value).containsValueType(env,valueParameterType.first);
                },valueParameterType.second+1);
              }
            } else if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <*,any>
              return new Pair<Predicate,Integer>((env, value) -> { 
                if((value instanceof escm.type.Pair) == false) {
                  return false;
                }
                return ((escm.type.Pair)value).containsKeyType(env,parameterType.first);
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

      // Abstract collection types
      // - Associative Collection
      case "ac": {
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
      case "oc": {
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
  private static Pair<Predicate,Integer> parseModuleClassOrInterfaceOrAlias(String type, int typeLength, String name, int index, int next) throws Exception{
    ObjectAccessChain moduleClassChain = new ObjectAccessChain(new Symbol(name));
    return new Pair<Predicate,Integer>((env, value) -> {
      Datum classOrInterfaceOrAlias = moduleClassChain.nullableLoadWithState(env);
      if(classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\" (index %d): %s",name,index,type);
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
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\" (index %d): %s",name,index,type);
      }
    }, next);
  }

  private static Pair<Predicate,Integer> parseLocalClassOrInterfaceOrAlias(String type, int typeLength, String name, int index, int next) {
    return new Pair<Predicate,Integer>((env, value) -> {
      Datum classOrInterfaceOrAlias = env.nullableGet(name);
      if(classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\" (index %d): %s",name,index,type);
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
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\" (index %d): %s",name,index,type);
      }
    }, next);
  }

  private static Pair<Predicate,Integer> parseClassOrInterfaceOrAlias(String type, int typeLength, String name, int index, int next) throws Exception{
    if(ObjectAccessChain.is(name)) {
      return parseModuleClassOrInterfaceOrAlias(type,typeLength,name,index,next);
    } else {
      return parseLocalClassOrInterfaceOrAlias(type,typeLength,name,index,next);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Union "|" Types
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
    throw new Exceptionf("Invalid Union Type (index %d): %s",index,type);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type Parsing Handle
  // TYPE ::=
  //   | primitiveREST
  //   | container<TYPE>REST
  //   | container<TYPE,TYPE>REST
  //   | classREST
  //   | interfaceREST
  //   | typealiasREST
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
        p = parseClassOrInterfaceOrAlias(type,typeLength,name,index,next);
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