// Author: Jordan Randleman - escm.type.procedure.types.TypeTree
// Purpose:
//    Generic parser for EScheme's <callable> type annotations.
//    See doc/types.md for more information on using EScheme's types.
//    Provides:
//      1. <ValueGenerator>: Type tree value generator interface.
//      2. <walk(key,env,gen)>: Keyword type tree walker (produces value)
//         - <key>: keyword type who's tree is being walked
//         - <env>: definition environment (only passed to value generator)
//         - <gen>: value generator during the tree walking process

package escm.type.procedure.types;
import java.util.ArrayList;
import escm.util.Pair;
import escm.util.error.Exceptionf;
import escm.type.Keyword;
import escm.type.Symbol;
import escm.vm.util.Environment;
import escm.vm.util.ObjectAccessChain;

public class TypeTree {
  ////////////////////////////////////////////////////////////////////////////
  // Type Parser Value Generator Method Signatures
  public static interface ValueGenerator<ParseValue> {
    public ParseValue primitiveAny();

    public ParseValue primitiveNum();
    public ParseValue primitiveInt();
    public ParseValue primitiveFlo();
    public ParseValue primitiveReal();
    public ParseValue primitiveExact();
    public ParseValue primitiveInexact();

    public ParseValue primitiveStr();
    public ParseValue primitiveChar();
    public ParseValue primitiveKey();
    public ParseValue primitiveBool();
    public ParseValue primitiveSym();
    public ParseValue primitiveVoid();

    public ParseValue primitiveNil();
    public ParseValue primitiveAtom();

    public ParseValue primitiveThread();
    public ParseValue primitiveMutex();

    public ParseValue primitiveFn();
    public ParseValue primitiveProcedure();
    public ParseValue primitiveSyntax();

    public ParseValue primitiveMetaobj();
    public ParseValue primitiveObject();
    public ParseValue primitiveClass();
    public ParseValue primitiveInterface();

    public ParseValue primitiveDottable();
    public ParseValue primitiveModule();

    public ParseValue primitivePort();
    public ParseValue primitiveIport();
    public ParseValue primitiveOport();

    public ParseValue primitiveTypeAlias();

    // Any parameterized types may be "null" to indicate "<any>"!
    public ParseValue containerVec(ParseValue valType);
    public ParseValue containerMap(ParseValue keyType, ParseValue valType);
    public ParseValue containerMap(ParseValue valType);
    public ParseValue containerPair(ParseValue keyType, ParseValue valType);
    public ParseValue containerPair(ParseValue valType);
    public ParseValue containerList(ParseValue valType);
    public ParseValue containerAC(ParseValue valType);
    public ParseValue containerOC(ParseValue valType);

    public ParseValue userType(String type, String name, Environment env) throws Exception;
    public ParseValue moduleUserType(String type, String name, ObjectAccessChain chain, Environment env) throws Exception;

    public ParseValue compound(ArrayList<ParseValue> orTypes);
  }
  
  ////////////////////////////////////////////////////////////////////////////
  // Parse Primitive Types
  private static <ParseValue> Pair<ParseValue,Integer> parsePrimitive(ValueGenerator<ParseValue> gen, String name, int next) {
    switch(name) {
      // Any type
      case "any": return new Pair<ParseValue,Integer>(gen.primitiveAny(),next);

      // Numeric types
      case "num": case "complex": return new Pair<ParseValue,Integer>(gen.primitiveNum(),next);
      case "int": return new Pair<ParseValue,Integer>(gen.primitiveInt(),next);
      case "flo": return new Pair<ParseValue,Integer>(gen.primitiveFlo(),next);
      case "real": return new Pair<ParseValue,Integer>(gen.primitiveReal(),next);
      case "exact": return new Pair<ParseValue,Integer>(gen.primitiveExact(),next);
      case "inexact": return new Pair<ParseValue,Integer>(gen.primitiveInexact(),next);

      // Common atomic types
      case "str": return new Pair<ParseValue,Integer>(gen.primitiveStr(),next);
      case "char": return new Pair<ParseValue,Integer>(gen.primitiveChar(),next);
      case "key": return new Pair<ParseValue,Integer>(gen.primitiveKey(),next);
      case "bool": return new Pair<ParseValue,Integer>(gen.primitiveBool(),next);
      case "sym": return new Pair<ParseValue,Integer>(gen.primitiveSym(),next);
      case "void": return new Pair<ParseValue,Integer>(gen.primitiveVoid(),next);

      // List-base types
      case "nil": return new Pair<ParseValue,Integer>(gen.primitiveNil(),next);
      case "atom": return new Pair<ParseValue,Integer>(gen.primitiveAtom(),next);

      // Concurrency types
      case "thread": return new Pair<ParseValue,Integer>(gen.primitiveThread(),next);
      case "mutex": return new Pair<ParseValue,Integer>(gen.primitiveMutex(),next);

      // Functional types
      case "fn": return new Pair<ParseValue,Integer>(gen.primitiveFn(),next);
      case "procedure": return new Pair<ParseValue,Integer>(gen.primitiveProcedure(),next);
      case "syntax": return new Pair<ParseValue,Integer>(gen.primitiveSyntax(),next);

      // Object-oriented types
      case "metaobj": return new Pair<ParseValue,Integer>(gen.primitiveMetaobj(),next);
      case "object": return new Pair<ParseValue,Integer>(gen.primitiveObject(),next);
      case "class": return new Pair<ParseValue,Integer>(gen.primitiveClass(),next);
      case "interface": return new Pair<ParseValue,Integer>(gen.primitiveInterface(),next);

      // Dottable types
      case "dottable": return new Pair<ParseValue,Integer>(gen.primitiveDottable(),next);

      // Module types
      case "module": return new Pair<ParseValue,Integer>(gen.primitiveModule(),next);

      // I/O types
      case "port": return new Pair<ParseValue,Integer>(gen.primitivePort(),next);
      case "inport": return new Pair<ParseValue,Integer>(gen.primitiveIport(),next);
      case "outport": return new Pair<ParseValue,Integer>(gen.primitiveOport(),next);

      // Type Alias types
      case "type-alias": return new Pair<ParseValue,Integer>(gen.primitiveTypeAlias(),next);

      // Non-primitive type
      default: return null;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Collection Types
  private static boolean parsedAnyParameter(String type, int startIndex, int endIndex) {
    return startIndex+3 == endIndex && type.startsWith("any",startIndex);
  }

  private static <ParseValue> Pair<ParseValue,Integer> parseCollection(String type, Environment env, ValueGenerator<ParseValue> gen, int typeLength, String name, int next) throws Exception{
    switch(name) {
      // Common collection types
      // - Vector
      case "vec": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"vec<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerVec(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerVec(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerVec(null),
            next
          );
        }
      }
      // - Map
      case "map": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength) {
            throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
          }
          // Double Parameters
          if(type.charAt(parameterType.second) == ',') {
            Pair<ParseValue,Integer> valueParameterType = parseType(type,env,gen,typeLength,parameterType.second+1);
            if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
              throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",parameterType.second+1,type);
            }
            // parsed <any,*> or <*,any> parameter
            if(parsedAnyParameter(type,next+1,parameterType.second)) {
              if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <any,any>
                return new Pair<ParseValue,Integer>(
                  gen.containerMap(null,null),
                  valueParameterType.second+1
                );
              } else { // <any,*>
                return new Pair<ParseValue,Integer>(
                  gen.containerMap(null,valueParameterType.first),
                  valueParameterType.second+1
                );
              }
            } else if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <*,any>
              return new Pair<ParseValue,Integer>(
                gen.containerMap(parameterType.first,null),
                valueParameterType.second+1
              );
            }
            return new Pair<ParseValue,Integer>(
              gen.containerMap(parameterType.first,valueParameterType.first),
              valueParameterType.second+1
            );
          } 
          // Single Parameter
          if(type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"map<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerMap(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerMap(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerMap(null),
            next
          );
        }
      }

      // List-base collection types
      // - Pair
      case "pair": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength) {
            throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
          }
          // Double Parameters
          if(type.charAt(parameterType.second) == ',') {
            Pair<ParseValue,Integer> valueParameterType = parseType(type,env,gen,typeLength,parameterType.second+1);
            if(valueParameterType.second >= typeLength || type.charAt(valueParameterType.second) != '>') {
              throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",parameterType.second+1,type);
            }
            // parsed <any,*> or <*,any> parameter
            if(parsedAnyParameter(type,next+1,parameterType.second)) {
              if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <any,any>
                return new Pair<ParseValue,Integer>(
                  gen.containerPair(null,null),
                  valueParameterType.second+1
                );
              } else { // <any,*>
                return new Pair<ParseValue,Integer>(
                  gen.containerPair(null,valueParameterType.first),
                  valueParameterType.second+1
                );
              }
            } else if(parsedAnyParameter(type,parameterType.second+1,valueParameterType.second)) { // <*,any>
              return new Pair<ParseValue,Integer>(
                gen.containerPair(parameterType.first,null),
                valueParameterType.second+1
              );
            }
            return new Pair<ParseValue,Integer>(
              gen.containerPair(parameterType.first,valueParameterType.first),
              valueParameterType.second+1
            );
          } 
          // Single Parameter
          if(type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"pair<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerPair(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerPair(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerPair(null),
            next
          );
        }
      }
      // - List
      case "list": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"list<\" (index %d): %s",next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerList(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerList(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerList(null),
            next
          );
        }
      }

      // Abstract collection types
      // - Associative Collection
      case "ac": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerAC(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerAC(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerAC(null),
            next
          );
        }
      }
      // - Ordered Collection
      case "oc": {
        // Parameterized
        if(next < typeLength && type.charAt(next) == '<') {
          Pair<ParseValue,Integer> parameterType = parseType(type,env,gen,typeLength,next+1);
          if(parameterType.second >= typeLength || type.charAt(parameterType.second) != '>') {
            throw new Exceptionf("Invalid Type \"%s<\" (index %d): %s",name,next+1,type);
          }
          // <any> parameter
          if(parsedAnyParameter(type,next+1,parameterType.second)) {
            return new Pair<ParseValue,Integer>(
              gen.containerOC(null),
              parameterType.second+1
            );
          }
          return new Pair<ParseValue,Integer>(
            gen.containerOC(parameterType.first),
            parameterType.second+1
          );
        // No Parameter
        } else {
          return new Pair<ParseValue,Integer>(
            gen.containerOC(null),
            next
          );
        }
      }

      // Non-collection types
      default: {
        return null;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Class/Interface Types
  private static <ParseValue> Pair<ParseValue,Integer> parseModuleClassOrInterfaceOrAlias(String type, Environment env, ValueGenerator<ParseValue> gen, String name, int next) throws Exception{
    ObjectAccessChain chain = new ObjectAccessChain(new Symbol(name));
    return new Pair<ParseValue,Integer>(gen.moduleUserType(type,name,chain,env),next);
  }

  private static <ParseValue> Pair<ParseValue,Integer> parseLocalClassOrInterfaceOrAlias(String type, Environment env, ValueGenerator<ParseValue> gen, String name, int next) throws Exception {
    return new Pair<ParseValue,Integer>(gen.userType(type,name,env),next);
  }

  private static <ParseValue> Pair<ParseValue,Integer> parseClassOrInterfaceOrAlias(String type, Environment env, ValueGenerator<ParseValue> gen, String name, int next) throws Exception{
    if(ObjectAccessChain.is(name)) {
      return parseModuleClassOrInterfaceOrAlias(type,env,gen,name,next);
    } else {
      return parseLocalClassOrInterfaceOrAlias(type,env,gen,name,next);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Union "|" Types
  private static <ParseValue> Pair<ParseValue,Integer> parseRest(String type, Environment env, ValueGenerator<ParseValue> gen, int typeLength, int index, Pair<ParseValue,Integer> firstType) throws Exception {
    if(firstType.second >= typeLength) return firstType;
    char delimiter = type.charAt(firstType.second);
    if(delimiter == '>' || delimiter == ',') return firstType;
    if(delimiter == '|') {
      ArrayList<ParseValue> orTypes = new ArrayList<ParseValue>();
      orTypes.add(firstType.first);
      int idx = firstType.second;
      while(idx < typeLength && type.charAt(idx) == '|') {
        Pair<ParseValue,Integer> orType = parseType(type,env,gen,typeLength,idx+1);
        orTypes.add(orType.first);
        idx = orType.second;
      }
      return new Pair<ParseValue,Integer>(gen.compound(orTypes),idx);
    }
    throw new Exceptionf("Invalid Union Type (index %d): %s",index,type);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type Parsing Handle
  // TYPE ::=
  //   | primitiveREST
  //   | collection<TYPE>REST
  //   | collection<TYPE,TYPE>REST
  //   | collectionREST
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

  private static <ParseValue> Pair<ParseValue,Integer> parseType(String type, Environment env, ValueGenerator<ParseValue> gen, int typeLength, int index) throws Exception {
    if(index >= typeLength) {
      throw new Exceptionf("Invalid Empty Type (index %d): %s",index,type);
    }
    int next = index;
    while(next < typeLength && !isTypeDelimiter(type.charAt(next))) {
      ++next;
    }
    String name = type.substring(index,next);
    Pair<ParseValue,Integer> p = parsePrimitive(gen,name,next);
    if(p == null) {
      p = parseCollection(type,env,gen,typeLength,name,next);
      if(p == null) {
        p = parseClassOrInterfaceOrAlias(type,env,gen,name,next);
      }
    }
    if(p == null) {
      throw new Exceptionf("Invalid Type (index %d): %s",index,type);
    }
    return parseRest(type,env,gen,typeLength,index,p);
  }

  public static <ParseValue> ParseValue walk(Keyword type, Environment env, ValueGenerator<ParseValue> gen) throws Exception {
    String name = type.value();
    return parseType(name,env,gen,name.length(),1).first;
  }

  public static <ParseValue> ParseValue walk(String type, Environment env, ValueGenerator<ParseValue> gen) throws Exception {
    return parseType(type,env,gen,type.length(),0).first;
  }
}