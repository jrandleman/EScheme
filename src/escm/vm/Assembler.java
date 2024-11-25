// Author: Jordan Randleman - escm.vm.Assembler
// Purpose:
//    Wrapper class containing the EScheme bytecode assembler logic.
//    Invoke via "Assembler.run()".
//
//    Converts instructions as scheme data (from "compile" or "bytecode")
//    into instructions runnable by the interpreter.
//
//    Performs validation and transformations including:
//      0. Verify instruction type/structure guarentees
//      1. Convert "load-closure" syntax into a "load" instruction

package escm.vm;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.primitive.syntax.CorePrimitives;
import escm.type.Character;
import escm.type.number.Real;
import escm.type.procedure.CompoundProcedure;
import escm.type.procedure.types.TypeChecker;
import escm.util.error.Exceptionf;
import escm.vm.util.Instruction;
import escm.vm.util.ObjectAccessChain;

public class Assembler {
  ////////////////////////////////////////////////////////////////////////////
  // Instruction identification
  private static int getInstructionIndex(Datum instruction, String s) throws Exception {
    switch(s) {
      case "define":       return Instruction.DEFINE;
      case "set!":         return Instruction.SET;
      case "defined?":     return Instruction.DEFINEDP;
      case "define-type":  return Instruction.DEFINE_TYPE;
      case "ifn":          return Instruction.IFN;
      case "jump":         return Instruction.JUMP;
      case "quote":        return Instruction.QUOTE;
      case "load":         return Instruction.LOAD;
      case "call":         return Instruction.CALL;
      case "push":         return Instruction.PUSH;
      case "pop":          return Instruction.POP;
      case "return":       return Instruction.RETURN;
      case "load-closure": return Instruction.Pseudo.LOAD_CLOSURE;
      default: throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction/syntax!", instruction.profile());
    }
  }


  private static boolean instructionIsNullary(Pair instruction) {
    return instruction.length() == 1;
  }


  private static boolean instructionIsUnary(Pair instruction) {
    return instruction.length() == 2;
  }


  private static Datum getInstructionArgument(Pair instruction) throws Exception {
    if(!instructionIsUnary(instruction))
      throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction!", instruction.profile());
    return ((Pair)instruction.cdr()).car();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Syntax Pseudo-Instruction Parsing
  private static boolean hasDocstring(Datum d) {
    return d instanceof Pair && ((Pair)d).car() instanceof escm.type.String;
  }


  private static boolean isPeriodSymbol(Datum s) {
    return s instanceof Symbol && ((Symbol)s).value().equals(".");
  }


  // Returns <null> if <params> isn't variadic
  private static Symbol parseUnaryVariadicParameter(Pair params) {
    if(params.length() != 2 || !isPeriodSymbol(params.car())) return null;
    Datum variadicSymbol = ((Pair)params.cdr()).car();
    if(variadicSymbol instanceof Symbol) return (Symbol)variadicSymbol;
    return null;
  }


  private static class SyntaxParameterComponents {
    ArrayList<String> typeNames = null;
    ArrayList<TypeChecker.Predicate> types = null;
    ArrayList<Symbol> names = null;
    Symbol variadicName = null;
    public SyntaxParameterComponents(ArrayList<String> typeNames, ArrayList<TypeChecker.Predicate> types, ArrayList<Symbol> names, Symbol variadicName) {
      this.typeNames = typeNames;
      this.types = types;
      this.names = names;
      this.variadicName = variadicName;
    }
  }


  private static SyntaxParameterComponents parseSyntaxParameters(Pair instruction, Datum params) throws Exception {
    // No params
    if(params instanceof Nil) 
      return new SyntaxParameterComponents(null, null,new ArrayList<Symbol>(),null);
    // Unary variadic (if the "param list" is a single symbol)
    if(params instanceof Symbol)
      return new SyntaxParameterComponents(null, null,new ArrayList<Symbol>(),(Symbol)params);
    // Invalid params
    if(!(params instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    Pair paramsPair = (Pair)params;
    // Unary variadic (if the "param list" is of 2 symbols with the first as the "." symbol)
    Symbol variadicParam = parseUnaryVariadicParameter(paramsPair);
    if(variadicParam != null)
      return new SyntaxParameterComponents(null, null,new ArrayList<Symbol>(),variadicParam);
    // Parse params
    ArrayList<String> typeNames = new ArrayList<String>();
    ArrayList<TypeChecker.Predicate> types = new ArrayList<TypeChecker.Predicate>();
    ArrayList<Symbol> parameters = new ArrayList<Symbol>();
    Keyword type = null;
    boolean typesFound = false;
    while(params instanceof Pair) {
      paramsPair = (Pair)params;
      Datum first = paramsPair.car();
      if(first instanceof Keyword) {
        if(type != null)
          throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
        type = (Keyword)first;
        typesFound = true;
      } else {
        if(!(first instanceof Symbol))
          throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
        if(type == null) {
          typeNames.add(null);
          types.add(null);
        } else {
          try {
            typeNames.add(type.display().substring(1));
            types.add(TypeChecker.getPredicate(type));
          } catch(Exception e) {
            throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax: %s", instruction.profile(), e.getMessage());
          }
        }
        parameters.add((Symbol)first);
        type = null;
      }
      params = paramsPair.cdr();
    }
    // Shortcut to avoid type checking typeless signatures
    if(typesFound == false) {
      typeNames = null;
      types = null;
    }
    // No variadic
    if(params instanceof Nil) 
      return new SyntaxParameterComponents(typeNames, types, parameters, null);
    // Has variadic
    if(!(params instanceof Symbol))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    return new SyntaxParameterComponents(typeNames, types, parameters, (Symbol)params);
  }


  private static CompoundProcedure generateCompoundProcedure(Pair instruction) throws Exception {
    if(!(instruction.cdr() instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    String docstring = "";
    // Note for <returnTypeList> & <returnTypeNameList>:
    //   Individual elts may be null to denote typeless return values.
    ArrayList<TypeChecker.Predicate> returnTypeList = new ArrayList<TypeChecker.Predicate>();
    ArrayList<String> returnTypeNameList = new ArrayList<String>();
    // Note for <typesList>, <typeNamesList>: 
    //   Inner arrays may be null to denote typeless signatures, and
    //   individual elts may be null to denote typeless parameters.
    ArrayList<ArrayList<TypeChecker.Predicate>> typesLists = new ArrayList<ArrayList<TypeChecker.Predicate>>();
    ArrayList<ArrayList<String>> typeNamesLists = new ArrayList<ArrayList<String>>();
    ArrayList<ArrayList<Symbol>> paramsList = new ArrayList<ArrayList<Symbol>>();
    ArrayList<Symbol> variadicParamList = new ArrayList<Symbol>();
    ArrayList<ArrayList<Instruction>> instructionsList = new ArrayList<ArrayList<Instruction>>();
    Datum iterator = instruction.cdr();
    if(hasDocstring(iterator)) {
      Pair p = (Pair)iterator;
      docstring = ((escm.type.String)p.car()).value();
      iterator = p.cdr();
      if(!(iterator instanceof Pair))
        throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    }
    boolean noReturnTypes = true;
    boolean noParameterTypes = true;
    while(iterator instanceof Pair) {
      Pair iteratorPair = (Pair)iterator;
      escm.util.Pair<Keyword,Datum> returnTypeAndClause = CorePrimitives.Fn.analyzeClause(iteratorPair.car());
      if(returnTypeAndClause == null)
        throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
      // Account for return type
      Keyword returnType = returnTypeAndClause.first; // <null> if no return type
      if(returnType == null) {
        returnTypeNameList.add(null);
        returnTypeList.add(null);
      } else {
        returnTypeNameList.add(returnType.display().substring(1));
        returnTypeList.add(TypeChecker.getPredicate(returnType));
      }
      noReturnTypes = noReturnTypes && returnType == null;
      // Account for parameters and body
      Datum clause = returnTypeAndClause.second;
      if(!(clause instanceof Pair))
        throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
      Pair clausePair = (Pair)clause;
      SyntaxParameterComponents params = parseSyntaxParameters(instruction,clausePair.car());
      typeNamesLists.add(params.typeNames);
      typesLists.add(params.types);
      paramsList.add(params.names);
      variadicParamList.add(params.variadicName);
      instructionsList.add(run(clausePair.cdr()));
      noParameterTypes = noParameterTypes && params.types == null;
      iterator = iteratorPair.cdr();
    }
    if(noReturnTypes) {
      returnTypeNameList = null;
      returnTypeList = null;
    }
    if(noParameterTypes) {
      typeNamesLists = null;
      typesLists = null;
    }
    return new CompoundProcedure(
      docstring,
      returnTypeNameList,
      returnTypeList,
      typeNamesLists,
      typesLists,
      paramsList,
      variadicParamList,
      instructionsList
    );
  }



  ////////////////////////////////////////////////////////////////////////////
  // Individual instruction assemblage
  private static Instruction assembleInstruction(Datum instruction) throws Exception {
    // instruction identification
    if(!(instruction instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction!", instruction.profile());
    Pair instructionPair = (Pair)instruction;
    if(!(instructionPair.car() instanceof Symbol))
      throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction!", instruction.profile());
    // instruction assembly
    switch(getInstructionIndex(instruction,((Symbol)instructionPair.car()).value())) {
      ////////////////////////////////////////////////////////////////////////
      // (define <symbol>)
      case Instruction.DEFINE: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Symbol))
          throw new Exceptionf("ASM ERROR: \"define\" instruction %s must have a symbolic arg!", instruction.profile());
        Symbol argSymbol = (Symbol)arg;
        if(ObjectAccessChain.is(argSymbol)) {
          return new Instruction(Instruction.DEFINE,new ObjectAccessChain(argSymbol));
        }
        return new Instruction(Instruction.DEFINE,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (set! <symbol>)
      case Instruction.SET: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Symbol))
          throw new Exceptionf("ASM ERROR: \"set!\" instruction %s must have a symbolic arg!", instruction.profile());
        Symbol argSymbol = (Symbol)arg;
        if(ObjectAccessChain.is(argSymbol)) {
          return new Instruction(Instruction.SET,new ObjectAccessChain(argSymbol));
        }
        return new Instruction(Instruction.SET,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (defined? <symbol>)
      case Instruction.DEFINEDP: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Symbol))
          throw new Exceptionf("ASM ERROR: \"defined?\" instruction %s must have a symbolic arg!", instruction.profile());
        Symbol argSymbol = (Symbol)arg;
        if(ObjectAccessChain.is(argSymbol)) {
          return new Instruction(Instruction.DEFINEDP,new ObjectAccessChain(argSymbol));
        }
        return new Instruction(Instruction.DEFINEDP,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (define-type <symbol>)
      case Instruction.DEFINE_TYPE: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Symbol))
          throw new Exceptionf("ASM ERROR: \"define-type\" instruction %s must have a symbolic arg!", instruction.profile());
        Symbol argSymbol = (Symbol)arg;
        if(ObjectAccessChain.is(argSymbol)) {
          return new Instruction(Instruction.DEFINE_TYPE,new ObjectAccessChain(argSymbol));
        }
        return new Instruction(Instruction.DEFINE_TYPE,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (ifn <integer>)
      case Instruction.IFN: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Real) || !((Real)arg).isInteger())
          throw new Exceptionf("ASM ERROR: \"if\" instruction %s must have an integer arg!", instruction.profile());
        Real branchAmount = (Real)arg;
        if(branchAmount.isZero())
          throw new Exceptionf("ASM ERROR: \"if\" instruction %s must have a non-0 arg!", instruction.profile());
        return new Instruction(Instruction.IFN,new Character(branchAmount.intValue())); // wrap java <int> in an escm <Character>
      }

      ////////////////////////////////////////////////////////////////////////
      // (jump <integer>)
      case Instruction.JUMP: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Real) || !((Real)arg).isInteger())
          throw new Exceptionf("ASM ERROR: \"jump\" instruction %s must have an integer arg!", instruction.profile());
        Real branchAmount = (Real)arg;
        if(branchAmount.isZero())
          throw new Exceptionf("ASM ERROR: \"jump\" instruction %s must have a non-0 arg!", instruction.profile());
        return new Instruction(Instruction.JUMP,new Character(branchAmount.intValue())); // wrap java <int> in an escm <Character>
      }

      ////////////////////////////////////////////////////////////////////////
      // (quote <datum>)
      case Instruction.QUOTE: {
        return new Instruction(Instruction.QUOTE,getInstructionArgument(instructionPair));
      }

      ////////////////////////////////////////////////////////////////////////
      // (load <datum>)
      case Instruction.LOAD: {
        Datum arg = getInstructionArgument(instructionPair);
        if(arg instanceof Symbol) {
          Symbol argSymbol = (Symbol)arg;
          if(ObjectAccessChain.is(argSymbol)) {
            return new Instruction(Instruction.LOAD,new ObjectAccessChain(argSymbol));
          }
        }
        return new Instruction(Instruction.LOAD,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (load-closure (<arg> ...) <instruction> ...)
      case Instruction.Pseudo.LOAD_CLOSURE: {
        return new Instruction(Instruction.LOAD,generateCompoundProcedure(instructionPair));
      }

      ////////////////////////////////////////////////////////////////////////
      // (call <datum>)
      case Instruction.CALL: {
        Datum arg = getInstructionArgument(instructionPair);
        if(arg instanceof Symbol) {
          Symbol argSymbol = (Symbol)arg;
          if(ObjectAccessChain.is(argSymbol)) {
            return new Instruction(Instruction.CALL,new ObjectAccessChain(argSymbol));
          }
        }
        return new Instruction(Instruction.CALL,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (push) (push <datum>)
      case Instruction.PUSH: {
        if(instructionIsNullary(instructionPair))
          return new Instruction(Instruction.PUSH,null);
        Datum arg = getInstructionArgument(instructionPair);
        if(arg instanceof Symbol) {
          Symbol argSymbol = (Symbol)arg;
          if(ObjectAccessChain.is(argSymbol)) {
            return new Instruction(Instruction.PUSH,new ObjectAccessChain(argSymbol));
          }
        }
        return new Instruction(Instruction.PUSH,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (pop)
      case Instruction.POP: {
        if(!instructionIsNullary(instructionPair))
          throw new Exceptionf("ASM ERROR: \"pop\" instruction %s shouldn't have any args!", instruction.profile());
        return new Instruction(Instruction.POP,null);
      }

      ////////////////////////////////////////////////////////////////////////
      // (return) (return <datum>)
      case Instruction.RETURN: {
        if(instructionIsNullary(instructionPair))
          return new Instruction(Instruction.RETURN,null);
        Datum arg = getInstructionArgument(instructionPair);
        if(arg instanceof Symbol) {
          Symbol argSymbol = (Symbol)arg;
          if(ObjectAccessChain.is(argSymbol)) {
            return new Instruction(Instruction.RETURN,new ObjectAccessChain(argSymbol));
          }
        }
        return new Instruction(Instruction.RETURN,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // should be unreachable but just in case ...
      default: {
        throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction/syntax!", instruction.profile());
      }
    }
  }



  ////////////////////////////////////////////////////////////////////////////
  // Core assembly loop
  public static ArrayList<Instruction> run(Datum instructionsAsData) throws Exception {
    ArrayList<Instruction> instructions = new ArrayList<Instruction>();
    // Empty instruction set
    if(instructionsAsData instanceof Nil) return instructions;
    if(!(instructionsAsData instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't a valid set of bytecode instructions!", instructionsAsData.profile());
    // Filled instruction set
    Datum iterator = instructionsAsData;
    while(iterator instanceof Pair) {
      Pair iteratorPair = (Pair)iterator;
      instructions.add(assembleInstruction(iteratorPair.car()));
      iterator = iteratorPair.cdr();
    }
    return instructions;
  }
}