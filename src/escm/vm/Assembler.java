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
//      1. Ensure "jump"/"if" branch amounts stay within the instruction set scope
//      2. Convert "load-closure" syntax into a "load" instruction

package escm.vm;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.number.Real;
import escm.type.procedure.CompoundProcedure;
import escm.type.number.Inexact;
import escm.util.Exceptionf;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.util.ObjectAccessChain;

public class Assembler {
  ////////////////////////////////////////////////////////////////////////////
  // Instruction identification
  private static int getInstructionIndex(Datum instruction, String s) throws Exception {
    if(s.equals("define"))       return Instruction.DEFINE;
    if(s.equals("set!"))         return Instruction.SET;

    if(s.equals("defined?"))     return Instruction.DEFINEDP;

    if(s.equals("ifn"))          return Instruction.IFN;
    if(s.equals("jump"))         return Instruction.JUMP;

    if(s.equals("load"))         return Instruction.LOAD;
    if(s.equals("load-symbol"))  return Instruction.LOAD_SYMBOL;

    if(s.equals("call"))         return Instruction.CALL;

    if(s.equals("push"))         return Instruction.PUSH;
    if(s.equals("pop"))          return Instruction.POP;

    if(s.equals("return"))       return Instruction.RETURN;

    if(s.equals("load-closure")) return Instruction.Pseudo.LOAD_CLOSURE;

    throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction/syntax!", instruction.profile());
  }


  private static boolean instructionIsNullary(Pair instruction) {
    return instruction.cdr() instanceof Nil;
  }


  private static boolean instructionIsUnary(Pair instruction) {
    return instruction.cdr() instanceof Pair && ((Pair)instruction.cdr()).cdr() instanceof Nil;
  }


  private static Datum getInstructionArgument(Pair instruction) throws Exception {
    if(!instructionIsUnary(instruction))
      throw new Exceptionf("ASM ERROR: %s isn't a valid bytecode instruction!", instruction.profile());
    return ((Pair)instruction.cdr()).car();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Bound Branch Instructions
  private static void boundBranchAmounts(ArrayList<Instruction> instructions) {
    for(int i = 0, n = instructions.size(); i < n; ++i) {
      Instruction instruction = instructions.get(i);
      if(instruction.operation == Instruction.IFN || instruction.operation == Instruction.JUMP) {
        int branchAmount = ((Inexact)instruction.argument).intValue();
        if(branchAmount < 0 && Math.abs(branchAmount) > i) {
          instruction.argument = new Inexact(-i);
        } else if(branchAmount > 0 && branchAmount > n-i) {
          instruction.operation = Instruction.RETURN;
          instruction.argument = null;
        }
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Syntax Pseudo-Instruction Parsing
  private static class SyntaxComponents {
    public ArrayList<ArrayList<Symbol>> paramsList;
    public ArrayList<Symbol> variadicParamList;
    public ArrayList<ArrayList<Instruction>> instructionsList;
    public SyntaxComponents(ArrayList<ArrayList<Symbol>> paramsList, ArrayList<Symbol> variadicParamList, ArrayList<ArrayList<Instruction>> instructionsList) {
      this.paramsList = paramsList;
      this.variadicParamList = variadicParamList;
      this.instructionsList = instructionsList;
    }
  }


  private static escm.util.Pair<ArrayList<Symbol>,Symbol> parseSyntaxParameters(Pair instruction, Datum params) throws Exception {
    // No params
    if(params instanceof Nil) 
      return new escm.util.Pair<ArrayList<Symbol>,Symbol>(new ArrayList<Symbol>(),null);
    // Unary variadic (if the "param list" is a single symbol)
    if(params instanceof Symbol)
      return new escm.util.Pair<ArrayList<Symbol>,Symbol>(new ArrayList<Symbol>(),(Symbol)params);
    // Invalid params
    if(!(params instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    Pair paramsPair = (Pair)params;
    // Unary variadic (if the "param list" is of 2 items with the first as the "." symbol)
    if(paramsPair.car() instanceof Symbol && ((Symbol)paramsPair.car()).value().equals(".")) {
      return new escm.util.Pair<ArrayList<Symbol>,Symbol>(new ArrayList<Symbol>(),(Symbol)paramsPair.car());
    }
    // Parse params
    ArrayList<Symbol> parameters = new ArrayList<Symbol>();
    while(params instanceof Pair) {
      paramsPair = (Pair)params;
      if(!(paramsPair.car() instanceof Symbol))
        throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
      parameters.add((Symbol)paramsPair.car());
      params = paramsPair.cdr();
    }
    // No variadic
    if(params instanceof Nil) return new escm.util.Pair<ArrayList<Symbol>,Symbol>(parameters,null);
    // Has variadic
    if(!(params instanceof Symbol))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    return new escm.util.Pair<ArrayList<Symbol>,Symbol>(parameters,(Symbol)params);
  }


  private static SyntaxComponents parseSyntaxComponents(Pair instruction) throws Exception {
    if(!(instruction.cdr() instanceof Pair))
      throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
    ArrayList<ArrayList<Symbol>> paramsList = new ArrayList<ArrayList<Symbol>>();
    ArrayList<Symbol> variadicParamList = new ArrayList<Symbol>();
    ArrayList<ArrayList<Instruction>> instructionsList = new ArrayList<ArrayList<Instruction>>();
    Datum iterator = instruction.cdr();
    while(iterator instanceof Pair) {
      Datum clause = ((Pair)iterator).car();
      if(!(clause instanceof Pair))
        throw new Exceptionf("ASM ERROR: %s isn't valid bytecode syntax!", instruction.profile());
      Pair clausePair = (Pair)clause;
      escm.util.Pair<ArrayList<Symbol>,Symbol> params = parseSyntaxParameters(instruction,clausePair.car());
      paramsList.add(params.first);
      variadicParamList.add(params.second);
      instructionsList.add(run(clausePair.cdr()));
      iterator = ((Pair)iterator).cdr();
    }
    return new SyntaxComponents(paramsList,variadicParamList,instructionsList);
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
      // (ifn <integer>)
      case Instruction.IFN: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Real) || !((Real)arg).isInteger())
          throw new Exceptionf("ASM ERROR: \"if\" instruction %s must have an integer arg!", instruction.profile());
        int branchAmount = ((Real)arg).intValue();
        if(branchAmount == 0)
          throw new Exceptionf("ASM ERROR: \"if\" instruction %s must have a non-0 arg!", instruction.profile());
        return new Instruction(Instruction.IFN,new Inexact(branchAmount));
      }

      ////////////////////////////////////////////////////////////////////////
      // (jump <integer>)
      case Instruction.JUMP: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Real) || !((Real)arg).isInteger())
          throw new Exceptionf("ASM ERROR: \"jump\" instruction %s must have an integer arg!", instruction.profile());
        int branchAmount = ((Real)arg).intValue();
        if(branchAmount == 0)
        if(branchAmount == 0)
          throw new Exceptionf("ASM ERROR: \"jump\" instruction %s must have a non-0 arg!", instruction.profile());
        return new Instruction(Instruction.JUMP,new Inexact(branchAmount));
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
      // (load-symbol <datum>)
      case Instruction.LOAD_SYMBOL: {
        Datum arg = getInstructionArgument(instructionPair);
        if(!(arg instanceof Symbol))
          throw new Exceptionf("ASM ERROR: \"load-symbol\" instruction %s must have a symbolic arg!", instruction.profile());
        return new Instruction(Instruction.LOAD_SYMBOL,arg);
      }

      ////////////////////////////////////////////////////////////////////////
      // (load-closure (<arg> ...) <instruction> ...)
      case Instruction.Pseudo.LOAD_CLOSURE: {
        SyntaxComponents components = parseSyntaxComponents(instructionPair);
        for(ArrayList<Instruction> instructions : components.instructionsList)
          boundBranchAmounts(instructions);
        CompoundProcedure compoundProcedure = new CompoundProcedure(components.paramsList,components.variadicParamList,components.instructionsList);
        return new Instruction(Instruction.LOAD,compoundProcedure);
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