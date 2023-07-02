// Author: Jordan Randleman - escm.vm.Compiler
// Purpose:
//    Wrapper class containing the EScheme->bytecode compiler logic.
//    Converts a given escm expression as scheme data into escm bytecode as scheme data.
//    Invoke via "Compiler.run()".

package escm.vm;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Vector;
import escm.type.Hashmap;
import escm.type.Symbol;
import escm.type.Nil;
import escm.type.procedure.Procedure;
import escm.type.procedure.SyntaxProcedure;
import escm.type.number.Exact;
import escm.util.Trampoline;
import escm.primitive.MetaPrimitives;
import escm.vm.type.Callable;
import escm.vm.type.OrderedCollection;
import escm.vm.util.ObjectAccessChain;
import escm.vm.util.Environment;

public class Compiler {
  ////////////////////////////////////////////////////////////////////////////
  // Static Symbolic Constants
  private static final Symbol LOAD = new Symbol("load");
  private static final Symbol CALL = new Symbol("call");
  private static final Symbol PUSH = new Symbol("push");

  private static final Symbol VECTOR = new Symbol("vector");
  private static final Symbol HASHMAP = new Symbol("hashmap");


  ////////////////////////////////////////////////////////////////////////////
  // Helper Function(s)
  private static boolean isTaggedList(Pair p, String tag) {
    return p.car() instanceof Symbol && ((Symbol)p.car()).value().equals(tag);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compiling <escm-bytecode>
  private static Trampoline.Bounce compileEscmBytecode(Pair d, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(d.cdr());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compiling Atomic Literals
  private static Trampoline.Bounce compileAtomicLiteral(Datum d, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Pair.List(Pair.List(LOAD,d)));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compiling Vector Literals (must evaluate their contents)
  private static Trampoline.Bounce generateVectorCallInstructions(Datum v, int count, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    if(v instanceof Nil) return continuation.run(Pair.List(Pair.List(PUSH,VECTOR),Pair.List(CALL,new Exact(-count))));
    Pair vPair = (Pair)v;
    Datum hd = vPair.car();
    if(!(hd instanceof Pair) && !(hd instanceof Vector) && !(hd instanceof Hashmap)) {
      return () -> generateVectorCallInstructions(vPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
        return continuation.run(new Pair(Pair.List(PUSH,hd),applicationInstructions));
      });
    } else {
      return () -> run(hd,definitionEnvironment,(valueInstructions) -> () -> {
        Datum instructions = Pair.binaryAppend(valueInstructions,Pair.List(Pair.List(PUSH)));
        return generateVectorCallInstructions(vPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
          return continuation.run(Pair.binaryAppend(instructions,applicationInstructions));
        });
      });
    }
  }


  private static Trampoline.Bounce compileVectorLiteral(Vector v, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    return generateVectorCallInstructions((Datum)((OrderedCollection)v.toList()).reverse(),1,definitionEnvironment,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compiling Hashmap Literals (must evaluate their contents)
  private static Trampoline.Bounce generateHashmapCallInstructions(Datum h, int count, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    if(h instanceof Nil) return continuation.run(Pair.List(Pair.List(PUSH,HASHMAP),Pair.List(CALL,new Exact(-count))));
    Pair hPair = (Pair)h;
    Datum hd = hPair.car();
    if(!(hd instanceof Pair) && !(hd instanceof Vector) && !(hd instanceof Hashmap)) {
      return () -> generateHashmapCallInstructions(hPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
        return continuation.run(new Pair(Pair.List(PUSH,hd),applicationInstructions));
      });
    } else {
      return () -> run(hd,definitionEnvironment,(valueInstructions) -> () -> {
        Datum instructions = Pair.binaryAppend(valueInstructions,Pair.List(Pair.List(PUSH)));
        return generateHashmapCallInstructions(hPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
          return continuation.run(Pair.binaryAppend(instructions,applicationInstructions));
        });
      });
    }
  }


  private static Trampoline.Bounce compileHashmapLiteral(Hashmap h, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    return generateHashmapCallInstructions((Datum)((OrderedCollection)h.toList()).reverse(),1,definitionEnvironment,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compiling Applications
  private static Trampoline.Bounce compileProcedureApplication(Datum app, int count, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    if(!(app instanceof Pair)) return continuation.run(Pair.List(Pair.List(CALL,new Exact(-count))));
    Pair appPair = (Pair)app;
    Datum hd = appPair.car();
    if(!(hd instanceof Pair) && !(hd instanceof Vector) && !(hd instanceof Hashmap)) {
      return () -> compileProcedureApplication(appPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
        return continuation.run(new Pair(Pair.List(PUSH,hd),applicationInstructions));
      });
    } else {
      return () -> run(hd,definitionEnvironment,(valueInstructions) -> () -> {
        Datum instructions = Pair.binaryAppend(valueInstructions,Pair.List(Pair.List(PUSH)));
        return compileProcedureApplication(appPair.cdr(),count+1,definitionEnvironment,(applicationInstructions) -> () -> {
          return continuation.run(Pair.binaryAppend(instructions,applicationInstructions));
        });
      });
    }
  }


  private static Trampoline.Bounce compileMacroApplication(Symbol name, SyntaxProcedure macro, Datum macroArgs, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = MetaPrimitives.Apply.convertListToArrayList(macroArgs);
    if(name.hasSourceInformation()) {
      return macro.loadWithInvocationSource(name.source()).callWith(args,(expandedExpr) -> () -> run(expandedExpr,definitionEnvironment,continuation));
    } else {
      return macro.callWith(args,(expandedExpr) -> () -> run(expandedExpr,definitionEnvironment,continuation));
    }
  }


  private static SyntaxProcedure isModuleMacro(Environment definitionEnvironment, Symbol moduleMacro) {
    try {
      Datum value = (new ObjectAccessChain(moduleMacro)).loadWithState(definitionEnvironment);
      if(!(value instanceof SyntaxProcedure)) return null;
      return (SyntaxProcedure)value;
    } catch(Exception e) {
      return null;
    }
  }


  private static SyntaxProcedure isMacroApplication(Environment definitionEnvironment, Datum head) {
    if(!(head instanceof Symbol)) return null;
    Symbol headSymbol = (Symbol)head;
    // Check for module macro invocation
    if(ObjectAccessChain.is(headSymbol)) 
      return isModuleMacro(definitionEnvironment,headSymbol);
    // Check for local macro invocation
    return MetaPrimitives.IsSyntax.logic(definitionEnvironment,headSymbol);
  }


  private static Trampoline.Bounce compileApplication(Pair d, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    Datum car = d.car();
    SyntaxProcedure macro = isMacroApplication(definitionEnvironment,car);
    if(macro != null) {
      return compileMacroApplication((Symbol)car,macro,d.cdr(),definitionEnvironment,continuation);
    } else {
      // Reverse the procedure application expression to use the faster negative <call> argument @ runtime
      return compileProcedureApplication((Datum)d.reverse(),0,definitionEnvironment,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Core compilation dispatch
  public static Trampoline.Bounce run(Datum d, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    if(d instanceof Vector) 
      return compileVectorLiteral((Vector)d,definitionEnvironment,continuation);
    if(d instanceof Hashmap) 
      return compileHashmapLiteral((Hashmap)d,definitionEnvironment,continuation);
    if(!(d instanceof Pair)) 
      return compileAtomicLiteral(d,continuation);
    Pair expr = (Pair)d;
    if(isTaggedList(expr,"bytecode")) 
      return compileEscmBytecode(expr,continuation);
    return compileApplication(expr,definitionEnvironment,continuation);
  }
}