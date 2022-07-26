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
import escm.type.Symbol;
import escm.type.Number;
import escm.util.Trampoline;
import escm.primitive.ListPrimitives;
import escm.primitive.MetaPrimitives;
import escm.vm.type.Callable;

public class Compiler {
  ////////////////////////////////////////////////////////////////////////////
  // Static Instruction Symbol Constants
  private static final Symbol LOAD = new Symbol("load");
  private static final Symbol CALL = new Symbol("call");
  private static final Symbol PUSH = new Symbol("push");


  ////////////////////////////////////////////////////////////////////////////
  // Static Thread-Safe Macro Registry
  public static final ConcurrentHashMap<String,Callable> MACRO_REGISTRY = new ConcurrentHashMap<String,Callable>();


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
  // Compiling Applications
  private static Trampoline.Bounce compileProcedureApplication(Datum app, int count, Trampoline.Continuation continuation) throws Exception {
    if(!(app instanceof Pair)) return continuation.run(Pair.List(Pair.List(CALL,new Number(-count))));
    Pair appPair = (Pair)app;
    if(!(appPair.car() instanceof Pair)) {
      return () -> compileProcedureApplication(appPair.cdr(),count+1,(applicationInstructions) -> {
        return continuation.run(new Pair(Pair.List(PUSH,appPair.car()),applicationInstructions));
      });
    } else {
      return () -> run(appPair.car(),(valueInstructions) -> () -> {
        Datum instructions = ListPrimitives.Append.binaryAppend(valueInstructions,Pair.List(Pair.List(PUSH)));
        return compileProcedureApplication(appPair.cdr(),count+1,(applicationInstructions) -> {
          return continuation.run(ListPrimitives.Append.binaryAppend(instructions,applicationInstructions));
        });
      });
    }
  }


  private static Trampoline.Bounce compileApplication(Pair d, Trampoline.Continuation continuation) throws Exception {
    // Expand Macro
    if(d.car() instanceof Symbol && MACRO_REGISTRY.containsKey(((Symbol)d.car()).value())) {
      ArrayList<Datum> args = MetaPrimitives.Apply.convertListToArrayList(d.cdr());
      return MACRO_REGISTRY.get(((Symbol)d.car()).value()).callWith(args,(expandedExpr) -> () -> run(expandedExpr,continuation));
    // Compile procedure application
    } else {
      // Reverse the application expression to use the faster negative <call> argument @ runtime
      return compileProcedureApplication(ListPrimitives.Reverse.logic(d),0,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Core compilation dispatch
  public static Trampoline.Bounce run(Datum d, Trampoline.Continuation continuation) throws Exception {
    if(!(d instanceof Pair)) 
      return compileAtomicLiteral(d,continuation);
    Pair expr = (Pair)d;
    if(isTaggedList(expr,"bytecode")) 
      return compileEscmBytecode(expr,continuation);
    return compileApplication(expr,continuation);
  }
}