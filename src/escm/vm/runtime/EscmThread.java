// Author: Jordan Randleman - escm.vm.runtime.EscmThread
// Purpose:
//    Class for the current EScheme session's thread. Used to track a variety
//      of variables in a global way environmentally while maintaining thread
//      local semantics.

package escm.vm.runtime;
import java.util.ArrayList;
import java.util.Stack;
import java.math.BigInteger;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.PrimitiveProcedure;
import escm.primitive.UtilityPrimitives;
import escm.vm.type.Environment;

public abstract class EscmThread extends Thread {
  // <escm.type.Thread> thread-local object
  public escm.type.Thread currentThread = null;
  // <CallStack> thread-local call stack
  public Stack<String> callStack = new Stack<String>();
  // <escm.primitive.UtilityPrimitives.DynamicWind> thread-local set of winds
  public Datum dynamicWinds = Nil.VALUE;
  // <escm.primitive.UtilityPrimitives.WithExceptionHandler> thread-local set of winds
  public Datum currentExceptionHandlers = Pair.List(
    new PrimitiveProcedure(
      "unhandled-exception-handler",
      (params, cont) -> {
        ArrayList<Datum> args = new ArrayList<Datum>(1+params.size());
        args.add(new escm.type.String("unhandled exception:"));
        args.addAll(params);
        return cont.run(UtilityPrimitives.Error.logic(args));
      }));
  // <escm.util.UniqueSymbolString> thread-local counter
  public BigInteger uniqueCounter = BigInteger.ZERO;
  // <escm.type.Thread> thread-local dynamic environment
  public Environment dynamicEnvironment = new Environment();
  // Other <EscmThread> properties
  public EscmThread(escm.type.Thread currentThread){
    super();
    this.currentThread = currentThread;
  }
  public EscmThread(escm.type.Thread currentThread, String name){
    super((Runnable)null,name);
    this.currentThread = currentThread;
  }
  public EscmThread(escm.type.Thread currentThread, Runnable runnable, String name){
    super(runnable,name);
    this.currentThread = currentThread;
  }
  public abstract void run();
};