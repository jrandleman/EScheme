// Author: Jordan Randleman - escm.vm.runtime.EscmThread
// Purpose:
//    Class for the current EScheme session's thread. Used to track a variety
//      of variables in a global way environmentally while maintaining thread
//      local semantics.

package escm.vm.runtime;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.math.BigInteger;
import escm.util.Pair;
import escm.type.Datum;
import escm.type.Nil;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.type.procedure.PrimitiveProcedure;
import escm.primitive.UtilityPrimitives;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;

public abstract class EscmThread extends Thread {
  ////////////////////////////////////////////////////////////////////////////
  // EScheme <Datum> "current thread" handle
  public escm.type.concurrent.Thread currentThread = null;


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local call stack [used by <CallStack>]
  public ArrayDeque<Pair<String,SourceInformation>> callStack = EscmCallStack.newCallStack();


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local set of winds [used by <escm.primitive.UtilityPrimitives.DynamicWind>]
  public Datum dynamicWinds = Nil.VALUE;


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local set of winds [used by <escm.primitive.UtilityPrimitives.WithExceptionHandler>]
  public Datum currentExceptionHandlers = escm.type.Pair.List(
    new PrimitiveProcedure(
      "unhandled-exception-handler",
      (params, cont) -> {
        ArrayList<Datum> args = new ArrayList<Datum>(1+params.size());
        args.add(new escm.type.String("unhandled exception:"));
        args.addAll(params);
        return cont.run(UtilityPrimitives.Error.logic(args));
      }));


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local gensym counter [used by <escm.util.UniqueSymbolString>]
  public BigInteger uniqueCounter = BigInteger.ZERO;


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local dynamic environment [used by <escm.type.concurrent.Thread>]
  public Environment dynamicEnvironment = new Environment();


  ////////////////////////////////////////////////////////////////////////////
  // Thread-local current input & output ports [used by <escm.type.port.Port>]
  public InputPort currentInputPort   = InputPort.STDIN;
  public OutputPort currentOutputPort = OutputPort.STDOUT;


  ////////////////////////////////////////////////////////////////////////////
  // Constrcutors
  public EscmThread(escm.type.concurrent.Thread currentThread) {
    super();
    this.currentThread = currentThread;
  }
  
  public EscmThread(escm.type.concurrent.Thread currentThread, String name) {
    super((Runnable)null,name);
    this.currentThread = currentThread;
  }
  
  public EscmThread(escm.type.concurrent.Thread currentThread, Runnable runnable, String name) {
    super(runnable,name);
    this.currentThread = currentThread;
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Abstract <run()> method to implement
  public abstract void run();
};