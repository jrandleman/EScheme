// Author: Jordan Randleman - escm.primitive.ConcurrentPrimitives
// Purpose:
//    Java primitives for concurrency procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.Void;
import escm.type.Vector;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.callable.Signature;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.runtime.GlobalState;

public class ConcurrentPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // thread
  public static class Thread extends Primitive {
    public java.lang.String escmName() {
      return "thread";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread"),new Symbol("<thunk-callable>")),
        Pair.List(new Symbol("thread"),new Symbol("<name-string>"),new Symbol("<thunk-callable>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nCreate a new thread that invokes <callable-thunk> upon being passed to\n<thread-start!>. Returns <name-string> (defaults to a random string) if\npassed to <thread-name>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      String name = null;
      Datum callable = null;
      if(totalParams != 1 && totalParams != 2) 
        throw new Exceptionf("'(thread <optional-name-string> <callable-thunk>) didn't receive 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof Callable))
          throw new Exceptionf("'(thread <optional-name-string> <callable-thunk>) arg %s isn't a callable!", parameters.get(0).profile());
        callable = parameters.get(0);
      } else { // if(totalParams == 2)
        if(!(parameters.get(0) instanceof escm.type.String))
          throw new Exceptionf("'(thread <optional-name-string> <callable-thunk>) name 1st arg %s isn't a string!", parameters.get(0).profile());
        if(!(parameters.get(1) instanceof Callable))
          throw new Exceptionf("'(thread <optional-name-string> <callable-thunk>) callable 2nd arg %s isn't a callable!", parameters.get(1).profile());
        name = ((escm.type.String)parameters.get(0)).value();
        callable = parameters.get(1);
      }
      if(name == null) return new escm.type.concurrent.Thread(callable);
      return new escm.type.concurrent.Thread(name,callable);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread?
  public static class IsThread extends Primitive {
    public java.lang.String escmName() {
      return "thread?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns whether <obj> is a thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(thread? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.concurrent.Thread);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-name
  public static class ThreadName extends Primitive {
    public java.lang.String escmName() {
      return "thread-name";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-name"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns <thread>'s name.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-name <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.concurrent.Thread)parameters.get(0)).getName());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-id
  public static class ThreadId extends Primitive {
    public java.lang.String escmName() {
      return "thread-id";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-id"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns <thread>'s unique id.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-id <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.concurrent.Thread)parameters.get(0)).threadId());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-runnable
  public static class ThreadRunnable extends Primitive {
    public java.lang.String escmName() {
      return "thread-runnable";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-runnable"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns <thread>'s callable thunk runnable.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-runnable <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.concurrent.Thread)parameters.get(0)).getRunnable();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-status
  public static class ThreadStatus extends Primitive {
    public java.lang.String escmName() {
      return "thread-status";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-status"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns <thread>'s status as a symbolic name:\n  'ready | 'running | 'blocked | 'waiting | 'timed-waiting | 'finished";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-status <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new Symbol(((escm.type.concurrent.Thread)parameters.get(0)).getStatus());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-yield
  public static class ThreadYield extends Primitive {
    public java.lang.String escmName() {
      return "thread-yield";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-yield"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nHints that the runtime may temporarily pause this thread if needed.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(thread-yield) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      escm.type.concurrent.Thread.yield();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set-daemon!
  public static class ThreadSetDaemonBang extends Primitive {
    public java.lang.String escmName() {
      return "thread-set-daemon!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-set-daemon!"),new Symbol("<thread>"),new Symbol("<status-boolean>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nTries to set <thread> to be a JVM daemon thread or not, based on\n<boolean-status>. Returns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.concurrent.Thread) || !(parameters.get(1) instanceof Boolean))
        throw new Exceptionf("'(thread-set-daemon! <thread> <status>) didn't receive exactly 1 thread & 1 boolean: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Thread)parameters.get(0)).setDaemon(parameters.get(1).isTruthy()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-daemon?
  public static class ThreadIsDaemon extends Primitive {
    public java.lang.String escmName() {
      return "thread-daemon?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-daemon?"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns whether <thread> is a JVM daemon thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-daemon? <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Thread)parameters.get(0)).isDaemon());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-priority
  public static class ThreadPriority extends Primitive {
    public java.lang.String escmName() {
      return "thread-priority";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-priority"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns <thread>'s priority, between <*min-priority*> and <*max-priority*>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-priority <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.concurrent.Thread)parameters.get(0)).getPriority());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set-priority!
  public static class ThreadSetPriorityBang extends Primitive {
    public java.lang.String escmName() {
      return "thread-set-priority!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-set-priority!"),new Symbol("<thread>"),new Symbol("<priority-integer>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nTries to set <thread>'s priority to be <int-priority>, which must be between\n<*min-priority*> and <*max-priority*>. Returns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.concurrent.Thread) || !(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
        throw new Exceptionf("'(thread-set-priority! <thread> <priority>) didn't receive exactly 1 thread & 1 integer: %s", Exceptionf.profileArgs(parameters));
      int priority = ((Real)parameters.get(1)).intValue();
      if(priority < escm.type.concurrent.Thread.MIN_PRIORITY || priority > escm.type.concurrent.Thread.MAX_PRIORITY) {
        throw new Exceptionf("'(thread-set-priority! <thread> <priority>) <priority> %d beyond system bounds [%d,%d]: %s", 
          priority,
          escm.type.concurrent.Thread.MIN_PRIORITY,
          escm.type.concurrent.Thread.MAX_PRIORITY,
          Exceptionf.profileArgs(parameters));
      }
      return Boolean.valueOf(((escm.type.concurrent.Thread)parameters.get(0)).setPriority(priority));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-start!
  public static class ThreadStartBang extends Primitive {
    public java.lang.String escmName() {
      return "thread-start!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-start!"),new Symbol("<thread>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nStarts \"<thread> ...\" and invokes their callable thunk runnables.\nReturns a list of threads that failed to start (e.g. already started).";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(thread-start! <thread> ...) expects at least 1 thread: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.concurrent.Thread> threads = new ArrayList<escm.type.concurrent.Thread>();
      for(Datum d : parameters) {
        if(!(d instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-start! <thread> ...) invalid non-thread %s given: %s", d.profile(), Exceptionf.profileArgs(parameters));
        threads.add((escm.type.concurrent.Thread)d);
      }
      Datum failedThreads = Nil.VALUE;
      for(escm.type.concurrent.Thread thread : threads) {
        if(thread.start() == false) failedThreads = new Pair(thread,failedThreads);
      }
      return failedThreads;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-join!
  public static class ThreadJoinBang extends Primitive {
    public java.lang.String escmName() {
      return "thread-join!";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-join!"),new Symbol("<thread>")),
        Pair.List(new Symbol("thread-join!"),new Symbol("<thread>"),new Symbol("<milliseconds-timeout>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nWaits for <thread> to join. If given <max-ms-to-wait>, waits that\nmany milliseconds prior returning control to the calling thread.\nReturns whether <thread> was interrupted.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      int millisToWaitAtMostFor = -1;
      escm.type.concurrent.Thread thread;
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) arg %s isn't a thread!", parameters.get(0).profile());
        thread = (escm.type.concurrent.Thread)parameters.get(0);
      } else if(totalParams == 2) {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) 1st arg %s isn't a thread!", parameters.get(0).profile());
        thread = (escm.type.concurrent.Thread)parameters.get(0);
        if(!(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) 2nd arg %s isn't an integer!", parameters.get(1).profile());
        millisToWaitAtMostFor = ((Real)parameters.get(1)).intValue();
      } else {
        throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) expects 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      }
      if(millisToWaitAtMostFor == -1) return Boolean.valueOf(thread.join());
      return Boolean.valueOf(thread.join(millisToWaitAtMostFor));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-interrupted?
  public static class ThreadIsInterrupted extends Primitive {
    public java.lang.String escmName() {
      return "thread-interrupted?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-interrupted?"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns whether <thread> was interrupted.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-interrupted? <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Thread)parameters.get(0)).isInterrupted());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-interrupt!
  public static class ThreadInterruptBang extends Primitive {
    public java.lang.String escmName() {
      return "thread-interrupt!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("thread-interrupt!"),new Symbol("<thread>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nTries to interrupt <thread>, and returns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Thread))
        throw new Exceptionf("'(thread-interrupt! <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Thread)parameters.get(0)).interrupt());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // interrupted?!
  public static class IsInterruptedBang extends Primitive {
    public java.lang.String escmName() {
      return "interrupted?!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("interrupted?!"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns whether the current thread has been interrupted.\nAlso clears its \"interrupted?\" flag afterwards.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(interrupted?!) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(escm.type.concurrent.Thread.interrupted());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sleep
  public static class Sleep extends Primitive {
    public java.lang.String escmName() {
      return "sleep";
    }

    public Datum signature() {
      return Pair.List(new Symbol("sleep"),new Symbol("<millisecond-integer>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nHas the current thread sleep for <ms-to-sleep> milliseconds.\nReturns whether was interrupted.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Real) || !((Real)parameters.get(0)).isInteger())
        throw new Exceptionf("'(sleep <sleep-millis-time>) didn't receive exactly 1 integer: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(escm.type.concurrent.Thread.sleep(((Real)parameters.get(0)).intValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-thread
  public static class CurrentThread extends Primitive {
    public java.lang.String escmName() {
      return "current-thread";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-thread"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturns the current thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-thread) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return escm.type.concurrent.Thread.currentThread();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // parallel
  public static class Parallel extends Primitive {
    public java.lang.String escmName() {
      return "parallel";
    }

    public Datum signature() {
      return Pair.List(new Symbol("parallel"),new Symbol("<callable-thunk>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nRun the given \"<callable-thunk> ...\" items in parallel.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0)
        throw new Exceptionf("'(parallel <callable> ...) expects at least 1 thunk callable: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.concurrent.Thread> threads = new ArrayList<escm.type.concurrent.Thread>();
      for(Datum param : parameters) {
        if(!(param instanceof Callable))
          throw new Exceptionf("'(parallel <callable> ...) arg %s isn't a thunk callable: %s", param.profile(), Exceptionf.profileArgs(parameters));
        threads.add(new escm.type.concurrent.Thread(param));
      }
      for(escm.type.concurrent.Thread thread : threads) thread.start();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-define'
  public static class ThreadDefineApostrophe extends Primitive {
    public java.lang.String escmName() {
      return "thread-define'";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-define'"),new Symbol("<symbol>"),new Symbol("<value>")),
        Pair.List(new Symbol("thread-define'"),new Symbol("<thread>"),new Symbol("<symbol>"),new Symbol("<value>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nBind <symbolic-variable-name> to <value> in <thread>'s (defaults to the\n\"meta-thread\") dynamic environment (effectively a thread-local global\nenvironment).\n\nUse the <thread-define> macro to pass <symbolic-variable-name> as a literal.\n\nNote that the \"meta-thread\" is a pseudo-thread accessable by all threads:\n  Thread dynamic environments \"inherit\" value bindings from the\n  \"meta-thread\" by caching a copy of them upon reference.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 2 && totalParams != 3)
        throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) didn't receive 2 or 3 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 2) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 1st arg %s isn't a symbol: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        GlobalState.metaThreadDynamicEnvironment.define((Symbol)parameters.get(0),parameters.get(1));
      } else {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 2nd arg %s isn't a symbol: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        ((escm.type.concurrent.Thread)parameters.get(0)).define((Symbol)parameters.get(1),parameters.get(2));
      }
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set!'
  public static class ThreadSetBangApostrophe extends Primitive {
    public java.lang.String escmName() {
      return "thread-set!'";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-set!'"),new Symbol("<symbol>"),new Symbol("<value>")),
        Pair.List(new Symbol("thread-set!'"),new Symbol("<thread>"),new Symbol("<symbol>"),new Symbol("<value>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nSet <symbolic-variable-name> to <value> in <thread>'s (defaults to the\n\"meta-thread\") dynamic environment (effectively a thread-local global\nenvironment).\n\nUse the <thread-set!> macro to pass <symbolic-variable-name> as a literal.\n\nNote that the \"meta-thread\" is a pseudo-thread accessable by all threads:\n  Thread dynamic environments \"inherit\" value bindings from the\n  \"meta-thread\" by caching a copy of them upon reference.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 2 && totalParams != 3)
        throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) didn't receive 2 or 3 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 2) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 1st arg %s isn't a symbol: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        Symbol varName = (Symbol)parameters.get(0);
        if(!GlobalState.metaThreadDynamicEnvironment.has(varName))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) %s isn't a variable in the meta thread dynamic environment: %s", 
            varName.value(), Exceptionf.profileArgs(parameters));
        GlobalState.metaThreadDynamicEnvironment.set(varName,parameters.get(1));
      } else {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 2nd arg %s isn't a symbol: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        ((escm.type.concurrent.Thread)parameters.get(0)).set((Symbol)parameters.get(1),parameters.get(2));
      }
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-get'
  public static class ThreadGetApostrophe extends Primitive {
    public java.lang.String escmName() {
      return "thread-get'";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-get'"),new Symbol("<symbol>")),
        Pair.List(new Symbol("thread-get'"),new Symbol("<thread>"),new Symbol("<symbol>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nGet <symbolic-variable-name>'s value in <thread>'s (defaults to the\n\"meta-thread\") dynamic environment (effectively a thread-local global\nenvironment).\n\nUse the <thread-get> macro to pass <symbolic-variable-name> as a literal.\n\nNote that the \"meta-thread\" is a pseudo-thread accessable by all threads:\n  Thread dynamic environments \"inherit\" value bindings from the\n  \"meta-thread\" by caching a copy of them upon reference.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 1 && totalParams != 2)
        throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) didn't receive 1 or 2 args (optional thread, variable symbol): %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 1st arg %s isn't a symbolic variable name: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        Symbol varName = (Symbol)parameters.get(0);
        if(!GlobalState.metaThreadDynamicEnvironment.has(varName))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) %s isn't a variable in the meta thread dynamic environment: %s", 
            varName.value(), Exceptionf.profileArgs(parameters));
        return GlobalState.metaThreadDynamicEnvironment.get(varName);
      } else {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 2nd arg %s isn't a symbolic variable name: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        return ((escm.type.concurrent.Thread)parameters.get(0)).get((Symbol)parameters.get(1));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-defined?'
  public static class ThreadIsDefinedApostrophe extends Primitive {
    public java.lang.String escmName() {
      return "thread-defined?'";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-defined?'"),new Symbol("<symbol>")),
        Pair.List(new Symbol("thread-defined?'"),new Symbol("<thread>"),new Symbol("<symbol>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturn whether <symbolic-variable-name> is defined in <thread>'s (defaults to the\n\"meta-thread\") dynamic environment (effectively a thread-local global\nenvironment).\n\nUse the <thread-get> macro to pass <symbolic-variable-name> as a literal.\n\nNote that the \"meta-thread\" is a pseudo-thread accessable by all threads:\n  Thread dynamic environments \"inherit\" value bindings from the\n  \"meta-thread\" by caching a copy of them upon reference.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 1 && totalParams != 2)
        throw new Exceptionf("'(thread-defined?' <optional-thread> <var-name>) didn't receive 1 or 2 args (optional thread, variable symbol): %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-defined?' <optional-thread> <var-name>) 1st arg %s isn't a symbolic variable name: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        return Boolean.valueOf(GlobalState.metaThreadDynamicEnvironment.has((Symbol)parameters.get(0)));
      } else {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-defined?' <optional-thread> <var-name>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-defined?' <optional-thread> <var-name>) 2nd arg %s isn't a symbolic variable name: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        return ((escm.type.concurrent.Thread)parameters.get(0)).has((Symbol)parameters.get(1));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-dynamic-environment
  public static class ThreadDynamicEnvironment extends Primitive {
    public java.lang.String escmName() {
      return "thread-dynamic-environment";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("thread-dynamic-environment")),
        Pair.List(new Symbol("thread-dynamic-environment"),new Symbol("<thread>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Threads\nReturn an associative list of the variables (and their values!) defined\nin <thread>'s (defaults to the \"meta-thread\") dynamic environment.\n\nNote that the \"meta-thread\" is a pseudo-thread accessable by all threads:\n  Thread dynamic environments \"inherit\" value bindings from the\n  \"meta-thread\" by caching a copy of them upon reference.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 0 && totalParams != 1)
        throw new Exceptionf("'(thread-dynamic-environment <optional-thread>) didn't receive 0 or 1 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 0) {
        return GlobalState.metaThreadDynamicEnvironment.bindingsAsAssocList();
      } else {
        if(!(parameters.get(0) instanceof escm.type.concurrent.Thread))
          throw new Exceptionf("'(thread-dynamic-environment <optional-thread>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        return ((escm.type.concurrent.Thread)parameters.get(0)).dynamicEnvironment();
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex
  public static class Mutex extends Primitive {
    public java.lang.String escmName() {
      return "mutex";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("mutex")),
        Pair.List(new Symbol("mutex"),new Symbol("<name-string>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nCreate a new reentrant-lock mutex. Returns <name-str> (defaults to a random string)\nif passed to <mutex-name>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams == 0) {
        return new escm.type.concurrent.Mutex();
      } else if(totalParams == 1) {
        Datum param = parameters.get(0);
        if(!(param instanceof escm.type.String))
          throw new Exceptionf("'(mutex <optional-name-string>) 1st arg %s isn't a name string: %s", param.profile(), Exceptionf.profileArgs(parameters));
        return new escm.type.concurrent.Mutex(((escm.type.String)param).value());
      } else {
        throw new Exceptionf("'(mutex <optional-name-string>) didn't receive either 0 or 1 args: %s", Exceptionf.profileArgs(parameters));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex?
  public static class IsMutex extends Primitive {
    public java.lang.String escmName() {
      return "mutex?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns whether <obj> is a mutex.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(mutex? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.concurrent.Mutex);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-name
  public static class MutexName extends Primitive {
    public java.lang.String escmName() {
      return "mutex-name";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-name"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns <mutex>'s name.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-name <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      String name = ((escm.type.concurrent.Mutex)parameters.get(0)).getName();
      if(name == null) return Boolean.FALSE;
      return new escm.type.String(name);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-specific
  public static class MutexSpecific extends Primitive {
    public java.lang.String escmName() {
      return "mutex-specific";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-specific"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nGet the variable value \"specifically associated\" with <mutex>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-specific <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.concurrent.Mutex)parameters.get(0)).getSpecific();
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // mutex-set-specific!
  public static class MutexSetSpecificBang extends Primitive {
    public java.lang.String escmName() {
      return "mutex-set-specific!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-set-specific!"),new Symbol("<mutex>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nSet the variable value \"specifically associated\" with <mutex> to <obj>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-set-specific! <mutex> <value>) didn't receive exactly 1 mutex & 1 obj: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.concurrent.Mutex)parameters.get(0)).setSpecific(parameters.get(1));
      return Void.VALUE;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // mutex-lock!
  public static class MutexLockBang extends Primitive {
    public java.lang.String escmName() {
      return "mutex-lock!";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("mutex-lock!"),new Symbol("<mutex>")),
        Pair.List(new Symbol("mutex-lock!"),new Symbol("<mutex>"),new Symbol("<milliseconds-timeout>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns whether managed to acquired the lock prior to <millisecond-timeout>\n(defaults to Infinity).";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2)
        throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) didn't receive exactly 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lock = parameters.get(0);
      if(!(lock instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) 1st arg %s isn't a mutex: %s", lock.profile(), Exceptionf.profileArgs(parameters));
      if(parameters.size() == 2) {
        Datum timeout = parameters.get(1);
        if(!(timeout instanceof Real) || !((Real)timeout).isInteger())
          throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) 2nd arg %s isn't an integer: %s", timeout.profile(), Exceptionf.profileArgs(parameters));
        return Boolean.valueOf(((escm.type.concurrent.Mutex)lock).lock(((Real)timeout).intValue()));
      } else {
        ((escm.type.concurrent.Mutex)lock).lock();
        return Boolean.TRUE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-unlock!
  public static class MutexUnlockBang extends Primitive {
    public java.lang.String escmName() {
      return "mutex-unlock!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-unlock!"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nUnlocks <mutex>. Returns #f if the current thread wasn't locking <mutex>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-unlock! <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Mutex)parameters.get(0)).unlock());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-locked?
  public static class MutexIsLocked extends Primitive {
    public java.lang.String escmName() {
      return "mutex-locked?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-locked?"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns whether <mutex> is locked by any thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-locked? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Mutex)parameters.get(0)).isLocked());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-queue-length
  public static class MutexQueueLength extends Primitive {
    public java.lang.String escmName() {
      return "mutex-queue-length";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-queue-length"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns the total number of threads waiting to acquire <mutex>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-queue-length <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.concurrent.Mutex)parameters.get(0)).queueLength());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-queued?
  public static class MutexIsQueued extends Primitive {
    public java.lang.String escmName() {
      return "mutex-queued?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-queued?"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns whether any thread is waiting for <mutex>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-queued? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Mutex)parameters.get(0)).isQueued());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-hold-count
  public static class MutexHoldCount extends Primitive {
    public java.lang.String escmName() {
      return "mutex-hold-count";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-hold-count"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nGet the total number of holds on <mutex> by the current thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-hold-count <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.concurrent.Mutex)parameters.get(0)).holdCount());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-held?
  public static class MutexIsHeld extends Primitive {
    public java.lang.String escmName() {
      return "mutex-held?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("mutex-held?"),new Symbol("<mutex>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Mutexes\nReturns whether <mutex> is held by the current thread.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.concurrent.Mutex))
        throw new Exceptionf("'(mutex-held? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.concurrent.Mutex)parameters.get(0)).isHeld());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // promise
  //
  // ; returns a <promise-handle> ::= [ <promise-value> <resolved-or-rejected> <thread> ]
  // (define (promise promise-lambda)
  //   (def promise-thread
  //     (thread
  //       #(handle
  //         0
  //         (call/cc
  //           (lambda (resolve)
  //             (promise-lambda 
  //               resolve 
  //               (lambda (promise-result) 
  //                 (handle 1 #f) 
  //                 (resolve promise-result))))))))
  //   (def handle [#f #t promise-thread]) ; defaults to <resolved>
  //   (thread-start! promise-thread)
  //   handle)
  public static class Promise extends Primitive {
    public java.lang.String escmName() {
      return "promise";
    }

    public Datum signature() {
      return Pair.List(new Symbol("promise"),new Symbol("<promise-lambda>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Promises\nEScheme's implementation of JavaScript's <Promise> concurrency paradigm.\n\nReturns a <promise-handle> value to <await> the promise with later on.\n\n<promise-lambda> should accept two unary callables as its arguments: the \n<resolve> and <reject> procedures.\n  * Call <resolve> with the success value of the finished promise.\n  * Call <reject> with an error value for the promise.\n  * Note that if neither's called, <promise-lambda>'s final value resolves.\n\nExample use:\n\n  (define (test resolve?)\n    (promise\n      (lambda (resolve reject)\n        (if resolve? \n            (resolve 'success!)\n            (reject 'failed!)))))\n\n  (define (success result) result)\n  (define (failure error) error)\n\n  ; Prints 'success!\n  (println (await (test #t) success failure))\n\n  ; Prints 'failed!\n  (println (await (test #f) success failure))\n\n  ; Prints '(success! success! success!)\n  (define all-promises (await-all (test #t) (test #t) (test #t)))\n  (println (await all-promises success failure))\n\n  ; Prints 'failed!\n  (define all-promises (await-all (test #t) (test #f) (test #t)))\n  (println (await all-promises success failure))";
    }

    private static PrimitiveProcedure getResolve(Trampoline.Continuation continuation) {
      return new PrimitiveProcedure("resolve",
        new Callable() {
          public String docstring() {
            return "Resolve a value to successfully complete a <promise>'s execution.";
          }
          public Datum signature() {
            return Pair.List(new Symbol("resolve"),new Symbol("<obj>"));
          }
          public Trampoline.Bounce callWith(ArrayList<Datum> result, Trampoline.Continuation ignored) throws Exception {
            if(result.size() != 1)
              throw new Exceptionf("'(resolve <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(result));
            return continuation.run(result.get(0));
          }
        });
    }

    private static PrimitiveProcedure getReject(Vector handle, Trampoline.Continuation continuation) {
      return new PrimitiveProcedure("reject",
        new Callable() {
          public String docstring() {
            return "Rejects a value to trigger an error during a <promise>'s execution.";
          }
          public Datum signature() {
            return Pair.List(new Symbol("reject"),new Symbol("<obj>"));
          }
          public Trampoline.Bounce callWith(ArrayList<Datum> result, Trampoline.Continuation ignored) throws Exception {
            if(result.size() != 1)
              throw new Exceptionf("'(reject <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(result));
            handle.set(1,Boolean.FALSE); // set <rejected>
            return continuation.run(result.get(0));
          }
        });
    }

    public static Datum logic(Callable promiseLambda) throws Exception {
      Vector handle = new Vector();
      handle.push(Boolean.FALSE); // store the promise value
      handle.push(Boolean.TRUE); // default promise to <resolved>
      escm.type.concurrent.Thread promiseThread = new escm.type.concurrent.Thread(new Callable() {
        public String docstring() {
          return "Runnable procedure for a <promise> object's internal thread.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(Procedure.DEFAULT_NAME));
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> ignoredArgs, Trampoline.Continuation continuation) throws Exception {
          Trampoline.Continuation savePromiseValueContinuation = (result) -> () -> {
            handle.set(0,result); // save promise result
            return continuation.run(Void.VALUE);
          };
          ArrayList<Datum> promiseArgs = new ArrayList<Datum>(2);
          promiseArgs.add(getResolve(savePromiseValueContinuation));
          promiseArgs.add(getReject(handle,savePromiseValueContinuation));
          return promiseLambda.callWith(promiseArgs,savePromiseValueContinuation);
        }
      });
      handle.push(promiseThread);
      promiseThread.start();
      return handle;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(promise <promise-lambda>) didn't receive exactly 1 callable: %s", Exceptionf.profileArgs(parameters));
      return logic((Callable)parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // promise?
  //
  // (define (promise? obj)
  //   (and (vector? obj) (= (len obj) 3) (thread? (obj 2))))
  public static class IsPromiseP extends Primitive {
    public java.lang.String escmName() {
      return "promise?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("promise?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Promises\nDetermine if <obj> came from <promise>.";
    }

    public static boolean logic(Datum obj) throws Exception {
      if(!(obj instanceof Vector)) return false;
      Vector vec = (Vector)obj;
      synchronized(vec) {
        return vec.size() == 3 && vec.get(2) instanceof escm.type.concurrent.Thread;
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(promise? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // await
  //
  // NOTE: The below EScheme code doesn't account for the optional first
  //       <timeout> argument (defaults to #f).
  //
  // (define (await promise resolve-lambda reject-lambda)
  //   (if (promise? promise)
  //       (begin
  //         (thread-join! (promise 2))
  //         (if (promise 1)
  //             (resolve-lambda (promise 0))
  //             (reject-lambda (promise 0))))
  //       (resolve-lambda promise)))
  public static class Await extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "await";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("await"),new Symbol("<promise>"),new Symbol("<resolve-lambda>"),new Symbol("<reject-lambda>")),
        Pair.List(new Symbol("await"),new Symbol("<timeout-ms>"),new Symbol("<promise>"),new Symbol("<resolve-lambda>"),new Symbol("<reject-lambda>")));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Promises\nPause the current thread's execution until <promise> either resolves or \nrejects. If it rejects, the value is passed to <reject-lambda>. Otherwise,\nthe value is passed to <resolve-lambda>.\n\nIf <timeout-ms> is provided, <await> will wait for the promise's thread to\ncomplete at most <timeout> milliseconds, before <reject>ing with the 'timeout\nsymbol.\n\nNote that it's safe to pass non-promises to <await>: they're treated as \nvalues of immediately resolved promises.\n\n<await> conceptually mirrors JavaScript's <then>/<catch> promise methods.\n\nSee <promise>'s <help> entry for an example use!";
    }

    private static Integer parseTimeout(int n, ArrayList<Datum> parameters) {
      if(n != 4) return null;
      Datum obj = parameters.get(0);
      if(obj instanceof Real) {
        Real r = (Real)obj;
        if(r.isInteger()) return r.intValue();
      }
      return null;
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n != 3 && n != 4)
        throw new Exceptionf("'(await <optional-timeout> <promise> <resolve-lambda> <reject-lambda>) didn't receive 3 or 4 args: %s", Exceptionf.profileArgs(parameters));
      Integer timeout = parseTimeout(n,parameters);
      Datum promise = parameters.get(n-3);
      Datum resolveLambda = parameters.get(n-2);
      Datum rejectLambda = parameters.get(n-1);
      if(!(resolveLambda instanceof Callable))
        throw new Exceptionf("'(await <optional-timeout> <promise> <resolve-lambda> <reject-lambda>) <resolve-lambda> isn't callable: %s", Exceptionf.profileArgs(parameters));
      if(!(rejectLambda instanceof Callable))
        throw new Exceptionf("'(await <optional-timeout> <promise> <resolve-lambda> <reject-lambda>) <resolve-lambda> isn't callable: %s", Exceptionf.profileArgs(parameters));
      if(IsPromiseP.logic(promise)) {
        Vector handle = (Vector)promise;
        if(timeout != null) {
          escm.type.concurrent.Thread t = (escm.type.concurrent.Thread)handle.get(2);
          t.join(timeout);
          if(!t.getStatus().equals("finished")) {
            ArrayList<Datum> timeoutArgs = new ArrayList<Datum>(1);
            timeoutArgs.add(new Symbol("timeout"));
            return ((Callable)rejectLambda).callWith(timeoutArgs,continuation);
          }
        } else {
          ((escm.type.concurrent.Thread)handle.get(2)).join();
        }
        ArrayList<Datum> args = new ArrayList<Datum>(1);
        args.add(handle.get(0));
        if(handle.get(1).isTruthy()) {
          return ((Callable)resolveLambda).callWith(args,continuation);
        } else {
          return ((Callable)rejectLambda).callWith(args,continuation);
        }
      } else {
        ArrayList<Datum> args = new ArrayList<Datum>(1);
        args.add(promise);
        return ((Callable)resolveLambda).callWith(args,continuation);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // await-all
  //
  // (define *await-all-timeout* 50) ; milliseconds
  //
  // (define (escm-promise-finished? promise-thread)
  //   (thread-join! promise-thread *await-all-timeout*)
  //   (eq? (thread-status promise-thread) 'finished))
  //
  // (define (await-all . promises-ordered-collection)
  //   (when (= (len promises-ordered-collection) 1)
  //     (def arg (car promises-ordered-collection))
  //     (if (and (oc? arg) (not (promise? arg))) 
  //         (set! promises-ordered-collection arg)))
  //   (promise
  //     (lambda (resolve reject)
  //       (def n (len promises-ordered-collection))
  //       (def unfinished? (vector-unfold #(= %1 n) (lambda (_) #t) #(+ %1 1) 0))
  //       (def promises (ac->vector promises-ordered-collection)) ; Theta(1) promise random-access
  //       (def still-working? #t)
  //       (while (still-working?)
  //         (set! still-working? #f)
  //         (def idx 0)
  //         (while ((< idx n))
  //           (when (unfinished? idx)
  //             (def p (promises idx))
  //             (if (promise? p)
  //                 (if (escm-promise-finished? (p 2))
  //                     (begin
  //                       (if (p 1) (promises idx (p 0)) (reject (p 0)))
  //                       (unfinished? idx #f))
  //                     (set! still-working? #t))
  //                 (unfinished? idx #f))) ; don't need to finish non-promises
  //           (set! idx (+ idx 1))))
  //       (resolve (vector->list promises)))))
  public static class AwaitAll extends Primitive {
    private static final int AWAIT_ALL_CYCLE_PERIOD = 50; // milliseconds

    public java.lang.String escmName() {
      return "await-all";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("await-all"),new Symbol("<promises-ordered-collection>")),
        Pair.List(new Symbol("await-all"),new Symbol("<promise>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:Concurrency:Promises\nGet a new promise that either resolves to a list of the <promise>s resolved\nresults, or rejects with the first error encountered. Akin to JavaScript's\n<Promise.all()>.\n\n<await-all> can be called in two ways:\n  1. (await-all <promises-ordered-collection>)\n  2. (await-all <promise> ...)\n\nNote that it's safe to pass non-promises to <await-all>: they're treated as \nvalues of immediately resolved promises.\n\nSee <promise>'s <help> entry for an example use!";
    }

    private ArrayList<Datum> parsePromises(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 1) {
        Datum arg = parameters.get(0);
        if(arg instanceof OrderedCollection && !IsPromiseP.logic(arg)) {
          parameters = ((OrderedCollection)arg).toArrayList();
        }
      }
      return parameters;
    }

    private boolean promiseFinished(Vector handle) throws Exception {
      escm.type.concurrent.Thread promiseThread = (escm.type.concurrent.Thread)handle.get(2);
      promiseThread.join(AWAIT_ALL_CYCLE_PERIOD);
      return promiseThread.getStatus().equals("finished");
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      ArrayList<Datum> promises = parsePromises(parameters);
      int n = promises.size();
      return Promise.logic(new Callable() {
        public String docstring() {
          return "An <await-all> promise lambda: resolves to a list of resolved values, or\nrejects with the first error.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(Procedure.DEFAULT_NAME),new Symbol("<resolve>"),new Symbol("<reject>"));
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> result, Trampoline.Continuation ignored) throws Exception {
          Callable resolve = (Callable)result.get(0);
          Callable reject = (Callable)result.get(1);
          Datum[] resolvedValues = new Datum[n];
          boolean[] unfinished = new boolean[n];
          for(int i = 0; i < n; ++i) {
            unfinished[i] = true;
          }
          boolean stillWorking = true;
          while(stillWorking) {
            stillWorking = false;
            for(int i = 0; i < n; ++i) {
              if(unfinished[i] == true) {
                Datum p = promises.get(i);
                if(IsPromiseP.logic(p)) {
                  Vector handle = (Vector)p;
                  if(promiseFinished(handle)) {
                    if(handle.get(1).isTruthy()) {
                      resolvedValues[i] = handle.get(0);
                    } else {
                      ArrayList<Datum> failArgs = new ArrayList<Datum>();
                      failArgs.add(handle.get(0));
                      return reject.callWith(failArgs,ignored);
                    }
                    unfinished[i] = false;
                  } else {
                    stillWorking = true;
                  }
                } else {
                  unfinished[i] = false; // don't need to finish non-promises
                  resolvedValues[i] = p;
                }
              }
            }
          }
          Datum resolvedList = Nil.VALUE;
          for(int i = resolvedValues.length-1; i >= 0; --i) {
            resolvedList = new Pair(resolvedValues[i],resolvedList);
          }
          ArrayList<Datum> winArgs = new ArrayList<Datum>();
          winArgs.add(resolvedList);
          return resolve.callWith(winArgs,ignored);
        }
      });
    }
  }
}