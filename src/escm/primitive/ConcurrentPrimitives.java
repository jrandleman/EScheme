// Author: Jordan Randleman - escm.primitive.ConcurrentPrimitives
// Purpose:
//    Java primitives for concurrency procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Void;
import escm.type.Boolean;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.Main;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.Environment;
import escm.vm.runtime.EscmThread;
import escm.vm.runtime.GlobalState;

public class ConcurrentPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // thread
  public static class Thread implements Primitive {
    public java.lang.String escmName() {
      return "thread";
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
      if(name == null) return new escm.type.Thread(callable);
      return new escm.type.Thread(name,callable);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread?
  public static class IsThread implements Primitive {
    public java.lang.String escmName() {
      return "thread?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(thread? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Thread);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-name
  public static class ThreadName implements Primitive {
    public java.lang.String escmName() {
      return "thread-name";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-name <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.Thread)parameters.get(0)).getName());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-id
  public static class ThreadId implements Primitive {
    public java.lang.String escmName() {
      return "thread-id";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-id <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Number(((escm.type.Thread)parameters.get(0)).getId());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-runnable
  public static class ThreadRunnable implements Primitive {
    public java.lang.String escmName() {
      return "thread-runnable";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-runnable <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Thread)parameters.get(0)).getRunnable();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-status
  public static class ThreadStatus implements Primitive {
    public java.lang.String escmName() {
      return "thread-status";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-status <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Symbol(((escm.type.Thread)parameters.get(0)).getStatus());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-yield
  public static class ThreadYield implements Primitive {
    public java.lang.String escmName() {
      return "thread-yield";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(thread-yield) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      escm.type.Thread.yield();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set-daemon!
  public static class ThreadSetDaemonBang implements Primitive {
    public java.lang.String escmName() {
      return "thread-set-daemon!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.Thread) || !(parameters.get(1) instanceof Boolean))
        throw new Exceptionf("'(thread-set-daemon! <thread> <status>) didn't receive exactly 1 thread & 1 boolean: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Thread)parameters.get(0)).setDaemon(((Boolean)parameters.get(1)) == Boolean.TRUE);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-daemon?
  public static class ThreadIsDaemon implements Primitive {
    public java.lang.String escmName() {
      return "thread-daemon?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-daemon? <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Thread)parameters.get(0)).isDaemon());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-priority
  public static class ThreadPriority implements Primitive {
    public java.lang.String escmName() {
      return "thread-priority";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-priority <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Number(((escm.type.Thread)parameters.get(0)).getPriority());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set-priority!
  public static class ThreadSetPriorityBang implements Primitive {
    public java.lang.String escmName() {
      return "thread-set-priority!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.Thread) || !(parameters.get(1) instanceof escm.type.Number))
        throw new Exceptionf("'(thread-set-priority! <thread> <priority>) didn't receive exactly 1 thread & 1 number: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Thread)parameters.get(0)).setPriority(((escm.type.Number)parameters.get(1)).intValue());
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-start!
  public static class ThreadStartBang implements Primitive {
    public java.lang.String escmName() {
      return "thread-start!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-start! <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Thread)parameters.get(0)).start();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-join!
  public static class ThreadJoinBang implements Primitive {
    public java.lang.String escmName() {
      return "thread-join!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      int millisToWaitAtMostFor = -1;
      escm.type.Thread thread;
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) arg %s isn't a thread!", parameters.get(0).profile());
        thread = (escm.type.Thread)parameters.get(0);
      } else if(totalParams == 2) {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) 1st arg %s isn't a thread!", parameters.get(0).profile());
        thread = (escm.type.Thread)parameters.get(0);
        if(!(parameters.get(1) instanceof escm.type.Number))
          throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) 2nd arg %s isn't a number!", parameters.get(1).profile());
        millisToWaitAtMostFor = ((escm.type.Number)parameters.get(1)).intValue();
      } else {
        throw new Exceptionf("'(thread-join! <thread> <optional-millis-max-wait>) expects 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      }
      if(millisToWaitAtMostFor == -1) return Boolean.valueOf(thread.join());
      return Boolean.valueOf(thread.join(millisToWaitAtMostFor));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-interrupted?
  public static class ThreadIsInterrupted implements Primitive {
    public java.lang.String escmName() {
      return "thread-interrupted?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-interrupted? <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Thread)parameters.get(0)).isInterrupted());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-interrupt!
  public static class ThreadInterruptBang implements Primitive {
    public java.lang.String escmName() {
      return "thread-interrupt!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Thread))
        throw new Exceptionf("'(thread-interrupt! <thread>) didn't receive exactly 1 thread: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Thread)parameters.get(0)).interrupt();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-thread-interrupted?!
  public static class IsInterruptedBang implements Primitive {
    public java.lang.String escmName() {
      return "current-thread-interrupted?!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(interrupted?!) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(escm.type.Thread.interrupted());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-thread-sleep
  public static class Sleep implements Primitive {
    public java.lang.String escmName() {
      return "current-thread-sleep";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Number))
        throw new Exceptionf("'(sleep <sleep-millis-time>) didn't receive exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(escm.type.Thread.sleep(((escm.type.Number)parameters.get(0)).intValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-thread
  public static class CurrentThread implements Primitive {
    public java.lang.String escmName() {
      return "current-thread";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-thread) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return escm.type.Thread.currentThread();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // parallel
  public static class Parallel implements Primitive {
    public java.lang.String escmName() {
      return "parallel";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0)
        throw new Exceptionf("'(parallel <callable> ...) expects at least 1 thunk callable: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.Thread> threads = new ArrayList<escm.type.Thread>();
      for(Datum param : parameters) {
        if(!(param instanceof Callable))
          throw new Exceptionf("'(parallel <callable> ...) arg %s isn't a thunk callable: %s", param.profile(), Exceptionf.profileArgs(parameters));
        threads.add(new escm.type.Thread(param));
      }
      for(escm.type.Thread thread : threads) thread.start();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-define'
  public static class ThreadDefineApostrophe implements Primitive {
    public java.lang.String escmName() {
      return "thread-define'";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 2 && totalParams != 3)
        throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) didn't receive 2 or 3 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 2) {
        if(!(parameters.get(0) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 1st arg %s isn't a symbol: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        GlobalState.metaThreadDynamicEnvironment.define(((escm.type.Symbol)parameters.get(0)).value(),parameters.get(1));
      } else {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-define' <optional-thread> <var-name> <value>) 2nd arg %s isn't a symbol: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        ((escm.type.Thread)parameters.get(0)).define(((escm.type.Symbol)parameters.get(1)).value(),parameters.get(2));
      }
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-set!'
  public static class ThreadSetBangApostrophe implements Primitive {
    public java.lang.String escmName() {
      return "thread-set!'";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 2 && totalParams != 3)
        throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) didn't receive 2 or 3 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 2) {
        if(!(parameters.get(0) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 1st arg %s isn't a symbol: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        String varName = ((escm.type.Symbol)parameters.get(0)).value();
        if(!GlobalState.metaThreadDynamicEnvironment.has(varName))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) %s isn't a variable in the meta thread dynamic environment: %s", 
            varName, Exceptionf.profileArgs(parameters));
        GlobalState.metaThreadDynamicEnvironment.set(varName,parameters.get(1));
      } else {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-set!' <optional-thread> <var-name> <value>) 2nd arg %s isn't a symbol: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        ((escm.type.Thread)parameters.get(0)).set(((escm.type.Symbol)parameters.get(1)).value(),parameters.get(2));
      }
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-get'
  public static class ThreadGetApostrophe implements Primitive {
    public java.lang.String escmName() {
      return "thread-get'";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 1 && totalParams != 2)
        throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) didn't receive 1 or 2 args (optional thread, variable symbol): %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 1) {
        if(!(parameters.get(0) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 1st arg %s isn't a symbolic variable name: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        String varName = ((escm.type.Symbol)parameters.get(0)).value();
        if(!GlobalState.metaThreadDynamicEnvironment.has(varName))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) %s isn't a variable in the meta thread dynamic environment: %s", 
            varName, Exceptionf.profileArgs(parameters));
        return GlobalState.metaThreadDynamicEnvironment.get(varName);
      } else {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.Symbol))
          throw new Exceptionf("'(thread-get' <optional-thread> <var-name>) 2nd arg %s isn't a symbolic variable name: %s", 
            parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
        return ((escm.type.Thread)parameters.get(0)).get(((escm.type.Symbol)parameters.get(1)).value());
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // thread-dynamic-environment
  public static class ThreadDynamicEnvironment implements Primitive {
    public java.lang.String escmName() {
      return "thread-dynamic-environment";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams != 0 && totalParams != 1)
        throw new Exceptionf("'(thread-dynamic-environment <optional-thread>) didn't receive 0 or 1 args: %s", 
          Exceptionf.profileArgs(parameters));
      if(totalParams == 0) {
        return GlobalState.metaThreadDynamicEnvironment.bindingsAsAssocList();
      } else {
        if(!(parameters.get(0) instanceof escm.type.Thread))
          throw new Exceptionf("'(thread-dynamic-environment <optional-thread>) 1st arg %s isn't a thread: %s", 
            parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
        return ((escm.type.Thread)parameters.get(0)).dynamicEnvironment();
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex
  public static class Mutex implements Primitive {
    public java.lang.String escmName() {
      return "mutex";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParams = parameters.size();
      if(totalParams == 0) {
        return new escm.type.Mutex();
      } else if(totalParams == 1) {
        Datum param = parameters.get(0);
        if(!(param instanceof escm.type.String))
          throw new Exceptionf("'(mutex <optional-name-string>) 1st arg %s isn't a name string: %s", param.profile(), Exceptionf.profileArgs(parameters));
        return new escm.type.Mutex(((escm.type.String)param).value());
      } else {
        throw new Exceptionf("'(mutex <optional-name-string>) didn't receive either 0 or 1 args: %s", Exceptionf.profileArgs(parameters));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex?
  public static class IsMutex implements Primitive {
    public java.lang.String escmName() {
      return "mutex?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(mutex? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Mutex);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-name
  public static class MutexName implements Primitive {
    public java.lang.String escmName() {
      return "mutex-name";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-name <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      String name = ((escm.type.Mutex)parameters.get(0)).getName();
      if(name == null) return Boolean.FALSE;
      return new escm.type.String(name);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-specific
  public static class MutexSpecific implements Primitive {
    public java.lang.String escmName() {
      return "mutex-specific";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-specific <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Mutex)parameters.get(0)).getSpecific();
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // mutex-set-specific!
  public static class MutexSetSpecificBang implements Primitive {
    public java.lang.String escmName() {
      return "mutex-set-specific!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-set-specific! <mutex> <value>) didn't receive exactly 1 mutex & 1 obj: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Mutex)parameters.get(0)).setSpecific(parameters.get(1));
      return Void.VALUE;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // mutex-lock!
  public static class MutexLockBang implements Primitive {
    public java.lang.String escmName() {
      return "mutex-lock!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2)
        throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) didn't receive exactly 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lock = parameters.get(0);
      if(!(lock instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) 1st arg %s isn't a mutex: %s", lock.profile(), Exceptionf.profileArgs(parameters));
      if(parameters.size() == 2) {
        Datum timeout = parameters.get(1);
        if(!(timeout instanceof escm.type.Number))
          throw new Exceptionf("'(mutex-lock! <mutex> <optional-millis-timeout>) 2nd arg %s isn't a number: %s", timeout.profile(), Exceptionf.profileArgs(parameters));
        return Boolean.valueOf(((escm.type.Mutex)lock).lock(((escm.type.Number)timeout).intValue()));
      } else {
        ((escm.type.Mutex)lock).lock();
        return Boolean.TRUE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-unlock!
  public static class MutexUnlockBang implements Primitive {
    public java.lang.String escmName() {
      return "mutex-unlock!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-unlock! <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Mutex)parameters.get(0)).unlock());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-locked?
  public static class MutexIsLocked implements Primitive {
    public java.lang.String escmName() {
      return "mutex-locked?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-locked? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Mutex)parameters.get(0)).isLocked());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-queue-length
  public static class MutexQueueLength implements Primitive {
    public java.lang.String escmName() {
      return "mutex-queue-length";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-queue-length <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Number(((escm.type.Mutex)parameters.get(0)).queueLength());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-queued?
  public static class MutexIsQueued implements Primitive {
    public java.lang.String escmName() {
      return "mutex-queued?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-queued? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Mutex)parameters.get(0)).isQueued());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-hold-count
  public static class MutexHoldCount implements Primitive {
    public java.lang.String escmName() {
      return "mutex-hold-count";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-hold-count <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Number(((escm.type.Mutex)parameters.get(0)).holdCount());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mutex-held?
  public static class MutexIsHeld implements Primitive {
    public java.lang.String escmName() {
      return "mutex-held?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Mutex))
        throw new Exceptionf("'(mutex-held? <mutex>) didn't receive exactly 1 mutex: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Mutex)parameters.get(0)).isHeld());
    }
  }
}