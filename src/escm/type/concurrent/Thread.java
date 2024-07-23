// Author: Jordan Randleman - escm.type.concurrent.Thread
// Purpose:
//    Thread primitive type (wraps escm.vm.runtime.EscmThread).

package escm.type.concurrent;
import java.util.ArrayList;
import java.util.Objects;
import escm.util.Trampoline;
import escm.util.error.Exceptionf;
import escm.util.error.TopLevelError;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.procedure.PrimitiveProcedure;
import escm.vm.type.callable.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.runtime.EscmThread;
import escm.vm.runtime.GlobalState;

public class Thread extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Static min/max thread priority values
  public static final int MIN_PRIORITY = java.lang.Thread.MIN_PRIORITY;

  public static final int MAX_PRIORITY = java.lang.Thread.MAX_PRIORITY;


  ////////////////////////////////////////////////////////////////////////////
  // Static default runnable callable thunk name
  public static final String DEFAULT_RUNNABLE_NAME = "runnable";


  ////////////////////////////////////////////////////////////////////////////
  // Static Current Thread Object Getter
  public static Thread currentThread() {
    return ((EscmThread)java.lang.Thread.currentThread()).currentThread;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static Interruption Predicate + Clearing
  public static boolean interrupted() {
    return java.lang.Thread.interrupted();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static Sleep (returns whether was interrupted)
  public static boolean sleep(int millis) {
    try {
      java.lang.Thread.sleep(millis);
      return false;
    } catch(Exception e) {
      return true;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static Yield (hint to the scheduler)
  public static void yield() {
    java.lang.Thread.yield();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Thread Field
  private EscmThread thread;
  private Datum runnable;


  ////////////////////////////////////////////////////////////////////////////
  // Internal Thread Field Public Accessors
  public String getName() {
    return thread.getName();
  }

  public long getId() {
    return thread.getId();
  }

  public Datum getRunnable() {
    return runnable;
  }

  public String getStatus() {
    switch(thread.getState()) {
      case NEW: return "ready";
      case RUNNABLE: return "running";
      case BLOCKED: return "blocked";
      case WAITING: return "waiting";
      case TIMED_WAITING: return "timed-waiting";
      case TERMINATED: return "finished";
      default: return "unknown";
    }
  }

  public boolean isDaemon() {
    return thread.isDaemon();
  }

  public boolean setDaemon(boolean on) {
    try {
      thread.setDaemon(on);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  public boolean start() {
    try {
      thread.start();
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  public int getPriority() {
    return thread.getPriority();
  }

  public boolean setPriority(int priority) {
    try {
      if(priority < MIN_PRIORITY || priority > MAX_PRIORITY) return false;
      thread.setPriority(priority);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

   // (returns whether was interrupted)
  public boolean join() {
    try {
      thread.join();
      return false;
    } catch(Exception e) {
      return true;
    }
  }

   // (returns whether was interrupted)
  public boolean join(int millisToWaitAtMostFor) {
    try {
      thread.join(millisToWaitAtMostFor);
      return false;
    } catch(Exception e) {
      return true;
    }
  }

  public boolean interrupt() {
    try {
      thread.interrupt();
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  public boolean isInterrupted() {
    return thread.isInterrupted();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor Helper Procedures
  private void initializeValues(String name, Datum thunk) {
    Trampoline.Continuation terminalContinuation = (value) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;
    this.runnable = thunk;
    this.thread = new EscmThread(this,name) {
      public void run() {
        try {
          Trampoline.resolve(((Callable)thunk).callWith(new ArrayList<Datum>(),terminalContinuation));
        } catch(Throwable t) {
          TopLevelError.report(t);
        }
      }
    };
  }


  private void initializeValues(Datum thunk) {
    Trampoline.Continuation terminalContinuation = (value) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;
    this.runnable = thunk;
    this.thread = new EscmThread(this) {
      public void run() {
        try {
          Trampoline.resolve(((Callable)thunk).callWith(new ArrayList<Datum>(),terminalContinuation));
        } catch(Throwable t) {
          TopLevelError.report(t);
        }
      }
    };
  }


  private Datum generateDatumFromCallable(Callable c) {
    return new PrimitiveProcedure(DEFAULT_RUNNABLE_NAME,c);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor(s)
  public Thread(String name, Callable thunk) {
    initializeValues(name,generateDatumFromCallable(thunk));
  }


  public Thread(String name, Datum thunk) throws Exceptionf {
    if(!(thunk instanceof Callable)) 
      throw new Exceptionf("Thread %s can't be created with non-callable: %s", name, thunk.profile());
    initializeValues(name,thunk);
  }


  public Thread(Callable thunk) {
    initializeValues(generateDatumFromCallable(thunk));
  }


  public Thread(Datum thunk) throws Exceptionf {
    if(!(thunk instanceof Callable)) 
      throw new Exceptionf("Thread can't be created with non-callable: %s", thunk.profile());
    initializeValues(thunk);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Defining a Variable in the Dynamic Environment
  public void define(Symbol name, Datum value) throws Exception {
    thread.dynamicEnvironment.define(name,value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Setting a Variable in the Dynamic Environment
  public void set(Symbol name, Datum value) throws Exception {
    if(!thread.dynamicEnvironment.has(name)) {
      if(!GlobalState.metaThreadDynamicEnvironment.has(name)) 
        throw new Exceptionf("Variable %s doesn't exist in escm.type.concurrent.Thread \"%s\"'s dynamic environment!", getName());
      thread.dynamicEnvironment.define(name,value);
    } else {
      thread.dynamicEnvironment.set(name,value);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getting a Variable from the Dynamic Environment
  public Datum get(Symbol name) throws Exception {
    if(!thread.dynamicEnvironment.has(name)) {
      if(!GlobalState.metaThreadDynamicEnvironment.has(name)) 
        throw new Exceptionf("Variable %s doesn't exist in escm.type.concurrent.Thread \"%s\"'s dynamic environment!", getName());
      Datum cachedCopy = GlobalState.metaThreadDynamicEnvironment.get(name).shallowCopy();
      thread.dynamicEnvironment.define(name,cachedCopy);
      return cachedCopy;
    } else {
      return thread.dynamicEnvironment.get(name);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Checking for a Variable in the Dynamic Environment
  //   => NOTE: due to caching from the meta thread giving us access to any
  //      value therein, we simply need to check if the value exists there
  //      (and hence can ignore <thread.dynamicEnvironment>).
  public Datum has(Symbol name) {
    return Boolean.valueOf(GlobalState.metaThreadDynamicEnvironment.has(name));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getting the Dynamic Environment as an Associative List
  public Datum dynamicEnvironment() throws Exception {
    return thread.dynamicEnvironment.bindingsAsAssocList();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "thread";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Thread && ((Thread)o).thread == this.thread;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    return "Thread: id=" + thread.getId() + ", name=" + thread.getName();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),thread);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    return "#<thread (id=" + thread.getId() + ") " + thread.getName() + ">";
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization WRITE Semantics
  private Object writeReplace() throws Exception {
    return new SerializedThread(thread.getName(),runnable);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public Thread quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Thread loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Thread loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Thread shallowCopy() {
    return this;
  }
}