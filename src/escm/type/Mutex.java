// Author: Jordan Randleman - escm.type.Mutex
// Purpose:
//    Mutex primitive type (wraps java.util.concurrent.locks.ReentrantLock).

package escm.type;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.TimeUnit;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Mutex extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Lock, Specific, & Name Field
  private static class State {
    private ReentrantLock lock = new ReentrantLock();
    private Datum specific = Void.VALUE;
  };
  private State state = new State();
  private java.lang.String name = null;


  ////////////////////////////////////////////////////////////////////////////
  // Internal Lock Field Public Accessors
  public java.lang.String getName() {
    return name;
  }

  public synchronized Datum getSpecific() {
    return state.specific;
  }

  public synchronized void setSpecific(Datum newValue) {
    state.specific = newValue;
  }

  public void lock() {
    state.lock.lock();
  }

  public boolean lock(long millis) {
    try {
      return state.lock.tryLock(millis,TimeUnit.MILLISECONDS);
    } catch(Exception e) {
      return false;
    }
  }

  public boolean unlock() {
    try {
      state.lock.unlock();
      return true;
    } catch(Exception e) {
      return false;
    }
  }

  public boolean isLocked() {
    return state.lock.isLocked();
  }

  public int queueLength() {
    return state.lock.getQueueLength();
  }

  public boolean isQueued() {
    return state.lock.hasQueuedThreads();
  }

  public int holdCount() {
    return state.lock.getHoldCount();
  }

  public boolean isHeld() {
    return state.lock.isHeldByCurrentThread();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor(s)
  public Mutex(java.lang.String name, Datum specific) {
    this.name = name;
    this.state.specific = specific;
  }


  public Mutex(java.lang.String name) {
    this.name = name;
  }


  public Mutex(Datum specific) {
    this.state.specific = specific;
  }


  public Mutex() {}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "mutex";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Mutex && ((Mutex)o).state.lock == this.state.lock;
  }

  public boolean equals(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),state.lock);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(name == null) return "#<mutex-" + Boolean.valueOf(isLocked()).display() + ">";
    return "#<mutex-" + Boolean.valueOf(isLocked()).display() + "-" + name + ">";
  }

  public java.lang.String write() {
    return display();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Mutex loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  private Mutex(java.lang.String name, State state) {
    this.name = name;
    this.state = state;
  }

  public Mutex loadWithName(java.lang.String name) throws Exception {
    if(this.name != null) return this;
    return new Mutex(name,this.state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}