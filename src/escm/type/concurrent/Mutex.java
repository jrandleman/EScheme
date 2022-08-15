// Author: Jordan Randleman - escm.type.concurrent.Mutex
// Purpose:
//    Mutex primitive type (wraps java.util.concurrent.locks.ReentrantLock).

package escm.type.concurrent;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.TimeUnit;
import java.util.Objects;
import escm.type.Datum;
import escm.type.Boolean;
import escm.type.Void;
import escm.vm.util.ExecutionState;

public class Mutex extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Lock, Specific, & Name Field
  private static class State {
    private ReentrantLock lock = new ReentrantLock();
    private Datum specific = Void.VALUE;
  };
  private State state = new State();
  private String name = null;


  ////////////////////////////////////////////////////////////////////////////
  // Internal Lock Field Public Accessors
  public String getName() {
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
  public Mutex(String name, Datum specific) {
    this.name = name;
    this.state.specific = specific;
  }


  public Mutex(String name) {
    this.name = name;
  }


  public Mutex(Datum specific) {
    this.state.specific = specific;
  }


  public Mutex() {}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
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

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),state.lock);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    if(name == null) {
      if(isLocked()) {
        return "#<mutex (locked)>";
      } else {
        return "#<mutex (unlocked)>";
      }
    } else {
      if(isLocked()) {
        return "#<mutex (locked) " + name + ">";
      } else {
        return "#<mutex (unlocked) " + name + ">";
      }
    }
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Mutex loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  private Mutex(String name, State state) {
    this.name = name;
    this.state = state;
  }

  public Mutex loadWithName(String name) {
    if(this.name != null) return this;
    return new Mutex(name,this.state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Mutex copy() {
    return this;
  }
}