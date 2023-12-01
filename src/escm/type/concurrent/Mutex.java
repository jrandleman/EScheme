// Author: Jordan Randleman - escm.type.concurrent.Mutex
// Purpose:
//    Mutex primitive type (wraps java.util.concurrent.locks.ReentrantLock).

package escm.type.concurrent;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.TimeUnit;
import java.util.Objects;
import java.io.Serializable;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.Void;
import escm.vm.util.ExecutionState;

public class Mutex extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Lock, Specific, & Name Field
  private ReentrantLock lock = new ReentrantLock();
  private Datum specific = Void.VALUE;
  private String name = null;


  ////////////////////////////////////////////////////////////////////////////
  // Internal Lock Field Public Accessors
  public String getName() {
    return name;
  }

  public synchronized Datum getSpecific() {
    return specific;
  }

  public synchronized void setSpecific(Datum newValue) {
    specific = newValue;
  }

  public void lock() {
    lock.lock();
  }

  public boolean lock(long millis) {
    try {
      return lock.tryLock(millis,TimeUnit.MILLISECONDS);
    } catch(Exception e) {
      return false;
    }
  }

  public boolean unlock() {
    try {
      lock.unlock();
      return true;
    } catch(Exception e) {
      return false;
    }
  }

  public boolean isLocked() {
    return lock.isLocked();
  }

  public int queueLength() {
    return lock.getQueueLength();
  }

  public boolean isQueued() {
    return lock.hasQueuedThreads();
  }

  public int holdCount() {
    return lock.getHoldCount();
  }

  public boolean isHeld() {
    return lock.isHeldByCurrentThread();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor(s)
  public Mutex(String name, Datum specific) {
    this.name = name;
    this.specific = specific;
  }


  public Mutex(String name) {
    this.name = name;
  }


  public Mutex(Datum specific) {
    this.specific = specific;
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
    return o instanceof Mutex && ((Mutex)o).lock == this.lock;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    if(name == null) {
      if(isLocked()) {
        return "Mutex (locked)";
      } else {
        return "Mutex (unlocked)";
      }
    } else {
      if(isLocked()) {
        return "Mutex (locked) named \""+name+"\"";
      } else {
        return "Mutex (unlocked) named \""+name+"\"";
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),lock);
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
  // Quoting semantics for the VM's interpreter
  public Mutex quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Mutex loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  private Mutex(String name, ReentrantLock lock, Datum specific) {
    this.name = name;
    this.lock = lock;
    this.specific = specific;
  }

  public Mutex loadWithName(String name) {
    if(this.name != null) return this;
    return new Mutex(name,this.lock,this.specific);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Mutex shallowCopy() {
    return this;
  }
}