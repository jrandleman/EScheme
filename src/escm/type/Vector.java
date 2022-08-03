// Author: Jordan Randleman - escm.type.Vector
// Purpose:
//    Mutable vector primitive type.
//
//    Provides:
//      - static Vector append(ArrayList<Vector> vects)
//
//      - Vector()
//      - Vector(ArrayList<Datum> arr)
//      - Vector(int size, Datum fillValue)
//
//      - int size()
//
//      - Datum get(int idx)
//      - Datum memq(Datum d)
//      - Datum memv(Datum d)
//      - Datum member(Datum d)
//
//      - void set(int idx, Datum d)
//      - void fill(Datum d)
//      - void grow(int growTotal, Datum d)
//      - void insert(int idx, Datum d)
//      - Datum delete(int idx)
//
//      - void push(Datum d)
//      - void pushFront(Datum d)
//      - Datum pop()
//      - Datum popFront()
//
//      - Vector reverse()
//      - Vector subvector(int idx, int length)
//
//      - callWith(...) // given an index, returns the entry at that position

package escm.type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Objects;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.vm.type.ExecutionState;
import escm.vm.type.Callable;

public class Vector extends Datum implements Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Factory Generator
  static public Vector append(ArrayList<Vector> vects) {
    java.util.Vector<Datum> appended = new java.util.Vector<Datum>();
    for(Vector v : vects) appended.addAll(v.value);
    return new Vector(appended);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Value
  private java.util.Vector<Datum> value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Vector() {
    value = new java.util.Vector<Datum>();
  }

  public Vector(ArrayList<Datum> arr) {
    value = new java.util.Vector<Datum>(arr);
  }

  public Vector(int size, Datum fillValue) throws Exception {
    if(size < 0) throw new Exceptionf("VECTOR [CTOR]: Invalid negative <fill> size %d", size);
    value = new java.util.Vector<Datum>();
    for(int i = 0; i < size; ++i) value.add(fillValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Size
  public int size() {
    return value.size();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getter & Has?
  public Datum get(int idx) throws Exception {
    synchronized(value) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [GET]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      return value.get(idx);
    }
  }

  public Datum memq(Datum d) {
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i)
        if(value.get(i).eq(d)) return new Exact(i);
      return Boolean.FALSE;
    }
  }

  public Datum memv(Datum d) {
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i)
        if(value.get(i).eqv(d)) return new Exact(i);
      return Boolean.FALSE;
    }
  }

  public Datum member(Datum d) {
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i)
        if(value.get(i).equals(d)) return new Exact(i);
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Mutators
  public void set(int idx, Datum d) throws Exception {
    synchronized(value) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [SET!]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      value.set(idx,d);
    }
  }

  public void fill(Datum d) {
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i)
        value.set(i,d);
    }
  }

  public void grow(int growTotal, Datum d) throws Exception {
    if(growTotal < 0)
      throw new Exceptionf("VECTOR [GROW]: Invalid negative grow-length %d for vector %s", growTotal, write());
    synchronized(value) {
      for(int i = 0; i < growTotal; ++i) value.add(d);
    }
  }

  public void insert(int idx, Datum d) throws Exception {
    synchronized(value) {
      if(idx < 0 || idx > value.size())
        throw new Exceptionf("VECTOR [INSERT]: Invalid index %d for vector %s", idx, write());
      value.insertElementAt(d,idx);
    }
  }

  public Datum delete(int idx) throws Exception {
    synchronized(value) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [DELETE]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      return value.remove(idx);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Push & Pop Ops
  public void push(Datum d) {
    value.add(d);
  }

  public void pushFront(Datum d) {
    value.insertElementAt(d,0);
  }

  public Datum pop() throws Exception {
    synchronized(value) {
      int n = value.size();
      if(n == 0) throw new Exceptionf("VECTOR [POP]: Can't pop an empty vector!");
      return value.remove(n-1);
    }
  }

  public Datum popFront() throws Exception {
    synchronized(value) {
      int n = value.size();
      if(n == 0) throw new Exceptionf("VECTOR [POP-FRONT]: Can't pop an empty vector!");
      return value.remove(0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Synthesizers
  private Vector(java.util.Vector<Datum> arr) {
    value = arr;
  }

  public Vector reverse() {
    java.util.Vector<Datum> copy = new java.util.Vector<Datum>(value);
    Collections.reverse(copy);
    return new Vector(copy);
  }

  public Vector subvector(int idx, int length) throws Exception {
    if(idx < 0)
      throw new Exceptionf("VECTOR [SUBVECTOR]: Invalid negative index %d for vector %s", idx, write());
    java.util.Vector<Datum> subvect = new java.util.Vector<Datum>();
    synchronized(value) {
      for(int n = value.size(), count = 0; idx < n && count < length; ++idx, ++count)
        subvect.add(value.get(idx));
    }
    return new Vector(subvect);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Callable (unary index getter)
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    if(arguments.size() != 1)
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects exactly 1 integer index arg: %s", Exceptionf.profileArgs(arguments));
    Datum idxDatum = arguments.get(0);
    if(!(idxDatum instanceof Real) || !((Real)idxDatum).isInteger())
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects exactly 1 integer index arg: %s", Exceptionf.profileArgs(arguments));
    int index = ((Real)idxDatum).intValue();
    synchronized(value) {
      if(index < 0 || index >= value.size())
        throw new Exceptionf("VECTOR [CALLABLE-GET]: Invalid index %d (size %d) for vector %s", index, value.size(), write());
      return continuation.run(value.get(index));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "vector";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(o instanceof Vector) {
      Vector v = (Vector)o;
      synchronized(value) {
        synchronized(v.value) {
          if(v.value.size() != value.size()) return false;
          for(int i = 0, n = value.size(); i < n; ++i)
            if(!value.get(i).eq(v.value.get(i))) return false;
        }
      }
      return true;
    }
    return false;
  }

  public boolean eqv(Object o) {
    if(o instanceof Vector) {
      Vector v = (Vector)o;
      synchronized(value) {
        synchronized(v.value) {
          if(v.value.size() != value.size()) return false;
          for(int i = 0, n = value.size(); i < n; ++i)
            if(!value.get(i).eqv(v.value.get(i))) return false;
        }
      }
      return true;
    }
    return false;
  }

  public boolean equals(Object o) {
    if(o instanceof Vector) {
      Vector v = (Vector)o;
      synchronized(value) {
        synchronized(v.value) {
          if(v.value.size() != value.size()) return false;
          for(int i = 0, n = value.size(); i < n; ++i)
            if(!value.get(i).equals(v.value.get(i))) return false;
        }
      }
      return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    StringBuilder sb = new StringBuilder("[");
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i) {
        sb.append(value.get(i).display());
        if(i+1 < n) sb.append(' ');
      }
    }
    sb.append(']');
    return sb.toString();
  }

  public java.lang.String write() {
    StringBuilder sb = new StringBuilder("[");
    synchronized(value) {
      for(int i = 0, n = value.size(); i < n; ++i) {
        sb.append(value.get(i).write());
        if(i+1 < n) sb.append(' ');
      }
    }
    sb.append(']');
    return sb.toString();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Vector loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Vector loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    java.util.Vector<Datum> cpy = new java.util.Vector<Datum>();
    synchronized(value) {
      for(Datum d : value) cpy.add(d);
    }
    return new Vector(cpy);
  }
}