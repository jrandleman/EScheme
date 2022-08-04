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
//      - void pushAll(ArrayList<Vector> vects)
//      - Datum pop()
//      - Datum popFront()
//
//      - Vector reverse()
//      - Vector subvector(int idx, int length)
//
//      - Datum toList()
//      - Hashmap toHashmap()
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
    ArrayList<Datum> appended = new ArrayList<Datum>();
    for(Vector v : vects) {
      synchronized(v) {
        appended.addAll(v.value);
      }
    }
    return new Vector(0,appended);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Internal Value
  private ArrayList<Datum> value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Vector() {
    value = new ArrayList<Datum>();
  }

  public Vector(ArrayList<Datum> arr) {
    value = new ArrayList<Datum>(arr);
  }

  public Vector(int size, Datum fillValue) throws Exception {
    if(size < 0) throw new Exceptionf("VECTOR [CTOR]: Invalid negative <fill> size %d", size);
    value = new ArrayList<Datum>();
    for(int i = 0; i < size; ++i) value.add(fillValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Size
  public int size() {
    synchronized(this) {
      return value.size();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getter & Has?
  public Datum get(int idx) throws Exception {
    synchronized(this) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [GET]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      return value.get(idx);
    }
  }

  public Datum memq(Datum d) {
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i)
        if(value.get(i).eq(d)) return new Exact(i);
      return Boolean.FALSE;
    }
  }

  public Datum member(Datum d) {
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i)
        if(value.get(i).equal(d)) return new Exact(i);
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Mutators
  public void set(int idx, Datum d) throws Exception {
    synchronized(this) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [SET!]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      value.set(idx,d);
    }
  }

  public void fill(Datum d) {
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i)
        value.set(i,d);
    }
  }

  public void grow(int growTotal, Datum d) throws Exception {
    if(growTotal < 0)
      throw new Exceptionf("VECTOR [GROW]: Invalid negative grow-length %d for vector %s", growTotal, write());
    synchronized(this) {
      for(int i = 0; i < growTotal; ++i) value.add(d);
    }
  }

  public void insert(int idx, Datum d) throws Exception {
    synchronized(this) {
      if(idx < 0 || idx > value.size())
        throw new Exceptionf("VECTOR [INSERT]: Invalid index %d for vector %s", idx, write());
      value.add(idx,d);
    }
  }

  public Datum delete(int idx) throws Exception {
    synchronized(this) {
      if(idx < 0 || idx >= value.size())
        throw new Exceptionf("VECTOR [DELETE]: Invalid index %d (size %d) for vector %s", idx, value.size(), write());
      return value.remove(idx);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Push & Pop Ops
  public void push(Datum d) {
    synchronized(this) {
      value.add(d);
    }
  }

  public void pushFront(Datum d) {
    synchronized(this) {
      value.add(0,d);
    }
  }

  public void pushAll(ArrayList<Vector> vects) {
    synchronized(this) {
      for(Vector v : vects) value.addAll(v.value);
    }
  }

  public Datum pop() throws Exception {
    synchronized(this) {
      int n = value.size();
      if(n == 0) throw new Exceptionf("VECTOR [POP]: Can't pop an empty vector!");
      return value.remove(n-1);
    }
  }

  public Datum popFront() throws Exception {
    synchronized(this) {
      int n = value.size();
      if(n == 0) throw new Exceptionf("VECTOR [POP-FRONT]: Can't pop an empty vector!");
      return value.remove(0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Synthesizers
  Vector(int ignore, ArrayList<Datum> arr) {
    value = arr;
  }

  public Vector reverse() {
    synchronized(this) {
      ArrayList<Datum> copy = new ArrayList<Datum>(value);
      Collections.reverse(copy);
      return new Vector(0,copy);
    }
  }

  public Vector subvector(int idx, int length) throws Exception {
    if(idx < 0)
      throw new Exceptionf("VECTOR [SUBVECTOR]: Invalid negative index %d for vector %s", idx, write());
    ArrayList<Datum> subvect = new ArrayList<Datum>();
    synchronized(this) {
      for(int n = value.size(), count = 0; idx < n && count < length; ++idx, ++count)
        subvect.add(value.get(idx));
    }
    return new Vector(0,subvect);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Coercions
  public Datum toList() {
    Datum lis = escm.type.Nil.VALUE;
    synchronized(this) {
      for(int i = value.size()-1; i >= 0; --i)
        lis = new escm.type.Pair(value.get(i),lis);
    }
    return lis; 
  }

  public Hashmap toHashmap() throws Exception {
    synchronized(this) {
      int n = value.size();
      if(n % 2 != 0)
        throw new Exceptionf("VECTOR [TO-HASHMAP]: Vector %s (size %d) doesn't have an even number of items", write(), n);
      Hashmap h = new Hashmap();
      for(int i = 0; i < n; i += 2) {
        h.set(value.get(i),value.get(i+1));
      }
      return h;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Callable (unary index getter)
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    if(arguments.size() != 1)
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects exactly 1 integer index arg for vector %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum idxDatum = arguments.get(0);
    if(!(idxDatum instanceof Real) || !((Real)idxDatum).isInteger())
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects exactly 1 integer index arg for vector %s: %s", write(), Exceptionf.profileArgs(arguments));
    int index = ((Real)idxDatum).intValue();
    synchronized(this) {
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
    return o instanceof Vector && ((Vector)o).value == value;
  }

  public boolean equal(Object o) {
    if(o instanceof Vector) {
      Vector v = (Vector)o;
      synchronized(this) {
        synchronized(v) {
          if(v.value.size() != value.size()) return false;
          for(int i = 0, n = value.size(); i < n; ++i)
            if(!value.get(i).equal(v.value.get(i))) return false;
        }
      }
      return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    StringBuilder sb = new StringBuilder("[");
    synchronized(this) {
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
    synchronized(this) {
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
  public Vector copy() {
    synchronized(this) {
      return new Vector(0,new ArrayList<Datum>(value));
    }
  }
}