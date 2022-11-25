// Author: Jordan Randleman - escm.type.Vector
// Purpose:
//    Mutable vector primitive type.
//
//    Provides:
//      - static Vector append(ArrayList<Vector> vects)
//
//      - Vector()
//      - Vector(ArrayList<Datum> arr)
//      - Vector(Datum[] arr)
//      - Vector(int size, Datum fillValue)
//
//      - int size()
//
//      - Datum get(int idx)
//      - Trampoline.Bounce indexOf(Callable eqp, Datum d, Trampoline.Continuation c)
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
//      - String toEscmString()
//      - Hashmap toHashmap()
//
//      - callWith(...) // given an index, returns the entry at that position

package escm.type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Objects;
import java.util.Arrays;
import escm.type.bool.Boolean;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.vm.util.ExecutionState;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.Callable;

public class Vector extends Datum implements AssociativeCollection, Callable {
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

  public Vector(Datum[] arr) {
    value = new ArrayList<Datum>(Arrays.asList(arr));
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

  private Trampoline.Bounce indexOfRecur(Callable eqp, Datum d, int n, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= n) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    synchronized(this) {
      args.add(value.get(i));
    }
    args.add(d);
    return eqp.callWith(args,(isEqp) -> () -> {
      if(isEqp.isTruthy()) return continuation.run(new Exact(i));
      return indexOfRecur(eqp,d,n,i+1,continuation);
    });
  }

  public Trampoline.Bounce indexOf(Callable eqp, Datum d, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return indexOfRecur(eqp,d,value.size(),0,continuation);
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

  public escm.type.String toEscmString() throws Exception {
    StringBuilder sb = new StringBuilder();
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i) {
        Datum val = value.get(i);
        if(!(val instanceof escm.type.Character))
          throw new Exceptionf("VECTOR [TO-STRING]: Vector %s contains a non-character value %s", write(), val.profile());
        sb.append(val.display());
      }
    }
    return new escm.type.String(sb.toString());
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
  public Vector loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Vector loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Vector copy() {
    synchronized(this) {
      return new Vector(0,new ArrayList<Datum>(value));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    synchronized(this) {
      int n = value.size();
      if(n == 0) throw new Exceptionf("VECTOR [HEAD]: Can't get head of an empty vector!");
      return value.get(0);
    }
  }

  public Datum tail() throws Exception {
    synchronized(this) {
      return subvector(1,value.size()-1);
    }
  }

  public int length() {
    return size();
  }

  //////////////////////////////////////
  // fold
  //////////////////////////////////////

  private static Trampoline.Bounce FoldArrayIter(Callable c, Datum seed, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length+1);
    args.add(seed);
    for(int i = 0; i < acs.length; ++i) {
      synchronized(acs[i]) {
        if(acPos >= acs[i].length()) return continuation.run(seed);
        args.add(((Vector)acs[i]).get(acPos));
      }
    }
    return c.callWith(args,(acc) -> () -> FoldArrayIter(c,acc,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return FoldArrayIter(c,seed,acs,0,continuation);
  }

  ///////////////////////////////////////
  // map
  ///////////////////////////////////////

  private static Trampoline.Bounce MapIter(Callable c, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      synchronized(acs[i]) {
        if(acPos >= acs[i].length()) return continuation.run(Nil.VALUE);
        args.add(((Vector)acs[i]).get(acPos));
      }
    }
    return c.callWith(args,(mappedValue) -> () -> MapIter(c,acs,acPos+1,(mappedRest) -> () -> continuation.run(new Pair(mappedValue,mappedRest))));
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return MapIter(c,acs,0,(mappedList) -> () -> continuation.run(((AssociativeCollection)mappedList).toACVector()));
  }

  //////////////////////////////////////
  // for-each
  //////////////////////////////////////

  private static Trampoline.Bounce ForEachIter(Callable c, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      synchronized(acs[i]) {
        if(acPos >= acs[i].length()) return continuation.run(Void.VALUE);
        args.add(((Vector)acs[i]).get(acPos));
      }
    }
    return c.callWith(args,(ignore) -> () -> ForEachIter(c,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return ForEachIter(c,acs,0,continuation);
  }

  //////////////////////////////////////
  // filter
  //////////////////////////////////////

  private Trampoline.Bounce filterIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(acPos >= value.size()) return continuation.run(Nil.VALUE);
      Datum hd = value.get(acPos);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(hd);
      return predicate.callWith(args,(shouldKeep) -> () -> {
        if(shouldKeep.isTruthy()) {
          return filterIter(predicate,acPos+1,(filteredRest) -> () -> continuation.run(new Pair(hd,filteredRest)));
        }
        return filterIter(predicate,acPos+1,continuation);
      });
    }
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,0,(filteredList) -> () -> continuation.run(((AssociativeCollection)filteredList).toACVector()));
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private static final Exact ZERO = new Exact();
  private static final Exact ONE = new Exact(1);

  private Trampoline.Bounce countIter(Callable predicate, Number n, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(acPos >= value.size()) return continuation.run(n);
      Datum hd = value.get(acPos);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(hd);
      return predicate.callWith(args,(shouldCount) -> () -> {
        if(shouldCount.isTruthy()) {
          return countIter(predicate,n.add(ONE),acPos+1,continuation);
        }
        return countIter(predicate,n,acPos+1,continuation);
      });
    }
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return countIter(predicate,ZERO,0,continuation);
  }

  //////////////////////////////////////
  // remove (inverse of list)
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(acPos >= value.size()) return continuation.run(Nil.VALUE);
      Datum hd = value.get(acPos);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(hd);
      return predicate.callWith(args,(shouldRemove) -> () -> {
        if(shouldRemove.isTruthy()) return removeIter(predicate,acPos+1,continuation); 
        return removeIter(predicate,acPos+1,(removedRest) -> () -> continuation.run(new Pair(hd,removedRest)));
      });
    }
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return removeIter(predicate,0,(removedList) -> () -> continuation.run(((AssociativeCollection)removedList).toACVector()));
  }

  //////////////////////////////////////
  // val
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("'val invalid index %s for vector value", key.profile());
    Real r = (Real)key;
    synchronized(this) {
      if(r.lt(ZERO) || r.gte(new Exact(value.size())))
        throw new Exceptionf("'val index %s out of vector range [0,%d)", r.write(), value.size());
      return value.get(r.intValue());
    }
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private Trampoline.Bounce keyIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(acPos >= value.size()) {
        if(predicate instanceof Datum) {
          throw new Exceptionf("'key no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
        }
        throw new Exceptionf("'key no value in %s satisfies value predicate %s", write(), predicate);
      }
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(acPos));
      return predicate.callWith(args,(matchedKey) -> () -> {
        if(matchedKey.isTruthy()) return continuation.run(new Exact(acPos));
        return keyIter(predicate,acPos+1,continuation);
      });
    }
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return keyIter(predicate,0,continuation);
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    Vector v = ((Vector)acs[0]).copy();
    ArrayList<Vector> vs = new ArrayList<Vector>();
    for(int i = 1; i < acs.length; ++i) {
      vs.add((Vector)acs[i]);
    }
    v.pushAll(vs);
    return v;
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("'delete invalid index %s for vector deletion", key.profile());
    int deleteIdx = ((Real)key).intValue();
    synchronized(this) {
      if(deleteIdx < 0 || deleteIdx >= value.size())
        throw new Exceptionf("'delete index %d out of vector range [0,%d)", deleteIdx, value.size());
      Vector v = copy();
      v.delete(deleteIdx);
      return v;
    }
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum newValue) throws Exception {
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("'conj invalid index %s for vector", key.profile());
    synchronized(this) {
      int n = value.size();
      int idx = ((Real)key).intValue();
      if(idx < -1 || idx > n)
        throw new Exceptionf("'conj index %d extends vector bounds [-1,%d)", idx, n);
      Vector v = copy();
      if(idx == -1) {
        v.pushFront(newValue);
      } else if(idx == n) {
        v.push(newValue);
      } else {
        v.set(idx,newValue);
      }
      return v;
    }
  }

  //////////////////////////////////////
  // drop
  //////////////////////////////////////

  public AssociativeCollection drop(int amount) throws Exception {
    synchronized(this) {
      int n = value.size();
      if(amount < 0 || amount > n)
        throw new Exceptionf("'drop invalid drop amount %d for vector %s", n, profile());
      return subvector(amount,n-amount);
    }
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    synchronized(this) {
      if(amount < 0 || amount > value.size())
        throw new Exceptionf("'take invalid take amount %d for vector %s", value.size(), profile());
      return subvector(0,amount);
    }
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  public Datum toACList() throws Exception {
    return toList();
  }

  public String toACString() throws Exception {
    StringBuilder sb = new StringBuilder();
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i) {
        Datum val = value.get(i);
        if(!(val instanceof Character))
          throw new Exceptionf("'ac->string can't convert vector with non-char %s into a string", val.profile());
        sb.append(val.display());
      }
      return new String(sb.toString());
    }
  }

  public Vector toACVector() throws Exception {
    return copy();
  }

  public Hashmap toACHashmap() throws Exception {
    Hashmap h = new Hashmap();
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i) {
        h.set(new Exact(i),value.get(i));
      }
      return h;
    }
  }

  //////////////////////////////////////
  // union set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValues(Callable eltPredicate, Datum elt, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(!(values instanceof Pair)) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(elt);
    args.add(((Pair)values).car());
    return eltPredicate.callWith(args,(match) -> () -> {
      if(match.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValues(eltPredicate,elt,((Pair)values).cdr(),continuation);
    });
  }

  private Trampoline.Bounce UnionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int i, Vector v, int j, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(values);
    synchronized(v) {
      if(j >= v.size()) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? (Vector)acs[i] : (Vector)acs[i+1],0,values,continuation);
      Datum elt = v.get(j);
      return inValues(eltPredicate, elt, values, (isInValues) -> () -> {
        if(isInValues.isTruthy()) {
          return UnionArrayIter(eltPredicate,acs,i,v,j+1,values,continuation);
        }
        return UnionArrayIter(eltPredicate,acs,i,v,j+1,new Pair(elt,values),continuation);
      });
    }
  }

  public Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return UnionArrayIter(eltPredicate,acs,0,(Vector)acs[0],0,Nil.VALUE,(valuesList) -> () -> {
      Datum vl = valuesList; // lambda arg must be <final>
      Vector v = new Vector();
      while(vl instanceof Pair) {
        Pair p = (Pair)vl;
        v.push(p.car());
        vl = p.cdr();
      }
      return continuation.run(v);
    });
  }

  //////////////////////////////////////
  // intersection set operation
  //////////////////////////////////////

  private Trampoline.Bounce itemIntersectsAC(Callable eltPredicate, Vector ac, int acIdx, Datum item, Trampoline.Continuation continuation) throws Exception {
    synchronized(ac) {
      if(acIdx >= ac.size()) return continuation.run(Boolean.FALSE);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(item);
      args.add(ac.get(acIdx));
      return eltPredicate.callWith(args,(isSameItem) -> () -> {
        if(isSameItem.isTruthy()) return continuation.run(Boolean.TRUE);
        return itemIntersectsAC(eltPredicate,ac,acIdx+1,item,continuation);
      });
    }
  }

  private Trampoline.Bounce itemIntersects(Callable eltPredicate, AssociativeCollection[] acs, int i, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(Boolean.TRUE);
    return itemIntersectsAC(eltPredicate,(Vector)acs[i],0,item,(itemInAC) -> () -> {
      if(itemInAC.isTruthy()) return () -> itemIntersects(eltPredicate,acs,i+1,item,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  private Trampoline.Bounce IntersectionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int acIdx, Vector v, int vIdx, Datum intersectingValues, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= acs.length) return continuation.run(intersectingValues);
    synchronized(v) {
      if(vIdx >= v.size()) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? (Vector)acs[acIdx] : (Vector)acs[acIdx+1],0,intersectingValues,continuation);
      Datum elt = v.get(vIdx);
      return inValues(eltPredicate, elt, intersectingValues, (isInValues) -> () -> {
        if(!isInValues.isTruthy()) {
          return itemIntersects(eltPredicate,acs,0,elt,(intersects) -> () -> {
            if(intersects.isTruthy()) {
              return IntersectionArrayIter(eltPredicate,acs,acIdx,v,vIdx+1,new Pair(elt,intersectingValues),continuation);
            }
            return IntersectionArrayIter(eltPredicate,acs,acIdx,v,vIdx+1,intersectingValues,continuation);
          });
        }
        return IntersectionArrayIter(eltPredicate,acs,acIdx,v,vIdx+1,intersectingValues,continuation);
      });
    }
  }

  public Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return IntersectionArrayIter(eltPredicate,acs,0,(Vector)acs[0],0,Nil.VALUE,(intersectingValues) -> () -> {
      Datum ivs = intersectingValues; // lambda arg must be <final>
      Vector v = new Vector();
      while(ivs instanceof Pair) {
        Pair p = (Pair)ivs;
        v.push(p.car());
        ivs = p.cdr();
      }
      return continuation.run(v);
    });
  }

  //////////////////////////////////////
  // difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValuesVector(Callable eltPredicate, Datum item, Vector valVector, int i, Trampoline.Continuation continuation) throws Exception {
    synchronized(valVector) {
      if(i >= valVector.size()) return continuation.run(Boolean.FALSE);
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(item);
      args.add(valVector.get(i));
      return eltPredicate.callWith(args,(sameItem) -> () -> {
        if(sameItem.isTruthy()) return continuation.run(Boolean.TRUE);
        return inValuesVector(eltPredicate,item,valVector,i+1,continuation);
      });
    }
  }

  private Trampoline.Bounce binaryDifferenceArrayIter(Datum acc, Callable eltPredicate, Datum valList, Vector valVector, Trampoline.Continuation continuation) throws Exception {
    if(!(valList instanceof Pair)) return continuation.run(acc);
    Pair p = (Pair)valList;
    return inValuesVector(eltPredicate,p.car(),valVector,0,(inVector) -> () -> {
      if(inVector.isTruthy()) {
        return binaryDifferenceArrayIter(acc,eltPredicate,p.cdr(),valVector,continuation);
      }
      return binaryDifferenceArrayIter(new Pair(p.car(),acc),eltPredicate,p.cdr(),valVector,continuation);
    });
  }

  private Trampoline.Bounce DifferenceArrayIter(Callable eltPredicate, Datum lhs, AssociativeCollection[] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(lhs);
    return binaryDifferenceArrayIter(Nil.VALUE,eltPredicate,lhs,(Vector)acs[i],(differenceArray) -> () -> DifferenceArrayIter(eltPredicate,differenceArray,acs,i+1,continuation));
  }

  public Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArrayIter(eltPredicate,acs[0].toACList(),acs,1,(differenceList) -> () -> {
      Datum dl = differenceList; // lambda arg must be <final>
      Vector v = new Vector();
      while(dl instanceof Pair) {
        Pair p = (Pair)dl;
        v.push(p.car());
        dl = p.cdr();
      }
      return continuation.run(v);
    });
  }

  //////////////////////////////////////
  // symmetric difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce binarySymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection a, AssociativeCollection b, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArray(eltPredicate,new AssociativeCollection[]{a,b},(ab) -> () -> {
      return DifferenceArray(eltPredicate,new AssociativeCollection[]{b,a},(ba) -> () -> {
        return UnionArray(eltPredicate,new AssociativeCollection[]{(AssociativeCollection)ab,(AssociativeCollection)ba},continuation);
      });
    });
  }

  private Trampoline.Bounce SymmetricDifferenceArrayIter(Callable eltPredicate, AssociativeCollection lhs, AssociativeCollection[] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run((Datum)lhs);
    return binarySymmetricDifferenceArray(eltPredicate,lhs,acs[i],(symDiff) -> () -> {
      return SymmetricDifferenceArrayIter(eltPredicate,(AssociativeCollection)symDiff,acs,i+1,continuation);
    });
  }

  public Trampoline.Bounce SymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return SymmetricDifferenceArrayIter(eltPredicate,acs[0],acs,1,continuation);
  }
}