// Author: Jordan Randleman - escm.type.Vector
// Purpose:
//    Mutable vector primitive type.
//
//    => NOTE THAT AC/OC PRIMITIVES EXPECT ALL ARGS TO BE VECTORS!
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
//      - Vector reversed()
//      - Vector subvector(int idx, int length)
//
//      - Datum toList()
//      - String toEscmString()
//      - Hashmap toHashmap()
//
//      - Datum toReverseList() // used internally
//
//      - callWith(...) // given an index, returns the entry at that position. alternatively give a value too to set!

package escm.type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;
import escm.type.bool.Boolean;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.vm.util.Environment;
import escm.vm.util.ExecutionState;
import escm.type.procedure.types.TypeChecker.Predicate;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.callable.Callable;

public class Vector extends Datum implements OrderedCollection, Callable {
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

  private Trampoline.Bounce indexOfRecur(Callable eqp, Datum d, int i, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(i >= value.size()) return continuation.run(Boolean.FALSE);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(value.get(i));
      args.add(d);
      return eqp.callWith(args,(isEqp) -> () -> {
        if(isEqp.isTruthy()) return continuation.run(new Exact(i));
        return indexOfRecur(eqp,d,i+1,continuation);
      });
    }
  }

  public Trampoline.Bounce indexOf(Callable eqp, Datum d, Trampoline.Continuation continuation) throws Exception {
    return indexOfRecur(eqp,d,0,continuation);
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

  public Vector reversed() {
    synchronized(this) {
      ArrayList<Datum> copy = new ArrayList<Datum>(value);
      Collections.reverse(copy);
      return new Vector(0,copy);
    }
  }

  public Vector subvector(int idx, int length) throws Exception {
    if(length <= 0) return new Vector(0,new ArrayList<Datum>());
    if(idx < 0)
      throw new Exceptionf("VECTOR [SUBVECTOR]: can't get subvector of %s with negative index %d", write(), idx);
    synchronized(this) {
      ArrayList<Datum> subvect = new ArrayList<Datum>(length);
      for(int n = value.size(), count = 0; idx < n && count < length; ++idx, ++count) {
        subvect.add(value.get(idx));
      }
      return new Vector(0,subvect);
    }
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
  // Internal Coercions
  public Datum toReverseList() {
    Datum lis = escm.type.Nil.VALUE;
    synchronized(this) {
      for(int i = 0, n = value.size(); i < n; ++i)
        lis = new escm.type.Pair(value.get(i),lis);
    }
    return lis;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Callable (unary index getter)
  public java.lang.String docstring() {
    synchronized(this) {
      int n = value.size();
      if(n == 0) {
        return "Vector of length 0. Hashcode is "+hashCode();
      } else if(n == 1) {
        return "Vector of length 1. Apply to index 0 to get the item. Set index 0 by applying\nto 0 and the new value. Hashcode is "+hashCode()+".";
      } else {
        return "Vector of length "+n+". Apply to any index from 0 to "+(n-1)+" to get an item.\nSet those indices by applying to an index and the new value.\nHashcode is "+hashCode()+".";
      }
    }
  }

  public Datum signature() {
    return Pair.List(
      Pair.List(this,new Symbol("<index>")),
      Pair.List(this,new Symbol("<index>"),new Symbol("<obj>")));
  }

  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    int n = arguments.size();
    if(n == 0)
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects exactly 1 integer index arg for vector %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum idxDatum = arguments.get(0);
    if(!(idxDatum instanceof Real) || !((Real)idxDatum).isInteger())
      throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects an integer index arg for vector %s: %s", write(), Exceptionf.profileArgs(arguments));
    int index = ((Real)idxDatum).intValue();
    synchronized(this) {
      if(index < 0 || index >= value.size())
        throw new Exceptionf("VECTOR [CALLABLE-GET]: Invalid index %d (size %d) for vector %s", index, value.size(), write());
      if(n == 1) return continuation.run(value.get(index));
      if(n == 2) {
        value.set(index,arguments.get(1));
        return continuation.run(Void.VALUE);
      }
    }
    throw new Exceptionf("VECTOR [CALLABLE-GET]: Expects 1 or 2 integer index arg(s) for vector %s: %s", write(), Exceptionf.profileArgs(arguments));
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
  // Quoting semantics for the VM's interpreter
  // => WARNING: THIS WILL INFINITELY RECURSE ON CYCLIC VECTORS/HASHMAPS!
  public Vector quote(ExecutionState state) {
    synchronized(this) {
      int n = value.size();
      ArrayList<Datum> quoted = new ArrayList<Datum>(n);
      for(Datum d : value) {
        quoted.add(d.quote(state));
      }
      return new Vector(0,quoted);
    }
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
  public Vector shallowCopy() {
    synchronized(this) {
      return new Vector(0,new ArrayList<Datum>(value));
    }
  }


  //////////////////////////////////////////////////////////////////////
  // Type Checking
  public boolean containsType(Environment env, Predicate typePredicate) throws Exception {
    synchronized(this) {
      int n = value.size();
      for(int i = 0; i < n; ++i) {
        if(typePredicate.check(env,value.get(i)) == false) return false;
      }
      return true;
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

  public AssociativeCollection tail() throws Exception {
    synchronized(this) {
      return subvector(1,value.size()-1);
    }
  }

  public int length() {
    return size();
  }

  //////////////////////////////////////
  // fold (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldIter(Callable c, Datum seed, Vector v, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(v) {
      if(acPos >= v.length()) return continuation.run(seed);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(seed);
      args.add(v.get(acPos));
      return c.callWith(args,(acc) -> () -> foldIter(c,acc,v,acPos+1,continuation));
    }
  }

  public Trampoline.Bounce fold(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return foldIter(c,seed,this,0,continuation);
  }

  //////////////////////////////////////
  // fold (binary+)
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
  // map (unary)
  ///////////////////////////////////////

  private static Trampoline.Bounce mapIter(Callable c, Vector v, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(v) {
      if(acPos >= v.length()) return continuation.run(Nil.VALUE);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(v.get(acPos));
      return c.callWith(args,(mappedValue) -> () -> mapIter(c,v,acPos+1,(mappedRest) -> () -> continuation.run(new Pair(mappedValue,mappedRest))));
    }
  }

  public Trampoline.Bounce map(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return mapIter(c,this,0,(mappedList) -> () -> continuation.run(((AssociativeCollection)mappedList).toACVector()));
  }

  ///////////////////////////////////////
  // map (binary+)
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
  // for-each (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce forEachIter(Callable c, Vector v, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(v) {
      if(acPos >= v.length()) return continuation.run(Void.VALUE);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(v.get(acPos));
      return c.callWith(args,(ignore) -> () -> forEachIter(c,v,acPos+1,continuation));
    }
  }

  public Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return forEachIter(c,this,0,continuation);
  }

  //////////////////////////////////////
  // for-each (binary+)
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

  // Helper also used by <sort>
  private Trampoline.Bounce filterFrom(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,acPos,(filteredList) -> () -> continuation.run(((AssociativeCollection)filteredList).toACVector()));
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterFrom(predicate,0,continuation);
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private Trampoline.Bounce countIter(Callable predicate, int n, int acPos, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(acPos >= value.size()) return continuation.run(new Exact(n));
      Datum hd = value.get(acPos);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(hd);
      return predicate.callWith(args,(shouldCount) -> () -> {
        if(shouldCount.isTruthy()) {
          return countIter(predicate,n+1,acPos+1,continuation);
        }
        return countIter(predicate,n,acPos+1,continuation);
      });
    }
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return countIter(predicate,0,0,continuation);
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
      throw new Exceptionf("VECTOR [VAL]: invalid index %s for vector value", key.profile());
    Real r = (Real)key;
    synchronized(this) {
      if(r.isNegative() || r.gte(new Exact(value.size())))
        throw new Exceptionf("VECTOR [VAL]: index %s out of vector range [0,%d)", r.write(), value.size());
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
          throw new Exceptionf("VECTOR [KEY]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
        }
        throw new Exceptionf("VECTOR [KEY]: no value in %s satisfies value predicate %s", write(), predicate);
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

  public AssociativeCollection append(AssociativeCollection ac) throws Exception {
    ArrayList<Datum> appended = null;
    synchronized(this) {
      appended = new ArrayList<Datum>(value);
    }
    Vector v = (Vector)ac;
    synchronized(v) {
      appended.addAll(v.value);
    }
    return new Vector(0,appended);
  }

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    ArrayList<Datum> appended = new ArrayList<Datum>();
    for(int i = 0; i < acs.length; ++i) {
      Vector v = (Vector)acs[i];
      synchronized(v) {
        appended.addAll(v.value);
      }
    }
    return new Vector(0,appended);
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("VECTOR [DELETE]: invalid index %s for vector deletion", key.profile());
    int deleteIdx = ((Real)key).intValue();
    synchronized(this) {
      if(deleteIdx < 0 || deleteIdx >= value.size())
        throw new Exceptionf("VECTOR [DELETE]: index %d out of vector range [0,%d)", deleteIdx, value.size());
      Vector v = shallowCopy();
      v.delete(deleteIdx);
      return v;
    }
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum newValue) throws Exception {
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("VECTOR [CONJ]: invalid index %s for vector", key.profile());
    synchronized(this) {
      int n = value.size();
      int idx = ((Real)key).intValue();
      if(idx < -1 || idx > n)
        throw new Exceptionf("VECTOR [CONJ]: index %d violates vector bounds [-1,%d]", idx, n);
      Vector v = shallowCopy();
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
    amount = Math.max(0,amount);
    synchronized(this) {
      int n = value.size();
      if(amount > n) return new Vector();
      return subvector(amount,n-amount);
    }
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    amount = Math.max(0,amount);
    synchronized(this) {
      int n = value.size();
      if(amount > n) return subvector(0,n);
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
          throw new Exceptionf("VECTOR [AC->STRING]: can't convert vector with non-char %s into a string", val.profile());
        sb.append(val.display());
      }
      return new String(sb.toString());
    }
  }

  public Vector toACVector() throws Exception {
    return shallowCopy();
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
      ArrayList<Datum> vals = new ArrayList<Datum>();
      while(vl instanceof Pair) {
        Pair p = (Pair)vl;
        vals.add(p.car());
        vl = p.cdr();
      }
      return continuation.run(new Vector(0,vals));
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
      ArrayList<Datum> vals = new ArrayList<Datum>();
      while(ivs instanceof Pair) {
        Pair p = (Pair)ivs;
        vals.add(p.car());
        ivs = p.cdr();
      }
      return continuation.run(new Vector(0,vals));
    });
  }

  //////////////////////////////////////
  // difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValuesVector(Callable eltPredicate, Datum item, Vector valVector, int i, Trampoline.Continuation continuation) throws Exception {
    synchronized(valVector) {
      if(i >= valVector.size()) return continuation.run(Boolean.FALSE);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
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
      ArrayList<Datum> vals = new ArrayList<Datum>();
      while(dl instanceof Pair) {
        Pair p = (Pair)dl;
        vals.add(p.car());
        dl = p.cdr();
      }
      return continuation.run(new Vector(0,vals));
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


  ////////////////////////////////////////////////////////////////////////////
  // <OrderedCollection> Instance Methods

  //////////////////////////////////////
  // constructor
  //////////////////////////////////////

  public OrderedCollection conj(Datum value) throws Exception {
    ArrayList<Datum> conjd;
    synchronized(this) {
      conjd = new ArrayList<Datum>(this.value);
    }
    conjd.add(value);
    return new Vector(0,conjd);
  }

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////

  public OrderedCollection init() throws Exception {
    synchronized(this) {
      if(value.size() == 0) throw new Exceptionf("VECTOR [INIT]: can't get <init> of []!");
      ArrayList<Datum> initVals = new ArrayList<Datum>(value);
      initVals.remove(value.size()-1);
      return new Vector(0,initVals);
    }
  }

  public Datum last() throws Exception {
    synchronized(this) {
      if(value.size() == 0) throw new Exceptionf("VECTOR [LAST]: can't get <last> of []!");
      return value.get(value.size()-1);
    }
  }

  //////////////////////////////////////
  // slicing
  //////////////////////////////////////

  private Trampoline.Bounce sliceGetLastIdx(int i, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(i >= value.size()) return continuation.run(new Exact(i));
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(i));
      return continuePredicate.callWith(args,(shouldContinue) -> () -> {
        if(!shouldContinue.isTruthy()) return continuation.run(new Exact(i));
        return sliceGetLastIdx(i+1,continuePredicate,continuation);
      });
    }
  }

  public OrderedCollection slice(int startIdx) throws Exception {
    synchronized(this) {
      return slice(startIdx,value.size());
    }
  }

  public OrderedCollection slice(int startIdx, int length) throws Exception {
    synchronized(this) {
      if(startIdx < 0 || length <= 0 || startIdx >= value.size()) return new Vector();
      int sliceLength = Math.min(length,value.size()-startIdx);
      ArrayList<Datum> sliced = new ArrayList<Datum>(sliceLength);
      for(int count = 0; count < sliceLength; ++count, ++startIdx) {
        sliced.add(value.get(startIdx));
      }
      return new Vector(0,sliced);
    }
  }

  public Trampoline.Bounce slice(int startIdx, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      int idx = startIdx <= 0 ? 0 : startIdx;
      if(idx >= value.size()) return continuation.run(new Vector());
      return sliceGetLastIdx(idx,continuePredicate,(endIdx) -> () -> {
        synchronized(this) {
          return continuation.run(subvector(idx,((Real)endIdx).intValue()-idx));
        }
      });
    }
  }

  //////////////////////////////////////
  // reversing
  //////////////////////////////////////

  public OrderedCollection reverse() throws Exception {
    return reversed();
  }

  //////////////////////////////////////
  // removing items
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, int i, boolean increasing, int mod, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if((increasing && i >= value.size()) || (!increasing && i < 0)) return continuation.run(shallowCopy());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(i));
      return predicate.callWith(args,(shouldRemove) -> () -> {
        if(shouldRemove.isTruthy()) {
          synchronized(this) {
            ArrayList<Datum> removed = new ArrayList<Datum>(value);
            if(i < removed.size()) removed.remove(i);
            return continuation.run(new Vector(0,removed));
          }
        }
        return removeIter(predicate,i+mod,increasing,mod,continuation);
      });
    }
  }

  public Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return removeIter(predicate,0,true,1,continuation);
  }

  public Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return removeIter(predicate,value.size()-1,false,-1,continuation);
    }
  }

  //////////////////////////////////////
  // skipping
  //////////////////////////////////////

  private Trampoline.Bounce skipIter(Callable predicate, int i, boolean increasing, int mod, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if((increasing && i >= value.size()) || (!increasing && i < 0)) return continuation.run(Boolean.FALSE);
      Datum val = value.get(i);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(val);
      return predicate.callWith(args,(keepSkipping) -> () -> {
        if(keepSkipping.isTruthy()) return skipIter(predicate,i+mod,increasing,mod,continuation);
        return continuation.run(val);
      });
    }
  }

  public Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return skipIter(predicate,0,true,1,continuation);
  }

  public Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return skipIter(predicate,value.size()-1,false,-1,continuation);
    }
  }

  //////////////////////////////////////
  // fold-right (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldRightIter(Callable c, Datum seed, int eltIdx, Vector v, Trampoline.Continuation continuation) throws Exception { // -> Datum
    ArrayList<Datum> params = new ArrayList<Datum>(1);
    synchronized(v) {
      if(eltIdx >= v.value.size()) return continuation.run(seed);  
      params.add(v.value.get(eltIdx));
    }
    return () -> foldRightIter(c,seed,eltIdx+1,v,(acc) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }

  public Trampoline.Bounce foldRight(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception {
    return foldRightIter(c,seed,0,this,continuation);
  }

  //////////////////////////////////////
  // fold-right (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldRightArray(Callable c, Datum seed, int eltIdx, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> params = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      Vector v = (Vector)acs[i];
      synchronized(v) {
        if(eltIdx >= v.value.size()) return continuation.run(seed);  
        params.add(v.value.get(eltIdx));
      }
    }
    return () -> FoldRightArray(c,seed,eltIdx+1,acs,(acc) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }


  public Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return FoldRightArray(c,seed,0,acs,continuation);
  }

  //////////////////////////////////////
  // key-right
  //////////////////////////////////////

  private Trampoline.Bounce keyRight(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx < 0 || idx >= value.size()) {
        if(predicate instanceof Datum) {
          throw new Exceptionf("VECTOR [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), ((Datum)predicate).profile());
        }
        throw new Exceptionf("VECTOR [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), predicate);
      }
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(value.get(idx));
      return predicate.callWith(args,(shouldGetKey) -> () -> {
        if(!shouldGetKey.isTruthy()) return keyRight(predicate,idx-1,continuation);
        return continuation.run(new Exact(idx));
      });
    }
  }

  public Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return keyRight(predicate,value.size()-1,(key) -> () -> {
        if(key instanceof Real) return continuation.run(key);
        if(predicate instanceof Datum) {
          throw new Exceptionf("VECTOR [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
        }
        throw new Exceptionf("VECTOR [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), predicate);
      }); 
    }
  }

  //////////////////////////////////////
  // dropping
  //////////////////////////////////////

  public OrderedCollection dropRight(int length) throws Exception {
    synchronized(this) {
      return subvector(0,value.size()-length);
    }
  }

  private Trampoline.Bounce dropWhile(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx >= value.size()) return continuation.run(new Vector());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(idx));
      return predicate.callWith(args,(keepDropping) -> () -> {
        synchronized(this) {
          if(!keepDropping.isTruthy()) return continuation.run(subvector(idx,value.size()));
          return dropWhile(predicate,idx+1,continuation);
        }
      });
    }
  }

  public Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropWhile(predicate,0,continuation);
  }

  private Trampoline.Bounce dropRightWhile(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx < 0) return continuation.run(new Vector());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(idx));
      return predicate.callWith(args,(keepDropping) -> () -> {
        synchronized(this) {
          if(!keepDropping.isTruthy()) return continuation.run(subvector(0,idx+1));
          return dropRightWhile(predicate,idx-1,continuation);
        }
      });
    }
  }

  public Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return dropRightWhile(predicate,value.size()-1,continuation);
    }
  }

  //////////////////////////////////////
  // taking
  //////////////////////////////////////

  public OrderedCollection takeRight(int length) throws Exception {
    synchronized(this) {
      int n = value.size();
      if(length >= n) return shallowCopy();
      return subvector(n-length,n);
    }
  }

  private Trampoline.Bounce takeWhile(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx >= value.size()) return continuation.run(shallowCopy());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(idx));
      return predicate.callWith(args,(keepTaking) -> () -> {
        synchronized(this) {
          if(!keepTaking.isTruthy()) return continuation.run(subvector(0,idx));
          return takeWhile(predicate,idx+1,continuation);
        }
      });
    }
  }

  public Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return takeWhile(predicate,0,continuation);
  }

  private Trampoline.Bounce takeRightWhile(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx < 0) return continuation.run(shallowCopy());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(value.get(idx));
      return predicate.callWith(args,(keepTaking) -> () -> {
        synchronized(this) {
          if(!keepTaking.isTruthy()) return continuation.run(subvector(idx+1,value.size()));
          return takeRightWhile(predicate,idx-1,continuation);
        }
      });
    }
  }

  public Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      return takeRightWhile(predicate,value.size()-1,continuation);
    }
  }

  //////////////////////////////////////
  // sorting
  //////////////////////////////////////

  private Datum mergeSortedHalves(Vector lhs, Datum hd, Vector rhs) throws Exception {
    ArrayList<Datum> appended = new ArrayList<Datum>();
    synchronized(lhs) {
      appended.addAll(lhs.value);
    }
    appended.add(hd);
    synchronized(rhs) {
      appended.addAll(rhs.value);
    }
    return new Vector(0,appended);
  }

  private Trampoline.Bounce sort(Callable binaryPredicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx >= value.size()) return continuation.run(new Vector());
      Datum hd = value.get(idx);
      Callable trueCondPrimitive = new Callable() {
        public java.lang.String docstring() {
          return "Quicksort: move values to the left of the pivot.";
        }
        public Datum signature() { 
          return Pair.List(new Symbol("escm-sort-in-lhs?"),new Symbol("<obj>"),new Symbol("<obj>")); 
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          params.add(hd);
          return binaryPredicate.callWith(params,cont);
        }
      };
      Callable falseCondPrimitive = new Callable() {
        public java.lang.String docstring() {
          return "Quicksort: move values to the right of the pivot.";
        }
        public Datum signature() { 
          return Pair.List(new Symbol("escm-sort-in-rhs?"),new Symbol("<obj>"),new Symbol("<obj>")); 
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          params.add(hd);
          return binaryPredicate.callWith(params,(value) -> () -> cont.run(Boolean.valueOf(!value.isTruthy())));
        }
      };
      PrimitiveProcedure trueCond = new PrimitiveProcedure("escm-sort-in-lhs?", trueCondPrimitive);
      PrimitiveProcedure falseCond = new PrimitiveProcedure("escm-sort-in-rhs?", falseCondPrimitive);
      return filterFrom(trueCond,idx+1,(lhs) -> () -> {
        return ((OrderedCollection)lhs).sort(binaryPredicate,(sortedLhs) -> () -> {
          return filterFrom(falseCond,idx+1,(rhs) -> () -> {
            return ((OrderedCollection)rhs).sort(binaryPredicate,(sortedRhs) -> () -> {
              return continuation.run(mergeSortedHalves((Vector)sortedLhs,hd,(Vector)sortedRhs));
            });
          });
        });
      });
    }
  }

  public Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sort(binaryPredicate,0,continuation);
  }

  private Trampoline.Bounce sorted(Callable binaryPredicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx+1 >= value.size()) return continuation.run(Boolean.TRUE);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(value.get(idx));
      args.add(value.get(idx+1));
      return binaryPredicate.callWith(args,(isEq) -> () -> {
        if(isEq.isTruthy()) return sorted(binaryPredicate,idx+1,continuation);
        return continuation.run(Boolean.FALSE);
      });
    }
  }

  public Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sorted(binaryPredicate,0,continuation);
  }

  //////////////////////////////////////
  // merging
  //////////////////////////////////////

  private Datum toListFromIndex(int idx) throws Exception {
    Datum lis = escm.type.Nil.VALUE;
    synchronized(this) {
      for(int i = value.size()-1; i >= idx; --i)
        lis = new escm.type.Pair(value.get(i),lis);
    }
    return lis;
  }

  private static Trampoline.Bounce mergeIter(Callable binaryPredicate, Vector v1, Vector v2, int i1, int i2, Trampoline.Continuation continuation) throws Exception {
    synchronized(v1) {
      synchronized(v2) {
        if(i1 >= v1.value.size()) return continuation.run(v2.toListFromIndex(i2));
        if(i2 >= v2.value.size()) return continuation.run(v1.toListFromIndex(i1));
        Datum v1Elt = v1.value.get(i1);
        Datum v2Elt = v2.value.get(i2);
        ArrayList<Datum> args = new ArrayList<Datum>(2);
        args.add(v1Elt);
        args.add(v2Elt);
        return binaryPredicate.callWith(args,(lt) -> () -> {
          if(lt.isTruthy()) {
            return mergeIter(binaryPredicate,v1,v2,i1+1,i2,(merged) -> () -> continuation.run(new Pair(v1Elt,merged)));
          }
          return mergeIter(binaryPredicate,v1,v2,i1,i2+1,(merged) -> () -> continuation.run(new Pair(v2Elt,merged)));
        });
      }
    }
  }

  public Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception {
    return mergeIter(binaryPredicate,this,(Vector)oc,0,0,(mergedList) -> () -> continuation.run(((AssociativeCollection)mergedList).toACVector()));
  }

  //////////////////////////////////////
  // duplicate neighbor deletion
  //////////////////////////////////////

  private Trampoline.Bounce skipWhileHaveDuplicates(Callable binaryPredicate, Datum d, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx >= value.size()) return continuation.run(new Exact(idx));
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(d);
      args.add(value.get(idx));
      return binaryPredicate.callWith(args,(isEq) -> () -> {
        if(isEq.isTruthy()) {
          return skipWhileHaveDuplicates(binaryPredicate,d,idx+1,continuation);
        }
        return continuation.run(new Exact(idx));
      });
    }
  }

  private Trampoline.Bounce deleteListNeighborDuplicates(Callable binaryPredicate, int idx, Trampoline.Continuation continuation) throws Exception {
    synchronized(this) {
      if(idx+1 >= value.size()) return continuation.run(toListFromIndex(idx));
      Datum elt = value.get(idx);
      return skipWhileHaveDuplicates(binaryPredicate,elt,idx+1,(idxAfterDups) -> () -> {
        return deleteListNeighborDuplicates(binaryPredicate,((Real)idxAfterDups).intValue(),(deldList) -> () -> continuation.run(new Pair(elt,deldList)));
      });
    }
  }

  public Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return deleteListNeighborDuplicates(binaryPredicate,0,(deldList) -> () -> continuation.run(((AssociativeCollection)deldList).toACVector()));
  }

  //////////////////////////////////////
  // array-list conversion
  //////////////////////////////////////

  public ArrayList<Datum> toArrayList() {
    synchronized(this) {
      return new ArrayList<Datum>(value);
    }
  }
}