// Author: Jordan Randleman - escm.type.Hashmap
// Purpose:
//    Mutable hashmap primitive type.
//
//    => NOTE THAT AC/OC PRIMITIVES EXPECT ALL ARGS TO BE HASHMAPS!
//
//    Provides:
//      - static Hashmap merge(ArrayList<Hashmap> hashmaps)
//
//      - int size()
//
//      - Datum keys()   // returns a list
//      - Datum values() // returns a list
//
//      - boolean hasKey()
//      - boolean hasVal()
//
//      - Datum get(Datum key)
//      - boolean set(Datum key, Datum val) // returns whether replaced a prior value
//      - boolean del(Datum key)            // returns whether succeeded
//
//      - void addAll(ArrayList<Hashmap> hashmaps)
//
//      - Datum toList()
//      - Vector toVector()
//
//      - Datum toReverseList() // used internally
//
//      - callWith(...) // given a key, returns the value at that position. alternatively give a value too to set!

package escm.type;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.bool.Boolean;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.util.ExecutionState;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.callable.Callable;

public class Hashmap extends Datum implements AssociativeCollection, Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Static merger
  public static Hashmap merge(ArrayList<Hashmap> hashmaps) {
    ConcurrentHashMap<Datum,Datum> merged = new ConcurrentHashMap<Datum,Datum>();
    for(Hashmap h : hashmaps) merged.putAll(h.value);
    return new Hashmap(merged);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private ConcurrentHashMap<Datum,Datum> value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Hashmap() {
    value = new ConcurrentHashMap<Datum,Datum>();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Size
  public int size() {
    return value.size();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Key/Value Aggregate Getters
  public Datum keys() {
    Datum lis = Nil.VALUE;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      lis = new Pair(entry.getKey(),lis);
    }
    return lis;
  }

  public Datum values() {
    Datum lis = Nil.VALUE;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      lis = new Pair(entry.getValue(),lis);
    }
    return lis;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Containment
  public boolean hasKey(Datum key) {
    return value.containsKey(key);
  }

  public boolean hasVal(Datum val) {
    return value.containsValue(val);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getter/Setter & Deletion
  public Datum get(Datum key) throws Exception {
    Datum val = value.get(key);
    if(val == null)
      throw new Exceptionf("HASHMAP [GET]: Invalid key %s for hashmap %s", val.write(), write());
    return val;
  }

  public boolean set(Datum key, Datum val) {
    return value.put(key,val) != null;
  }

  public boolean del(Datum key) {
    return value.remove(key) != null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Merge All Hashmaps Into This One
  public void addAll(ArrayList<Hashmap> hashmaps) {
    for(Hashmap h : hashmaps) {
      value.putAll(h.value);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Coercions
  public Datum toList() {
    Datum lis = Nil.VALUE;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      lis = new Pair(entry.getKey(),new Pair(entry.getValue(),lis));
    }
    return lis;
  }


  public Vector toVector() {
    Vector v = new Vector();
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      v.push(entry.getKey());
      v.push(entry.getValue());
    }
    return v;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Internal Coercions
  public Datum toReverseList() {
    Datum lis = Nil.VALUE;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      lis = new Pair(entry.getValue(),new Pair(entry.getKey(),lis));
    }
    return lis;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Callable (unary key getter)
  public java.lang.String docstring() {
    int n = value.size();
    if(n == 0) {
      return "Hashmap of length 0. Hashcode is "+hashCode();
    } else {
      return "Hashmap of length "+n+".\nApply to one of its keys to get an associated value.\nSet those keys by applying to a key and the new value.\nHashcode is "+hashCode()+".";
    }
  }

  public Datum signature() {
    return Pair.List(
      Pair.List(this,new Symbol("<key>")),
      Pair.List(this,new Symbol("<key>"),new Symbol("<obj>")));
  }

  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    int n = arguments.size();
    if(n == 0)
      throw new Exceptionf("HASHMAP [CALLABLE-GET]: Expects 1 or 2 arg(s) for hashmap %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum key = arguments.get(0);
    if(n == 1) {
      Datum val = value.get(key);
      if(val == null)
        throw new Exceptionf("HASHMAP [CALLABLE-GET]: Invalid key %d (size %d) for hashmap %s", key, value.size(), write());
      return continuation.run(val);
    }
    if(n == 2) {
      value.put(key,arguments.get(1));
      return continuation.run(Void.VALUE);
    }
    throw new Exceptionf("HASHMAP [CALLABLE-GET]: Expects 1 or 2 arg(s) for hashmap %s: %s", write(), Exceptionf.profileArgs(arguments));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "hashmap";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Hashmap && ((Hashmap)o).value == value;
  }

  public boolean equal(Object o) {
    if(o instanceof Hashmap) {
      Hashmap h = (Hashmap)o;
      if(value.size() != h.value.size()) return false;
      for(Datum key : value.keySet()) {
        Datum val = h.value.get(key);
        if(val == null || value.get(key).equal(val) == false) return false;
      }
      return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    StringBuilder sb = new StringBuilder("{");
    int i = 0, n = value.size();
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      sb.append(entry.getKey().display());
      sb.append(' ');
      sb.append(entry.getValue().display());
      if(i+1 < n) sb.append(' ');
      ++i;
    }
    sb.append('}');
    return sb.toString();
  }

  public java.lang.String write() {
    StringBuilder sb = new StringBuilder("{");
    int i = 0, n = value.size();
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      sb.append(entry.getKey().write());
      sb.append(' ');
      sb.append(entry.getValue().write());
      if(i+1 < n) sb.append(' ');
      ++i;
    }
    sb.append('}');
    return sb.toString();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Hashmap loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Hashmap loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  private Hashmap(ConcurrentHashMap<Datum,Datum> value) {
    this.value = value;
  }

  public Hashmap shallowCopy() {
    return new Hashmap(new ConcurrentHashMap<Datum,Datum>(value));
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      return entry.getValue();
    }
    throw new Exceptionf("HASHMAP [HEAD]: Can't get head of an empty hashmap!");
  }

  public AssociativeCollection tail() throws Exception {
    if(value.size() == 0) throw new Exceptionf("HASHMAP [TAIL]: Can't get tail of an empty hashmap!");
    Hashmap h = new Hashmap();
    int count = 0;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      if(count++ > 0) {
        h.set(entry.getKey(),entry.getValue());
      }
    }
    return h;
  }

  public int length() {
    return value.size();
  }

  //////////////////////////////////////
  // misc helper functions
  //////////////////////////////////////

  private static class HashmapEntry {
    private Datum key = Void.VALUE;
    private Datum value = Void.VALUE;
    public HashmapEntry(Datum k, Datum v) { key = k; value = v; }
    public Datum getKey() { return key; }
    public Datum getValue() { return value; }
  }

  private static HashmapEntry[] convertHashmapToEntryArray(Hashmap h) {
    int i = 0, n = h.size();
    HashmapEntry[] hes = new HashmapEntry[n];
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : h.value.entrySet()) {
      if(i == n) break;
      hes[i++] = new HashmapEntry(entry.getKey(),entry.getValue());
    }
    // In case concurrency shortened our map mid-iteration
    if(i < n) return Arrays.copyOf(hes,i);
    return hes;
  }

  private static HashmapEntry[][] convertHashmapsToEntryArrays(AssociativeCollection[] acs) {
    HashmapEntry[][] hs = new HashmapEntry[acs.length][];
    for(int i = 0; i < hs.length; ++i) {
      hs[i] = convertHashmapToEntryArray((Hashmap)acs[i]);
    }
    return hs;
  }

  //////////////////////////////////////
  // fold (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldIter(Callable c, Datum seed, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(seed);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(seed);
    args.add(ac[acPos].getValue());
    return c.callWith(args,(acc) -> () -> foldIter(c,acc,ac,acPos+1,continuation));
  }

  public Trampoline.Bounce fold(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return foldIter(c,seed,convertHashmapToEntryArray(this),0,continuation);
  }

  //////////////////////////////////////
  // fold (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldArrayIter(Callable c, Datum seed, HashmapEntry[][] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length+1);
    args.add(seed);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length) return continuation.run(seed);
      args.add(acs[i][acPos].getValue());
    }
    return c.callWith(args,(acc) -> () -> FoldArrayIter(c,acc,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return FoldArrayIter(c,seed,convertHashmapsToEntryArrays(acs),0,continuation);
  }

  ///////////////////////////////////////
  // map (unary)
  ///////////////////////////////////////

  private static Hashmap convertAListToHashmap(Datum lis) {
    Hashmap h = new Hashmap();
    while(lis instanceof Pair) {
      Pair p = (Pair)lis;
      Pair entry = (Pair)p.car();
      h.set(entry.car(),entry.cdr());
      lis = p.cdr();
    }
    return h;
  }

  private static Trampoline.Bounce mapIter(Callable c, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(Nil.VALUE);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(ac[acPos].getValue());
    return c.callWith(args,(mappedValue) -> () -> mapIter(c,ac,acPos+1,(mappedRest) -> () -> continuation.run(new Pair(new Pair(ac[acPos].getKey(),mappedValue),mappedRest))));
  }

  public Trampoline.Bounce map(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return mapIter(c,convertHashmapToEntryArray(this),0,(mappedList) -> () -> continuation.run(convertAListToHashmap(mappedList)));
  }

  ///////////////////////////////////////
  // map (binary+)
  ///////////////////////////////////////

  private static Trampoline.Bounce MapIter(Callable c, HashmapEntry[][] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length) return continuation.run(Nil.VALUE);
      args.add(acs[i][acPos].getValue());
    }
    return c.callWith(args,(mappedValue) -> () -> MapIter(c,acs,acPos+1,(mappedRest) -> () -> continuation.run(new Pair(new Pair(acs[0][acPos].getKey(),mappedValue),mappedRest))));
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return MapIter(c,convertHashmapsToEntryArrays(acs),0,(mappedList) -> () -> continuation.run(convertAListToHashmap(mappedList)));
  }

  //////////////////////////////////////
  // for-each (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce forEachIter(Callable c, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(Void.VALUE);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(ac[acPos].getValue());
    return c.callWith(args,(ignore) -> () -> forEachIter(c,ac,acPos+1,continuation));
  }

  public Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return forEachIter(c,convertHashmapToEntryArray(this),0,continuation);
  }

  //////////////////////////////////////
  // for-each (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce ForEachIter(Callable c, HashmapEntry[][] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length) return continuation.run(Void.VALUE);
      args.add(acs[i][acPos].getValue());
    }
    return c.callWith(args,(ignore) -> () -> ForEachIter(c,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return ForEachIter(c,convertHashmapsToEntryArrays(acs),0,continuation);
  }

  //////////////////////////////////////
  // filter
  //////////////////////////////////////

  private static Trampoline.Bounce filterIter(Callable predicate, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(Nil.VALUE);
    Datum hd = ac[acPos].getValue();
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldKeep) -> () -> {
      if(shouldKeep.isTruthy()) {
        return filterIter(predicate,ac,acPos+1,(filteredRest) -> () -> continuation.run(new Pair(new Pair(ac[acPos].getKey(),hd),filteredRest)));
      }
      return filterIter(predicate,ac,acPos+1,continuation);
    });
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,convertHashmapToEntryArray(this),0,(filteredList) -> () -> continuation.run(convertAListToHashmap(filteredList)));
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private Trampoline.Bounce countIter(Callable predicate, HashmapEntry[] ac, int n, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(new Exact(n));
    Datum hd = ac[acPos].getValue();
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldCount) -> () -> {
      if(shouldCount.isTruthy()) {
        return countIter(predicate,ac,n+1,acPos+1,continuation);
      }
      return countIter(predicate,ac,n,acPos+1,continuation);
    });
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return countIter(predicate,convertHashmapToEntryArray(this),0,0,continuation);
  }

  //////////////////////////////////////
  // remove (inverse of filter)
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) return continuation.run(Nil.VALUE);
    Datum hd = ac[acPos].getValue();
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) return removeIter(predicate,ac,acPos+1,continuation);
      return removeIter(predicate,ac,acPos+1,(removedRest) -> () -> continuation.run(new Pair(new Pair(ac[acPos].getKey(),hd),removedRest)));
    });
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return removeIter(predicate,convertHashmapToEntryArray(this),0,(removedList) -> () -> continuation.run(convertAListToHashmap(removedList)));
  }

  //////////////////////////////////////
  // val
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    return get(key);
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private Trampoline.Bounce keyIter(Callable predicate, HashmapEntry[] ac, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= ac.length) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("HASHMAP [KEY]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("HASHMAP [KEY]: no value in %s satisfies value predicate %s", write(), predicate);
    }
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(ac[acPos].getValue());
    return predicate.callWith(args,(matchedKey) -> () -> {
      if(matchedKey.isTruthy()) return continuation.run(ac[acPos].getKey());
      return keyIter(predicate,ac,acPos+1,continuation);
    });
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return keyIter(predicate,convertHashmapToEntryArray(this),0,continuation);
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

  public AssociativeCollection append(AssociativeCollection ac) throws Exception {
    ConcurrentHashMap<Datum,Datum> appended = new ConcurrentHashMap<Datum,Datum>();
    appended.putAll(value);
    appended.putAll(((Hashmap)ac).value);
    return new Hashmap(appended);
  }

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    ConcurrentHashMap<Datum,Datum> appended = new ConcurrentHashMap<Datum,Datum>();
    for(int i = 0; i < acs.length; ++i) {
      appended.putAll(((Hashmap)acs[i]).value);
    }
    return new Hashmap(appended);
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    Hashmap cpy = shallowCopy();
    if(cpy.del(key) == false)
      throw new Exceptionf("HASHMAP [DELETE]: invalid key %s for hashmap %s deletion", key.profile(), cpy.write());
    return (AssociativeCollection)cpy;
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum newValue) throws Exception {
    Hashmap cpy = shallowCopy();
    cpy.set(key,newValue);
    return (AssociativeCollection)cpy;
  }

  //////////////////////////////////////
  // drop
  //////////////////////////////////////

  public AssociativeCollection drop(int amount) throws Exception {
    amount = Math.max(0,amount);
    ConcurrentHashMap<Datum,Datum> vals = new ConcurrentHashMap<Datum,Datum>();
    int i = 0;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      if(i++ >= amount) vals.put(entry.getKey(),entry.getValue());
    }
    return (AssociativeCollection)(new Hashmap(vals));
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    amount = Math.max(0,amount);
    ConcurrentHashMap<Datum,Datum> vals = new ConcurrentHashMap<Datum,Datum>();
    int i = 0;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      if(i++ < amount) vals.put(entry.getKey(),entry.getValue());
    }
    return (AssociativeCollection)(new Hashmap(vals));
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  private static boolean validIndex(Datum d, Exact n) {
    if(!(d instanceof Real)) return false;
    Real r = (Real)d;
    return r.isInteger() && !r.isNegative() && r.lt(n);
  }

  public Datum toACList() throws Exception {
    Datum[] contents = new Datum[value.size()];
    Exact en = new Exact(contents.length);
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      Datum key = entry.getKey();
      if(!validIndex(key,en) || contents[((Real)key).intValue()] != null)
        throw new Exceptionf("HASHMAP [AC->LIST]: can't convert hashmap %s to an <ac-list>", write());
      contents[((Real)key).intValue()] = entry.getValue();
    }
    Datum l = Nil.VALUE;
    for(int i = contents.length-1; i >= 0; --i)
      l = new Pair(contents[i],l);
    return l;
  }

  public String toACString() throws Exception {
    escm.type.Character[] contents = new escm.type.Character[value.size()];
    Exact en = new Exact(contents.length);
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      Datum key = entry.getKey();
      if(!validIndex(key,en) || contents[((Real)key).intValue()] != null)
        throw new Exceptionf("HASHMAP [AC->STRING]: can't convert hashmap %s to an <ac-string>", write());
      Datum val = entry.getValue();
      if(!(val instanceof escm.type.Character))
        throw new Exceptionf("HASHMAP [AC->STRING]: can't convert hashmap %s to an <ac-string>", write());
      contents[((Real)key).intValue()] = (escm.type.Character)val;
    }
    StringBuilder sb = new StringBuilder();
    for(int i = 0; i < contents.length; ++i) 
      sb.append(contents[i].display());
    return new String(sb.toString());
  }

  public Vector toACVector() throws Exception {
    Datum[] contents = new Datum[value.size()];
    Exact en = new Exact(contents.length);
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : value.entrySet()) {
      Datum key = entry.getKey();
      if(!validIndex(key,en) || contents[((Real)key).intValue()] != null)
        throw new Exceptionf("HASHMAP [AC->VECTOR]: can't convert hashmap %s to an <ac-vector>", write());
      contents[((Real)key).intValue()] = entry.getValue();
    }
    return new Vector(contents);
  }

  public Hashmap toACHashmap() throws Exception {
    return shallowCopy();
  }

  //////////////////////////////////////
  // union set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValues(Callable eltPredicate, Datum elt, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(!(values instanceof Pair)) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(elt);
    args.add(((Pair)((Pair)values).car()).cdr());
    return eltPredicate.callWith(args,(match) -> () -> {
      if(match.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValues(eltPredicate,elt,((Pair)values).cdr(),continuation);
    });
  }

  private Trampoline.Bounce UnionArrayIter(Callable eltPredicate, HashmapEntry[][] acs, int i, HashmapEntry[] h, int j, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(values);
    if(j >= h.length) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? acs[i] : acs[i+1],0,values,continuation);
    HashmapEntry he = h[j];
    return inValues(eltPredicate, he.getValue(), values, (isInValues) -> () -> {
      if(isInValues.isTruthy()) {
        return UnionArrayIter(eltPredicate,acs,i,h,j+1,values,continuation);
      }
      return UnionArrayIter(eltPredicate,acs,i,h,j+1,new Pair(new Pair(he.getKey(),he.getValue()),values),continuation);
    });
  }

  public Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    HashmapEntry[][] acs_entries = convertHashmapsToEntryArrays(acs);
    return UnionArrayIter(eltPredicate,acs_entries,0,acs_entries[0],0,Nil.VALUE,(valuesList) -> () -> {
      Datum vl = valuesList; // lambda arg must be <final>
      ConcurrentHashMap<Datum,Datum> vals = new ConcurrentHashMap<Datum,Datum>();
      while(vl instanceof Pair) {
        Pair p = (Pair)vl;
        Pair kv = (Pair)p.car();
        vals.put(kv.car(),kv.cdr());
        vl = p.cdr();
      }
      return continuation.run(new Hashmap(vals));
    });
  }

  //////////////////////////////////////
  // intersection set operation
  //////////////////////////////////////

  private Trampoline.Bounce itemIntersectsAC(Callable eltPredicate, HashmapEntry[] ac, int acIdx, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= ac.length) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(item);
    args.add(ac[acIdx].getValue());
    return eltPredicate.callWith(args,(isSameItem) -> () -> {
      if(isSameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return itemIntersectsAC(eltPredicate,ac,acIdx+1,item,continuation);
    });
  }

  private Trampoline.Bounce itemIntersects(Callable eltPredicate, HashmapEntry[][] acs, int i, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(Boolean.TRUE);
    return itemIntersectsAC(eltPredicate,acs[i],0,item,(itemInAC) -> () -> {
      if(itemInAC.isTruthy()) return () -> itemIntersects(eltPredicate,acs,i+1,item,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  private Trampoline.Bounce IntersectionArrayIter(Callable eltPredicate, HashmapEntry[][] acs, int acIdx, HashmapEntry[] h, int hIdx, Datum intersectingValues, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= acs.length) return continuation.run(intersectingValues);
    if(hIdx >= h.length) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? acs[acIdx] : acs[acIdx+1],0,intersectingValues,continuation);
    return inValues(eltPredicate, h[hIdx].getValue(), intersectingValues, (isInValues) -> () -> {
      if(!isInValues.isTruthy()) {
        return itemIntersects(eltPredicate,acs,0,h[hIdx].getValue(),(intersects) -> () -> {
          if(intersects.isTruthy()) {
            return IntersectionArrayIter(eltPredicate,acs,acIdx,h,hIdx+1,new Pair(new Pair(h[hIdx].getKey(),h[hIdx].getValue()),intersectingValues),continuation);
          }
          return IntersectionArrayIter(eltPredicate,acs,acIdx,h,hIdx+1,intersectingValues,continuation);
        });
      }
      return IntersectionArrayIter(eltPredicate,acs,acIdx,h,hIdx+1,intersectingValues,continuation);
    });
  }

  public Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    HashmapEntry[][] acs_entries = convertHashmapsToEntryArrays(acs);
    return IntersectionArrayIter(eltPredicate,acs_entries,0,acs_entries[0],0,Nil.VALUE,(intersectingValues) -> () -> {
      Datum ivs = intersectingValues; // lambda arg must be <final>
      ConcurrentHashMap<Datum,Datum> vals = new ConcurrentHashMap<Datum,Datum>();
      while(ivs instanceof Pair) {
        Pair p = (Pair)ivs;
        Pair kv = (Pair)p.car();
        vals.put(kv.car(),kv.cdr());
        ivs = p.cdr();
      }
      return continuation.run(new Hashmap(vals));
    });
  }

  //////////////////////////////////////
  // difference set operation
  //////////////////////////////////////

  private Datum convertHashmapToAlist(Hashmap h) {
    Datum lis = Nil.VALUE;
    for(ConcurrentHashMap.Entry<Datum,Datum> entry : h.value.entrySet()) {
      lis = new Pair(new Pair(entry.getKey(),entry.getValue()),lis);
    }
    return lis;
  }

  private Trampoline.Bounce inValuesHashmap(Callable eltPredicate, Datum elt, HashmapEntry[] valHashmap, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= valHashmap.length) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(elt);
    args.add(valHashmap[i].getValue());
    return eltPredicate.callWith(args,(sameItem) -> () -> {
      if(sameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValuesHashmap(eltPredicate,elt,valHashmap,i+1,continuation);
    });
  }

  private Trampoline.Bounce binaryDifferenceArrayIter(Datum acc, Callable eltPredicate, Datum valList, HashmapEntry[] valHashmap, Trampoline.Continuation continuation) throws Exception {
    if(!(valList instanceof Pair)) return continuation.run(acc);
    Pair p = (Pair)valList;
    return inValuesHashmap(eltPredicate,((Pair)p.car()).cdr(),valHashmap,0,(inVector) -> () -> {
      if(inVector.isTruthy()) {
        return binaryDifferenceArrayIter(acc,eltPredicate,p.cdr(),valHashmap,continuation);
      }
      return binaryDifferenceArrayIter(new Pair(p.car(),acc),eltPredicate,p.cdr(),valHashmap,continuation);
    });
  }

  private Trampoline.Bounce DifferenceArrayIter(Callable eltPredicate, Datum lhs, HashmapEntry[][] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(lhs);
    return binaryDifferenceArrayIter(Nil.VALUE,eltPredicate,lhs,acs[i],(differenceArray) -> () -> DifferenceArrayIter(eltPredicate,differenceArray,acs,i+1,continuation));
  }

  public Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArrayIter(eltPredicate,convertHashmapToAlist((Hashmap)acs[0]),convertHashmapsToEntryArrays(acs),1,(differenceList) -> () -> {
      Datum dl = differenceList; // lambda arg must be <final>
      ConcurrentHashMap<Datum,Datum> vals = new ConcurrentHashMap<Datum,Datum>();
      while(dl instanceof Pair) {
        Pair p = (Pair)dl;
        Pair kv = (Pair)p.car();
        vals.put(kv.car(),kv.cdr());
        dl = p.cdr();
      }
      return continuation.run(new Hashmap(vals));
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