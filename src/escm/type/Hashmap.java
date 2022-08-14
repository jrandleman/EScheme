// Author: Jordan Randleman - escm.type.Hashmap
// Purpose:
//    Mutable hashmap primitive type.
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
//
//      - Datum get(Datum key)
//      - boolean set(Datum key, Datum val) // returns whether replaced a prior value
//      - boolean delete(Datum key)         // returns whether succeeded
//
//      - void addAll(ArrayList<Hashmap> hashmaps)
//
//      - Datum toList()
//      - Vector toVector()
//
//      - callWith(...) // given a key, returns the value at that position

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.ExecutionState;
import escm.vm.type.Callable;

public class Hashmap extends Datum implements Callable {
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

  public boolean delete(Datum key) {
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
  // Callable (unary key getter)
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    if(arguments.size() != 1)
      throw new Exceptionf("HASHMAP [CALLABLE-GET]: Expects exactly 1 key arg for hashmap %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum key = arguments.get(0);
    Datum val = value.get(key);
    if(val == null)
      throw new Exceptionf("HASHMAP [CALLABLE-GET]: Invalid key %s for hashmap %s", key.profile(), write());
    return continuation.run(val);
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

  public Hashmap copy() {
    return new Hashmap(new ConcurrentHashMap<Datum,Datum>(value));
  }
}