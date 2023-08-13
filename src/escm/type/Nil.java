// Author: Jordan Randleman - escm.type.Nil
// Purpose:
//    Nil primitive type, also known as the "empty list".
//
//    => NOTE THAT AC/OC PRIMITIVES EXPECT ALL ARGS TO BE LISTS!

package escm.type;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.util.ExecutionState;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.OrderedCollection;
import escm.vm.type.Callable;

public class Nil extends Datum implements OrderedCollection {
  ////////////////////////////////////////////////////////////////////////////
  // Static Value
  public static final Nil VALUE = new Nil();


  ////////////////////////////////////////////////////////////////////////////
  // Private Constructor (use <VALUE>)
  private Nil(){}


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "nil";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Nil;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "()";
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Nil loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Nil loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Nil copy() {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    throw new Exceptionf("NIL [HEAD]: can't get <head> (<car>) of NIL!");
  }

  public AssociativeCollection tail() throws Exception {
    throw new Exceptionf("NIL [TAIL]: can't get <tail> (<cdr>) of NIL!");
  }

  public int length() {
    return 0;
  }

  //////////////////////////////////////
  // fold, map, for-each, filter
  //////////////////////////////////////

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return continuation.run(seed);
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Void
    return continuation.run(Void.VALUE);
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // count, remove
  //////////////////////////////////////

  private static final Exact ZERO = new Exact();

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return continuation.run(ZERO);
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // val, key
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    throw new Exceptionf("NIL [VAL]: can't get value of key %s in NIL!", key.profile());
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum 
    if(predicate instanceof Datum) {
      throw new Exceptionf("NIL [KEY]: can't get key of predicate %s in NIL!", ((Datum)predicate).profile());
    }
    throw new Exceptionf("NIL [KEY]: can't get key of predicate %s in NIL!", predicate);
    
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

  private static int getNonEmptyCount(AssociativeCollection[] acs) {
    int nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) ++nonEmptyCount;
    }
    return nonEmptyCount;
  }

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    int nonEmptyCount = getNonEmptyCount(acs);
    if(nonEmptyCount == 0) return Nil.VALUE;
    AssociativeCollection[] nonEmptyACs = new AssociativeCollection[nonEmptyCount];
    nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) nonEmptyACs[nonEmptyCount++] = acs[i];
    }
    return nonEmptyACs[0].AppendArray(nonEmptyACs);
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    throw new Exceptionf("NIL [DELETE]: can't delete with key %s from NIL!", key.profile());
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum value) throws Exception {
    if(key instanceof Real && ((Real)key).isInteger()) {
      int idx = ((Real)key).intValue();
      if(idx == 0 || idx == -1) return (AssociativeCollection)(new Pair(value,Nil.VALUE));
      throw new Exceptionf("NIL [CONJ]: index %d violates nil bounds [-1,0]", idx);
    }
    throw new Exceptionf("NIL [CONJ]: invalid index %s for nil", key.profile());
  }

  //////////////////////////////////////
  // drop & take
  //////////////////////////////////////

  public AssociativeCollection drop(int length) throws Exception {
    return Nil.VALUE;
  }


  public AssociativeCollection take(int length) throws Exception {
    return Nil.VALUE;
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  public Datum toACList() throws Exception {
    return this;
  }

  public String toACString() throws Exception {
    return new String();
  }

  public Vector toACVector() throws Exception {
    return new Vector();
  }

  public Hashmap toACHashmap() throws Exception {
    return new Hashmap();
  }

  //////////////////////////////////////
  // set operations
  //////////////////////////////////////

  public Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    int nonEmptyCount = getNonEmptyCount(acs);
    if(nonEmptyCount == 0) return continuation.run(Nil.VALUE);
    AssociativeCollection[] nonEmptyACs = new AssociativeCollection[nonEmptyCount];
    nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) nonEmptyACs[nonEmptyCount++] = acs[i];
    }
    return nonEmptyACs[0].UnionArray(eltPredicate,nonEmptyACs,continuation);
  }

  public Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce SymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    int nonEmptyCount = getNonEmptyCount(acs);
    if(nonEmptyCount == 0) return continuation.run(Nil.VALUE);
    AssociativeCollection[] nonEmptyACs = new AssociativeCollection[nonEmptyCount];
    nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) nonEmptyACs[nonEmptyCount++] = acs[i];
    }
    return nonEmptyACs[0].SymmetricDifferenceArray(eltPredicate,nonEmptyACs,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // <OrderedCollection> Instance Methods

  //////////////////////////////////////
  // constructor
  //////////////////////////////////////

  public OrderedCollection conj(Datum value) throws Exception {
    return new Pair(value,Nil.VALUE);
  }

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////

  public OrderedCollection init() throws Exception {
    throw new Exceptionf("NIL [INIT]: can't get <init> of NIL!");
  }

  public Datum last() throws Exception {
    throw new Exceptionf("NIL [LAST]: can't get <last> of NIL!");
  }

  //////////////////////////////////////
  // slicing
  //////////////////////////////////////

  public OrderedCollection slice(int startIdx) throws Exception {
    return Nil.VALUE;
  }

  public OrderedCollection slice(int startIdx, int length) throws Exception {
    return Nil.VALUE;
  }

  public Trampoline.Bounce slice(int startIdx, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // reversing
  //////////////////////////////////////

  public OrderedCollection reverse() throws Exception {
    return Nil.VALUE;
  }

  //////////////////////////////////////
  // removing items
  //////////////////////////////////////

  public Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // skipping
  //////////////////////////////////////

  public Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Boolean.FALSE);
  }

  public Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Boolean.FALSE);
  }

  //////////////////////////////////////
  // fold-right
  //////////////////////////////////////

  public Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(seed);
  }

  //////////////////////////////////////
  // key-right
  //////////////////////////////////////

  public Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    if(predicate instanceof Datum) {
      throw new Exceptionf("NIL [key-right]: can't get the right key from NIL with predicate %s!", ((Datum)predicate).profile());
    }
    throw new Exceptionf("NIL [key-right]: can't get the right key from NIL with predicate %s!", predicate);
  }

  //////////////////////////////////////
  // dropping
  //////////////////////////////////////

  public OrderedCollection dropRight(int length) throws Exception {
    return Nil.VALUE;
  }

  public Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // taking
  //////////////////////////////////////

  public OrderedCollection takeRight(int length) throws Exception {
    return Nil.VALUE;
  }

  public Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  //////////////////////////////////////
  // sorting
  //////////////////////////////////////

  public Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }

  public Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Boolean.TRUE);
  }

  //////////////////////////////////////
  // merging
  //////////////////////////////////////

  public Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception {
    return continuation.run((Datum)oc);
  }

  //////////////////////////////////////
  // duplicate neighbor deletion
  //////////////////////////////////////

  public Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return continuation.run(Nil.VALUE);
  }
}