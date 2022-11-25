// Author: Jordan Randleman - escm.type.Nil
// Purpose:
//    Nil primitive type, also known as the "empty list".

package escm.type;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.util.ExecutionState;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.Callable;

public class Nil extends Datum implements AssociativeCollection {
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
    throw new Exceptionf("'() can't get <head> (<car>) of NIL!");
  }

  public Datum tail() throws Exception {
    throw new Exceptionf("'() can't get <tail> (<cdr>) of NIL!");
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
    throw new Exceptionf("'() can't get value of key %s in NIL!", key.profile());
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum 
    if(predicate instanceof Datum) {
      throw new Exceptionf("'() can't get key of predicate %s in NIL!", predicate);
    }
    throw new Exceptionf("'() can't get key of predicate %s in NIL!", predicate);
    
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////
  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    int nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) ++nonEmptyCount;
    }
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
    throw new Exceptionf("'() can't delete with key %s from NIL!", key.profile());
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum value) throws Exception {
    if(key instanceof Number && ((Number)key).eqs(ZERO)) return (AssociativeCollection)Pair.List(value);
    throw new Exceptionf("'() can't conj non-0 key %s with value %s onto NIL!", key.profile(), value.profile());
  }

  //////////////////////////////////////
  // drop & take
  //////////////////////////////////////

  public AssociativeCollection drop(int length) throws Exception {
    if(length == 0) return Nil.VALUE;
    throw new Exceptionf("'() can't drop %d items from NIL of length 0!", length);
  }


  public AssociativeCollection take(int length) throws Exception {
    if(length == 0) return Nil.VALUE;
    throw new Exceptionf("'() can't take %d items from NIL of length 0!", length);
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  public Datum toACList() throws Exception {
    return Nil.VALUE;
  }

  public String toACString() throws Exception {
    return new String("");
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
    int nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) ++nonEmptyCount;
    }
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
    int nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) ++nonEmptyCount;
    }
    if(nonEmptyCount == 0) return continuation.run(Nil.VALUE);
    AssociativeCollection[] nonEmptyACs = new AssociativeCollection[nonEmptyCount];
    nonEmptyCount = 0;
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() > 0) nonEmptyACs[nonEmptyCount++] = acs[i];
    }
    return nonEmptyACs[0].SymmetricDifferenceArray(eltPredicate,nonEmptyACs,continuation);
  }
}