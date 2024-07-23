// Author: Jordan Randleman - escm.vm.type.collection.OrderedCollection
// Purpose:
//    Primitive interface that all ordered core containers must implement
//    to be used by the ordered container primitives.

package escm.vm.type.collection;
import java.util.ArrayList;
import escm.type.Datum;
import escm.util.Trampoline;
import escm.util.error.Exceptionf;
import escm.vm.type.callable.Callable;

public interface OrderedCollection extends AssociativeCollection {
  //////////////////////////////////////////////////////////////////////
  // Static Methods
  static final int STRING = 1;
  static final int LIST   = 2;
  static final int VECTOR = 3;


  // Returns <oc2> as <oc1>'s type:
  public static OrderedCollection unifyType(OrderedCollection oc1, OrderedCollection oc2) throws Exception {
    if(escm.type.Pair.isList((Datum)oc1)) {
      if(escm.type.Pair.isList((Datum)oc2)) return oc2;
      return (OrderedCollection)oc2.toACList();
    }
    if(oc1 instanceof escm.type.Vector) {
      if(oc2 instanceof escm.type.Vector) return oc2;
      return (OrderedCollection)oc2.toACVector();
    }
    if(oc1 instanceof escm.type.String) {
      if(oc2 instanceof escm.type.String) return oc2;
      return (OrderedCollection)oc2.toACString();
    }
    throw new Exceptionf("Unknown <OrderedCollection> unification super type given: %s", ((Datum)oc1).profile());
  }


  // Unifies the types in <ocs> according to the following hierarchy:
  //   String < List < Vector
  private static void unifyTypes(int type, OrderedCollection[] ocs) throws Exception {
    if(type == LIST) {
      for(int i = 0; i < ocs.length; ++i) {
        if(!escm.type.Pair.isList((Datum)ocs[i])) {
          ocs[i] = (OrderedCollection)ocs[i].toACList();
        }
      }
    } else if(type == STRING) {
      for(int i = 0; i < ocs.length; ++i) {
        if(!(ocs[i] instanceof escm.type.String)) {
          ocs[i] = (OrderedCollection)ocs[i].toACString();
        }
      }
    } else if(type == VECTOR) {
      for(int i = 0; i < ocs.length; ++i) {
        if(!(ocs[i] instanceof escm.type.Vector)) {
          ocs[i] = (OrderedCollection)ocs[i].toACVector();
        }
      }
    } else {
      StringBuilder sb = new StringBuilder();
      for(int i = 0; i < ocs.length; ++i) {
        sb.append(((Datum)ocs[i]).profile());
        if(i+1 < ocs.length) sb.append(", ");
      }
      throw new Exceptionf("Unknown <OrderedCollection> type code \"%d\" given for unification: %s", type, sb.toString());
    }
  }


  // Check & extract AC's as a unified type from a parameter list
  public static OrderedCollection[] parseParameters(String fcnSignature, ArrayList<Datum> args, int acStartIdx) throws Exception {
    int i = Math.max(0,acStartIdx);
    int n = args.size();
    if(i >= n) {
      throw new Exceptionf("'%s arg #%d isn't an <oc> (ordered container)", fcnSignature, i+1);
    }
    int type = 0;
    OrderedCollection[] ocs = new OrderedCollection[n-i];
    for(int j = 0; i < n; ++i, ++j) {
      Datum oc = args.get(i);
      if(!(oc instanceof OrderedCollection)) {
        throw new Exceptionf("'%s arg #%d %s isn't an <oc> (ordered container)", fcnSignature, i+1, oc.profile());
      }
      ocs[j] = (OrderedCollection)oc;
      if(oc instanceof escm.type.String && type <= STRING) {
        type = STRING;
      } else if(escm.type.Pair.isList(oc) && type <= LIST) {
        type = LIST;
      } else if(oc instanceof escm.type.Vector && type <= VECTOR) {
        type = VECTOR;
      }
    }
    unifyTypes(type,ocs);
    return ocs;
  }


  //////////////////////////////////////////////////////////////////////
  // Instance Methods 
  //   => "<Name>Array" denotes a method that operates on the given array
  //      (that's had its ACs coerced to a single type), e.g. _not_ on 
  //      the <this> pointer (which is often in the given array anyway)
  OrderedCollection conj(Datum value) throws Exception;

  OrderedCollection init() throws Exception;
  Datum last() throws Exception;

  OrderedCollection slice(int startIdx) throws Exception; // <length> defaults to the end of the OrderedCollection
  OrderedCollection slice(int startIdx, int length) throws Exception;
  Trampoline.Bounce slice(int startIdx, Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  OrderedCollection reverse() throws Exception;

  Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection
  Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection
  Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  Trampoline.Bounce foldRight(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception; // -> Datum
  Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> Datum

  Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  OrderedCollection dropRight(int length) throws Exception;
  Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection
  Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  OrderedCollection takeRight(int length) throws Exception;
  Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection
  Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection
  Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception; // -> Boolean
  
  Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception; // -> OrderedCollection

  ArrayList<Datum> toArrayList();
}