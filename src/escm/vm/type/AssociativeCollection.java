// Author: Jordan Randleman - escm.vm.type.AssociativeCollection
// Purpose:
//    Primitive interface that all associative core containers must implement
//    to be used by the associative container primitives.

package escm.vm.type;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Void;
import escm.util.Pair;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;

public interface AssociativeCollection {
  //////////////////////////////////////////////////////////////////////
  // Static Methods
  static final int STRING  = 1;
  static final int LIST    = 2;
  static final int VECTOR  = 3;
  static final int HASHMAP = 4;


  // Returns <ac2> as <ac1>'s type:
  public static AssociativeCollection unifyType(AssociativeCollection ac1, AssociativeCollection ac2) throws Exception {
    if(escm.type.Pair.isList((Datum)ac1)) {
      if(escm.type.Pair.isList((Datum)ac2)) return ac2;
      return (AssociativeCollection)ac2.toACList();
    }
    if(ac1 instanceof escm.type.Vector) {
      if(ac2 instanceof escm.type.Vector) return ac2;
      return (AssociativeCollection)ac2.toACVector();
    }
    if(ac1 instanceof escm.type.String) {
      if(ac2 instanceof escm.type.String) return ac2;
      return (AssociativeCollection)ac2.toACString();
    }
    if(ac1 instanceof escm.type.Hashmap) {
      if(ac2 instanceof escm.type.Hashmap) return ac2;
      return (AssociativeCollection)ac2.toACHashmap();
    }
    throw new Exceptionf("Unknown <AssociativeCollection> unification super type given: %s", ((Datum)ac1).profile());
  }


  // Unifies the types in <acs> according to the following hierarchy:
  //   String < List < Vector < Hashmap
  private static void unifyTypes(int type, AssociativeCollection[] acs) throws Exception {
    if(type == LIST) {
      for(int i = 0; i < acs.length; ++i) {
        if(!escm.type.Pair.isList((Datum)acs[i])) {
          acs[i] = (AssociativeCollection)acs[i].toACList();
        }
      }
    } else if(type == STRING) {
      for(int i = 0; i < acs.length; ++i) {
        if(!(acs[i] instanceof escm.type.String)) {
          acs[i] = (AssociativeCollection)acs[i].toACString();
        }
      }
    } else if(type == VECTOR) {
      for(int i = 0; i < acs.length; ++i) {
        if(!(acs[i] instanceof escm.type.Vector)) {
          acs[i] = (AssociativeCollection)acs[i].toACVector();
        }
      }
    } else if(type == HASHMAP) {
      for(int i = 0; i < acs.length; ++i) {
        if(!(acs[i] instanceof escm.type.Hashmap)) {
          acs[i] = (AssociativeCollection)acs[i].toACHashmap();
        }
      }
    } else {
      StringBuilder sb = new StringBuilder();
      for(int i = 0; i < acs.length; ++i) {
        sb.append(((Datum)acs[i]).profile());
        if(i+1 < acs.length) sb.append(", ");
      }
      throw new Exceptionf("Unknown <AssociativeCollection> type code \"%d\" given for unification: %s", type, sb.toString());
    }
  }


  // Check & extract AC's as a unified type from a parameter list
  public static AssociativeCollection[] parseParameters(String fcnSignature, ArrayList<Datum> args, int acStartIdx) throws Exception {
    int i = Math.max(0,acStartIdx);
    int n = args.size();
    if(i >= n) {
      throw new Exceptionf("'%s arg #%d isn't an <ac> (associative container)", fcnSignature, i+1);
    }
    int type = 0;
    AssociativeCollection[] acs = new AssociativeCollection[n-i];
    for(int j = 0; i < n; ++i, ++j) {
      Datum ac = args.get(i);
      if(!(ac instanceof AssociativeCollection)) {
        throw new Exceptionf("'%s arg #%d %s isn't an <ac> (associative container)", fcnSignature, i+1, ac.profile());
      }
      acs[j] = (AssociativeCollection)ac;
      if(ac instanceof escm.type.String && type <= STRING) {
        type = STRING;
      } else if(escm.type.Pair.isList(ac) && type <= LIST) {
        type = LIST;
      } else if(ac instanceof escm.type.Vector && type <= VECTOR) {
        type = VECTOR;
      } else if(ac instanceof escm.type.Hashmap && type <= HASHMAP) {
        type = HASHMAP;
      }
    }
    unifyTypes(type,acs);
    return acs;
  }


  //////////////////////////////////////////////////////////////////////
  // Instance Methods 
  //   => "<Name>Array" denotes a method that operates on the given array
  //      (that's had its ACs coerced to a single type), e.g. _not_ on 
  //      the <this> pointer (which is often in the given array anyway)
  Datum head() throws Exception;
  AssociativeCollection tail() throws Exception;

  int length();

  Trampoline.Bounce fold(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception; // -> Datum
  Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> Datum

  Trampoline.Bounce map(Callable c, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection
  Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection

  Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception; // -> Void
  Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> Void

  Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection

  Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> Exact
  Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection

  Datum val(Datum key) throws Exception;

  Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception; // -> Datum

  AssociativeCollection append(AssociativeCollection ac) throws Exception;
  AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception;

  AssociativeCollection delete(Datum key) throws Exception; // returns <this> if deletion fails

  AssociativeCollection conj(Datum key, Datum value) throws Exception;

  AssociativeCollection drop(int length) throws Exception;
  AssociativeCollection take(int length) throws Exception;

  Datum toACList() throws Exception;
  escm.type.String toACString() throws Exception;
  escm.type.Vector toACVector() throws Exception;
  escm.type.Hashmap toACHashmap() throws Exception;

  Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection
  Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection
  Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection
  Trampoline.Bounce SymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception; // -> AssociativeCollection
}