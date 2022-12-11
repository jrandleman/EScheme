// Author: Jordan Randleman - escm.primitive.AssociativeCollectionPrimitives
// Purpose:
//    Java primitives for associative collection operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class AssociativeCollectionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // ac-associative-collection?
  public static class ACIsAssociativeCollection implements Primitive {
    public java.lang.String escmName() {
      return "ac-associative-collection?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(ac-associative-collection? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof AssociativeCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-ac?
  public static class ACIsAC implements Primitive {
    public java.lang.String escmName() {
      return "ac-ac?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(ac-ac? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof AssociativeCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-head
  public static class ACHead implements Primitive {
    public java.lang.String escmName() {
      return "ac-head";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-head <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).head();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-tail
  public static class ACTail implements Primitive {
    public java.lang.String escmName() {
      return "ac-tail";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-tail <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).tail();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-empty?
  public static class ACEmptyP implements Primitive {
    public java.lang.String escmName() {
      return "ac-empty?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-empty? <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((AssociativeCollection)parameters.get(0)).length() == 0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-length
  public static class ACLength implements Primitive {
    public java.lang.String escmName() {
      return "ac-length";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-length <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      Datum d = parameters.get(0);
      if(escm.type.Pair.isDottedList(d))
        throw new Exceptionf("'(ac-length <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((AssociativeCollection)d).length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-length+
  public static class ACLengthPlus implements Primitive {
    public java.lang.String escmName() {
      return "ac-length+";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-length+ <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      Datum d = parameters.get(0);
      if(escm.type.Pair.isDottedList(d)) return Boolean.FALSE;
      return new Exact(((AssociativeCollection)parameters.get(0)).length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-fold
  public static class ACFold implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-fold";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 3) 
        throw new Exceptionf("'(ac-fold <callable> <seed> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-fold <callable> <seed> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)parameters.get(0);
      Datum seed = parameters.get(1);
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-fold <callable> <seed> <associative-collection> ...)",parameters,2);
      return acs[0].FoldArray(c,seed,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-map
  public static class ACMap implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-map";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-map <callable> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-map <callable> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)parameters.get(0);
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-map <callable> <associative-collection> ...)",parameters,1);
      return acs[0].MapArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-for-each
  public static class ACForEach implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-for-each";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-for-each <callable> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-for-each <callable> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)parameters.get(0);
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-for-each <callable> <associative-collection> ...)",parameters,1);
      return acs[0].ForEachArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-filter
  public static class ACFilter implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-filter";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-filter <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-filter <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-filter <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).filter((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-count
  public static class ACCount implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-count";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-count <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-count <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-count <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).count((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-remove
  public static class ACRemove implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-remove";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-remove <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-remove <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-remove <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).remove((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-val
  public static class ACVal implements Primitive {
    public java.lang.String escmName() {
      return "ac-val";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-val <associative-collection> <key>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-val <associative-collection> <key>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).val(parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-key
  public static class ACKey implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-key";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-key <predicate?> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(ac-key <predicate?> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-key <predicate?> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).key((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-append
  public static class ACAppend implements Primitive {
    public java.lang.String escmName() {
      return "ac-append";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(ac-append <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-append <associative-collection> ...)",parameters,0);
      return (Datum)acs[0].AppendArray(acs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-delete
  public static class ACDelete implements Primitive {
    public java.lang.String escmName() {
      return "ac-delete";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-delete <associative-collection> <key>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-delete <associative-collection> <key>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).delete(parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-conj
  public static class ACConj implements Primitive {
    public java.lang.String escmName() {
      return "ac-conj";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3) 
        throw new Exceptionf("'(ac-conj <key> <val> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-conj <associative-collection> <key>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(2)).conj(parameters.get(0),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-take
  public static class ACTake implements Primitive {
    public java.lang.String escmName() {
      return "ac-take";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-take <associative-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-take <associative-collection> <length>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
        throw new Exceptionf("'(ac-take <associative-collection> <length>) 2nd arg isn't a valid <length>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).take(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-drop
  public static class ACDrop implements Primitive {
    public java.lang.String escmName() {
      return "ac-drop";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ac-drop <associative-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-drop <associative-collection> <length>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
        throw new Exceptionf("'(ac-drop <associative-collection> <length>) 2nd arg isn't a valid <length>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).drop(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-any?
  public static class ACAnyP implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-any?";
    }

    private Trampoline.Bounce iter(Callable predicate, ArrayList<Datum> parameters, int i, int n, Trampoline.Continuation continuation) throws Exception {
      if(i >= n) return continuation.run(Boolean.FALSE);
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(parameters.get(i));
      return predicate.callWith(args,(matched) -> () -> {
        if(matched.isTruthy()) return continuation.run(Boolean.TRUE);
        return iter(predicate,parameters,i+1,n,continuation);
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-any? <predicate?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum predicate = parameters.get(0);
      if(!(predicate instanceof Callable))
        throw new Exceptionf("'(ac-any? <predicate?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return iter((Callable)predicate,parameters,1,parameters.size(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-every?
  public static class ACEveryP implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-every?";
    }

    private Trampoline.Bounce iter(Callable predicate, ArrayList<Datum> parameters, int i, int n, Trampoline.Continuation continuation) throws Exception {
      if(i >= n) return continuation.run(Boolean.TRUE);
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(parameters.get(i));
      return predicate.callWith(args,(matched) -> () -> {
        if(!matched.isTruthy()) return continuation.run(Boolean.FALSE);
        return iter(predicate,parameters,i+1,n,continuation);
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-every? <predicate?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum predicate = parameters.get(0);
      if(!(predicate instanceof Callable))
        throw new Exceptionf("'(ac-every? <predicate?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return iter((Callable)predicate,parameters,1,parameters.size(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-ac->string
  public static class ACACToString implements Primitive {
    public java.lang.String escmName() {
      return "ac-ac->string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-ac->string <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACString();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-ac->list
  public static class ACACToList implements Primitive {
    public java.lang.String escmName() {
      return "ac-ac->list";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-ac->list <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACList();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-ac->vector
  public static class ACACToVector implements Primitive {
    public java.lang.String escmName() {
      return "ac-ac->vector";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-ac->vector <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACVector();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-ac->hashmap
  public static class ACACToHashmap implements Primitive {
    public java.lang.String escmName() {
      return "ac-ac->hashmap";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac-ac->hashmap <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACHashmap();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-union
  public static class ACUnion implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-union";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-union <predicate?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-union <predicate?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-union <predicate?> <associative-collection> ...)",parameters,1);
      return acs[0].UnionArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-intersection
  public static class ACIntersection implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-intersection";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-intersection <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-intersection <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-intersection <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].IntersectionArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-difference
  public static class ACDifference implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-difference";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-difference <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-difference <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-difference <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].DifferenceArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac-symmetric-difference
  public static class ACSymmetricDifference implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "ac-symmetric-difference";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(ac-symmetric-difference <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(ac-symmetric-difference <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(ac-symmetric-difference <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].SymmetricDifferenceArray(c,acs,continuation);
    }
  }
}