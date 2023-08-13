// Author: Jordan Randleman - escm.primitive.AssociativeCollectionPrimitives
// Purpose:
//    Java primitives for associative collection operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.OrderedCollection;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class AssociativeCollectionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // associative-collection?
  public static class IsAssociativeCollection extends Primitive {
    public java.lang.String escmName() {
      return "associative-collection?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(associative-collection? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof AssociativeCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // head
  public static class Head extends Primitive {
    public java.lang.String escmName() {
      return "head";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(head <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).head();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // tail
  public static class Tail extends Primitive {
    public java.lang.String escmName() {
      return "tail";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(tail <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).tail();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // empty?
  public static class EmptyP extends Primitive {
    public java.lang.String escmName() {
      return "empty?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(empty? <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((AssociativeCollection)parameters.get(0)).length() == 0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // length
  public static class Length extends Primitive {
    public java.lang.String escmName() {
      return "length";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(length <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      Datum d = parameters.get(0);
      if(Pair.isDottedList(d))
        throw new Exceptionf("'(length <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((AssociativeCollection)d).length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // length+
  public static class LengthPlus extends Primitive {
    public java.lang.String escmName() {
      return "length+";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(length+ <associative-collection>) expects exactly 1 <ac>: %s", Exceptionf.profileArgs(parameters));
      Datum d = parameters.get(0);
      if(Pair.isDottedList(d)) return Boolean.FALSE;
      return new Exact(((AssociativeCollection)parameters.get(0)).length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // fold
  public static class Fold extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "fold";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 3) 
        throw new Exceptionf("'(fold <callable> <seed> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(fold <callable> <seed> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(n == 3) {
        if(!(parameters.get(2) instanceof AssociativeCollection))
          throw new Exceptionf("'(fold <callable> <seed> <associative-collection> ...) 3rd arg isn't an <associative-collection>: %s", Exceptionf.profileArgs(parameters));
        return ((AssociativeCollection)parameters.get(2)).fold((Callable)parameters.get(0),parameters.get(1),continuation);
      } else {
        AssociativeCollection[] acs = AssociativeCollection.parseParameters("(fold <callable> <seed> <associative-collection> ...)",parameters,2);
        return acs[0].FoldArray((Callable)parameters.get(0),parameters.get(1),acs,continuation);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // map
  public static class Map extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "map";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 2) 
        throw new Exceptionf("'(map <callable> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(map <callable> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(n == 2) {
        if(!(parameters.get(1) instanceof AssociativeCollection))
          throw new Exceptionf("'(map <callable> <associative-collection> ...) 2nd arg isn't an <associative-collection>: %s", Exceptionf.profileArgs(parameters));
        return ((AssociativeCollection)parameters.get(1)).map((Callable)parameters.get(0),continuation);
      } else {
        AssociativeCollection[] acs = AssociativeCollection.parseParameters("(map <callable> <associative-collection> ...)",parameters,1);
        return acs[0].MapArray((Callable)parameters.get(0),acs,continuation);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // for-each
  public static class ForEach extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "for-each";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 2) 
        throw new Exceptionf("'(for-each <callable> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(for-each <callable> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(n == 2) {
        if(!(parameters.get(1) instanceof AssociativeCollection))
          throw new Exceptionf("'(for-each <callable> <associative-collection> ...) 2nd arg isn't an <associative-collection>: %s", Exceptionf.profileArgs(parameters));
        return ((AssociativeCollection)parameters.get(1)).forEach((Callable)parameters.get(0),continuation);
      } else {
        AssociativeCollection[] acs = AssociativeCollection.parseParameters("(for-each <callable> <associative-collection> ...)",parameters,1);
        return acs[0].ForEachArray((Callable)parameters.get(0),acs,continuation);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // filter
  public static class Filter extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "filter";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(filter <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(filter <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(filter <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).filter((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // count
  public static class Count extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "count";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(count <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(count <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(count <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).count((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // remove
  public static class Remove extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "remove";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(remove <callable> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(remove <callable> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(remove <callable> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).remove((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // val
  public static class Val extends Primitive {
    public java.lang.String escmName() {
      return "val";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(val <associative-collection> <key>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(val <associative-collection> <key>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).val(parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // key
  public static class Key extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "key";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(key <predicate?> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(key <predicate?> <associative-collection>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof AssociativeCollection))
        throw new Exceptionf("'(key <predicate?> <associative-collection>) 2nd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(1)).key((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // append
  public static class Append extends Primitive {
    public java.lang.String escmName() {
      return "append";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 0) return Nil.VALUE;
      if(n == 1) return parameters.get(0);
      Datum dotValue = parameters.get(n-1);
      if(!(dotValue instanceof AssociativeCollection)) {
        parameters.remove(n-1);
        Datum result = Nil.VALUE;
        if(n == 2) {
          result = parameters.get(0);
          if(!(result instanceof AssociativeCollection))
            throw new Exceptionf("'(append <associative-collection> ...) 1st arg isn't an <associative-collection>: %s, %s", Exceptionf.profileArgs(parameters), dotValue.profile());
        } else if(n == 3) { // testing against <3> b/c of the removed dot-value
          Datum param1 = parameters.get(0), param2 = parameters.get(1);
          if(!(param1 instanceof AssociativeCollection))
            throw new Exceptionf("'(append <associative-collection> ...) 1st arg isn't an <associative-collection>: %s, %s", Exceptionf.profileArgs(parameters), dotValue.profile());
          if(!(param2 instanceof AssociativeCollection))
            throw new Exceptionf("'(append <associative-collection> ...) 2nd arg isn't an <associative-collection>: %s, %s", Exceptionf.profileArgs(parameters), dotValue.profile());
          AssociativeCollection ac1 = (AssociativeCollection)param1;
          AssociativeCollection ac2 = AssociativeCollection.unifyType(ac1,(AssociativeCollection)param2);
          result = (Datum)ac1.append(ac2);
        } else {
          AssociativeCollection[] acs = AssociativeCollection.parseParameters("(append <associative-collection> ...)",parameters,0);
          result = (Datum)acs[0].AppendArray(acs);
        }
        if(Pair.isList(result)) return Pair.binaryAppend(result,dotValue);
        throw new Exceptionf("'(append <associative-collection> ...) Can't append non-<ac> %s to %s: %s, %s", dotValue.profile(), result.write(), Exceptionf.profileArgs(parameters), dotValue.profile());
      } else if(n == 2) {
        Datum param1 = parameters.get(0), param2 = parameters.get(1);
        if(!(param1 instanceof AssociativeCollection))
          throw new Exceptionf("'(append <associative-collection> ...) 1st arg isn't an <associative-collection>: %s", Exceptionf.profileArgs(parameters));
        if(!(param2 instanceof AssociativeCollection))
          throw new Exceptionf("'(append <associative-collection> ...) 2nd arg isn't an <associative-collection>: %s", Exceptionf.profileArgs(parameters));
        AssociativeCollection ac1 = (AssociativeCollection)param1;
        AssociativeCollection ac2 = AssociativeCollection.unifyType(ac1,(AssociativeCollection)param2);
        return (Datum)ac1.append(ac2);
      } else {
        AssociativeCollection[] acs = AssociativeCollection.parseParameters("(append <associative-collection> ...)",parameters,0);
        return (Datum)acs[0].AppendArray(acs);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // delete
  public static class Delete extends Primitive {
    public java.lang.String escmName() {
      return "delete";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(delete <associative-collection> <key>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(delete <associative-collection> <key>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).delete(parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // conj
  public static class Conj extends Primitive {
    public java.lang.String escmName() {
      return "conj";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 2) {
        if(!(parameters.get(1) instanceof OrderedCollection))
          throw new Exceptionf("'(conj <value> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
        return (Datum)((OrderedCollection)parameters.get(1)).conj(parameters.get(0));
      }
      if(parameters.size() != 3) 
        throw new Exceptionf("'(conj <key> <val> <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof AssociativeCollection))
        throw new Exceptionf("'(conj <key> <val> <associative-collection>) 3rd arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(2)).conj(parameters.get(0),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // take
  public static class Take extends Primitive {
    public java.lang.String escmName() {
      return "take";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(take <associative-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(take <associative-collection> <length>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
        throw new Exceptionf("'(take <associative-collection> <length>) 2nd arg isn't a valid <length>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).take(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // drop
  public static class Drop extends Primitive {
    public java.lang.String escmName() {
      return "drop";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(drop <associative-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof AssociativeCollection))
        throw new Exceptionf("'(drop <associative-collection> <length>) 1st arg isn't an <ac>: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Real) || !((Real)parameters.get(1)).isInteger())
        throw new Exceptionf("'(drop <associative-collection> <length>) 2nd arg isn't a valid <length>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((AssociativeCollection)parameters.get(0)).drop(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // any?
  public static class AnyP extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "any?";
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
        throw new Exceptionf("'(any? <predicate?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum predicate = parameters.get(0);
      if(!(predicate instanceof Callable))
        throw new Exceptionf("'(any? <predicate?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return iter((Callable)predicate,parameters,1,parameters.size(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // every?
  public static class EveryP extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "every?";
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
        throw new Exceptionf("'(every? <predicate?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum predicate = parameters.get(0);
      if(!(predicate instanceof Callable))
        throw new Exceptionf("'(every? <predicate?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return iter((Callable)predicate,parameters,1,parameters.size(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac->string
  public static class ACToString extends Primitive {
    public java.lang.String escmName() {
      return "ac->string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac->string <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACString();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac->list
  public static class ACToList extends Primitive {
    public java.lang.String escmName() {
      return "ac->list";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac->list <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACList();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac->vector
  public static class ACToVector extends Primitive {
    public java.lang.String escmName() {
      return "ac->vector";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac->vector <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACVector();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ac->hashmap
  public static class ACToHashmap extends Primitive {
    public java.lang.String escmName() {
      return "ac->hashmap";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof AssociativeCollection)) 
        throw new Exceptionf("'(ac->hashmap <associative-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((AssociativeCollection)parameters.get(0)).toACHashmap();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // union
  public static class Union extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "union";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(union <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(union <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(union <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].UnionArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // intersection
  public static class Intersection extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "intersection";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(intersection <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(intersection <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(intersection <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].IntersectionArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // difference
  public static class Difference extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "difference";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(difference <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(difference <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(difference <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].DifferenceArray(c,acs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symmetric-difference
  public static class SymmetricDifference extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "symmetric-difference";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(symmetric-difference <elt=?> <associative-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum fst = parameters.get(0);
      if(!(fst instanceof Callable) || (fst instanceof AssociativeCollection))
        throw new Exceptionf("'(symmetric-difference <elt=?> <associative-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)fst;
      AssociativeCollection[] acs = AssociativeCollection.parseParameters("(symmetric-difference <elt=?> <associative-collection> ...)",parameters,1);
      return acs[0].SymmetricDifferenceArray(c,acs,continuation);
    }
  }
}