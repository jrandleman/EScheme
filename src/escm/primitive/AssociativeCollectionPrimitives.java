// Author: Jordan Randleman - escm.primitive.AssociativeCollectionPrimitives
// Purpose:
//    Java primitives for associative collection operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.Keyword;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.callable.Signature;

public class AssociativeCollectionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // associative-collection?
  public static class IsAssociativeCollection extends Primitive {
    public java.lang.String escmName() {
      return "associative-collection?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("associative-collection?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns whether <obj> is an associative collection:\n    String | List | Vector | Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("head"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nGet the first item in <ac>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("tail"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nGet everything after the first item in <ac>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("empty?"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns whether <ac> is empty.";
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

    public Datum signature() {
      return Pair.List(new Symbol("length"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns <ac>'s length. Aliased by <len>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("length+"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns <ac>'s length. If <ac> is a dotted-list, returns #f.\nThis is instead of triggering an error, as <length> would.";
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

    public Datum signature() {
      return Pair.List(new Symbol("fold"),new Symbol("<callable>"),new Symbol("<seed>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nAccumulate the values in \"<ac> ...\" from left to right by applying\n<callable> to \"<previous-result>\" and <item> with <seed-obj> acting\nas the initial \"<previous-result>\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("map"),new Symbol("<callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nCreates a new associative collection by applying <callable> to each item in\n\"<ac> ...\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("for-each"),new Symbol("<callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nApplies <callable> to each item in \"<ac> ...\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("filter"),new Symbol("<keep?-callable>"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nCreates a new associative collection by filtering items in <ac> that don't\nsatisfy the <keep?-callable> callable.";
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

    public Datum signature() {
      return Pair.List(new Symbol("count"),new Symbol("<predicate?-callable>"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nCount the number of times <predicate?-callable> is satisfied in <ac>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("remove"),new Symbol("<predicate?-callable>"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nCreates a new associative collection by removing items in <ac> that\nsatisfy the <predicate?-callable> callable.";
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

    public Datum signature() {
      return Pair.List(new Symbol("val"),new Symbol("<associative-collection>"),new Symbol("<key>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nGet the value in <ac> associated to <key>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("key"),new Symbol("<predicate?-callable>"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nGet the first key in <ac> who's associated value satisfies <predicate?-callable>.";
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("append")),
        Pair.List(new Symbol("append"),new Symbol("<obj>")),
        Pair.List(new Symbol("append"),new Symbol("<symbol>"),Signature.VARIADIC),
        Pair.List(new Symbol("append"),new Symbol("<keyword>"),Signature.VARIADIC),
        Pair.List(new Symbol("append"),new Symbol("<list>"),Signature.VARIADIC,new Symbol("<obj>")),
        Pair.List(new Symbol("append"),new Symbol("<associative-collection>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nAliased by <+>. Given nothing, returns NIL. Given '<obj>', returns <obj>.\n\nGiven '<symbol> ...':\n  Returns a new symbol of '<symbol> ...' concatenated with one another.\n\nGiven '<keyword> ...':\n  Returns a new keyword of '<keyword> ...' concatenated with one another.\n\nGiven '<ac> ...':\n  Creates a new associative collection by appending '<ac> ...' together.\n\n    => Note that the '<ac> ...' values will have their types unified according\n       to the following hierarchy: String < List < Vector < Hashmap";
    }

    private static Symbol appendSymbols(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) {
        if(!(p instanceof Symbol))
          throw new Exceptionf("'(append <symbol> ...) received a non-symbol object %s!", p.profile());
        sb.append(((Symbol)p).value());
      }
      return new Symbol(sb.toString());
    }

    private static Keyword appendKeywords(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) {
        if(!(p instanceof Keyword))
          throw new Exceptionf("'(append <keyword> ...) received a non-keyword object %s!", p.profile());
        sb.append(((Keyword)p).value().substring(1));
      }
      return new Keyword(sb.toString());
    }

    private static Datum appendAssociativeContainers(int n, ArrayList<Datum> parameters) throws Exception {
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

    // Used by <+>
    public static boolean isAppendable(Datum d) {
      return d instanceof Symbol || d instanceof Keyword || d instanceof AssociativeCollection;
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 2) return n == 0 ? Nil.VALUE : parameters.get(0);
      Datum firstObj = parameters.get(0);
      if(firstObj instanceof Symbol) return appendSymbols(parameters);
      if(firstObj instanceof Keyword) return appendKeywords(parameters);
      return appendAssociativeContainers(n,parameters);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // delete
  public static class Delete extends Primitive {
    public java.lang.String escmName() {
      return "delete";
    }

    public Datum signature() {
      return Pair.List(new Symbol("delete"),new Symbol("<associative-collection>"),new Symbol("<key>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturn a copy of <ac> without <key>'s association.";
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("conj"),new Symbol("<value>"),new Symbol("<associative-collection>")),
        Pair.List(new Symbol("conj"),new Symbol("<key>"),new Symbol("<value>"),new Symbol("<associative-collection>")));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nGiven 3 args:\n  Returns a copy of <ac> with <val> associated to <key>.\nGiven 2 args:\n  Returns a copy of <oc> with <val> added as efficiently as possible.\n  <val>'s position will depend on <ac>'s specific collection type.\nNote:\n  <ac> denotes an associative-collection\n  <oc> denotes an ordered-collection";
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

    public Datum signature() {
      return Pair.List(new Symbol("take"),new Symbol("<associative-collection>"),new Symbol("<length>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns <length> items taken from the left of <ac>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("drop"),new Symbol("<associative-collection>"),new Symbol("<length>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns <ac> with <length> items dropped from its left side.";
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

    public Datum signature() {
      return Pair.List(new Symbol("any?"),new Symbol("<predicate?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns whether any of the <ac>s satisfy <predicate?-callable>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("every?"),new Symbol("<predicate?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns whether every one of the <ac>s satisfy <predicate?-callable>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("ac->string"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nConvert <ac> to a string (note that its keys must be increasing integers\nstarting from 0, and its values must be characters).";
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

    public Datum signature() {
      return Pair.List(new Symbol("ac->list"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nConvert <ac> to a list (note that its keys must be increasing integers starting\nfrom 0).";
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

    public Datum signature() {
      return Pair.List(new Symbol("ac->vector"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nConvert <ac> to a vector (note that its keys must be increasing integers\nstarting from 0).";
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

    public Datum signature() {
      return Pair.List(new Symbol("ac->hashmap"),new Symbol("<associative-collection>"));
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nConvert <ac> to a hashmap.";
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

    public Datum signature() {
      return Pair.List(new Symbol("union"),new Symbol("<elt=?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns the set union of values (compared by <elt=?>) in \"<ac> ...\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("intersection"),new Symbol("<elt=?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns the intersection of values (compared by <elt=?>) between \"<ac> ...\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("difference"),new Symbol("<elt=?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns the set difference of values (compared by <elt=?>) in \"<ac> ...\".\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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

    public Datum signature() {
      return Pair.List(new Symbol("symmetric-difference"),new Symbol("<elt=?-callable>"),new Symbol("<associative-collection>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Associative-Collections\nReturns the set symmetric-difference of values (compared by <elt=?>) in\n\"<ac> ...\". SymDiff(a,b) = Union(Diff(a,b),Diff(b,a))\n\n  => Note that the \"<ac> ...\" values will have their types unified according\n     to the following hierarchy: String < List < Vector < Hashmap";
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