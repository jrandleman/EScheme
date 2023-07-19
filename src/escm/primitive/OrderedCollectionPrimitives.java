// Author: Jordan Randleman - escm.primitive.OrderedCollectionPrimitives
// Purpose:
//    Java primitives for ordered collection operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.OrderedCollection;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class OrderedCollectionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // ordered-collection?
  public static class IsOrderedCollection extends Primitive {
    public java.lang.String escmName() {
      return "ordered-collection?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(ordered-collection? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OrderedCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc?
  public static class IsOC extends Primitive {
    public java.lang.String escmName() {
      return "oc?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oc? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OrderedCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // init
  public static class Init extends Primitive {
    public java.lang.String escmName() {
      return "init";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(init <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).init();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // last
  public static class Last extends Primitive {
    public java.lang.String escmName() {
      return "last";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(last <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(0)).last();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // slice
  public static class Slice extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "slice";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2 || parameters.size() > 3) 
        throw new Exceptionf("'(slice <ordered-collection> <start-index> <optional-end-predicate-or-length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof OrderedCollection))
        throw new Exceptionf("'(slice <ordered-collection> <start-index> <optional-end-predicate-or-length>) 1st arg isn't an <oc>: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(slice <ordered-collection> <start-index> <optional-end-predicate-or-length>) 2nd arg isn't an index: %s", Exceptionf.profileArgs(parameters));
      if(parameters.size() == 2) {
        return continuation.run((Datum)((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue()));
      }
      if(ListPrimitives.isValidSize(parameters.get(2))) {
        return continuation.run((Datum)((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue(),((Real)parameters.get(2)).intValue()));
      }
      if(parameters.get(2) instanceof Callable) {
        return ((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue(),(Callable)parameters.get(2),continuation);
      }
      throw new Exceptionf("'(slice <ordered-collection> <start-index> <optional-end-predicate-or-length>) invalid 3rd arg: %s", Exceptionf.profileArgs(parameters));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // reverse
  public static class Reverse extends Primitive {
    public java.lang.String escmName() {
      return "reverse";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(reverse <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).reverse();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // remove-first
  public static class RemoveFirst extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "remove-first";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(remove-first <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).removeFirst((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // remove-last
  public static class RemoveLast extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "remove-last";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(remove-last <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).removeLast((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // skip
  public static class Skip extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "skip";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(skip <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).skip((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // skip-right
  public static class SkipRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "skip-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(skip-right <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).skipRight((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // fold-right
  public static class FoldRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "fold-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 3) 
        throw new Exceptionf("'(fold-right <callable> <seed> <ordered-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(fold-right <callable> <seed> <ordered-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)parameters.get(0);
      Datum seed = parameters.get(1);
      OrderedCollection[] ocs = OrderedCollection.parseParameters("(fold-right <callable> <seed> <ordered-collection> ...)",parameters,2);
      return ocs[0].FoldRightArray(c,seed,ocs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // key-right
  public static class KeyRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "key-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(key-right <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).keyRight((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // drop-right
  public static class DropRight extends Primitive {
    public java.lang.String escmName() {
      return "drop-right";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof OrderedCollection) || !ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(drop-right <ordered-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).dropRight(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // drop-while
  public static class DropWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "drop-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(drop-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).dropWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // drop-right-while
  public static class DropRightWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "drop-right-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(drop-right-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).dropRightWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // take-right
  public static class TakeRight extends Primitive {
    public java.lang.String escmName() {
      return "take-right";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof OrderedCollection) || !ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(take-right <ordered-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).takeRight(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // take-while
  public static class TakeWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "take-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(take-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).takeWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // take-right-while
  public static class TakeRightWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "take-right-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(take-right-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).takeRightWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sort
  public static class Sort extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "sort";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(sort <binary-predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).sort((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sorted?
  public static class IsSorted extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "sorted?";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(sorted? <binary-predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).sorted((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // merge
  public static class Merge extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "merge";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 3 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection) || !(parameters.get(2) instanceof OrderedCollection))
        throw new Exceptionf("'(merge <binary-predicate?> <ordered-collection> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      OrderedCollection oc1 = (OrderedCollection)parameters.get(1);
      OrderedCollection oc2 = OrderedCollection.unifyType(oc1,(OrderedCollection)parameters.get(2));
      return oc1.merge((Callable)parameters.get(0),oc2,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // delete-neighbor-duplicates
  public static class DeleteNeighborDuplicates extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "delete-neighbor-duplicates";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(delete-neighbor-duplicates <elt=?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).deleteNeighborDuplicates((Callable)parameters.get(0),continuation);
    }
  }
}