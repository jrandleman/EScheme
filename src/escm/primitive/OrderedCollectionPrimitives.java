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
  // oc-ordered-collection?
  public static class OCIsOrderedCollection implements Primitive {
    public java.lang.String escmName() {
      return "oc-ordered-collection?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oc-ordered-collection? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OrderedCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-oc?
  public static class OCIsOC implements Primitive {
    public java.lang.String escmName() {
      return "oc-oc?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(oc-oc? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OrderedCollection);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-conj
  public static class OCConj implements Primitive {
    public java.lang.String escmName() {
      return "oc-conj";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(1) instanceof OrderedCollection)) 
        throw new Exceptionf("'(oc-conj <value> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(1)).conj(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-init
  public static class OCInit implements Primitive {
    public java.lang.String escmName() {
      return "oc-init";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(oc-init <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).init();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-last
  public static class OCLast implements Primitive {
    public java.lang.String escmName() {
      return "oc-last";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(oc-last <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(0)).last();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-slice
  public static class OCSlice implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-slice";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2 || parameters.size() > 3) 
        throw new Exceptionf("'(oc-slice <ordered-collection> <start-index> <end-predicate-or-length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-slice <ordered-collection> <start-index> <end-predicate-or-length>) 1st arg isn't an <oc>: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(oc-slice <ordered-collection> <start-index> <end-predicate-or-length>) 2nd arg isn't an index: %s", Exceptionf.profileArgs(parameters));
      if(parameters.size() == 2) {
        return continuation.run((Datum)((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue()));
      }
      if(ListPrimitives.isValidSize(parameters.get(2))) {
        return continuation.run((Datum)((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue(),((Real)parameters.get(2)).intValue()));
      }
      if(parameters.get(2) instanceof Callable) {
        return ((OrderedCollection)parameters.get(0)).slice(((Real)parameters.get(1)).intValue(),(Callable)parameters.get(2),continuation);
      }
      throw new Exceptionf("'(oc-slice <ordered-collection> <start-index> <end-predicate-or-length>) invalid 3rd arg: %s", Exceptionf.profileArgs(parameters));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-reverse
  public static class OCReverse implements Primitive {
    public java.lang.String escmName() {
      return "oc-reverse";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OrderedCollection)) 
        throw new Exceptionf("'(oc-reverse <ordered-collection>) expects exactly 1 <oc>: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).reverse();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-remove-first
  public static class OCRemoveFirst implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-remove-first";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-remove-first <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).removeFirst((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-remove-last
  public static class OCRemoveLast implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-remove-last";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-remove-last <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).removeLast((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-skip
  public static class OCSkip implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-skip";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-skip <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).skip((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-skip-right
  public static class OCSkipRight implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-skip-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-skip-right <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).skipRight((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-fold-right
  public static class OCFoldRight implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-fold-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 3) 
        throw new Exceptionf("'(oc-fold-right <callable> <seed> <ordered-collection> ...) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(oc-fold-right <callable> <seed> <ordered-collection> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      Callable c = (Callable)parameters.get(0);
      Datum seed = parameters.get(1);
      OrderedCollection[] ocs = OrderedCollection.parseParameters("(oc-fold-right <callable> <seed> <ordered-collection> ...)",parameters,2);
      return ocs[0].FoldRightArray(c,seed,ocs,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-key-right
  public static class OCKeyRight implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-key-right";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-key-right <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).keyRight((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-drop-right
  public static class OCDropRight implements Primitive {
    public java.lang.String escmName() {
      return "oc-drop-right";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof OrderedCollection) || !ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(oc-drop-right <ordered-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).dropRight(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-drop-while
  public static class OCDropWhile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-drop-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-drop-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).dropWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-drop-right-while
  public static class OCDropRightWhile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-drop-right-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-drop-right-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).dropRightWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-take-right
  public static class OCTakeRight implements Primitive {
    public java.lang.String escmName() {
      return "oc-take-right";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof OrderedCollection) || !ListPrimitives.isValidSize(parameters.get(1)))
        throw new Exceptionf("'(oc-take-right <ordered-collection> <length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return (Datum)((OrderedCollection)parameters.get(0)).takeRight(((Real)parameters.get(1)).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-take-while
  public static class OCTakeWhile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-take-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-take-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).takeWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-take-right-while
  public static class OCTakeRightWhile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-take-right-while";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-take-right-while <predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).takeRightWhile((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-sort
  public static class OCSort implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-sort";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-sort <binary-predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).sort((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-sorted?
  public static class OCIsSorted implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-sorted?";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-sorted? <binary-predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).sorted((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-merge
  public static class OCMerge implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-merge";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 3 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection) || !(parameters.get(2) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-merge <binary-predicate?> <ordered-collection> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      OrderedCollection oc1 = (OrderedCollection)parameters.get(1);
      OrderedCollection oc2 = OrderedCollection.unifyType(oc1,(OrderedCollection)parameters.get(2));
      return oc1.merge((Callable)parameters.get(0),oc2,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // oc-delete-neighbor-duplicates
  public static class OCDeleteNeighborDuplicates implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "oc-delete-neighbor-duplicates";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Callable) || !(parameters.get(1) instanceof OrderedCollection))
        throw new Exceptionf("'(oc-delete-neighbor-duplicates <binary-predicate?> <ordered-collection>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return ((OrderedCollection)parameters.get(1)).deleteNeighborDuplicates((Callable)parameters.get(0),continuation);
    }
  }
}