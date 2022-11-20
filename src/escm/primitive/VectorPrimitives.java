// Author: Jordan Randleman - escm.primitive.VectorPrimitives
// Purpose:
//    Java primitives for vector procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.Nil;
import escm.type.Void;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class VectorPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // vector
  public static class Vector implements Primitive {
    public java.lang.String escmName() {
      return "vector";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return new escm.type.Vector(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-vector
  public static class MakeVector implements Primitive {
    public java.lang.String escmName() {
      return "make-vector";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(make-vector <length> <fill-value>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum length = parameters.get(0);
      Datum fillVal = parameters.get(1);
      if(!ListPrimitives.isValidSize(length))
        throw new Exceptionf("'(make-vector <length> <fill-value>) 1st arg is an invalid length: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Vector(((Real)length).intValue(),fillVal);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-length
  public static class VectorLength implements Primitive {
    public java.lang.String escmName() {
      return "vector-length";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-length <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.Vector)parameters.get(0)).size());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-ref
  public static class VectorRef implements Primitive {
    public java.lang.String escmName() {
      return "vector-ref";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-ref <vector> <index>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum i = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-ref <vector> <index>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(i))
        throw new Exceptionf("'(vector-ref <vector> <index>) 2nd arg is an invalid index: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).get(((Real)i).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-set!
  public static class VectorSetBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-set!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3)
        throw new Exceptionf("'(vector-set! <vector> <index> <obj>) expects exactly 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum i = parameters.get(1);
      Datum o = parameters.get(2);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-set! <vector> <index> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(i))
        throw new Exceptionf("'(vector-set! <vector> <index> <obj>) 2nd arg is an invalid index: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)v).set(((Real)i).intValue(),o);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-fill!
  public static class VectorFillBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-fill!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-fill! <vector> <fill-value>) invalid args: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)parameters.get(0)).fill(parameters.get(1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-grow!
  public static class VectorGrowBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-grow!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3)
        throw new Exceptionf("'(vector-grow! <vector> <length> <fill-value>) expects exactly 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum l = parameters.get(1);
      Datum o = parameters.get(2);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-grow! <vector> <length> <fill-value>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(l))
        throw new Exceptionf("'(vector-grow! <vector> <length> <fill-value>) 2nd arg is an invalid length: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)v).grow(((Real)l).intValue(),o);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-insert!
  public static class VectorInsertBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-insert!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3)
        throw new Exceptionf("'(vector-insert! <vector> <index> <obj>) expects exactly 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum i = parameters.get(1);
      Datum o = parameters.get(2);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-insert! <vector> <index> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(i))
        throw new Exceptionf("'(vector-insert! <vector> <index> <obj>) 2nd arg is an invalid index: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)v).insert(((Real)i).intValue(),o);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-delete!
  public static class VectorDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-delete!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-delete! <vector> <index>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum i = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-delete! <vector> <index>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(i))
        throw new Exceptionf("'(vector-delete! <vector> <index>) 2nd arg is an invalid index: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).delete(((Real)i).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-push!
  public static class VectorPushBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-push!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-push! <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-push! <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)v).push(o);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-push-front!
  public static class VectorPushFrontBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-push-front!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-push-front! <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-push-front! <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      ((escm.type.Vector)v).pushFront(o);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-pop!
  public static class VectorPopBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-pop!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-pop! <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).pop();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-pop-front!
  public static class VectorPopFrontBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-pop-front!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-pop-front! <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).popFront();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-append
  public static class VectorAppend implements Primitive {
    public java.lang.String escmName() {
      return "vector-append";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(vector-append <vector> ...) expects at least 1 vector: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.Vector> appends = new ArrayList<escm.type.Vector>();
      for(Datum d : parameters) {
        if(!(d instanceof escm.type.Vector))
          throw new Exceptionf("'(vector-append <vector> ...) invalid non-vector %s given: %s", d.profile(), Exceptionf.profileArgs(parameters));
        appends.add((escm.type.Vector)d);
      }
      return escm.type.Vector.append(appends);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-append!
  public static class VectorAppendBang implements Primitive {
    public java.lang.String escmName() {
      return "vector-append!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(vector-append! <vector> ...) expects at least 1 vector: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.Vector> appends = new ArrayList<escm.type.Vector>();
      Datum appendedTo = parameters.get(0);
      if(!(appendedTo instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-append! <vector> ...) invalid non-vector %s given: %s", appendedTo.profile(), Exceptionf.profileArgs(parameters));
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum d = parameters.get(i);
        if(!(d instanceof escm.type.Vector))
          throw new Exceptionf("'(vector-append! <vector> ...) invalid non-vector %s given: %s", d.profile(), Exceptionf.profileArgs(parameters));
        appends.add((escm.type.Vector)d);
      }
      ((escm.type.Vector)appendedTo).pushAll(appends);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-reverse
  public static class VectorReverse implements Primitive {
    public java.lang.String escmName() {
      return "vector-reverse";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-reverse <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).reverse();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // subvector
  public static class Subvector implements Primitive {
    public java.lang.String escmName() {
      return "subvector";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2 || parameters.size() > 3)
        throw new Exceptionf("'(subvector <vector> <index> <optional-length>) invalid number of args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum i = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(subvector <vector> <index> <optional-length>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      if(!ListPrimitives.isValidSize(i))
        throw new Exceptionf("'(subvector <vector> <index> <optional-length>) 2nd arg is an invalid index: %s", Exceptionf.profileArgs(parameters));
      escm.type.Vector vect = (escm.type.Vector)v;
      int idx = ((Real)i).intValue();
      int length = vect.size();
      if(parameters.size() == 3) {
        Datum l = parameters.get(2);
        if(!ListPrimitives.isValidSize(l))
          throw new Exceptionf("'(subvector <vector> <index> <optional-length>) 3rd arg is an invalid length: %s", Exceptionf.profileArgs(parameters));
        length = ((Real)l).intValue();
      }
      return vect.subvector(idx,length);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-memq
  public static class VectorMemq implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-memq";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-memq <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-memq <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).indexOf(ListPrimitives.Memq.getGlobalEqpProcedure(),o,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-member
  public static class VectorMember implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-member";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-member <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-member <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).indexOf(ListPrimitives.Member.getGlobalEqualpProcedure(),o,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-sort
  public static class VectorSort implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-sort";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-sort <predicate?> <vector>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(vector-sort <predicate?> <vector>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(!(target instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-sort <predicate?> <vector>) 2nd arg %s isn't a vector: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return ListPrimitives.Sort.logic((Callable)procedure,((escm.type.Vector)target).toList(),(sorted) -> () -> {
        return continuation.run(TypeCoercionPrimitives.ListToVector.logic(sorted));
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-sorted?
  public static class IsVectorSorted implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-sorted?";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-sorted? <predicate?> <vector>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(vector-sorted? <predicate?> <vector>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(!(target instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-sorted? <predicate?> <vector>) 2nd arg %s isn't a vector: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return ListPrimitives.IsSorted.logic((Callable)procedure,((escm.type.Vector)target).toList(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector?
  public static class IsVector implements Primitive {
    public java.lang.String escmName() {
      return "vector?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(vector? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Vector);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-empty?
  public static class IsVectorEmpty implements Primitive {
    public java.lang.String escmName() {
      return "vector-empty?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-empty? <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Vector)parameters.get(0)).size() == 0);
    }
  }
}