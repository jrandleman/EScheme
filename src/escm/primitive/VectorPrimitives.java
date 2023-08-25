// Author: Jordan Randleman - escm.primitive.VectorPrimitives
// Purpose:
//    Java primitives for vector procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Void;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.callable.Signature;

public class VectorPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // vector
  public static class Vector extends Primitive {
    public java.lang.String escmName() {
      return "vector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return new escm.type.Vector(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-vector
  public static class MakeVector extends Primitive {
    public java.lang.String escmName() {
      return "make-vector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("make-vector"),new Symbol("<length>"),new Symbol("<fill-value>"));
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
  // vector-set!
  public static class VectorSetBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-set!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-set!"),new Symbol("<vector>"),new Symbol("<index>"),new Symbol("<obj>"));
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
  public static class VectorFillBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-fill!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-fill!"),new Symbol("<vector>"),new Symbol("<fill-value>"));
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
  public static class VectorGrowBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-grow!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-grow!"),new Symbol("<vector>"),new Symbol("<length>"),new Symbol("<fill-value>"));
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
  public static class VectorInsertBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-insert!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-insert!"),new Symbol("<vector>"),new Symbol("<index>"),new Symbol("<obj>"));
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
  public static class VectorDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-delete!"),new Symbol("<vector>"),new Symbol("<index>"));
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
  public static class VectorPushBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-push!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-push!"),new Symbol("<vector>"),new Symbol("<obj>"));
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
  public static class VectorPushFrontBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-push-front!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-push-front!"),new Symbol("<vector>"),new Symbol("<obj>"));
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
  public static class VectorPopBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-pop!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-pop!"),new Symbol("<vector>"));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-pop! <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).pop();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-pop-front!
  public static class VectorPopFrontBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-pop-front!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-pop-front!"),new Symbol("<vector>"));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-pop-front! <vector>) expects exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).popFront();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-append!
  public static class VectorAppendBang extends Primitive {
    public java.lang.String escmName() {
      return "vector-append!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-append!"),new Symbol("<vector>"),Signature.VARIADIC);
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
  // vector-unfold
  public static class VectorUnfold extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-unfold";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-unfold"),new Symbol("<break?-callable>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(vector-unfold <break?-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(vector-unfold <break?-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(vector-unfold <break?-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(vector-unfold <break?-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return ListPrimitives.Unfold.logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),(resultList) -> () -> {
        return continuation.run(((AssociativeCollection)resultList).toACVector());
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-unfold-right
  public static class VectorUnfoldRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-unfold-right";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-unfold-right"),new Symbol("<break?-callable>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(vector-unfold-right <break?-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(vector-unfold-right <break?-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(vector-unfold-right <break?-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(vector-unfold-right <break?-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return ListPrimitives.UnfoldRight.logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),(resultList) -> () -> {
        return continuation.run(((AssociativeCollection)resultList).toACVector());
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-memq
  public static class VectorMemq extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-memq";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-memq"),new Symbol("<vector>"),new Symbol("<obj>"));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-memq <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-memq <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).indexOf(ListPrimitives.Memq.getEqpProcedure(this.definitionEnvironment),o,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector-member
  public static class VectorMember extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "vector-member";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector-member"),new Symbol("<vector>"),new Symbol("<obj>"));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(vector-member <vector> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum v = parameters.get(0);
      Datum o = parameters.get(1);
      if(!(v instanceof escm.type.Vector))
        throw new Exceptionf("'(vector-member <vector> <obj>) 1st arg isn't a vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)v).indexOf(ListPrimitives.Member.getEqualpProcedure(this.definitionEnvironment),o,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector?
  public static class IsVector extends Primitive {
    public java.lang.String escmName() {
      return "vector?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector?"),new Symbol("<obj>"));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(vector? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Vector);
    }
  }
}