// Author: Jordan Randleman - escm.primitive.FunctionalPrimitives
// Purpose:
//    Java primitives for functional programming support.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.type.oo.EscmObject;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.callable.Signature;

public class FunctionalPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // id
  public static class Id extends Primitive {
    public java.lang.String escmName() {
      return "id";
    }

    public Datum signature() {
      return Pair.List(new Symbol("id"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Functional\nReturns <obj>. Useful for certain higher-level procedures.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(id <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return parameters.get(0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // procedure?
  public static class IsProcedure extends Primitive {
    public java.lang.String escmName() {
      return "procedure?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("procedure?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Functional\nReturns whether <obj> is a procedure.\nPrefer <callable?> for more generic code.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(procedure? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Procedure);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // callable?
  public static class IsCallable extends Primitive {
    public java.lang.String escmName() {
      return "callable?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("callable?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Functional\nReturns whether <obj> is applicable. Equivalent to:\n  (or (procedure? <obj>)\n      (functor? <obj>)\n      (class? <obj>)\n      (string? <obj>)\n      (vector? <obj>)\n      (hashmap? <obj>))";
    }

    public static boolean logic(Datum d) {
      if(d instanceof EscmObject) return ((EscmObject)d).isFunctor();
      return d instanceof Callable;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(callable? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // compose
  public static class Compose extends Primitive {
    public java.lang.String escmName() {
      return "compose";
    }

    public Datum signature() {
      return Pair.List(new Symbol("compose"),new Symbol("<callable>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Functional\nCreate a new, variadic procedure that is the composition of \"<callable> ...\".\nAliased by <*> if only given callables.";
    }

    private static Trampoline.Bounce applyComposedCallables(Datum result, int i, ArrayList<Datum> parameters, Trampoline.Continuation cont) throws Exception {
      if(i < 0) return cont.run(result);
      ArrayList<Datum> params = new ArrayList<Datum>(1);
      params.add(result);
      return ((Callable)parameters.get(i)).callWith(params,(intermediateResult) -> () -> {
        return applyComposedCallables(intermediateResult,i-1,parameters,cont);
      });
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      int totalCallables = parameters.size();
      if(totalCallables < 1) 
        throw new Exceptionf("'(compose <callable> <callable> ...) expects at least 1 callable args: %s", Exceptionf.profileArgs(parameters));
      for(Datum param : parameters)
        if(!(param instanceof Callable))
          throw new Exceptionf("'(compose <callable> <callable> ...) arg %s isn't a callable: %s", param.profile(), Exceptionf.profileArgs(parameters));
      if(totalCallables == 1) return parameters.get(0);
      Callable fstFcn = (Callable)parameters.get(totalCallables-1);
      Callable composedProcedures = new Callable() {
        public String docstring() {
          return "Composed series of procedures wrapped up in a single callable.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(Procedure.DEFAULT_NAME),new Symbol("<arg>"),Signature.VARIADIC);
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          return fstFcn.callWith(params,(result) -> () -> {
            return applyComposedCallables(result,totalCallables-2,parameters,cont);
          });
        }
      };
      return new PrimitiveProcedure(Procedure.DEFAULT_NAME,composedProcedures);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // bind
  public static class Bind extends Primitive {
    public java.lang.String escmName() {
      return "bind";
    }

    public Datum signature() {
      return Pair.List(new Symbol("bind"),new Symbol("<callable>"),new Symbol("<arg>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Functional\nCreate a new procedure by binding \"<arg> ...\" as arguments to <callable>.\nAliased by <+> if only given non-<associative-collection> callables.";
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      int totalCallables = parameters.size();
      if(totalCallables < 1) 
        throw new Exceptionf("'(bind <callable> <arg> ...) expects at least 1 callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable)) 
        throw new Exceptionf("'(bind <callable> <arg> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(totalCallables == 1) return parameters.get(0);
      Callable p = (Callable)parameters.get(0);
      parameters.remove(0);
      Callable bindPrimitive = new Callable() {
        public String docstring() {
          return "Bound set of arguments to a procedure wrapped up in a single callable.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(Procedure.DEFAULT_NAME),new Symbol("<arg>"),Signature.VARIADIC);
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          ArrayList<Datum> args = new ArrayList<Datum>(parameters);
          args.addAll(params);
          return p.callWith(args,cont);
        }
      };
      return new PrimitiveProcedure(Procedure.DEFAULT_NAME,bindPrimitive);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }
}