// Author: Jordan Randleman - escm.primitive.FunctionalPrimitives
// Purpose:
//    Java primitives for functional programming support.

package escm.primitive;
import java.util.ArrayList;
import java.util.Collections;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.bool.Boolean;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.type.oo.EscmObject;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;

public class FunctionalPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // id
  public static class Id extends Primitive {
    public java.lang.String escmName() {
      return "id";
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
      Callable composedProcedures = (params, cont) -> {
        return fstFcn.callWith(params,(result) -> () -> {
          return applyComposedCallables(result,totalCallables-2,parameters,cont);
        });
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

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      int totalCallables = parameters.size();
      if(totalCallables < 1) 
        throw new Exceptionf("'(bind <callable> <arg> ...) expects at least 1 callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable)) 
        throw new Exceptionf("'(bind <callable> <arg> ...) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(totalCallables == 1) return parameters.get(0);
      ArrayList<Datum> parametersCopy = new ArrayList<Datum>(parameters);
      Callable p = (Callable)parametersCopy.get(0);
      parametersCopy.remove(0);
      Callable bindPrimitive = (params, cont) -> {
        ArrayList<Datum> args = new ArrayList<Datum>(parametersCopy);
        args.addAll(params);
        return p.callWith(args,cont);
      };
      return new PrimitiveProcedure(Procedure.DEFAULT_NAME,bindPrimitive);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }
}