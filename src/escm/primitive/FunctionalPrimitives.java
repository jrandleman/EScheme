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
import escm.vm.type.AssociativeCollection;

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

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(callable? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum o = parameters.get(0);
      if(o instanceof EscmObject) return Boolean.valueOf(((EscmObject)o).isFunctor());
      return Boolean.valueOf(o instanceof Callable);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // compose
  public static class Compose extends Primitive {
    public java.lang.String escmName() {
      return "compose";
    }

    public static Datum convertArrayListToList(ArrayList<Datum> vals) {
      Datum lis = Nil.VALUE;
      for(int i = vals.size()-1; i >= 0; --i) 
        lis = new Pair(vals.get(i),lis);
      return lis;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(compose <callable> <callable> ...) expects at least 2 callable args: %s", Exceptionf.profileArgs(parameters));
      for(Datum param : parameters)
        if(!(param instanceof Callable))
          throw new Exceptionf("'(compose <callable> <callable> ...) arg %s isn't a callable: %s", param.profile(), Exceptionf.profileArgs(parameters));
      ArrayList<Datum> parametersCopy = new ArrayList<Datum>(parameters);
      Collections.reverse(parametersCopy);
      Callable fstFcn = (Callable)parametersCopy.get(0);
      parametersCopy.remove(0);
      Callable composedProcedures = (params, cont) -> {
        Callable foldPrimitive = (foldLambdaParams, foldLambdaCont) -> {
          Callable p = (Callable)foldLambdaParams.get(1);
          foldLambdaParams.remove(1);
          return p.callWith(foldLambdaParams,foldLambdaCont);
        };
        PrimitiveProcedure foldProcedure = new PrimitiveProcedure(Procedure.DEFAULT_NAME,foldPrimitive);
        AssociativeCollection[] lis = new AssociativeCollection[]{(AssociativeCollection)convertArrayListToList(parametersCopy)};
        return fstFcn.callWith(params,(seed) -> () -> {
          return AssociativeCollectionPrimitives.Fold.logic(foldProcedure,seed,lis,cont);
        });
      };
      return new PrimitiveProcedure(Procedure.DEFAULT_NAME,composedProcedures);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // bind
  public static class Bind extends Primitive {
    public java.lang.String escmName() {
      return "bind";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(bind <callable> <arg> ...) expects at least 1 callable & 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable)) 
        throw new Exceptionf("'(bind <callable> <arg> ...) 1st arg %s isn't a callable: %s", parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
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
  }
}