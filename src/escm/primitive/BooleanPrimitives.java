// Author: Jordan Randleman - escm.primitive.BooleanPrimitives
// Purpose:
//    Java primitives for boolean operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class BooleanPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // boolean?
  public static class IsBoolean extends Primitive {
    public java.lang.String escmName() {
      return "boolean?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("boolean?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(boolean? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Boolean);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // not
  public static class Not extends Primitive {
    public java.lang.String escmName() {
      return "not";
    }

    public Datum signature() {
      return Pair.List(new Symbol("not"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(not <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(!parameters.get(0).isTruthy());
    }
  }
}