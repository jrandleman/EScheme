// Author: Jordan Randleman - escm.primitive.BooleanPrimitives
// Purpose:
//    Java primitives for boolean operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Boolean;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class BooleanPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // boolean?
  public static class IsBoolean implements Primitive {
    public java.lang.String escmName() {
      return "boolean?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(boolean? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Boolean);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // not
  public static class Not implements Primitive {
    public java.lang.String escmName() {
      return "not";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(not <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(!parameters.get(0).isTruthy());
    }
  }
}