// Author: Jordan Randleman - escm.primitive.EqualityPrimitives
// Purpose:
//    Java primitives for equality procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.Primitive;

public class EqualityPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // eq?
  public static class IsEq extends Primitive {
    public java.lang.String escmName() {
      return "eq?";
    }

    public static Datum logic(ArrayList<Datum> parameters) {
      for(int i = 0, n = parameters.size()-1; i < n; ++i)
        if(!parameters.get(i).eq(parameters.get(i+1)))
          return Boolean.FALSE;
      return Boolean.TRUE;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(eq? <obj> ...) expects at least 1 argument: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // equal?
  public static class IsEqual extends Primitive {
    public java.lang.String escmName() {
      return "equal?";
    }

    public static Datum logic(ArrayList<Datum> parameters) {
      for(int i = 0, n = parameters.size()-1; i < n; ++i)
        if(!parameters.get(i).equal(parameters.get(i+1)))
          return Boolean.FALSE;
      return Boolean.TRUE;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(equal? <obj> ...) expects at least 1 argument: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }
}