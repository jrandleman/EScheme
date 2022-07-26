// Author: Jordan Randleman - escm.primitive.EqualityPrimitives
// Purpose:
//    Java primitives for equality procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class EqualityPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // eq?
  public static class IsEq implements Primitive {
    public java.lang.String escmName() {
      return "eq?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 0) throw new Exceptionf("'(eq? <obj> ...) expects at least 1 argument: %s", Exceptionf.profileArgs(parameters));
      for(int i = 0; i < n-1; ++i)
        if(!parameters.get(i).eq(parameters.get(i+1)))
          return Boolean.FALSE;
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // equal?
  public static class IsEqual implements Primitive {
    public java.lang.String escmName() {
      return "equal?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 0) throw new Exceptionf("'(equal? <obj> ...) expects at least 1 argument: %s", Exceptionf.profileArgs(parameters));
      for(int i = 0; i < n-1; ++i)
        if(!parameters.get(i).equal(parameters.get(i+1)))
          return Boolean.FALSE;
      return Boolean.TRUE;
    }
  }
}