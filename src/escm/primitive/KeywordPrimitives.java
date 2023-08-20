// Author: Jordan Randleman - escm.primitive.KeywordPrimitives
// Purpose:
//    Java primitives for keyword procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.Primitive;

public class KeywordPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // keyword?
  public static class IsKeyword extends Primitive {
    public java.lang.String escmName() {
      return "keyword?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(keyword? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Keyword);
    }
  }
}