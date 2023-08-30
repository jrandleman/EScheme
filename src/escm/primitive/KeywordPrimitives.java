// Author: Jordan Randleman - escm.primitive.KeywordPrimitives
// Purpose:
//    Java primitives for keyword procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class KeywordPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // keyword?
  public static class IsKeyword extends Primitive {
    public java.lang.String escmName() {
      return "keyword?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("keyword?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a keyword.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(keyword? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Keyword);
    }
  }
}