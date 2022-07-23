// Author: Jordan Randleman - escm.primitive.KeywordPrimitives
// Purpose:
//    Java primitives for keyword procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Boolean;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class KeywordPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // keyword-append
  public static class KeywordAppend implements Primitive {
    public java.lang.String escmName() {
      return "keyword-append";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(keyword-append <keyword> ...) requires at least 1 keyword arg: %s", Exceptionf.profileArgs(parameters));
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) {
        if(!(p instanceof Keyword))
          throw new Exceptionf("'(keyword-append <keyword> ...) received a non-keyword object %s!", p.profile());
        sb.append(((Keyword)p).value().substring(1));
      }
      return new Keyword(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // keyword?
  public static class IsKeyword implements Primitive {
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