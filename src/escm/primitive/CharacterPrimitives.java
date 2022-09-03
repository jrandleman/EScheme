// Author: Jordan Randleman - escm.primitive.CharacterPrimitives
// Purpose:
//    Java primitives for char operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class CharacterPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // char?
  public static class IsChar implements Primitive {
    public java.lang.String escmName() {
      return "char?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(char? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Character);
    }
  }
}