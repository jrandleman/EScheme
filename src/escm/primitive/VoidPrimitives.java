// Author: Jordan Randleman - escm.primitive.VoidPrimitives
// Purpose:
//    Java primitives for #<void> operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Boolean;
import escm.type.Void;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class VoidPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // void?
  public static class IsVoid implements Primitive {
    public java.lang.String escmName() {
      return "void?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(void? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Void);
    }
  }
}