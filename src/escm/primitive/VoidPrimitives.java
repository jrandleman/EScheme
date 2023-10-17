// Author: Jordan Randleman - escm.primitive.VoidPrimitives
// Purpose:
//    Java primitives for #<void> operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.Void;
import escm.type.Pair;
import escm.type.Symbol;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class VoidPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // void?
  public static class IsVoid extends Primitive {
    public java.lang.String escmName() {
      return "void?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("void?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Void\nReturns whether <obj> has type #<void>.\nReturned by mutative actions like <set!>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(void? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Void);
    }
  }
}