// Author: Jordan Randleman - escm.primitive.SymbolPrimitives
// Purpose:
//    Java primitives for symbol procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class SymbolPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // symbol?
  public static class IsSymbol extends Primitive {
    public java.lang.String escmName() {
      return "symbol?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("symbol?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a symbol.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(symbol? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Symbol);
    }
  }
}