// Author: Jordan Randleman - escm.primitive.SymbolPrimitives
// Purpose:
//    Java primitives for symbol procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class SymbolPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // symbol-append
  public static class SymbolAppend extends Primitive {
    public java.lang.String escmName() {
      return "symbol-append";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(symbol-append <symbol> ...) requires at least 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) {
        if(!(p instanceof Symbol))
          throw new Exceptionf("'symbol-append received a non-symbol object %s!", p.profile());
        sb.append(((Symbol)p).value());
      }
      return new Symbol(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symbol?
  public static class IsSymbol extends Primitive {
    public java.lang.String escmName() {
      return "symbol?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(symbol? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Symbol);
    }
  }
}