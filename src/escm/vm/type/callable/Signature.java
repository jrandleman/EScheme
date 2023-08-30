// Author: Jordan Randleman - escm.vm.type.callable.Signature
// Purpose:
//    Primitive interface for callables to enable introspection 
//    on their signatures.

package escm.vm.type.callable;
import java.io.Serializable;
import escm.type.Datum;
import escm.type.Symbol;

public interface Signature extends Serializable {
  public static final Symbol VARIADIC = new Symbol("...");
  public Datum signature();
}