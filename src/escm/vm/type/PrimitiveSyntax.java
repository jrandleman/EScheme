// Author: Jordan Randleman - escm.vm.type.PrimitiveSyntax
// Purpose:
//    Primitive abstract class that all Java syntax primitives must extend
//    (either this or "PrimitiveSyntaxCallable") to be used as EScheme macros.

package escm.vm.type;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.vm.util.Environment;

public abstract class PrimitiveSyntax implements Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract Datum callWith(ArrayList<Datum> parameters) throws Exception;
}