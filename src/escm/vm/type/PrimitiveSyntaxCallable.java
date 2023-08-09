// Author: Jordan Randleman - escm.vm.type.PrimitiveSyntaxCallable
// Purpose:
//    Primitive abstract class that all Java syntax primitives must extend
//    (either this or "PrimitiveSyntax") to be used as EScheme macros.

package escm.vm.type;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.util.Trampoline;
import escm.vm.util.Environment;

public abstract class PrimitiveSyntaxCallable implements Callable, Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception;
}