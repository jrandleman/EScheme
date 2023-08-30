// Author: Jordan Randleman - escm.vm.type.primitive.PrimitiveSyntaxCallable
// Purpose:
//    Primitive abstract class that all Java syntax primitives must extend
//    (either this or "PrimitiveSyntax") to be used as EScheme macros.

package escm.vm.type.primitive;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.util.Trampoline;
import escm.vm.util.Environment;
import escm.vm.type.callable.Callable;

public abstract class PrimitiveSyntaxCallable implements Callable, Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract String docstring();
  public abstract Datum signature();
  public abstract Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception;
}