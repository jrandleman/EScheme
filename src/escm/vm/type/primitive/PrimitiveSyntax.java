// Author: Jordan Randleman - escm.vm.type.primitive.PrimitiveSyntax
// Purpose:
//    Primitive abstract class that all Java syntax primitives must extend
//    (either this or "PrimitiveSyntaxCallable") to be used as EScheme macros.

package escm.vm.type.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.vm.util.Environment;
import escm.vm.type.callable.DocString;
import escm.vm.type.callable.Signature;

public abstract class PrimitiveSyntax implements DocString, Signature {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract String docstring();
  public abstract Datum signature();
  public abstract Datum callWith(ArrayList<Datum> parameters) throws Exception;
}