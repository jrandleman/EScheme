// Author: Jordan Randleman - escm.vm.type.PrimitiveCallable
// Purpose:
//    Primitive abstract class that all Java primitives must extend 
//    (either this or "Primitive") to be used as EScheme procedures.

package escm.vm.type;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.util.Trampoline;
import escm.vm.util.Environment;

public abstract class PrimitiveCallable implements Callable, Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception;
}