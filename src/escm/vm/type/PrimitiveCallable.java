// Author: Jordan Randleman - escm.vm.type.PrimitiveCallable
// Purpose:
//    Primitive interface that all Java primitives must implement 
//    (either this or "Primitive") to be used as EScheme procedures.

package escm.vm.type;
import java.util.ArrayList;
import escm.type.Datum;
import escm.util.Trampoline;

public interface PrimitiveCallable extends Callable {
  public String escmName();
  public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception;
}