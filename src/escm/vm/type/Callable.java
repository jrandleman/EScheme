// Author: Jordan Randleman - escm.vm.type.Callable
// Purpose:
//    Primitive interface that all callable escm objects must implement
//    to be used as EScheme procedures.

package escm.vm.type;
import java.util.ArrayList;
import escm.type.Datum;
import escm.util.Trampoline;

public interface Callable {
  public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception;
}