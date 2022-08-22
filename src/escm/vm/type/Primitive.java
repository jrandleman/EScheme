// Author: Jordan Randleman - escm.vm.type.Primitive
// Purpose:
//    Primitive interface that all Java primitives must implement 
//    (either this or "PrimitiveCallable") to be used as EScheme 
//    procedures.

package escm.vm.type;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;

public interface Primitive extends Serializable {
  public String escmName();
  public Datum callWith(ArrayList<Datum> parameters) throws Exception;
}