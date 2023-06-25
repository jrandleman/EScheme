// Author: Jordan Randleman - escm.vm.type.Primitive
// Purpose:
//    Primitive abstract class that all Java primitives must extend
//    (either this or "PrimitiveCallable") to be used as EScheme 
//    procedures.

package escm.vm.type;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.vm.util.Environment;

public abstract class Primitive implements Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract Datum callWith(ArrayList<Datum> parameters) throws Exception;
}