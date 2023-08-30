// Author: Jordan Randleman - escm.vm.type.primitive.Primitive
// Purpose:
//    Primitive abstract class that all Java primitives must extend
//    (either this or "PrimitiveCallable") to be used as EScheme 
//    procedures.

package escm.vm.type.primitive;
import java.util.ArrayList;
import java.io.Serializable;
import escm.type.Datum;
import escm.vm.util.Environment;
import escm.vm.type.callable.DocString;
import escm.vm.type.callable.Signature;

public abstract class Primitive implements DocString, Signature, Serializable {
  public Environment definitionEnvironment;
  public abstract String escmName();
  public abstract String docstring();
  public abstract Datum signature();
  public abstract Datum callWith(ArrayList<Datum> parameters) throws Exception;
}