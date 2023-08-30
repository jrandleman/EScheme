// Author: Jordan Randleman - escm.vm.type.callable.DocString
// Purpose:
//    Primitive interface for callables to enable introspection 
//    on their associated documentation.

package escm.vm.type.callable;
import java.io.Serializable;

public interface DocString extends Serializable {
  public String docstring();
}