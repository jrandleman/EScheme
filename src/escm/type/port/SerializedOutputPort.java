// Author: Jordan Randleman - escm.type.port.SerializedOutputPort
// Purpose:
//    Serialized OutputPort state container. Stores the absolute file path
//    associated with the source OutputPort.
//
//    Deserialized OutputPort objects are always initialized in "write" mode.

package escm.type.port;
import java.io.Serializable;

public class SerializedOutputPort implements Serializable {
  ////////////////////////////////////////////////////////////////////////////
  // Private State Information
  private String path = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public SerializedOutputPort(String path) {
    this.path = path;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization READ Semantics
  private Object readResolve() throws Exception {
    if(path.equals(OutputPort.STDOUT.sourceName())) {
      return OutputPort.STDOUT;
    } else {
      return new OutputPort(path,false);
    }
  }
}