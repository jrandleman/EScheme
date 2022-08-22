// Author: Jordan Randleman - escm.type.port.SerializedInputPort
// Purpose:
//    Serialized InputPort state container. Stores the absolute file path
//    associated with the source InputPort.
//
//    Deserialized InputPort objects are always initialized to the start
//    of the file.

package escm.type.port;
import java.io.Serializable;

public class SerializedInputPort implements Serializable {
  ////////////////////////////////////////////////////////////////////////////
  // Private State Information
  private String path = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public SerializedInputPort(String path) {
    this.path = path;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization READ Semantics
  private Object readResolve() throws Exception {
    if(path.equals(InputPort.STDIN.sourceName())) {
      return InputPort.STDIN;
    } else {
      return new InputPort(path);
    }
  }
}