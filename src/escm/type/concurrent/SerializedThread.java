// Author: Jordan Randleman - escm.type.concurrent.SerializedThread
// Purpose:
//    Serialized Thread state container. Stores the thread information 
//    of its "pre-run" state, namely its name & runnable callable. 
//
//    No state information is preserved of threads mid-run, since the 
//    concept of a mid-execution sub-process doesn't transfer between 
//    machines.

package escm.type.concurrent;
import java.io.Serializable;
import escm.type.Datum;

public class SerializedThread implements Serializable {
  ////////////////////////////////////////////////////////////////////////////
  // Private State Information
  private String name = null;
  private Datum runnable = null;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public SerializedThread(String name, Datum runnable) {
    this.name = name;
    this.runnable = runnable;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization READ Semantics
  private Object readResolve() throws Exception {
    return new escm.type.concurrent.Thread(name,runnable);
  }
}