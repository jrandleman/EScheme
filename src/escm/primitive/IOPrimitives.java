// Author: Jordan Randleman - escm.primitive.IOPrimitives
// Purpose:
//    Java primitives for I/O procedures.

package escm.primitive;
import java.util.ArrayList;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Boolean;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.type.port.Eof;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;
import escm.vm.runtime.GlobalState;

public class IOPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // pretty-print
  public static class PrettyPrint implements Primitive {
    public java.lang.String escmName() {
      return "pretty-print";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(pretty-print <optional-output-port> <obj>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Datum printed = null;
      OutputPort port = null;
      if(parameters.size() == 2) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof OutputPort))
          throw new Exceptionf("'(pretty-print <optional-output-port> <obj>) 1st arg isn't an output port: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)portDatum;
        printed = parameters.get(1);
      } else {
        port = OutputPort.getCurrent();
        printed = parameters.get(0);
      }
      port.print(printed.pprint());
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(false);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // write
  public static class Write implements Primitive {
    public java.lang.String escmName() {
      return "write";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(write <optional-output-port> <obj>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Datum printed = null;
      OutputPort port = null;
      if(parameters.size() == 2) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof OutputPort))
          throw new Exceptionf("'(write <optional-output-port> <obj>) 1st arg isn't an output port: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)portDatum;
        printed = parameters.get(1);
      } else {
        port = OutputPort.getCurrent();
        printed = parameters.get(0);
      }
      port.print(printed.write());
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(false);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // display
  public static class Display implements Primitive {
    public java.lang.String escmName() {
      return "display";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(display <optional-output-port> <obj>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Datum printed = null;
      OutputPort port = null;
      if(parameters.size() == 2) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof OutputPort))
          throw new Exceptionf("'(display <optional-output-port> <obj>) 1st arg isn't an output port: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)portDatum;
        printed = parameters.get(1);
      } else {
        port = OutputPort.getCurrent();
        printed = parameters.get(0);
      }
      port.print(printed.display());
      if(GlobalState.inREPL && port.isStdout()) {
        if(!(printed instanceof escm.type.String)) {
          GlobalState.setLastPrintedANewline(false);
        } else {
          String str = ((escm.type.String)printed).value();
          GlobalState.setLastPrintedANewline(str.charAt(str.length()-1) == '\n');
        }
      }
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // newline
  public static class Newline implements Primitive {
    public java.lang.String escmName() {
      return "newline";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1) 
        throw new Exceptionf("'(newline <optional-output-port>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      OutputPort port = null;
      if(parameters.size() == 1) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof OutputPort))
          throw new Exceptionf("'(newline <optional-output-port>) arg isn't an output port: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)portDatum;
      } else {
        port = OutputPort.getCurrent();
      }
      port.newline();
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(true);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // read
  public static class Read implements Primitive {
    public java.lang.String escmName() {
      return "read";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1) 
        throw new Exceptionf("'(read <optional-input-port>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      InputPort port = null;
      if(parameters.size() == 1) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof InputPort))
          throw new Exceptionf("'(read <optional-input-port>) arg isn't an input port: %s", Exceptionf.profileArgs(parameters));
        port = (InputPort)portDatum;
      } else {
        port = InputPort.getCurrent();
      }
      Datum readDatum = port.readDatum();
      if(readDatum == null) return escm.type.port.Eof.VALUE; // EOF in a <read> call yields an #eof
      return readDatum;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // read-string
  public static class ReadString implements Primitive {
    public java.lang.String escmName() {
      return "read-string";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(read-string <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      String readString = ((escm.type.String)parameters.get(0)).value().trim();
      if(readString.length() == 0) return escm.type.Void.VALUE; // (read-string "") => <void>
      escm.util.Pair<Datum,Integer> result = escm.vm.Reader.read(readString);
      String restOfString = readString.substring(result.second).trim();
      return new escm.type.Pair(result.first,new escm.type.String(restOfString));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // read-line
  public static class ReadLine implements Primitive {
    public java.lang.String escmName() {
      return "read-line";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1) 
        throw new Exceptionf("'(read-line <optional-input-port>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      InputPort port = null;
      if(parameters.size() == 1) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof InputPort))
          throw new Exceptionf("'(read-line <optional-input-port>) arg isn't an input port: %s", Exceptionf.profileArgs(parameters));
        port = (InputPort)portDatum;
      } else {
        port = InputPort.getCurrent();
      }
      String line = port.readLine();
      if(line == null) return escm.type.port.Eof.VALUE; // EOF in a <read> call yields an #eof
      return new escm.type.String(line);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // eof?
  public static class IsEof implements Primitive {
    public java.lang.String escmName() {
      return "eof?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eof? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Eof);
    }
  }
}