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
import escm.type.bool.Boolean;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.type.port.Eof;
import escm.util.Exceptionf;
import escm.vm.Reader;
import escm.vm.type.Primitive;
import escm.vm.util.SourceInformation;
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
          if(str.length() == 0) {
            GlobalState.setLastPrintedANewline(false);
          } else {
            GlobalState.setLastPrintedANewline(str.charAt(str.length()-1) == '\n');
          }
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
  // pretty-printf
  public static class PrettyPrintf implements Primitive {
    public java.lang.String escmName() {
      return "pretty-printf";
    }

    public static ArrayList<Datum> getStringfArgs(ArrayList<Datum> parameters, int argStartIndex) {
      ArrayList<Datum> args = new ArrayList<Datum>();
      for(int n = parameters.size(); argStartIndex < n; ++argStartIndex)
        args.add(parameters.get(argStartIndex));
      return args;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(pretty-printf <optional-output-port> <format-string> <arg> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      String printed = null;
      OutputPort port = null;
      if(parameters.get(0) instanceof OutputPort) {
        if(parameters.size() == 1)
          throw new Exceptionf("'(pretty-printf <optional-output-port> <format-string> <arg> ...) missing <format-string>: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String))
          throw new Exceptionf("'(pretty-printf <optional-output-port> <format-string> <arg> ...) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)parameters.get(0);
        String formatString = ((escm.type.String)parameters.get(1)).value();
        ArrayList<Datum> args = getStringfArgs(parameters,2);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(pretty-printf <optional-output-port> <format-string> <arg> ...)");
      } else {
        if(!(parameters.get(0) instanceof escm.type.String))
          throw new Exceptionf("'(pretty-printf <optional-output-port> <format-string> <arg> ...) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = OutputPort.getCurrent();
        String formatString = ((escm.type.String)parameters.get(0)).value();
        ArrayList<Datum> args = getStringfArgs(parameters,1);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(pretty-printf <optional-output-port> <format-string> <arg> ...)");
      }
      port.print((new escm.type.String(printed)).pprint());
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(false);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // writef
  public static class Writef implements Primitive {
    public java.lang.String escmName() {
      return "writef";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(writef <optional-output-port> <format-string> <arg> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      String printed = null;
      OutputPort port = null;
      if(parameters.get(0) instanceof OutputPort) {
        if(parameters.size() == 1)
          throw new Exceptionf("'(writef <optional-output-port> <format-string> <arg> ...) missing <format-string>: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String))
          throw new Exceptionf("'(writef <optional-output-port> <format-string> <arg> ...) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)parameters.get(0);
        String formatString = ((escm.type.String)parameters.get(1)).value();
        ArrayList<Datum> args = PrettyPrintf.getStringfArgs(parameters,2);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(writef <optional-output-port> <format-string> <arg> ...)");
      } else {
        if(!(parameters.get(0) instanceof escm.type.String))
          throw new Exceptionf("'(writef <optional-output-port> <format-string> <arg> ...) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = OutputPort.getCurrent();
        String formatString = ((escm.type.String)parameters.get(0)).value();
        ArrayList<Datum> args = PrettyPrintf.getStringfArgs(parameters,1);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(writef <optional-output-port> <format-string> <arg> ...)");
      }
      port.print((new escm.type.String(printed)).write());
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(false);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // displayf
  public static class Displayf implements Primitive {
    public java.lang.String escmName() {
      return "displayf";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(displayf <optional-output-port> <format-string> <arg> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      String printed = null;
      OutputPort port = null;
      if(parameters.get(0) instanceof OutputPort) {
        if(parameters.size() == 1)
          throw new Exceptionf("'(displayf <optional-output-port> <format-string> <arg> ...) missing <format-string>: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String))
          throw new Exceptionf("'(displayf <optional-output-port> <format-string> <arg> ...) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)parameters.get(0);
        String formatString = ((escm.type.String)parameters.get(1)).value();
        ArrayList<Datum> args = PrettyPrintf.getStringfArgs(parameters,2);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(displayf <optional-output-port> <format-string> <arg> ...)");
      } else {
        if(!(parameters.get(0) instanceof escm.type.String))
          throw new Exceptionf("'(displayf <optional-output-port> <format-string> <arg> ...) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
        port = OutputPort.getCurrent();
        String formatString = ((escm.type.String)parameters.get(0)).value();
        ArrayList<Datum> args = PrettyPrintf.getStringfArgs(parameters,1);
        printed = FormatPrimitives.Stringf.logic(formatString,args,"(displayf <optional-output-port> <format-string> <arg> ...)");
      }
      port.print((new escm.type.String(printed)).display());
      if(GlobalState.inREPL && port.isStdout()) {
        if(printed.length() == 0) {
          GlobalState.setLastPrintedANewline(false);
        } else {
          GlobalState.setLastPrintedANewline(printed.charAt(printed.length()-1) == '\n');
        }
      }
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

    private static SourceInformation createPseudoSourceInformation(escm.type.String string) {
      StringBuilder sb = new StringBuilder("#<string ");
      sb.append(string.write());
      sb.append('>');
      return new SourceInformation(sb.toString(),1,1);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(read-string <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      escm.type.String str = (escm.type.String)parameters.get(0);
      String readString = str.value().trim();
      if(readString.length() == 0) return escm.type.Void.VALUE; // (read-string "") => <void>
      escm.util.Pair<Datum,Integer> result = Reader.read(readString,createPseudoSourceInformation(str),Reader.GIVE_DETAILED_INCOMPLETE_ERRORS);
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