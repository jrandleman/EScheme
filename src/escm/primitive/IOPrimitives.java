// Author: Jordan Randleman - escm.primitive.IOPrimitives
// Purpose:
//    Java primitives for I/O procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.type.port.Eof;
import escm.type.number.Real;
import escm.util.error.Exceptionf;
import escm.vm.Reader;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.callable.Signature;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.GlobalState;

public class IOPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // pretty-print
  public static class PrettyPrint extends Primitive {
    public java.lang.String escmName() {
      return "pretty-print";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("pretty-print"),new Symbol("<obj>")),
        Pair.List(new Symbol("pretty-print"),new Symbol("<output-port>"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPretty-print <obj> to <output-port> in indented, machine-readable form.\n<output-port> defaults to (current-output-port).\nAliased by \"pprint\".\nNote that this will infinitely loop for cyclical structures!";
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
  public static class Write extends Primitive {
    public java.lang.String escmName() {
      return "write";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("write"),new Symbol("<obj>")),
        Pair.List(new Symbol("write"),new Symbol("<output-port>"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint <obj> to <output-port> in machine-readable form.\n<output-port> defaults to (current-output-port).\nNote that this will infinitely loop for cyclical structures!";
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
  public static class Display extends Primitive {
    public java.lang.String escmName() {
      return "display";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("display"),new Symbol("<obj>")),
        Pair.List(new Symbol("display"),new Symbol("<output-port>"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint <obj> to <output-port> in human-readable form.\n<output-port> defaults to (current-output-port).\nNote that this will infinitely loop for cyclical structures!\nAliased by <print>.";
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
  public static class Newline extends Primitive {
    public java.lang.String escmName() {
      return "newline";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("newline")),
        Pair.List(new Symbol("newline"),new Symbol("<output-port>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint a newline to <output-port>. Equivalent to: (display \"\\n\").\n<output-port> defaults to (current-output-port).";
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
  // println
  public static class Println extends Primitive {
    public java.lang.String escmName() {
      return "println";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("println"),new Symbol("<obj>")),
        Pair.List(new Symbol("println"),new Symbol("<output-port>"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint <obj> to <output-port> in human-readable form, followed by a newline.\n<output-port> defaults to (current-output-port).\nNote that this will infinitely loop for cyclical structures!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(println <optional-output-port> <obj>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Datum printed = null;
      OutputPort port = null;
      if(parameters.size() == 2) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof OutputPort))
          throw new Exceptionf("'(println <optional-output-port> <obj>) 1st arg isn't an output port: %s", Exceptionf.profileArgs(parameters));
        port = (OutputPort)portDatum;
        printed = parameters.get(1);
      } else {
        port = OutputPort.getCurrent();
        printed = parameters.get(0);
      }
      port.println(printed.display());
      if(GlobalState.inREPL && port.isStdout()) GlobalState.setLastPrintedANewline(true);
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // pretty-printf
  public static class PrettyPrintf extends Primitive {
    public java.lang.String escmName() {
      return "pretty-printf";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("pretty-printf"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC),
        Pair.List(new Symbol("pretty-printf"),new Symbol("<output-port>"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPretty-print formatted <format-string> with \"<arg> ...\" to <output-port>\nin indented, machine-readable form. <output-port> defaults to\n(current-output-port).\nAliased by \"pprintf\".\nNote that this will infinitely loop for cyclical structures!\n>> <format-string> is like Java's printf with unique formatting patterns:\n   ----------------------------------------------------------------------\n   %a = display anything\n   %wa = write anything\n   %pa = pretty-print anything\n   ----------------------------------------------------------------------\n   %... = display unpacked list/vector/hashmap\n   %w... = write unpacked list/vector/hashmap\n   %p... = pretty-print unpacked list/vector/hashmap\n   ----------------------------------------------------------------------\n   %n = number\n   %+n = number (show sign if positive too)\n   %,n = number with commas\n   %En = %en = number (coerced to exact)\n   %In = %in = number (coerced to inexact)\n   %#rn = %#Rn = number (in radix <#> from 2 to 36)\n   %#n = number (left-padded with 0s to a width of <#> characters)\n   %.#n = number (with <#> digits of precision)\n   -> IE \"%+e2rn\": make exact in binary with sign\n   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!\n            2) Can't have radix with I-coercion or precision!\n   ----------------------------------------------------------------------\n   %$ = display real finite as a dollar value\n   %,$ = display real finite as a dollar value seperated by commas\n   ----------------------------------------------------------------------\n   %s = display string\n   %#s = display string & pad left with # spaces\n   %-#s = display string & pad right with # spaces\n   %ws = write string\n   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)\n   ----------------------------------------------------------------------\n   %b  = bool\n   %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\"\n   ----------------------------------------------------------------------\n   %%  = \"%\" (escapes a \"%\")\n   ----------------------------------------------------------------------";
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
  public static class Writef extends Primitive {
    public java.lang.String escmName() {
      return "writef";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("writef"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC),
        Pair.List(new Symbol("writef"),new Symbol("<output-port>"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint formatted <format-string> with \"<arg> ...\" to <output-port> in\nmachine-readable form. <output-port> defaults to (current-output-port).\nNote that this will infinitely loop for cyclical structures!\n>> <format-string> is like Java's printf with unique formatting patterns:\n   ----------------------------------------------------------------------\n   %a = display anything\n   %wa = write anything\n   %pa = pretty-print anything\n   ----------------------------------------------------------------------\n   %... = display unpacked list/vector/hashmap\n   %w... = write unpacked list/vector/hashmap\n   %p... = pretty-print unpacked list/vector/hashmap\n   ----------------------------------------------------------------------\n   %n = number\n   %+n = number (show sign if positive too)\n   %,n = number with commas\n   %En = %en = number (coerced to exact)\n   %In = %in = number (coerced to inexact)\n   %#rn = %#Rn = number (in radix <#> from 2 to 36)\n   %#n = number (left-padded with 0s to a width of <#> characters)\n   %.#n = number (with <#> digits of precision)\n   -> IE \"%+e2rn\": make exact in binary with sign\n   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!\n            2) Can't have radix with I-coercion or precision!\n   ----------------------------------------------------------------------\n   %$ = display real finite as a dollar value\n   %,$ = display real finite as a dollar value seperated by commas\n   ----------------------------------------------------------------------\n   %s = display string\n   %#s = display string & pad left with # spaces\n   %-#s = display string & pad right with # spaces\n   %ws = write string\n   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)\n   ----------------------------------------------------------------------\n   %b  = bool\n   %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\"\n   ----------------------------------------------------------------------\n   %%  = \"%\" (escapes a \"%\")\n   ----------------------------------------------------------------------";
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
  public static class Displayf extends Primitive {
    public java.lang.String escmName() {
      return "displayf";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("displayf"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC),
        Pair.List(new Symbol("displayf"),new Symbol("<output-port>"),new Symbol("<format-string>"),new Symbol("<arg>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:IO\nPrint formatted <format-string> with \"<arg> ...\" to <output-port> in\nhuman-readable form. <output-port> defaults to (current-output-port).\nNote that this will infinitely loop for cyclical structures!\nAliased by <printf>.\n>> <format-string> is like Java's printf with unique formatting patterns:\n   ----------------------------------------------------------------------\n   %a = display anything\n   %wa = write anything\n   %pa = pretty-print anything\n   ----------------------------------------------------------------------\n   %... = display unpacked list/vector/hashmap\n   %w... = write unpacked list/vector/hashmap\n   %p... = pretty-print unpacked list/vector/hashmap\n   ----------------------------------------------------------------------\n   %n = number\n   %+n = number (show sign if positive too)\n   %,n = number with commas\n   %En = %en = number (coerced to exact)\n   %In = %in = number (coerced to inexact)\n   %#rn = %#Rn = number (in radix <#> from 2 to 36)\n   %#n = number (left-padded with 0s to a width of <#> characters)\n   %.#n = number (with <#> digits of precision)\n   -> IE \"%+e2rn\": make exact in binary with sign\n   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!\n            2) Can't have radix with I-coercion or precision!\n   ----------------------------------------------------------------------\n   %$ = display real finite as a dollar value\n   %,$ = display real finite as a dollar value seperated by commas\n   ----------------------------------------------------------------------\n   %s = display string\n   %#s = display string & pad left with # spaces\n   %-#s = display string & pad right with # spaces\n   %ws = write string\n   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)\n   ----------------------------------------------------------------------\n   %b  = bool\n   %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\"\n   ----------------------------------------------------------------------\n   %%  = \"%\" (escapes a \"%\")\n   ----------------------------------------------------------------------";
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
  public static class Read extends Primitive {
    public java.lang.String escmName() {
      return "read";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("read")),
        Pair.List(new Symbol("read"),new Symbol("<input-port>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nRead an EScheme datum from <input-port>.\n<input-port> defaults to (current-input-port).\nReturns #eof once reached <input-port>'s end.";
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
  public static class ReadString extends Primitive {
    public java.lang.String escmName() {
      return "read-string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("read-string"),new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:IO\nRead an EScheme datum from the string. Returns a list:\n  (<read-datum> <string-w/o-read-datum>)";
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
      return escm.type.Pair.List(result.first,new escm.type.String(restOfString));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // read-line
  public static class ReadLine extends Primitive {
    public java.lang.String escmName() {
      return "read-line";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("read-line")),
        Pair.List(new Symbol("read-line"),new Symbol("<input-port>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nRead a line of text as a string from <input-port>.\n<input-port> defaults to (current-input-port).\nReturns #eof once reached <input-port>'s end.";
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
  // read-char
  public static class ReadChar extends Primitive {
    public java.lang.String escmName() {
      return "read-char";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("read-char")),
        Pair.List(new Symbol("read-char"),new Symbol("<input-port>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nRead a char from <input-port>.\n<input-port> defaults to (current-input-port).\nReturns #eof once reached <input-port>'s end.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1) 
        throw new Exceptionf("'(read-char <optional-input-port>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      InputPort port = null;
      if(parameters.size() == 1) {
        Datum portDatum = parameters.get(0);
        if(!(portDatum instanceof InputPort))
          throw new Exceptionf("'(read-char <optional-input-port>) arg isn't an input port: %s", Exceptionf.profileArgs(parameters));
        port = (InputPort)portDatum;
      } else {
        port = InputPort.getCurrent();
      }
      Integer codepoint = port.readCharacter();
      if(codepoint == null) return escm.type.port.Eof.VALUE; // EOF in a <read> call yields an #eof
      return new escm.type.Character(codepoint.intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // read-chars
  public static class ReadChars extends Primitive {
    public java.lang.String escmName() {
      return "read-chars";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("read-chars"),new Symbol("<integer>")),
        Pair.List(new Symbol("read-chars"),new Symbol("<input-port>"),new Symbol("<integer>")));
    }

    public String docstring() {
      return "@help:Procedures:IO\nRead <integer> chars as a string from <input-port>.\n<input-port> defaults to (current-input-port).\nReturns #eof once reached <input-port>'s end.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 1 || n > 2) 
        throw new Exceptionf("'(read-chars <optional-input-port> <integer>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Datum intArg = null;
      Datum portArg = null;
      if(n == 1) {
        portArg = InputPort.getCurrent();
        intArg = parameters.get(0);
      } else { // n == 2
        portArg = parameters.get(0);
        intArg = parameters.get(1);
      }
      if(!ListPrimitives.isValidSize(intArg))
        throw new Exceptionf("'(read-chars <optional-input-port> <integer>) <integer> isn't a non-negative integer: %s", Exceptionf.profileArgs(parameters));
      if(!(portArg instanceof InputPort))
          throw new Exceptionf("'(read-chars <input-port> <integer>) 1st arg isn't an input port: %s", Exceptionf.profileArgs(parameters));
      String chars = ((InputPort)portArg).readCharacters(((Real)intArg).intValue());
      if(chars == null) return escm.type.port.Eof.VALUE; // EOF in a <read> call yields an #eof
      return new escm.type.String(chars);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // eof?
  public static class IsEof extends Primitive {
    public java.lang.String escmName() {
      return "eof?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("eof?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:IO\nReturns whether <obj> is the #eof object.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eof? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Eof);
    }
  }
}