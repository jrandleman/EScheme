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
import escm.util.Exceptionf;
import escm.vm.type.Primitive;
import escm.vm.runtime.GlobalState;

public class IOPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // write
  public static class Write implements Primitive {
    public java.lang.String escmName() {
      return "write";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(write <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      System.out.print(parameters.get(0).write());
      if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(false);
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
      if(parameters.size() != 1) 
        throw new Exceptionf("'(display <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      System.out.print(parameters.get(0).display());
      if(GlobalState.inREPL) {
        if(!(parameters.get(0) instanceof escm.type.String)) {
          GlobalState.setLastPrintedANewline(false);
        } else {
          String str = ((escm.type.String)parameters.get(0)).value();
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
      if(parameters.size() != 0) 
        throw new Exceptionf("'(newline) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      System.out.println("");
      if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(true);
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
      if(parameters.size() != 0) 
        throw new Exceptionf("'(read) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      Datum readDatum = escm.vm.Main.read(new BufferedReader(new InputStreamReader(System.in)));
      if(readDatum == null) return escm.type.Void.VALUE; // EOF in a <read> call yields a <void> object
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
}