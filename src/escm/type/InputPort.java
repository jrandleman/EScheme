// Author: Jordan Randleman - escm.type.InputPort
// Purpose:
//    Input port primitive type, to read from a file/stdin.
//
//    Provides:
//      - static STDIN [System.in]
//
//      - static InputPort getCurrent()
//      - static void setCurrent(InputPort o)
//
//      - long skip(long n)
//
//      - void mark(int readAheadLimit)
//      - boolean reset()
//
//      - boolean ready()
//
//      - String readCharacter()       // returns <null> if reached EOF
//      - String readCharacters(int n) // returns <null> if reached EOF
//      - String readLine()            // returns <null> if reached EOF
//      - Datum readDatum()            // returns <null> if reached EOF
//      - Datum readReplDatum()        // returns <null> if reached EOF

package escm.type;
import java.util.Objects;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileReader;
import escm.util.Exceptionf;
import escm.vm.Reader;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmThread;

public class InputPort extends Port {
  ////////////////////////////////////////////////////////////////////////////
  // Internal reader value
  private BufferedReader br = null;


  ////////////////////////////////////////////////////////////////////////////
  // Name
  private java.lang.String name = null;

  public java.lang.String sourceName() {
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static STDIN field
  private InputPort() {
    br = new BufferedReader(new InputStreamReader(System.in));
    name = "System.in";
  }


  public static final InputPort STDIN = new InputPort();


  ////////////////////////////////////////////////////////////////////////////
  // Handle the Current Input Port
  public static InputPort getCurrent() {
    return ((EscmThread)java.lang.Thread.currentThread()).currentInputPort;
  }

  public static void setCurrent(InputPort o) {
    ((EscmThread)java.lang.Thread.currentThread()).currentInputPort = o;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public InputPort(java.lang.String filename) throws Exception {
    try {
      br = new BufferedReader(new FileReader(filename));
      name = filename;
    } catch(Exception e) {
      throw new Exceptionf("Can't open port \"%s\" for input: %s", filename, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether can Read
  public boolean ready() throws Exception {
    try {
      return br.ready();
    } catch(Exception e) {
      throw new Exceptionf("Can't check if port \"%s\" is ready: %s", name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Readers
  private static void printReplPrompt() {
    if(!GlobalState.getLastPrintedANewline()) System.out.println("");
    System.out.print("> ");
    if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(false); // from the newline input by the user's <enter>/<return> key stroke
  }


  public java.lang.String readCharacter() throws Exception {
    try {
      int c = br.read();
      if(c == -1) return null;
      return new java.lang.String(new char[]{(char)c});
    } catch(Exception e) {
      throw new Exceptionf("Can't read a char from port \"%s\": %s", name, e);
    }
  }


  public java.lang.String readCharacters(int n) throws Exception {
    if(n == 0) return "";
    if(n < 0) {
      throw new Exceptionf("Can't read chars from port \"%s\" with negative char-count %d!", name, n);
    }
    try {
      char[] chars = new char[n];
      int readCount = br.read(chars,0,n);
      if(readCount == -1) return null;
      return new java.lang.String(chars);
    } catch(Exception e) {
      throw new Exceptionf("Can't read chars from port \"%s\": %s", name, e);
    }
  }


  public java.lang.String readLine() throws Exception {
    try {
      return br.readLine();
    } catch(Exception e) {
      throw new Exceptionf("Can't read line from port \"%s\": %s", name, e);
    }
  }


  public Datum readDatum() throws Exception {
    StringBuilder sb = new StringBuilder();
    while(true) {
      try {
        java.lang.String input = br.readLine();
        if(input == null) return null;
        if(input.length() == 0) continue;
        sb.append(input);
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString());
        if(this == STDIN) GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
        if(result.first instanceof Eof) return null; // only reading #eof is equivalent to typing ^D
        return result.first;
      } catch(Reader.IncompleteException e) {
        continue;
      }
    }
  }


  public Datum readReplDatum() throws Exception {
    printReplPrompt();
    StringBuilder sb = new StringBuilder();
    while(true) {
      try {
        java.lang.String input = br.readLine();
        if(input == null) { // EOF detected
          if(sb.length() == 0) {
            GlobalState.setLastPrintedANewline(false);
            return null;
          }
          sb = new StringBuilder();
          printReplPrompt();
          continue;
        }
        if(input.length() == 0) continue;
        sb.append(input);
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString());
        GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
        if(result.first instanceof Eof) return null; // only reading #eof is equivalent to typing ^D
        return result.first;
      } catch(Reader.IncompleteException e) {
        continue;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Skip past N Characters
  public long skip(long n) throws Exception {
    return br.skip(n);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Marker Support
  public void mark(int readAheadLimit) throws Exception {
    br.mark(readAheadLimit);
  }


  // Returns whether succeeded
  public boolean reset() {
    try {
      br.reset();
      return true;
    } catch(Exception e) {
      return false;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public void close() throws Exception {
    br.close();
  }


  public boolean isClosed() {
    try {
      br.ready();
      return true;
    } catch(Exception e) {
      return false;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "input-port";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof InputPort && ((InputPort)o).br.equals(br);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),name,br);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<input-port ["+name+"]>";
  }
}