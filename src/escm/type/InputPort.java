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
//      - boolean isStdin()
//
//      - long skip(long n)
//
//      - boolean ready()
//
//      - String peek()                // returns <null> if reached EOF
//      - String readCharacter()       // returns <null> if reached EOF
//      - String readCharacters(int n) // returns <null> if reached EOF
//      - String readLine()            // returns <null> if reached EOF
//      - Datum readDatum()            // returns <null> if reached EOF
//      - Datum readReplDatum()        // returns <null> if reached EOF

package escm.type;
import java.util.Objects;
import java.io.PushbackReader;
import java.io.InputStreamReader;
import java.io.FileReader;
import escm.util.Exceptionf;
import escm.vm.Reader;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmThread;

public class InputPort extends Port {
  ////////////////////////////////////////////////////////////////////////////
  // Internal reader value
  private PushbackReader pr = null;


  ////////////////////////////////////////////////////////////////////////////
  // Name
  private java.lang.String name = null;

  public java.lang.String sourceName() {
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static STDIN field
  private InputPort() {
    pr = new PushbackReader(new InputStreamReader(System.in));
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
      pr = new PushbackReader(new FileReader(filename));
      name = filename;
    } catch(Exception e) {
      throw new Exceptionf("Can't open port \"%s\" for input: %s", filename, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether Reading from STDIN
  public boolean isStdin() {
    return this == STDIN;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether can Read
  public boolean ready() throws Exception {
    try {
      return pr.ready();
    } catch(Exception e) {
      throw new Exceptionf("Can't check if port \"%s\" is ready: %s", name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Peek Logic
  public java.lang.String peek() throws Exception {
    try {
      int c = pr.read();
      pr.unread(c);
      if(c == -1) return null;
      return new java.lang.String(new char[]{(char)c});
    } catch(Exception e) {
      throw new Exceptionf("Can't peek port \"%s\": %s", name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Various Readers
  private static void printReplPrompt() {
    if(!GlobalState.getLastPrintedANewline()) System.out.println("");
    System.out.print("> ");
    if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(false); // from the newline input by the user's <enter>/<return> key stroke
  }


  public java.lang.String readCharacter() throws Exception {
    try {
      int c = pr.read();
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
      int readCount = pr.read(chars,0,n);
      if(readCount == -1) return null;
      return new java.lang.String(chars);
    } catch(Exception e) {
      throw new Exceptionf("Can't read chars from port \"%s\": %s", name, e);
    }
  }


  public java.lang.String readLine() throws Exception {
    try {
      StringBuilder sb = new StringBuilder();
      int c = pr.read();
      while(c != '\n' && c != -1) {
        sb.append((char)c);
        c = pr.read();
      }
      if(c == -1) return null;
      sb.append((char)c);
      return sb.toString();
    } catch(Exception e) {
      throw new Exceptionf("Can't read line from port \"%s\": %s", name, e);
    }
  }


  public Datum readReplDatum() throws Exception {
    printReplPrompt();
    StringBuilder sb = new StringBuilder();
    while(true) {
      try {
        java.lang.String input = readLine();
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
  // Datum Readers
  public Datum readDatum() throws Exception {
    StringBuilder sb = new StringBuilder();
    while(true) {
      try {
        int parenCount = 0;
        boolean foundLoneAtom = false;
        while(true) {
          int input = pr.read();
          // check for atom
          if(foundLoneAtom && parenCount == 0 && (input == -1 || Reader.isDelimiter((char)input))) {
            if(input != -1) pr.unread(input);
            break;
          }
          if(input == -1) return null;
          // register open paren
          if(input == '(') {
            sb.append((char)input);
            ++parenCount;
          // register close paren
          } else if(input == ')') {
            sb.append((char)input);
            --parenCount;
            if(parenCount <= 0) break;
          // account for whitespace
          } else if(Character.isWhitespace((char)input)) {
            sb.append((char)input);
          // skip comment
          } else if(input == ';') {
            input = pr.read();
            while(input != '\n' && input != -1) input = pr.read();
            if(input == -1) return null;
            sb.append('\n');
          // get string
          } else if(input == '"') {
            sb.append((char)input);
            int start = sb.length()-1;
            input = pr.read();
            while(input != -1) {
              if(input == '"') {
                // verify a non-escaped quote
                int j = sb.length()-1, escapeCount = 0;
                while(j > start && sb.charAt(j) == '\\') {
                  ++escapeCount;
                  --j;
                }
                sb.append((char)input);
                if(escapeCount % 2 == 0) break; // non-escaped <">
              } else {
                sb.append((char)input);
              }
              input = pr.read();
            }
            if(input == -1) return null;
            if(parenCount == 0) break;
          // account for reader shorthands
          } else if(Reader.isReaderShorthand((char)input) || input == '\\') {
            sb.append((char)input);
            if(input == ',') {
              input = pr.read();
              if(input == -1) return null;
              if(input == '@') {
                sb.append((char)input);
              } else {
                pr.unread(input);
              }
            }
          // account for regular token
          } else {
            sb.append((char)input);
            foundLoneAtom = true;
          }
        }
        // eat the rest of the line if reading from STDIN
        if(isStdin()) {
          int c = pr.read();
          while(c != '\n' && c != -1) c = pr.read();
        }
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString());
        if(isStdin()) GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
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
    return pr.skip(n);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public void close() throws Exception {
    try {
      if(isClosed() == false) pr.close();
    } catch(Exception e) {
      throw new Exceptionf("Can't close port \"%s\": %s", name, e);
    }
  }


  public boolean isClosed() {
    try {
      pr.ready();
      return false;
    } catch(Exception e) {
      return true;
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
    return o instanceof InputPort && ((InputPort)o).pr.equals(pr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),name,pr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<input-port ["+name+"]>";
  }
}