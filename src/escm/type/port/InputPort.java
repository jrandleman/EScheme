// Author: Jordan Randleman - escm.type.port.InputPort
// Purpose:
//    Input port primitive type, to read from a file/stdin.
//
//    Provides:
//      - static InputPort STDIN [System.in]
//
//      - static InputPort getCurrent()
//      - static void setCurrent(InputPort o)
//
//      - long lineNumber()
//      - long columnNumber()
//
//      - boolean isStdin()
//
//      - boolean ready()
//
//      - Integer peek()               // returns <null> if reached EOF
//      - Integer readCharacter()      // returns <null> if reached EOF
//      - String readCharacters(int n) // returns <null> if reached EOF
//      - String readLine()            // returns <null> if reached EOF
//      - Datum readReplDatum()        // returns <null> if reached EOF
//      - Datum readDatum()            // returns <null> if reached EOF

package escm.type.port;
import java.util.Objects;
import java.util.ArrayDeque;
import java.io.PushbackReader;
import java.io.InputStreamReader;
import java.io.FileReader;
import java.io.File;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.vm.Reader;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmThread;
import escm.primitive.FilePrimitives;

public class InputPort extends Port {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Reader Value
  private PushbackReader pr = null;
  private FileReader fileReader = null; // track for closing non-stdin files

  // To work with codepoints (rather than chars) in files
  private static final int PUSHBACK_READER_SIZE = 2;


  ////////////////////////////////////////////////////////////////////////////
  // Name
  private String name = null;

  public String sourceName() {
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Position
  private long lineNumber = 1;

  private long columnNumber = 1;

  private void updatePortPosition(int c) {
    if(c <= 0) return;
    if(c == '\n') {
      ++lineNumber;
       columnNumber = 1; 
    } else {
      ++columnNumber;
    }
  }

  private void updatePortPosition(CharSequence s) {
    if(s == null) return;
    for(int i = 0, n = s.length(); i < n; ++i) {
      updatePortPosition(s.charAt(i));
    }
  }

  private void updatePortPosition(char[] cs) {
    if(cs == null) return;
    for(int i = 0; i < cs.length && cs[i] != 0; ++i) {
      updatePortPosition(cs[i]);
    }
  }


  public synchronized long lineNumber() {
    return lineNumber;
  }

  public synchronized long columnNumber() {
    return columnNumber;
  }


  public synchronized String getPositionString() {
    return String.format("file=\"%s\", line=%d, column=%d", name, lineNumber, columnNumber);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static STDIN field
  private InputPort() {
    pr = new PushbackReader(new InputStreamReader(System.in),PUSHBACK_READER_SIZE);
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
  public InputPort(String filename) throws Exception {
    try {
      fileReader = new FileReader(filename);
      pr = new PushbackReader(fileReader,PUSHBACK_READER_SIZE);
      name = FilePrimitives.AbsolutePath.logic(filename);
    } catch(Exception e) {
      throw new Exceptionf("Can't open port \"%s\" for input: %s", filename, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether Reading from STDIN
  public boolean isStdin() {
    return pr.equals(STDIN.pr);
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
  public synchronized Integer peek() throws Exception {
    try {
      int high = pr.read();
      if(high == -1) return null;
      char highChar = (char)high;
      if(java.lang.Character.isHighSurrogate(highChar) == false) {
        pr.unread(high);
        if(high == -1) return null;
        return high;
      } else {
        int low = pr.read();
        char lowChar = (char)low;
        pr.unread(new char[]{highChar,lowChar});
        if(low == -1 || !java.lang.Character.isLowSurrogate(lowChar)) 
          throw new Exceptionf("Invalid surrogate pair with high value \"%c\"", highChar);
        return Character.toCodePoint(highChar,lowChar);
      }
    } catch(Exception e) {
      throw new Exceptionf("Can't peek port \"%s\": %s", name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Various Readers
  private long getNumberOfDigits(long num) {
    return (long)(Math.log10(num)+1);
  }


  private void printReplPrompt() {
    if(!GlobalState.getLastPrintedANewline()) System.out.println("");
    System.out.printf("[%d]> ", lineNumber);
    System.out.flush();
    if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(false); // from the newline input by the user's <enter>/<return> key stroke
  }


  private void printIncompleteReplPrompt() {
    long width = getNumberOfDigits(lineNumber);
    if(width > getNumberOfDigits(lineNumber-1)) {
      for(long i = 0, n = width+3; i < n; ++i) System.out.print(' ');
    } else {
      for(long i = 0, n = width+4; i < n; ++i) System.out.print(' ');
    }
    System.out.flush();
  }


  private Integer readCharacterLogic() throws Exception {
    int high = pr.read();
    char highChar = (char)high;
    if(java.lang.Character.isHighSurrogate(highChar) == false) {
      updatePortPosition(high);
      if(high == -1) return null;
      return high;
    } else {
      int low = pr.read();
      char lowChar = (char)low;
      if(low == -1 || !java.lang.Character.isLowSurrogate(lowChar)) 
        throw new Exceptionf("Invalid surrogate pair with high value \"%c\"", highChar);
      int codepoint = Character.toCodePoint(highChar,lowChar);
      updatePortPosition(codepoint);
      return codepoint;
    }
  }


  public synchronized Integer readCharacter() throws Exception {
    try {
      return readCharacterLogic();
    } catch(Exception e) {
      throw new Exceptionf("Can't read a char from port \"%s\": %s", name, e);
    }
  }


  public synchronized String readCharacters(int n) throws Exception {
    if(n == 0) return "";
    if(n < 0)
      throw new Exceptionf("Can't read chars from port \"%s\" with negative char-count %d!", name, n);
    try {
      StringBuilder sb = new StringBuilder();
      while(n > 0) {
        Integer codepoint = readCharacterLogic();
        if(codepoint == null) {
          if(sb.length() == 0) return null;
          return sb.toString();
        }
        sb.append(java.lang.Character.toString(codepoint.intValue()));
        --n;
      }
      return sb.toString();
    } catch(Exception e) {
      throw new Exceptionf("Can't read chars from port \"%s\": %s", name, e);
    }
  }


  public synchronized String readLine() throws Exception {
    try {
      StringBuilder sb = new StringBuilder();
      int c = pr.read();
      while(c != '\n' && c != -1) {
        sb.append((char)c);
        c = pr.read();
      }
      String s = sb.toString();
      updatePortPosition(s);
      if(c != -1) updatePortPosition('\n');
      if(s.length() == 0 && c == -1) return null;
      return s;
    } catch(Exception e) {
      throw new Exceptionf("Can't read line from port \"%s\": %s", name, e);
    }
  }


  public synchronized Datum readReplDatum() throws Exception {
    if(isClosed()) return null; // closing the input stream is equivalent to reading EOF
    printReplPrompt();
    StringBuilder sb = new StringBuilder();
    SourceInformation replDatumSourceStart = new SourceInformation(name,lineNumber,columnNumber);
    while(true) {
      try {
        String input = readLine();
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
        sb.append('\n');
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString(),replDatumSourceStart.clone(),Reader.GIVE_EMPTY_INCOMPLETE_ERRORS);
        GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
        if(result.first instanceof Eof) return null; // only reading #eof is equivalent to typing ^D
        return result.first;
      } catch(Reader.IncompleteException e) {
        printIncompleteReplPrompt();
        continue;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Datum Reader
  private boolean isReaderLambdaLiteral(int input) throws Exception {
    if(input != (int)Reader.LAMBDA_LITERAL_PREFIX) return false;
    int next = pr.read();
    pr.unread(next);
    return next == (int)'(';
  }


  public synchronized Datum readDatum() throws Exception {
    StringBuilder sb = new StringBuilder();
    SourceInformation datumSourceStart = new SourceInformation(name,lineNumber,columnNumber);
    while(true) {
      try {
        ArrayDeque<java.lang.Character> containerStack = new ArrayDeque<java.lang.Character>();
        boolean foundLoneAtom = false;
        while(true) {
          int input = pr.read();
          // check for atom
          if(foundLoneAtom && containerStack.size() == 0 && (input == -1 || Reader.isDelimiter((char)input))) {
            if(input != -1) pr.unread(input);
            break;
          }
          if(input == -1) {
            if(containerStack.size() > 0) {
              char c = containerStack.pop();
              if(c == '(') {
                throw new Exceptionf("READ ERROR (for %s): Invalid parenthesis: missing a closing ')' for opening '('!\n>> Location: %s", write(), getPositionString());
              } else if(c == '[') {
                throw new Exceptionf("READ ERROR (for %s): Invalid bracket: missing a closing ']' for opening '['!\n>> Location: %s", write(), getPositionString());
              } else { // if(c == '{')
                throw new Exceptionf("READ ERROR (for %s): Invalid curly-brace: missing a closing '}' for opening '{'!\n>> Location: %s", write(), getPositionString());
              }
            }
            return null;
          }
          // register open paren/bracket/curly-brace
          if(input == '(' || input == '[' || input == '{') {
            updatePortPosition(input);
            sb.append((char)input);
            containerStack.push((char)input);
          // register close paren
          } else if(input == ')') {
            updatePortPosition(input);
            sb.append((char)input);
            if(containerStack.size() == 0) {
              throw new Exceptionf("READ ERROR (for %s): Invalid parenthesis: found a ')' prior an associated '('!\n>> Location: %s", write(), getPositionString());
            }
            char opener = containerStack.pop();
            if(opener != '(') {
              throw new Exceptionf("READ ERROR (for %s): Invalid parenthesis: found a closing ')' prior to closing '%c'!\n>> Location: %s", write(), opener, getPositionString());
            }
            if(containerStack.size() == 0) break;
          // register close bracket
          } else if(input == ']') {
            updatePortPosition(input);
            sb.append((char)input);
            if(containerStack.size() == 0) {
              throw new Exceptionf("READ ERROR (for %s): Invalid bracket: found a ']' prior an associated '['!\n>> Location: %s", write(), getPositionString());
            }
            char opener = containerStack.pop();
            if(opener != '[') {
              throw new Exceptionf("READ ERROR (for %s): Invalid bracket: found a closing ']' prior to closing '%c'!\n>> Location: %s", write(), opener, getPositionString());
            }
            if(containerStack.size() == 0) break;
          // register close curly-brace
          } else if(input == '}') {
            updatePortPosition(input);
            sb.append((char)input);
            if(containerStack.size() == 0) {
              throw new Exceptionf("READ ERROR (for %s): Invalid curly-brace: found a '}' prior an associated '{'!\n>> Location: %s", write(), getPositionString());
            }
            char opener = containerStack.pop();
            if(opener != '{') {
              throw new Exceptionf("READ ERROR (for %s): Invalid curly-brace: found a closing '}' prior to closing '%c'!\n>> Location: %s", write(), opener, getPositionString());
            }
            if(containerStack.size() == 0) break;
          // account for whitespace
          } else if(java.lang.Character.isWhitespace((char)input)) {
            updatePortPosition(input);
            sb.append((char)input);
          // skip comment
          } else if(input == ';') {
            updatePortPosition(input);
            input = pr.read();
            while(input != '\n' && input != -1) {
              updatePortPosition(input);
              input = pr.read();
            }
            if(input == -1) return null;
            updatePortPosition('\n');
            sb.append('\n');
          // get string
          } else if(input == '"') {
            updatePortPosition(input);
            sb.append((char)input);
            long stringStartLine = lineNumber;
            long stringStartColumn = columnNumber;
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
                updatePortPosition(input);
                sb.append((char)input);
                if(escapeCount % 2 == 0) break; // non-escaped <">
              } else {
                updatePortPosition(input);
                sb.append((char)input);
              }
              input = pr.read();
            }
            if(input == -1) {
              throw new Exceptionf("READ ERROR (for %s): Unterminating string literal detected!\n>> Location: %s", write(), getPositionString());
            }
            if(containerStack.size() == 0) break;
          // account for reader shorthands
          } else if(Reader.isReaderShorthand((char)input) || isReaderLambdaLiteral(input)) {
            updatePortPosition(input);
            sb.append((char)input);
            if(input == ',') {
              input = pr.read();
              if(input == -1) {
                throw new Exceptionf("READ ERROR (for %s): Incomplete \"unquote\" reader shorthand literal!\n>> Location: %s", write(), getPositionString());
              }
              if(input == '@') {
                updatePortPosition(input);
                sb.append((char)input);
              } else {
                pr.unread(input);
              }
            }
          // account for regular token
          } else {
            updatePortPosition(input);
            sb.append((char)input);
            foundLoneAtom = true;
          }
        }
        // eat the rest of the line if reading from STDIN
        if(isStdin()) {
          int c = pr.read();
          while(c != '\n' && c != -1) {
            updatePortPosition(c);
            c = pr.read();
          }
          if(c != -1) updatePortPosition(c);
        }
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString(),datumSourceStart.clone(),Reader.GIVE_EMPTY_INCOMPLETE_ERRORS);
        if(isStdin()) GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
        if(result.first instanceof Eof) return null; // only reading #eof is equivalent to typing ^D
        return result.first;
      } catch(Reader.IncompleteException e) {
        continue;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization WRITE Semantics
  private Object writeReplace() throws Exception {
    return new SerializedInputPort(name);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public synchronized void close() throws Exception {
    try {
      if(isClosed() == false) {
        pr.close();
        if(fileReader != null) fileReader.close();
      }
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
  public String type() {
    return "input-port";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof InputPort && ((InputPort)o).pr.equals(pr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    if(isClosed()) return "#<input-port (closed) "+name+">";
    return "#<input-port (open) "+name+">";
  }
}