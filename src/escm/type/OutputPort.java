// Author: Jordan Randleman - escm.type.OutputPort
// Purpose:
//    Output port primitive type, to write to a file.
//    Note that ALL write operations also flush automatically.
//
//    Provides:
//      - static STDOUT [System.out]
//
//      - static OutputPort getCurrent()
//      - static void setCurrent(OutputPort o)
//
//      - boolean isStdout()
//
//      - void newline()
//
//      - void print(Object o)
//      - void println(Object o)
//      - void printf(String fmt, Object... args)
//
//      - void write(Datum d)
//      - void display(Datum d)
//      - void pprint(Datum d)

package escm.type;
import java.util.Objects;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileWriter;
import escm.util.Exceptionf;
import escm.util.StringParser;
import escm.vm.runtime.EscmThread;

public class OutputPort extends Port {
  ////////////////////////////////////////////////////////////////////////////
  // Internal reader value
  private BufferedWriter bw = null;


  ////////////////////////////////////////////////////////////////////////////
  // Name
  private java.lang.String name = null;


  public java.lang.String sourceName() {
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static STDIN field
  private OutputPort() {
    bw = new BufferedWriter(new OutputStreamWriter(System.out));
    name = "System.out";
  }


  public static final OutputPort STDOUT = new OutputPort();


  ////////////////////////////////////////////////////////////////////////////
  // Handle the Current Output Port
  public static OutputPort getCurrent() {
    return ((EscmThread)java.lang.Thread.currentThread()).currentOutputPort;
  }

  public static void setCurrent(OutputPort o) {
    ((EscmThread)java.lang.Thread.currentThread()).currentOutputPort = o;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Factory Functions
  public OutputPort(java.lang.String filename, boolean append) throws Exception {
    try {
      bw = new BufferedWriter(new FileWriter(filename,append));
      name = filename;
    } catch(Exception e) {
      throw new Exceptionf("Can't open port \"%s\" for output: %s", filename, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether Writing to STDOUT
  public boolean isStdout() {
    return this == STDOUT;
  }


  ////////////////////////////////////////////////////////////////////////////
  // String Printing Functionality
  public void newline() throws Exception {
    try {
      bw.newLine();
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't print newline to port \"%s\": %s", name, e);
    }
  }


  public void print(Object o) throws Exception {
    java.lang.String s = o.toString();
    try {
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't print string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  public void println(Object o) throws Exception {
    java.lang.String s = o.toString()+"\n";
    try {
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't println string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  public void printf(java.lang.String fmt, Object... args) throws Exception {
    java.lang.String s = java.lang.String.format(fmt,args);
    try {
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't printf string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Datum Printing Functionality
  public void write(Datum d) throws Exception {
    try {
      java.lang.String s = d.write();
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't write datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  public void display(Datum d) throws Exception {
    try {
      java.lang.String s = d.display();
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't display datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  public void pprint(Datum d) throws Exception {
    try {
      java.lang.String s = d.pprint();
      bw.write(s,0,s.length());
      bw.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't pretty-print datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public void close() throws Exception {
    try {
      if(isClosed() == false) bw.close();
    } catch(Exception e) {
      throw new Exceptionf("Can't close port \"%s\": %s", name, e);
    }
  }


  public boolean isClosed() {
    try {
      bw.flush();
      return false;
    } catch(Exception e) {
      return true;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "output-port";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof OutputPort && ((OutputPort)o).bw.equals(bw);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),name,bw);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<output-port ["+name+"]>";
  }
}