// Author: Jordan Randleman - escm.type.port.OutputPort
// Purpose:
//    Output port primitive type, to write to a file.
//    Note that ALL write operations also flush automatically.
//
//    Provides:
//      - static OutputPort STDOUT [System.out]
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

package escm.type.port;
import java.util.Objects;
import java.io.Writer;
import java.io.OutputStreamWriter;
import java.io.FileWriter;
import java.io.File;
import escm.util.error.Exceptionf;
import escm.util.string.StringParser;
import escm.type.Datum;
import escm.vm.runtime.EscmThread;
import escm.primitive.FilePrimitives;

public class OutputPort extends Port {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Reader Value
  private Writer writer = null;


  ////////////////////////////////////////////////////////////////////////////
  // Name
  private String name = null;


  public String sourceName() {
    return name;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static STDIN field
  private OutputPort(int outOrErr) {
    if(outOrErr == 0) {
      writer = new OutputStreamWriter(System.out);
      name = "System.out";
    } else {
      writer = new OutputStreamWriter(System.err);
      name = "System.err";
    }
  }


  public static final OutputPort STDOUT = new OutputPort(0);

  public static final OutputPort STDERR = new OutputPort(1);


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
  public OutputPort(String filename, boolean append) throws Exception {
    try {
      writer = new FileWriter(filename,append);
      name = FilePrimitives.AbsolutePath.logic(filename);
    } catch(Exception e) {
      throw new Exceptionf("Can't open port \"%s\" for output: %s", filename, e);
    }
  }

  public OutputPort() throws Exception {
    try {
      File tempFile = File.createTempFile(Port.TEMP_FILE_PREFIX,Port.TEMP_FILE_SUFFIX);
      tempFile.deleteOnExit();
      writer = new FileWriter(tempFile);
      name = tempFile.getAbsolutePath();
    } catch(Exception e) {
      throw new Exceptionf("Can't open temp-port for output: %s", e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Whether Writing to STDOUT
  public boolean isStdout() {
    return writer.equals(STDOUT.writer);
  }


  ////////////////////////////////////////////////////////////////////////////
  // String Printing Functionality
  public void newline() throws Exception {
    try {
      writer.write('\n');
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't print newline to port \"%s\": %s", name, e);
    }
  }


  public void print(Object o) throws Exception {
    String s = o.toString();
    try {
      writer.write(s);
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't print string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  public void println(Object o) throws Exception {
    String s = o.toString();
    try {
      writer.write(s+"\n");
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't println string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  public void printf(String fmt, Object... args) throws Exception {
    String s = String.format(fmt,args);
    try {
      writer.write(s);
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't printf string \"%s\" to port \"%s\": %s", StringParser.escape(s), name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Datum Printing Functionality
  public void write(Datum d) throws Exception {
    try {
      writer.write(d.write());
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't write datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  public void display(Datum d) throws Exception {
    try {
      writer.write(d.display());
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't display datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  public void pprint(Datum d) throws Exception {
    try {
      writer.write(d.pprint());
      writer.flush();
    } catch(Exception e) {
      throw new Exceptionf("Can't pretty-print datum %s to port \"%s\": %s", d.profile(), name, e);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Java Serialization WRITE Semantics
  private Object writeReplace() throws Exception {
    return new SerializedOutputPort(name);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Port Closing Semantics
  public void close() throws Exception {
    try {
      if(isClosed() == false) writer.close();
    } catch(Exception e) {
      throw new Exceptionf("Can't close port \"%s\": %s", name, e);
    }
  }


  public boolean isClosed() {
    try {
      writer.flush();
      return false;
    } catch(Exception e) {
      return true;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "output-port";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof OutputPort && ((OutputPort)o).writer.equals(writer);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    if(isClosed()) return "#<output-port (closed) "+name+">";
    return "#<output-port (open) "+name+">";
  }
}