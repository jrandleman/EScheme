// Author: Jordan Randleman - escm.primitive.PortPrimitives
// Purpose:
//    Java primitives to manipulate <port> objects (represents a file handle).

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.Void;
import escm.type.port.Port;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.type.number.Exact;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;

public class PortPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // open-input-file
  public static class OpenInputFile extends Primitive {
    public java.lang.String escmName() {
      return "open-input-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("open-input-file"),new Symbol("<filename-string>"));
    }

    public String docstring() {
      return "Returns an input-port file handle to read from <filename-string>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-input-file <filename-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return new InputPort(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-output-file
  public static class OpenOutputFile extends Primitive {
    public java.lang.String escmName() {
      return "open-output-file";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("open-output-file")),
        Pair.List(new Symbol("open-output-file"),new Symbol("<filename-string>")));
    }

    public String docstring() {
      return "Returns an output-port file handle to write to <filename-string>.\nIf <filename-string> exists, it is cleared.\n\nIf <filename-string> isn't given, generates a temporary file.\nAccess a temporary file's path via <port-path>.\nTemporary files are automatically deleted upon exit by the VM.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 0) return new OutputPort();
      if(n != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-output-file <optional-filename-string>) didn't receive no-args or 1 string: %s", Exceptionf.profileArgs(parameters));
      return new OutputPort(((escm.type.String)parameters.get(0)).value(),false);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-output-file+
  public static class OpenOutputFilePlus extends Primitive {
    public java.lang.String escmName() {
      return "open-output-file+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("open-output-file+"),new Symbol("<filename-string>"));
    }

    public String docstring() {
      return "Returns an output-port file handle to write to <filename-string>.\nIf <filename-string> exists, it is appended to.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-output-file+ <filename-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return new OutputPort(((escm.type.String)parameters.get(0)).value(),true);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // close-port!
  public static class ClosePortBang extends Primitive {
    public java.lang.String escmName() {
      return "close-port!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("close-port!"),new Symbol("<port>"));
    }

    public String docstring() {
      return "Closes <port> if it hasn't been closed yet.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(close-port! <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      ((Port)parameters.get(0)).close();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // port-path
  public static class PortPath extends Primitive {
    public java.lang.String escmName() {
      return "port-path";
    }

    public Datum signature() {
      return Pair.List(new Symbol("port-path"),new Symbol("<port>"));
    }

    public String docstring() {
      return "Returns the <port>'s path as a string.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(port-path <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((Port)parameters.get(0)).sourceName());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // port-position
  public static class PortPosition extends Primitive {
    public java.lang.String escmName() {
      return "port-position";
    }

    public Datum signature() {
      return Pair.List(new Symbol("port-position"),new Symbol("<input-port>"));
    }

    public String docstring() {
      return "Returns a pair: <input-port>'s (<line-number> . <column-number>)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof InputPort))
        throw new Exceptionf("'(port-position <input-port>) didn't receive 1 input-port: %s", Exceptionf.profileArgs(parameters));
      InputPort ip = (InputPort)parameters.get(0);
      return new escm.type.Pair(new Exact(ip.lineNumber()),new Exact(ip.columnNumber()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // port?
  public static class PortP extends Primitive {
    public java.lang.String escmName() {
      return "port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("port?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a port.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Port);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // input-port?
  public static class InputPortP extends Primitive {
    public java.lang.String escmName() {
      return "input-port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("input-port?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is an input-port.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(input-port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof InputPort);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // output-port?
  public static class OutputPortP extends Primitive {
    public java.lang.String escmName() {
      return "output-port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("output-port?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is an output-port.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(output-port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OutputPort);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // temp-port?
  public static class TempPortP extends Primitive {
    public java.lang.String escmName() {
      return "temp-port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("temp-port?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a port pointing to a temporary file.\nAccess a temporary file's path via <port-path>.\nTemporary files are automatically deleted upon exit by the VM.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(temp-port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Port && ((Port)parameters.get(0)).isTemporary());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-port?
  public static class OpenPortP extends Primitive {
    public java.lang.String escmName() {
      return "open-port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("open-port?"),new Symbol("<port>"));
    }

    public String docstring() {
      return "Returns whether <port> is still open.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(open-port? <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((Port)parameters.get(0)).isOpen());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // closed-port?
  public static class ClosedPortP extends Primitive {
    public java.lang.String escmName() {
      return "closed-port?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("closed-port?"),new Symbol("<port>"));
    }

    public String docstring() {
      return "Returns whether <port> is closed.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(closed-port? <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((Port)parameters.get(0)).isClosed());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-input-port
  public static class CurrentInputPort extends Primitive {
    public java.lang.String escmName() {
      return "current-input-port";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-input-port"));
    }

    public String docstring() {
      return "Returns the current input-port, used as the default value for <read> etc.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-input-port) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return InputPort.getCurrent();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-output-port
  public static class CurrentOutputPort extends Primitive {
    public java.lang.String escmName() {
      return "current-output-port";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-output-port"));
    }

    public String docstring() {
      return "Returns the current output-port, used as the default value for <write> etc.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-output-port) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return OutputPort.getCurrent();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-input-file
  public static class CallWithInputFile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-input-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("call-with-input-file"),new Symbol("<filename-string>"),new Symbol("<unary-callable>"));
    }

    public String docstring() {
      return "Invoke <unary-callable> with (open-input-file <filename-string>) as its argument.\nAutomatically close that given port upon <unary-callable>'s return, and yield\n<unary-callable>'s return value.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(call-with-input-file <string> <unary-callable>) didn't receive 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(call-with-input-file <string> <unary-callable>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum cal = parameters.get(1);
      if(!(cal instanceof Callable))
        throw new Exceptionf("'(call-with-input-file <string> <unary-callable>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      InputPort ip = new InputPort(((escm.type.String)str).value());
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(ip);
      return ((Callable)cal).callWith(args,(value) -> () -> {
        ip.close();
        return continuation.run(value);
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-output-file
  public static class CallWithOutputFile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-output-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("call-with-output-file"),new Symbol("<filename-string>"),new Symbol("<unary-callable>"));
    }

    public String docstring() {
      return "Invoke <unary-callable> with (open-output-file <filename-string>) as its argument.\nAutomatically close that given port upon <unary-callable>'s return, and yield\n<unary-callable>'s return value.\nNote that <filename-string> is cleared if it exists.";
    }

    public static Trampoline.Bounce logic(String primitiveName, boolean append, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(%s <string> <unary-callable>) didn't receive 2 args: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(%s <string> <unary-callable>) 1st arg isn't a string: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum cal = parameters.get(1);
      if(!(cal instanceof Callable))
        throw new Exceptionf("'(%s <string> <unary-callable>) 1st arg isn't a string: %s", primitiveName, Exceptionf.profileArgs(parameters));
      OutputPort op = new OutputPort(((escm.type.String)str).value(),append);
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(op);
      return ((Callable)cal).callWith(args,(value) -> () -> {
        op.close();
        return continuation.run(value);
      });
    } 

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return logic(escmName(),false,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-output-file+
  public static class CallWithOutputFilePlus extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-output-file+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("call-with-output-file+"),new Symbol("<filename-string>"),new Symbol("<unary-callable>"));
    }

    public String docstring() {
      return "Invoke <unary-callable> with (open-output-file <filename-string>) as its argument.\nAutomatically close that given port upon <unary-callable>'s return, and yield\n<unary-callable>'s return value.\nNote that <filename-string> is appended to if it exists.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return CallWithOutputFile.logic(escmName(),true,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-input-from-file
  public static class WithInputFromFile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-input-from-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("with-input-from-file"),new Symbol("<filename-string>"),new Symbol("<thunk-callable>"));
    }

    public String docstring() {
      return "Invoke <thunk-callable> with (open-input-file <filename-string>) as the\ncurrent-input-port. Automatically close that given port upon <thunk-callable>'s\nreturn, and yield <thunk-callable>'s return value.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(with-input-from-file <string> <thunk-callable>) didn't receive 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(with-input-from-file <string> <thunk-callable>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum cal = parameters.get(1);
      if(!(cal instanceof Callable))
        throw new Exceptionf("'(with-input-from-file <string> <thunk-callable>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      InputPort originalInputPort = InputPort.getCurrent();
      InputPort ip = new InputPort(((escm.type.String)str).value());
      InputPort.setCurrent(ip);
      ArrayList<Datum> args = new ArrayList<Datum>();
      return ((Callable)cal).callWith(args,(value) -> () -> {
        InputPort.setCurrent(originalInputPort);
        ip.close();
        return continuation.run(value);
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-output-to-file
  public static class WithOutputToFile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-output-to-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("with-output-to-file"),new Symbol("<filename-string>"),new Symbol("<thunk-callable>"));
    }

    public String docstring() {
      return "Invoke <thunk-callable> with (open-output-file <filename-string>) as the\ncurrent-output-port. Automatically close that given port upon <thunk-callable>'s\nreturn, and yield <thunk-callable>'s return value.\nNote that <filename-string> is cleared if it exists.";
    }

    public static Trampoline.Bounce logic(String primitiveName, boolean append, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(%s <string> <thunk-callable>) didn't receive 2 args: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(%s <string> <thunk-callable>) 1st arg isn't a string: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum cal = parameters.get(1);
      if(!(cal instanceof Callable))
        throw new Exceptionf("'(%s <string> <thunk-callable>) 1st arg isn't a string: %s", primitiveName, Exceptionf.profileArgs(parameters));
      OutputPort originalOutputPort = OutputPort.getCurrent();
      OutputPort op = new OutputPort(((escm.type.String)str).value(),append);
      OutputPort.setCurrent(op);
      ArrayList<Datum> args = new ArrayList<Datum>();
      return ((Callable)cal).callWith(args,(value) -> () -> {
        OutputPort.setCurrent(originalOutputPort);
        op.close();
        return continuation.run(value);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return logic(escmName(),false,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-output-to-file+
  public static class WithOutputToFilePlus extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-output-to-file+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("with-output-to-file+"),new Symbol("<filename-string>"),new Symbol("<thunk-callable>"));
    }

    public String docstring() {
      return "Invoke <thunk-callable> with (open-output-file <filename-string>) as the\ncurrent-output-port. Automatically close that given port upon <thunk-callable>'s\nreturn, and yield <thunk-callable>'s return value.\nNote that <filename-string> is appended to if it exists.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return WithOutputToFile.logic(escmName(),true,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // peek-port
  public static class PeekPort extends Primitive {
    public java.lang.String escmName() {
      return "peek-port";
    }

    public Datum signature() {
      return Pair.List(new Symbol("peek-port"),new Symbol("<input-port>"));
    }

    public String docstring() {
      return "Peek the first character in <input-port>. Returns #eof if empty.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof InputPort))
        throw new Exceptionf("'(peek-port <input-port>) didn't receive 1 input-port: %s", Exceptionf.profileArgs(parameters));
      Integer codepoint = ((InputPort)parameters.get(0)).peek();
      if(codepoint == null) return escm.type.port.Eof.VALUE;
      return new escm.type.Character(codepoint.intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // stdin?
  public static class IsStdinP extends Primitive {
    public java.lang.String escmName() {
      return "stdin?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stdin?"),new Symbol("<input-port>"));
    }

    public String docstring() {
      return "Returns whether <input-port> handles the program's standard input.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof InputPort))
        throw new Exceptionf("'(stdin? <input-port>) didn't receive 1 input-port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((InputPort)parameters.get(0)).isStdin());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // stdin?
  public static class IsStdoutP extends Primitive {
    public java.lang.String escmName() {
      return "stdout?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stdout?"),new Symbol("<output-port>"));
    }

    public String docstring() {
      return "Returns whether <output-port> handles the program's standard output.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof OutputPort))
        throw new Exceptionf("'(stdout? <output-port>) didn't receive 1 input-port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((OutputPort)parameters.get(0)).isStdout());
    }
  }
}