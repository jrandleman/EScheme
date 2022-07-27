// Author: Jordan Randleman - escm.primitive.PortPrimitives
// Purpose:
//    Java primitives for system operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Port;
import escm.type.InputPort;
import escm.type.OutputPort;
import escm.type.Void;
import escm.type.Boolean;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class PortPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // open-input-file
  public static class OpenInputFile implements Primitive {
    public java.lang.String escmName() {
      return "open-input-file";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-input-file <filename-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return new InputPort(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-output-file
  public static class OpenOutputFile implements Primitive {
    public java.lang.String escmName() {
      return "open-output-file";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-output-file <filename-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return new OutputPort(((escm.type.String)parameters.get(0)).value(),false);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-output-file+
  public static class OpenOutputFilePlus implements Primitive {
    public java.lang.String escmName() {
      return "open-output-file+";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(open-output-file+ <filename-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return new OutputPort(((escm.type.String)parameters.get(0)).value(),true);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // close-port!
  public static class ClosePortBang implements Primitive {
    public java.lang.String escmName() {
      return "close-port!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(close-port! <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      ((Port)parameters.get(0)).close();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // port?
  public static class PortP implements Primitive {
    public java.lang.String escmName() {
      return "port?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Port);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // input-port?
  public static class InputPortP implements Primitive {
    public java.lang.String escmName() {
      return "input-port?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(input-port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof InputPort);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // output-port?
  public static class OutputPortP implements Primitive {
    public java.lang.String escmName() {
      return "output-port?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(output-port? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof OutputPort);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // open-port?
  public static class OpenPortP implements Primitive {
    public java.lang.String escmName() {
      return "open-port?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(open-port? <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((Port)parameters.get(0)).isOpen());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // closed-port?
  public static class ClosedPortP implements Primitive {
    public java.lang.String escmName() {
      return "closed-port?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Port))
        throw new Exceptionf("'(closed-port? <port>) didn't receive 1 port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((Port)parameters.get(0)).isClosed());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-input-port
  public static class CurrentInputPort implements Primitive {
    public java.lang.String escmName() {
      return "current-input-port";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-input-port) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return InputPort.getCurrent();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-output-port
  public static class CurrentOutputPort implements Primitive {
    public java.lang.String escmName() {
      return "current-output-port";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(current-output-port) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return OutputPort.getCurrent();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-input-file
  public static class CallWithInputFile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-input-file";
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
  public static class CallWithOutputFile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-output-file";
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
  public static class CallWithOutputFilePlus implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-output-file+";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return CallWithOutputFile.logic(escmName(),true,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-input-from-file
  public static class WithInputFromFile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-input-from-file";
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
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(ip);
      return ((Callable)cal).callWith(args,(value) -> () -> {
        InputPort.setCurrent(originalInputPort);
        ip.close();
        return continuation.run(value);
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-output-to-file
  public static class WithOutputToFile implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-output-to-file";
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
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(op);
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
  public static class WithOutputToFilePlus implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-output-to-file+";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return WithOutputToFile.logic(escmName(),true,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // mark-input-port
  public static class MarkInputPort implements Primitive {
    public java.lang.String escmName() {
      return "mark-input-port";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(mark-input-port <input-port> <read-ahead-limit-number>) didn't receive 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum ipDatum = parameters.get(0);
      if(!(ipDatum instanceof InputPort))
        throw new Exceptionf("'(mark-input-port <input-port> <read-ahead-limit-number>) 1st arg isn't an input-port: %s", Exceptionf.profileArgs(parameters));
      Datum readAheadLimitDatum = parameters.get(1);
      if(!(readAheadLimitDatum instanceof escm.type.Number))
        throw new Exceptionf("'(mark-input-port <input-port> <read-ahead-limit-number>) 2nd arg isn't a number: %s", Exceptionf.profileArgs(parameters));
      ((InputPort)ipDatum).mark(((escm.type.Number)readAheadLimitDatum).intValue());
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // reset-input-port
  public static class ResetInputPort implements Primitive {
    public java.lang.String escmName() {
      return "reset-input-port";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof InputPort))
        throw new Exceptionf("'(reset-input-port <input-port> <read-ahead-limit-number>) didn't receive 1 input-port: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((InputPort)parameters.get(0)).reset());
    }
  }
}