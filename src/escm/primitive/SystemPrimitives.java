// Author: Jordan Randleman - escm.primitive.SystemPrimitives
// Purpose:
//    Java primitives for system operations.

package escm.primitive;
import java.util.ArrayList;
import java.util.Calendar;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.File;
import escm.type.Datum;
import escm.type.Boolean;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.ExecuteSystemCommand;
import escm.vm.type.Callable;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.runtime.GlobalState;

public class SystemPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Get the EScheme version number
  public static final double VERSION = 5.0;


  ////////////////////////////////////////////////////////////////////////////
  // Get the EXIT message
  public static String getExitMessage() {
    int timeOfDay = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
    if(timeOfDay >= 4 && timeOfDay < 12){
      return "Have a great day!";
    } else if(timeOfDay >= 12 && timeOfDay < 16){
      return "Enjoy your afternoon!";
    } else if(timeOfDay >= 16 && timeOfDay < 21){
      return "Have a nice evening!";
    } else {
      return "Good night! Sleep well :)";
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // exit
  public static class Exit implements Primitive {
    public java.lang.String escmName() {
      return "exit";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1) throw new Exceptionf("'(exit <optional-code>) received more than 1 arg: %s", Exceptionf.profileArgs(parameters));
      int code = 0;
      if(parameters.size() == 1) {
        Datum exitCode = parameters.get(0);
        if(!(exitCode instanceof escm.type.Number))
          throw new Exceptionf("'(exit <optional-code>) code %s isn't an integer: %s", exitCode.profile(), Exceptionf.profileArgs(parameters));
        code = ((escm.type.Number)exitCode).intValue();
      }
      // Print the exit msg iff in a REPL session
      if(GlobalState.inREPL) {
        if(!GlobalState.getLastPrintedANewline()) System.out.println("");
        System.out.println(getExitMessage());
      }
      System.exit(code);
      return escm.type.Void.VALUE; // never triggered
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-read
  public static class FileRead implements Primitive {
    public java.lang.String escmName() {
      return "file-read";
    }
    
    private static Datum convertReadExpressionsToReadExpression(ArrayList<Datum> contents) {
      if(contents.size() == 1) return contents.get(0);
      Datum expression = escm.type.Nil.VALUE;
      for(int i = contents.size()-1; i >= 0; --i)
        expression = new escm.type.Pair(contents.get(i),expression);
      return new escm.type.Pair(new escm.type.Symbol("begin"),expression);
    }

    public static String slurpFile(String filename, String callerName) throws Exception {
      try {
        return Files.readString(Path.of(filename));
      } catch(Exception e) {
        throw new Exceptionf("'%s couldn't read from file \"%s\"", callerName, filename);
      }
    }

    public static ArrayList<Datum> readBufferAsArrayList(String buffer) throws Exception {
      ArrayList<Datum> contents = new ArrayList<Datum>();
      buffer = buffer.trim();
      if(buffer.length() == 0) return contents;
      Integer n = buffer.length();
      escm.util.Pair<Datum,Integer> result = escm.vm.Reader.read(buffer);
      contents.add(result.first);
      buffer = buffer.substring(result.second).trim();
      while(result.second != n && buffer.length() > 0) {
        n = buffer.length();
        result = escm.vm.Reader.read(buffer);
        contents.add(result.first);
        buffer = buffer.substring(result.second).trim();
      }
      return contents;
    }

    public static Datum readBuffer(String buffer) throws Exception {
      ArrayList<Datum> contents = readBufferAsArrayList(buffer);
      if(contents.size() == 0) return escm.type.Void.VALUE;
      return convertReadExpressionsToReadExpression(contents);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-read <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return readBuffer(slurpFile(((escm.type.String)parameters.get(0)).value(),"file-read"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-read-string
  public static class FileReadString implements Primitive {
    public java.lang.String escmName() {
      return "file-read-string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-read-string <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(FileRead.slurpFile(((escm.type.String)parameters.get(0)).value(),"file-read-string"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-write
  public static class FileWrite implements Primitive {
    public java.lang.String escmName() {
      return "file-write";
    }
    
    public static void writeStringToFile(String filename, String str, String callerName) throws Exception {
      try {
        Files.writeString(Path.of(filename),str);
      } catch(Exception e) {
        throw new Exceptionf("'%s couldn't write to file \"%s\"", filename, callerName);
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-write <filename> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      writeStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).write(),"file-write");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-display
  public static class FileDisplay implements Primitive {
    public java.lang.String escmName() {
      return "file-display";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-display <filename> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.writeStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).display(),"file-display");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-delete!
  public static class FileDelete implements Primitive {
    public java.lang.String escmName() {
      return "file-delete!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-delete! <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Files.deleteIfExists(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file?
  public static class IsFile implements Primitive {
    public java.lang.String escmName() {
      return "file?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file? <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Files.exists(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load
  public static class Load implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load";
    }

    public static Trampoline.Bounce evalEachExpression(Environment env, ArrayList<Datum> exprs, int i, Trampoline.Continuation continuation) throws Exception {
      int n = exprs.size();
      if(i >= n) return continuation.run(escm.type.Void.VALUE);
      Trampoline.Continuation nextContinuation;
      if(i+1 == n) {
        nextContinuation = continuation;
      } else {
        nextContinuation = (value) -> () -> {
          if(value instanceof escm.type.Eof) return continuation.run(escm.type.Void.VALUE);
          return evalEachExpression(env,exprs,i+1,continuation);
        };
      }
      return escm.vm.Compiler.run(exprs.get(i),(compiled) -> () -> {
        return escm.vm.Interpreter.run(new ExecutionState(env,escm.vm.Assembler.run(compiled)),nextContinuation);
      });
    }

    public static Trampoline.Bounce loadFileInEnvironment(Environment env, String filename, Trampoline.Continuation continuation) throws Exception {
      String buffer = FileRead.slurpFile(filename,"load");
      ArrayList<Datum> exprs = FileRead.readBufferAsArrayList(buffer);
      return evalEachExpression(env,exprs,0,continuation);
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(load <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return loadFileInEnvironment(GlobalState.globalEnvironment,((escm.type.String)parameters.get(0)).value(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // system
  public static class ExecuteCommand implements Primitive {
    public java.lang.String escmName() {
      return "system";
    }
    
    private static String[] convertStringListToStringArray(Datum list, String listContentType, ArrayList<Datum> parameters) throws Exception {
      ArrayList<String> strs = new ArrayList<String>();
      while(list instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)list;
        Datum content = par.car();
        if(!(content instanceof escm.type.String))
          throw new Exceptionf("'(system <cmd-str> <optional-env-var-str-list> <optional-dir-str>) %s list value %s isn't a string: %s", listContentType, content.profile(), Exceptionf.profileArgs(parameters));
        strs.add(((escm.type.String)content).value());
        list = par.cdr();
      }
      return strs.toArray(new String[strs.size()]);
    }

    private static String parseCommands(ArrayList<Datum> parameters) throws Exception {
      Datum cmd = parameters.get(0);
      if(cmd instanceof escm.type.String) return ((escm.type.String)cmd).value();
      throw new Exceptionf("'(system <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <cmds> isn't a string: %s", Exceptionf.profileArgs(parameters));
    }

    private static Datum parseEnvironmentVariables(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) return null;
      Datum envp = parameters.get(1);
      if(envp instanceof escm.type.Pair) return envp;
      throw new Exceptionf("'(system <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <env-vars> isn't a str list: %s", Exceptionf.profileArgs(parameters));
    }

    private static File parseWorkingDirectory(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 3) return null;
      Datum dir = parameters.get(2);
      if(dir instanceof escm.type.String) return new File(((escm.type.String)dir).value());
      throw new Exceptionf("'(system <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <dir> isn't a str: %s", Exceptionf.profileArgs(parameters));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 3) 
        throw new Exceptionf("'(system <cmd-str> <optional-env-var-str-list> <optional-dir-str>) didn't receive correct number of args: %s", Exceptionf.profileArgs(parameters));
      String cmd = parseCommands(parameters);
      Datum envp = parseEnvironmentVariables(parameters);
      File dir = parseWorkingDirectory(parameters);
      ExecuteSystemCommand.Result result = null;
      if(envp == null) {
        result = ExecuteSystemCommand.run(cmd);
      } else {
        String[] envArray = convertStringListToStringArray(envp,"environment variables",parameters);
        result = ExecuteSystemCommand.run(cmd,envArray,dir);
      }
      return escm.type.Pair.List(new escm.type.String(result.out),new escm.type.String(result.err),new escm.type.Number(result.exit));
    }
  }
}