// Author: Jordan Randleman - escm.primitive.SystemPrimitives
// Purpose:
//    Java primitives for system operations.

package escm.primitive;
import java.util.ArrayList;
import java.util.Stack;
import java.util.Calendar;
import java.io.File;
import escm.type.Datum;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.Void;
import escm.util.Pair;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.ExecuteSystemCommand;
import escm.vm.type.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmCallStack;

public class SystemPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Get the EScheme version number
  public static final double VERSION = 7.0;


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
        if(!(exitCode instanceof Real) || !((Real)exitCode).isInteger())
          throw new Exceptionf("'(exit <optional-code>) code %s isn't an integer: %s", exitCode.profile(), Exceptionf.profileArgs(parameters));
        code = ((Real)exitCode).intValue();
      }
      // Print the exit msg iff in a REPL session
      if(GlobalState.inREPL) {
        if(!GlobalState.getLastPrintedANewline()) System.out.println("");
        System.out.println(getExitMessage());
      }
      System.exit(code);
      return Void.VALUE; // never triggered
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load
  public static class Load implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load";
    }

    private static Trampoline.Bounce evalEachExpression(ArrayList<Datum> exprs, int i, Stack<Pair<String,SourceInformation>> originalCallStack, Trampoline.Continuation continuation) throws Exception {
      int n = exprs.size();
      if(i >= n) return continuation.run(Void.VALUE);
      Trampoline.Continuation nextContinuation;
      if(i+1 == n) {
        nextContinuation = continuation;
      } else {
        nextContinuation = (value) -> () -> {
          EscmCallStack.restore(originalCallStack); // residue frames may reside after call/cc nonsense
          if(value instanceof escm.type.port.Eof) return continuation.run(Void.VALUE);
          return evalEachExpression(exprs,i+1,originalCallStack,continuation);
        };
      }
      return escm.vm.Compiler.run(exprs.get(i),(compiled) -> () -> {
        return escm.vm.Interpreter.run(new ExecutionState(GlobalState.globalEnvironment,escm.vm.Assembler.run(compiled)),nextContinuation);
      });
    }

    public static Trampoline.Bounce loadESchemeFile(String primitiveName, String filename, Trampoline.Continuation continuation) throws Exception {
      String buffer = FilePrimitives.FileRead.slurpFile(filename,primitiveName);
      ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(filename,buffer);
      return evalEachExpression(exprs,0,EscmCallStack.copy(),continuation);
    }

    public static Trampoline.Bounce logic(String primitiveName, String filename, Trampoline.Continuation continuation) throws Exception {
      if(SerializationPrimitives.IsSerializedP.logic(filename) == true) {
        return SerializationPrimitives.loadSerializedFile(primitiveName,filename,continuation);
      } else {
        return loadESchemeFile(primitiveName,filename,continuation);
      }
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(load <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return logic("load",((escm.type.String)parameters.get(0)).value(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-from
  public static class LoadFrom implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-from";
    }

    public static String getPath(String dir, String file) {
      int n = dir.length();
      if(FilePrimitives.IsAbsolutePath.logic(file) || n == 0) return file;
      if(dir.charAt(n-1) == File.separatorChar) return dir + file;
      return dir + File.separator + file;
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(load-from <directory> <filename>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      Datum directory = parameters.get(0);
      Datum file = parameters.get(1);
      if(!(directory instanceof escm.type.String))
        throw new Exceptionf("'(load-from <directory> <filename>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      if(!(file instanceof escm.type.String))
        throw new Exceptionf("'(load-from <directory> <filename>) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      String filePath = getPath(((escm.type.String)directory).value(),((escm.type.String)file).value());
      return Load.logic("load-from",filePath,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-once
  public static class LoadOnce implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-once";
    }

    public static void registerLoadedFile(String filePath) {
      GlobalState.loadedOnceFiles.put(filePath,0);
    }

    public static boolean notLoadedYet(String filePath) {
      return GlobalState.loadedOnceFiles.containsKey(filePath) == false;
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(load-once <filename>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      String filePath = FilePrimitives.AbsolutePath.logic(((escm.type.String)parameters.get(0)).value());
      if(notLoadedYet(filePath)) {
        registerLoadedFile(filePath);
        return Load.logic("load-once",filePath,continuation);
      }
      return continuation.run(Void.VALUE);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-once-from
  public static class LoadOnceFrom implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-once-from";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(load-once-from <directory> <filename>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      Datum directory = parameters.get(0);
      Datum file = parameters.get(1);
      if(!(directory instanceof escm.type.String))
        throw new Exceptionf("'(load-once-from <directory> <filename>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      if(!(file instanceof escm.type.String))
        throw new Exceptionf("'(load-once-from <directory> <filename>) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      String filePath = LoadFrom.getPath(((escm.type.String)directory).value(),((escm.type.String)file).value());
      if(LoadOnce.notLoadedYet(filePath)) {
        LoadOnce.registerLoadedFile(filePath);
        return Load.logic("load-once-from",filePath,continuation);
      }
      return continuation.run(Void.VALUE);
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
      return escm.type.Pair.List(new escm.type.String(result.out),new escm.type.String(result.err),new Exact(result.exit));
    }
  }
}