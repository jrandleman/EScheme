// Author: Jordan Randleman - escm.primitive.SystemPrimitives
// Purpose:
//    Java primitives for system operations.

package escm.primitive;
import java.util.ArrayList;
import java.util.Stack;
import java.util.Calendar;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.Void;
import escm.type.Symbol;
import escm.type.Hashmap;
import escm.type.bool.Boolean;
import escm.type.oo.EscmModule;
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
import escm.vm.util.ObjectAccessChain;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmCallStack;
import escm.vm.runtime.installerGenerated.EscmPath;

public class SystemPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Get the EScheme version number
  public static final double VERSION = 9.0;


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
  public static class Exit extends Primitive {
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
  public static class Load extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "load";
    }

    public static String addStringPaths(String path1, String path2) {
      int n = path1.length();
      if(n == 0) return path2;
      if(path1.charAt(n-1) == File.separatorChar) return path1+path2;
      return path1+File.separator+path2;
    }

    public static String getPath(String dir, String file) {
      int n = dir.length();
      if(n == 0 || FilePrimitives.IsAbsolutePath.logic(file)) return file;
      return addStringPaths(dir,file);
    }

    public static Trampoline.Bounce evalEachExpression(ArrayList<Datum> exprs, int i, Stack<Pair<String,SourceInformation>> originalCallStack, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
      int n = exprs.size();
      if(i >= n) return continuation.run(Void.VALUE);
      Trampoline.Continuation nextContinuation;
      if(i+1 == n) {
        nextContinuation = continuation;
      } else {
        nextContinuation = (value) -> () -> {
          EscmCallStack.restore(originalCallStack); // residue frames may reside after call/cc nonsense
          if(value instanceof escm.type.port.Eof) return continuation.run(Void.VALUE);
          return evalEachExpression(exprs,i+1,originalCallStack,definitionEnvironment,continuation);
        };
      }
      return escm.vm.Compiler.run(exprs.get(i),definitionEnvironment,(compiled) -> () -> {
        return escm.vm.Interpreter.run(new ExecutionState(definitionEnvironment,escm.vm.Assembler.run(compiled)),nextContinuation);
      });
    }

    private static Trampoline.Bounce loadESchemeFile(String primitiveName, String filename, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
      if(SerializationPrimitives.IsSerializedP.logic(filename) == true) {
        return SerializationPrimitives.loadSerializedFile(primitiveName,filename,definitionEnvironment,continuation);
      } else {
        String buffer = FilePrimitives.FileRead.slurpFile(filename,primitiveName);
        ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(filename,buffer);
        return evalEachExpression(exprs,0,EscmCallStack.copy(),definitionEnvironment,continuation);
      }
    }

    public static Trampoline.Bounce logic(String primitiveName, String filename, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
      filename = FilePrimitives.AbsolutePath.logic(filename);
      LoadOnce.registerLoadedFile(definitionEnvironment,filename);
      return loadESchemeFile(primitiveName,filename,definitionEnvironment,continuation);
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      String directory = "", filename = "";
      if(n < 1 || n > 2)
        throw new Exceptionf("'(load <optional-directory> <filename>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
        filename = ((escm.type.String)parameters.get(0)).value();
      } else {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <directory> isn't a string: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
        directory = ((escm.type.String)parameters.get(0)).value();
        filename = ((escm.type.String)parameters.get(1)).value();
      }
      return logic("load",getPath(directory,filename),this.definitionEnvironment,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-once
  public static class LoadOnce extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-once";
    }

    private static Symbol loadOnceFiles = new Symbol("*load-once-files*");

    public static void registerLoadedFile(Environment definitionEnvironment, String filePath) throws Exception {
      Datum fileMap = definitionEnvironment.get(loadOnceFiles);
      if(fileMap instanceof Hashmap) {
        ((Hashmap)fileMap).set(new escm.type.String(filePath),Boolean.FALSE);
      }
    }

    public static boolean notLoadedYet(Environment definitionEnvironment, String filePath) throws Exception {
      Datum fileMap = definitionEnvironment.get(loadOnceFiles);
      return !(fileMap instanceof Hashmap) || !((Hashmap)fileMap).hasKey(new escm.type.String(filePath));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      String directory = "", filename = "";
      if(n < 1 || n > 2)
        throw new Exceptionf("'(load <optional-directory> <filename>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
        filename = ((escm.type.String)parameters.get(0)).value();
      } else {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <directory> isn't a string: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String)) 
          throw new Exceptionf("'(load <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
        directory = ((escm.type.String)parameters.get(0)).value();
        filename = ((escm.type.String)parameters.get(1)).value();
      }
      String filePath = Load.getPath(directory,filename);
      if(notLoadedYet(this.definitionEnvironment,filePath)) 
        return Load.logic("load-once",filePath,this.definitionEnvironment,continuation);
      return continuation.run(Void.VALUE);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // system
  public static class ExecuteCommand extends Primitive {
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


  ////////////////////////////////////////////////////////////////////////////
  // escm-get-module-name
  public static class EscmGetModuleName extends Primitive {
    public java.lang.String escmName() {
      return "escm-get-module-name";
    }

    public static Symbol logic(Symbol modulePath) throws Exception {
      if(ObjectAccessChain.is(modulePath)) {
        ArrayList<Symbol> path = ObjectAccessChain.parse(modulePath);
        return path.get(path.size()-1);
      } 
      return modulePath;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-get-module-name <module-path-symbol>) didn't receive exactly 1 symbol: %s", Exceptionf.profileArgs(parameters));
      return logic((Symbol)parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-load-module
  public static class EscmLoadModule extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "escm-load-module";
    }

    public static Path addPaths(Path path1, Path path2) {
      String s1 = path1.toString(), s2 = path2.toString();
      int n = s1.length();
      if(n == 0) return path2;
      if(s1.charAt(n-1) == File.separatorChar) return Path.of(s1+s2);
      return Path.of(s1+File.separator+s2);
    }

    private static ArrayList<Symbol> getModulePathSymbols(Symbol modulePath) throws Exception {
      if(ObjectAccessChain.is(modulePath)) return ObjectAccessChain.parse(modulePath);
      ArrayList<Symbol> path = new ArrayList<Symbol>();
      path.add(modulePath);
      return path;
    }

    private static String createModuleDirectoryString(ArrayList<Symbol> modulePath) {
      StringBuilder path = new StringBuilder(File.separator);
      for(int i = 0, n = modulePath.size()-1; i < n; ++i) {
        path.append(modulePath.get(i).value());
        path.append(File.separator);
      }
      return path.toString();
    }

    private static String extractFileWithoutExtension(String fileName) throws Exception {
      int idx = fileName.lastIndexOf('.');
      if(idx == -1) return fileName;
      return fileName.substring(0,idx);
    }

    private static String getModuleFileName(Path moduleDirectory, String moduleName) throws Exception {
      for(File entry : (new File(moduleDirectory.toString())).listFiles()) {
        String filename = entry.toPath().getFileName().toString();
        if(filename.length() != 0 && filename.charAt(0) != '.' && extractFileWithoutExtension(filename).equals(moduleName)) {
          return filename;
        }
      }
      return null;
    }

    private static String getModuleAbsoluteFilePath(String moduleName, ArrayList<Symbol> modulePath, String filePath, ArrayList<Datum> parameters) throws Exception {
      Path moduleDirectoryPath = Path.of(createModuleDirectoryString(modulePath));
      Path moduleAbsolutePath = Path.of(FilePrimitives.AbsolutePath.logic(filePath));
      while(moduleAbsolutePath != null) {
        Path p = addPaths(moduleAbsolutePath,moduleDirectoryPath);
        if(FilePrimitives.IsDirectoryP.logic(p)) {
          moduleAbsolutePath = p;
          break;
        }
        moduleAbsolutePath = moduleAbsolutePath.getParent();
      }
      if(moduleAbsolutePath == null)
        throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) module path doesn't exist: %s", Exceptionf.profileArgs(parameters));
      String moduleFileName = getModuleFileName(moduleAbsolutePath,moduleName);
      if(moduleFileName == null)
        throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) module path doesn't exist: %s", Exceptionf.profileArgs(parameters));
      return Load.addStringPaths(moduleAbsolutePath.toString(),moduleFileName);
    }

    private static Environment getModuleEnvironment() throws Exception {
      Environment moduleEnvironment = GlobalState.getJavaPrimitiveEnvironment();
      // Note that we manually interpret our EScheme `stdlib.scm` every time, even if the serialized version
      // is available. For some reason, while serialization decreases boot time (as expected), it can nearly 
      // double import time.
      String escmCode = Files.readString(Path.of(EscmPath.VALUE+File.separator+"src"+File.separator+"stdlib.scm"));
      // Further note that we know our stdlib don't store any continuations using call/cc
      //   upon loading, so we can afford evaluating it with a dummy continuation.
      Trampoline.Continuation terminalContinuation = (ignored) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;
      ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(EscmPath.VALUE+File.separator+"src"+File.separator+"stdlib.scm",escmCode);
      Trampoline.resolve(Load.evalEachExpression(exprs,0,new Stack<Pair<String,SourceInformation>>(),moduleEnvironment,terminalContinuation));
      moduleEnvironment.define(new Symbol("*import*"),Boolean.TRUE); // set <FALSE> in <GlobalState>
      return moduleEnvironment;
    }

    public static Trampoline.Bounce logic(String primitiveName, boolean forcedImport, String moduleName, String absoluteFilePath, Trampoline.Continuation continuation) throws Exception {
      if(forcedImport == false && GlobalState.importedModules.containsKey(absoluteFilePath))
        return continuation.run(GlobalState.importedModules.get(absoluteFilePath));
      Environment moduleEnvironment = getModuleEnvironment();
      EscmModule module = new EscmModule(moduleName,absoluteFilePath,moduleEnvironment);
      GlobalState.importedModules.put(absoluteFilePath,module);
      return Load.loadESchemeFile(primitiveName,absoluteFilePath,moduleEnvironment,(ignore) -> () -> {
        return continuation.run(module);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 1 || n > 2) 
        throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      Symbol moduleName = null;
      String filePath = ""; // defaults to the current working directory!
      if(n == 1) {
        if(!(parameters.get(0) instanceof Symbol)) 
          throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) <module-path> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
        moduleName = (Symbol)parameters.get(0);
        if(moduleName.hasSourceInformation()) {
          filePath = moduleName.source().fileName(); // import relative to the location of the importer's file (if available)
        }
      } else {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) <filepath> isn't a string: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof Symbol)) 
          throw new Exceptionf("'(escm-load-module <optional-filepath-string> <module-path-symbol>) <module-path> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
        filePath = ((escm.type.String)parameters.get(0)).value();
        moduleName = (Symbol)parameters.get(1);
      }
      ArrayList<Symbol> symbolicPath = getModulePathSymbols(moduleName);
      String moduleStringName = symbolicPath.get(symbolicPath.size()-1).value();
      String absoluteFilePath = getModuleAbsoluteFilePath(moduleStringName,symbolicPath,filePath,parameters);
      return logic("escm-load-module",false,moduleStringName,absoluteFilePath,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-reload-module
  public static class EscmReloadModule extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "escm-reload-module";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof EscmModule)) 
        throw new Exceptionf("'(escm-reload-module <module>) didn't receive exactly 1 module: %s", Exceptionf.profileArgs(parameters));
      EscmModule module = (EscmModule)parameters.get(0);
      return EscmLoadModule.logic("escm-reload-module",true,module.name(),module.absoluteFilePath(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // module?
  public static class IsModuleP extends Primitive {
    public java.lang.String escmName() {
      return "module?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(module? <obj>) didn't receive 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof EscmModule);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // module-path
  public static class ModulePath extends Primitive {
    public java.lang.String escmName() {
      return "module-path";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof EscmModule)) 
        throw new Exceptionf("'(module-path <module>) didn't receive exactly 1 module: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((EscmModule)parameters.get(0)).absoluteFilePath());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // module-bindings
  public static class ModuleBindings extends Primitive {
    public java.lang.String escmName() {
      return "module-bindings";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof EscmModule)) 
        throw new Exceptionf("'(module-bindings <module>) didn't receive exactly 1 module: %s", Exceptionf.profileArgs(parameters));
      return ((EscmModule)parameters.get(0)).bindingsAsList();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-define-parameter
  public static class EscmDefineParameter extends Primitive {
    public java.lang.String escmName() {
      return "escm-define-parameter";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-define-parameter <symbol> <obj>) invalid args: %s", Exceptionf.profileArgs(parameters));
      GlobalState.parameterEnvironment.define((Symbol)parameters.get(0),parameters.get(1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-parameter?
  public static class EscmIsParameterP extends Primitive {
    public java.lang.String escmName() {
      return "escm-parameter?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-parameter? <symbol>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(GlobalState.parameterEnvironment.has((Symbol)parameters.get(0)));
    }
  }
}