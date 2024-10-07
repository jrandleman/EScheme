// Author: Jordan Randleman - escm.primitive.SystemPrimitives
// Purpose:
//    Java primitives for system operations.

package escm.primitive;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Map;
import java.io.File;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Void;
import escm.type.Symbol;
import escm.type.Hashmap;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.bool.Boolean;
import escm.type.oo.EscmModule;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.util.ExecuteSystemCommand;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.callable.Signature;
import escm.vm.util.ObjectAccessChain;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.EscmCallStack;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.vm.runtime.installerGenerated.JvmPathPrefix;

public class SystemPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Get the EScheme version number
  public static final String VERSION = "1.0.0";


  ////////////////////////////////////////////////////////////////////////////
  // Get the EScheme command-line execution command
  public static final String ESCM_EXECUTION_COMMAND = " "+JvmPathPrefix.VALUE+"java -classpath \""+EscmPath.VALUE+File.separator+"bin\" Main ";


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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("exit")),
        Pair.List(new Symbol("exit"),new Symbol("<integer-code>")));
    }

    public String docstring() {
      return "@help:Procedures:System\nTerminate the current EScheme session with <integer-code> (defaults to 0).";
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("load"),new Symbol("<filename-string>")),
        Pair.List(new Symbol("load"),new Symbol("<directory-string>"),new Symbol("<filename-string>")));
    }

    public String docstring() {
      return "@help:Procedures:System\nReads and evaluates <filename-str>'s EScheme contents in the global environment.\nWorks for both regular & <serialize>d EScheme files.\nIf given <directory-str>, loads <filename-str> from <directory-str>. Use:\n\n  (load #path <filename-str>)\n\nas a portable alternative to (load <filename-str>) if <filename-str> is a\nrelative path, since <load> only operates relative to (current-directory).\n\nNote that <load-once> should be preferred to prevent cyclic loading.\n";
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

    public static Trampoline.Bounce evalEachExpression(ArrayList<Datum> exprs, int i, EscmCallStack.Frame originalCallStack, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
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
        return evalEachExpression(exprs,0,EscmCallStack.currentStackFrame(),definitionEnvironment,continuation);
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("load-once"),new Symbol("<filename-string>")),
        Pair.List(new Symbol("load-once"),new Symbol("<directory-string>"),new Symbol("<filename-string>")));
    }

    public String docstring() {
      return "@help:Procedures:System\nWorks exactly like <load>, but only loads unloaded files. Use:\n\n  (load-once #path <filename-str>)\n\nas a portable alternative to (load-once <filename-str>) if <filename-str> is a\nrelative path, since <load-once> only operates relative to (current-directory).";
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
        throw new Exceptionf("'(load-once <optional-directory> <filename>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load-once <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
        filename = ((escm.type.String)parameters.get(0)).value();
      } else {
        if(!(parameters.get(0) instanceof escm.type.String)) 
          throw new Exceptionf("'(load-once <optional-directory> <filename>) <directory> isn't a string: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1) instanceof escm.type.String)) 
          throw new Exceptionf("'(load-once <optional-directory> <filename>) <filename> isn't a string: %s", Exceptionf.profileArgs(parameters));
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("system"),new Symbol("<command-str>")),
        Pair.List(new Symbol("system"),new Symbol("<command-str>"),new Symbol("<env-var-str-list>")),
        Pair.List(new Symbol("system"),new Symbol("<command-str>"),new Symbol("<directory-str>")),
        Pair.List(new Symbol("system"),new Symbol("<command-str>"),new Symbol("<env-var-str-list>"),new Symbol("<directory-str>")),
        Pair.List(new Symbol("system"),new Symbol("<millisecond-timeout>"),new Symbol("<command-str>")),
        Pair.List(new Symbol("system"),new Symbol("<millisecond-timeout>"),new Symbol("<command-str>"),new Symbol("<env-var-str-list>")),
        Pair.List(new Symbol("system"),new Symbol("<millisecond-timeout>"),new Symbol("<command-str>"),new Symbol("<directory-str>")),
        Pair.List(new Symbol("system"),new Symbol("<millisecond-timeout>"),new Symbol("<command-str>"),new Symbol("<env-var-str-list>"),new Symbol("<directory-str>")));
    }

    public String docstring() {
      return "@help:Procedures:System\nExecutes a command, using the environment variable bindings in\n<env-var-str-list> (defaults to those of the current environment), \nin the <directory-str> directory (defaults to the current working \ndirectory).\n\nNote that each environment variable string in <env-var-str-list>\nshould follow the \"name=value\" format.\n\nIf <millisecond-timeout> (a real number) is given and exceeded, the\nspawned process will terminate. Note that this is NOT a hard cap \nthough: hence passing 0 as <millisecond-timeout> may still have \nsystem-wide side effects.\n\nUltimately passed to Java's <Runtime.getRuntime().exec()>.\nReferenced by <escm>.\n\nReturns a list:\n  (<command-stdout-str> <command-stderr-str> <command-exit-code>)";
    }
    
    private static String[] convertStringListToStringArray(Datum list, String listContentType, ArrayList<Datum> parameters) throws Exception {
      if(list == null) return null;
      ArrayList<String> strs = new ArrayList<String>();
      while(list instanceof escm.type.Pair) {
        escm.type.Pair par = (escm.type.Pair)list;
        Datum content = par.car();
        if(!(content instanceof escm.type.String))
          throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) %s list value %s isn't a string: %s", listContentType, content.profile(), Exceptionf.profileArgs(parameters));
        strs.add(((escm.type.String)content).value());
        list = par.cdr();
      }
      return strs.toArray(new String[strs.size()]);
    }

    public static Long parseMillisecondTimeout(ArrayList<Datum> parameters) throws Exception {
      Datum timeout = parameters.get(0);
      if(!(timeout instanceof Real)) return null;
      Real r = (Real)timeout;
      if(r.isNaN() || r.isNegative()) return (long)0;
      if(r.isPositive() && r.isInfinite()) return null;
      return r.longValue();
    }

    private static String parseCommand(Long timeout, ArrayList<Datum> parameters) throws Exception {
      int cmdIdx = timeout == null ? 0 : 1;
      if(parameters.size() < cmdIdx+1)
        throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) invalid number of args: %s", Exceptionf.profileArgs(parameters));
      Datum cmd = parameters.get(cmdIdx);
      if(cmd instanceof escm.type.String) return ((escm.type.String)cmd).value();
      throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <cmds> isn't a string: %s", Exceptionf.profileArgs(parameters));
    }

    private static String[] parseEnvironmentVariables(Long timeout, ArrayList<Datum> parameters) throws Exception {
      int envpIdx = timeout == null ? 1 : 2;
      if(parameters.size() < envpIdx+1) return null;
      Datum envp = parameters.get(envpIdx);
      if(envp instanceof escm.type.String) return null; // working directory
      if(escm.type.Pair.isList(envp)) return convertStringListToStringArray(envp,"environment variables",parameters);
      throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <env-vars> isn't a str list: %s", Exceptionf.profileArgs(parameters));
    }

    private static File parseWorkingDirectory(Long timeout, String[] envp, ArrayList<Datum> parameters) throws Exception {
      int dirIdx = timeout == null ? 2 : 3;
      if(envp == null) --dirIdx;
      if(parameters.size() < dirIdx+1) return null;
      Datum dir = parameters.get(dirIdx);
      if(dir instanceof escm.type.String) return new File(((escm.type.String)dir).value());
      throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) <dir> isn't a str: %s", Exceptionf.profileArgs(parameters));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 4) 
        throw new Exceptionf("'(system <optional-millisecond-timeout> <cmd-str> <optional-env-var-str-list> <optional-dir-str>) invalid number of args: %s", Exceptionf.profileArgs(parameters));
      Long timeout = parseMillisecondTimeout(parameters);
      String cmd = parseCommand(timeout,parameters);
      String[] envp = parseEnvironmentVariables(timeout,parameters);
      File dir = parseWorkingDirectory(timeout,envp,parameters);
      ExecuteSystemCommand.Result result = null;
      if(timeout == null) {
        result = ExecuteSystemCommand.run(cmd,envp,dir);
      } else {
        result = ExecuteSystemCommand.run(timeout.longValue(),cmd,envp,dir);
      }
      return escm.type.Pair.List(new escm.type.String(result.out),new escm.type.String(result.err),new Exact(result.exit));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm
  public static class ExecuteEscmProgram extends Primitive {
    public java.lang.String escmName() {
      return "escm";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("escm"),new Symbol("<escm-file>")),
        Pair.List(new Symbol("escm"),new Symbol("<escm-file>"),new Symbol("<argv>"),Signature.VARIADIC),
        Pair.List(new Symbol("escm"),new Symbol("<millisecond-timeout>"),new Symbol("<escm-file>")),
        Pair.List(new Symbol("escm"),new Symbol("<millisecond-timeout>"),new Symbol("<escm-file>"),new Symbol("<argv>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Procedures:System\nExecute an EScheme program in a seperate process. Effectively a wrapper \naround <system> that references <*escm-execution-command*>. Displays each \n<argv> to generate a single command string with <escm-file>.\n\nIf <millisecond-timeout> (a real number) is given and exceeded, the\nspawned process will terminate. Note that this is NOT a hard cap \nthough: hence passing 0 as <millisecond-timeout> may still have \nsystem-wide side effects.\n\nReturns a list:\n  (<program-stdout-str> <program-stderr-str> <program-exit-code>)";
    }

    private static String parseEscmProgramCommand(Long timeout, ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      int cmdIdx = timeout == null ? 0 : 1;
      if(n < cmdIdx+1)
        throw new Exceptionf("'(escm <optional-millisecond-timeout> <escm-file> <optional-argv> ...) invalid number of args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(cmdIdx) instanceof escm.type.String))
        throw new Exceptionf("'(escm <optional-millisecond-timeout> <escm-file> <optional-argv> ...) <escm-file> isn't a string: %s", Exceptionf.profileArgs(parameters));
      StringBuilder sb = new StringBuilder(ESCM_EXECUTION_COMMAND);
      for(; cmdIdx < n; ++cmdIdx) {
        sb.append(" ");
        sb.append(parameters.get(cmdIdx).display());
      }
      return sb.toString();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(escm <optional-millisecond-timeout> <escm-file> <optional-argv> ...) invalid number of args: %s", Exceptionf.profileArgs(parameters));
      Long timeout = ExecuteCommand.parseMillisecondTimeout(parameters);
      String cmd = parseEscmProgramCommand(timeout,parameters);
      ExecuteSystemCommand.Result result = null;
      if(timeout == null) {
        result = ExecuteSystemCommand.run(cmd);
      } else {
        result = ExecuteSystemCommand.run(timeout.longValue(),cmd);
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

    public Datum signature() {
      return Pair.List(new Symbol("escm-get-module-name"),new Symbol("<module-path-symbol>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nExtracts the ultimate module name from <module-path-symbol>.\nFor example:\n  * (escm-get-module-name 'Module) ; 'Module\n  * (escm-get-module-name 'Folder1.Folder2.Module) ; 'Module";
    }

    public static Symbol logic(Symbol modulePath) throws Exception {
      if(ObjectAccessChain.is(modulePath)) {
        Symbol[] path = ObjectAccessChain.parse(modulePath);
        return path[path.length-1];
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

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("escm-load-module"),new Symbol("<module-path-symbol>")),
        Pair.List(new Symbol("escm-load-module"),new Symbol("<filepath-string>"),new Symbol("<module-path-symbol>")));
    }

    public String docstring() {
      return "@help:Procedures:System\n<import> quotes its <module-path> argument & passes it to this function,\nwhich then returns the generated <module> object to be bound to a name.";
    }

    public static class InvalidModuleException extends Exceptionf {
      public InvalidModuleException(String fmt, Object ... args) {
        super(String.format(fmt,args));
      }
    }

    public static Path addPaths(Path path1, Path path2) {
      String s1 = path1.toString(), s2 = path2.toString();
      int n = s1.length();
      if(n == 0) return path2;
      if(s1.charAt(n-1) == File.separatorChar) return Path.of(s1+s2);
      return Path.of(s1+File.separator+s2);
    }

    private static Symbol[] getModulePathSymbols(Symbol modulePath) throws Exception {
      if(ObjectAccessChain.is(modulePath)) return ObjectAccessChain.parse(modulePath);
      Symbol[] path = new Symbol[1];
      path[0] = modulePath;
      return path;
    }

    private static String createModuleDirectoryString(Symbol[] modulePath) {
      StringBuilder path = new StringBuilder(File.separator);
      for(int i = 0, n = modulePath.length-1; i < n; ++i) {
        path.append(modulePath[i].value());
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

    private static String getModuleAbsoluteFilePath(String moduleName, Symbol[] modulePath, String filePath, ArrayList<Datum> parameters) throws Exception {
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
        throw new InvalidModuleException("'(escm-load-module <optional-filepath-string> <module-path-symbol>) module path doesn't exist: %s", Exceptionf.profileArgs(parameters));
      String moduleFileName = getModuleFileName(moduleAbsolutePath,moduleName);
      if(moduleFileName == null)
        throw new InvalidModuleException("'(escm-load-module <optional-filepath-string> <module-path-symbol>) module path doesn't exist: %s", Exceptionf.profileArgs(parameters));
      return Load.addStringPaths(moduleAbsolutePath.toString(),moduleFileName);
    }

    public static Environment getModuleEnvironment() throws Exception {
      Environment moduleEnvironment = GlobalState.getDefaultEnvironment();
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
      Symbol[] symbolicPath = getModulePathSymbols(moduleName);
      String moduleStringName = symbolicPath[symbolicPath.length-1].value();
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

    public Datum signature() {
      return Pair.List(new Symbol("escm-reload-module"),new Symbol("<module>"));
    }

    public String docstring() {
      return "@help:Procedures:System\n<reload> quotes its <module-alias-symbol> argument & passes it to this\nfunction, which then returns the generated <module> object to be bound\nto <reload>'s <module-alias-symbol>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("module?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nReturns whether <obj> is a module object.";
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

    public Datum signature() {
      return Pair.List(new Symbol("module-path"),new Symbol("<module>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nReturns the absolute file path of <module>'s original location.";
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

    public Datum signature() {
      return Pair.List(new Symbol("module-bindings"),new Symbol("<module>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nReturns a list of the symbols defined in <module>.\nBe warned: every module has its own copy of the standard library defined too!";
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

    public Datum signature() {
      return Pair.List(new Symbol("escm-define-parameter"),new Symbol("<symbol>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nInternal primitive used by <define-parameter>, which passes its quoted\ntarget variable to this function.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-define-parameter <symbol> <obj>) invalid args: %s", Exceptionf.profileArgs(parameters));
      GlobalState.parameterEnvironment.define((Symbol)parameters.get(0),parameters.get(1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-set-parameter!
  public static class EscmSetParameterBang extends Primitive {
    public java.lang.String escmName() {
      return "escm-set-parameter!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-set-parameter!"),new Symbol("<symbol>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nInternal primitive used by <set-parameter!>, which passes its quoted\ntarget variable to this function.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-set-parameter! <symbol> <obj>) invalid args: %s", Exceptionf.profileArgs(parameters));
      GlobalState.parameterEnvironment.set((Symbol)parameters.get(0),parameters.get(1));
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-get-parameter
  public static class EscmGetParameter extends Primitive {
    public java.lang.String escmName() {
      return "escm-get-parameter";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-get-parameter"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nInternal primitive used by <get-parameter>, which passes its quoted\ntarget variable to this function.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-get-parameter <symbol>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return GlobalState.parameterEnvironment.get((Symbol)parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-parameter?
  public static class EscmIsParameterP extends Primitive {
    public java.lang.String escmName() {
      return "escm-parameter?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-parameter?"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Procedures:System\nInternal primitive used by <parameter?>, which passes its quoted\ntarget variable to this function.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol)) 
        throw new Exceptionf("'(escm-parameter? <symbol>) invalid args: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(GlobalState.parameterEnvironment.has((Symbol)parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // getenv
  public static class GetEnv extends Primitive {
    public java.lang.String escmName() {
      return "getenv";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("getenv")),
        Pair.List(new Symbol("getenv"),new Symbol("<variable-name-string>")));
    }

    public String docstring() {
      return "@help:Procedures:System\nIf given no arguments, return a hashmap of name:value string environment \nvariable associations.\n\nIf given an environment variable name string, returns its string value. \nIf the given string is not an accessable environment variable, returns #f";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n > 1)
        throw new Exceptionf("'(getenv <optional-var-name-str>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        Datum varName = parameters.get(0);
        if(!(varName instanceof escm.type.String))
          throw new Exceptionf("'(getenv <optional-var-name-str>) <var-name> isn't a string: %s", Exceptionf.profileArgs(parameters));
        try {
          String value = System.getenv(((escm.type.String)varName).value());
          if(value == null) return Boolean.FALSE;
          return new escm.type.String(value);
        } catch(Exception e) {
          return Boolean.FALSE;
        }
      } else {
        Map<String, String> env = System.getenv();
        Hashmap vars = new Hashmap();
        for (String envName : env.keySet()) {
          vars.set(new escm.type.String(envName), new escm.type.String(env.get(envName)));
        }
        return vars;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // garbage-collector
  public static class GarbageCollector extends Primitive {
    public java.lang.String escmName() {
      return "garbage-collector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("garbage-collector"));
    }

    public String docstring() {
      return "@help:Procedures:System\nHints the JVM to launch its garbage collector (GC). Does not guarentee\nimmediate GC execution.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(garbage-collector) invalid args: %s", Exceptionf.profileArgs(parameters));
      System.gc();
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-stack
  public static class CallStack extends Primitive {
    public java.lang.String escmName() {
      return "call-stack";
    }

    public Datum signature() {
      return Pair.List(new Symbol("call-stack"));
    }

    public String docstring() {
      return "@help:Procedures:System\nReturns the current call-stack (prior to calling the primitive) as an\nassociative list: ((<function-name-string> <source-information>) ...)\n  * <source-information> := #f ; if doesn't exist, else:\n                          | (<filename-string> <line-number> <column-number>)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(call-stack) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum callStack = EscmCallStack.toDatum();
      // don't include this call to <call-stack>
      if(callStack instanceof escm.type.Pair) {
        return ((escm.type.Pair)callStack).cdr();
      }
      return callStack;
    }
  }
}