// Author: Jordan Randleman - Main
// Purpose:
//    Main file to start the EScheme VM.
//
// +-----------------------------------------------------------------------+
// | It is no exaggeration to regard this as the most fundamental idea in  |
// | programming:                                                          |
// |    The evaluator, which determines the meaning of expressions in a    |
// |    programming language, is just another program. - SICP vol2, p.360  |
// +-----------------------------------------------------------------------+

import java.util.ArrayList;
import java.io.File;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.port.InputPort;
import escm.util.Trampoline;
import escm.util.error.Exceptionf;
import escm.util.error.TopLevelError;
import escm.vm.Assembler;
import escm.vm.Compiler;
import escm.vm.Interpreter;
import escm.vm.type.callable.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.runtime.EscmCallStack;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.installerGenerated.JvmPathPrefix;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.vm.runtime.installerGenerated.JavaStdLibLoaderGenerator;
import escm.primitive.SystemPrimitives;
import escm.primitive.FilePrimitives;

public class Main {
  ////////////////////////////////////////////////////////////////////////////
  // Command-line flag options
  public static final String COMMAND_LINE_FLAGS = 
    "Command-line flags may be used to modify EScheme's behavior:\n"+
    "  1. -v, --version                    | Print EScheme version information\n"+
    "  2. -h, --help                       | Print this information\n"+
    "  3. -q, --quiet                      | Launch the REPL without ASCII art\n"+
    "  4. -l, --load <script> <arg1> ...   | Load <script> with <arg> ... as *argv* into the REPL\n"+
    "  5. -i, --import <module> <arg1> ... | Import <module> with <arg> ... as *argv* into the REPL\n"+
    "  6. <script> <arg1> ...              | Interpret <script> with <arg> ... as *argv*\n"+
    "  7. [no arguments]                   | Launch the REPL\n";


  ////////////////////////////////////////////////////////////////////////////
  // Evaluate an escm expression in the given environment
  public static void eval(Environment env, Datum d, Trampoline.Continuation continuation) throws Exception {
    Trampoline.resolve(Compiler.run(d,env,(compiled) -> () -> {
      return Interpreter.run(new ExecutionState(env,Assembler.run(compiled)),continuation);
    }));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our REPL
  private static void printReplIntro() {
    StringBuilder sb = new StringBuilder();
    sb.append("                        ,  ,\n");
    sb.append("                     ,  |\\ |\\\n");
    sb.append("                     |\\ '.`-.`-,\n");
    sb.append("                      \\`'-:  `; \\\n");
    sb.append("                       `-._'.  \\ \\\n");
    sb.append("                      ,_.-=`,`.`` ~,      WEET\n");
    sb.append("                       '--,. `    _ \\     WOOT!\n");
    sb.append("                          /  ___ (O),\\   /\n");
    sb.append("                         |  /X22\\  /_ \\\n");
    sb.append("                         |  \\XXI/  \\_\\|\n");
    sb.append("                         |`'.     .(\n");
    sb.append("                     __ / '. `````.'\\\n");
    sb.append("                    /,'`|   `````.-~\"~-.\n");
    sb.append("\n");
    sb.append(              "      Copyright (c) Jordan Candide Randleman 2021-2024\n");
    sb.append(String.format("             Eerina's Scheme (\u03bb): Version %s\n",SystemPrimitives.VERSION));
    sb.append(              "            Type (help) for Help, (exit) to Exit\n");
    sb.append("\n");
    sb.append("                        \u039b\u03c8\u0aea \u039b\u03b9\u03c5 \u03bc\u0391\u0aea\u03a9\n");
    sb.append("                         \u03b4\u0395\u03a4 \u03b4\u03c0\u03a4 \u03b2\u0392\n");
    System.out.print(sb.toString());
  }


  private static Datum readFullExpression() {
    while(true) {
      try {
        Datum readDatum = InputPort.STDIN.readReplDatum();
        // Account for EOF => triggers REPL termination!
        if(readDatum == null) {
          if(GlobalState.getLastPrintedANewline() == false) {
            System.out.print('\n');
          }
          System.out.println(SystemPrimitives.getExitMessage());
          System.exit(0);
        }
        return readDatum;
      } catch(Exception e) {
        System.err.printf("\n%s\n",e.getMessage());
      }
    }
  }


  private static void launchRepl(boolean launchQuietly, Environment globalEnvironment) throws Exception {
    // Note that we DON'T set whether we're in the REPL here, as such risks
    //   causing a read/write race condition should a script loaded by "-l" 
    //   spawn a thread!
    if(launchQuietly == false) {
      printReplIntro();
    } else {
      GlobalState.setLastPrintedANewline(true); // to print opening ">" on the 1st line
    }
    Trampoline.Continuation printContinuation = (value) -> () -> {
      if(!(value instanceof escm.type.Void)) System.out.printf("%s\n", value.pprint());
      return Trampoline.LAST_BOUNCE_SIGNAL;
    };
    while(true) {
      try {
        eval(globalEnvironment,readFullExpression(),printContinuation);
        EscmCallStack.clear(); // residue frames may reside after call/cc nonsense
      } catch(Throwable t) {
        TopLevelError.report(t);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our File Interpreter
  private static void loadScript(ParsedCommandLine parsedCmdLine) throws Exception {
    if(FilePrimitives.IsFileP.logic(Path.of(parsedCmdLine.scriptName)) == false)
      throw new Exceptionf("\"--load\": \"%s\" isn't a file!\n  %s", parsedCmdLine.scriptName, COMMAND_LINE_FLAGS.replaceAll("\n","\n  "));
    if(parsedCmdLine.loadingIntoREPL) GlobalState.inREPL = true; // trigger exit message to be printed
    Environment globalEnvironment = GlobalState.getDefaultEnvironment();
    Trampoline.resolve(SystemPrimitives.Load.logic("load",parsedCmdLine.scriptName,globalEnvironment,(value) -> () -> {
      if(parsedCmdLine.loadingIntoREPL) launchRepl(parsedCmdLine.launchingQuiet,globalEnvironment);
      return Trampoline.LAST_BOUNCE_SIGNAL;
    }));
  }


  private static void importScript(ParsedCommandLine parsedCmdLine) throws Exception {
    GlobalState.inREPL = true; // trigger exit message to be printed
    Symbol modulePath = new Symbol(parsedCmdLine.scriptName);
    Symbol moduleName = SystemPrimitives.EscmGetModuleName.logic(modulePath);
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(modulePath);
    Environment globalEnvironment = GlobalState.getDefaultEnvironment();
    try {
      Trampoline.resolve((new SystemPrimitives.EscmLoadModule()).callWith(args,(module) -> () -> {
        globalEnvironment.define(moduleName,module);
        launchRepl(parsedCmdLine.launchingQuiet,globalEnvironment);
        return Trampoline.LAST_BOUNCE_SIGNAL;
      }));
    } catch(SystemPrimitives.EscmLoadModule.InvalidModuleException e) {
      throw new Exceptionf("\"--import\": \"%s\" isn't a module!\n  %s", parsedCmdLine.scriptName, COMMAND_LINE_FLAGS.replaceAll("\n","\n  "));
    }
  }


  private static void launchScript(ParsedCommandLine parsedCmdLine) throws Exception {
    if(parsedCmdLine.importingIntoREPL) {
      importScript(parsedCmdLine);
    } else {
      loadScript(parsedCmdLine);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing the Unit Test Executor
  private static void executeUnitTests() throws Exception {
    ParsedCommandLine parsed = new ParsedCommandLine();
    parsed.scriptName = EscmPath.VALUE+File.separator+"test"+File.separator+"main.scm";
    loadScript(parsed);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing Java StdLib Loader Generation
  private static void generateJavaStdLibLoader() throws Exception {
    JavaStdLibLoaderGenerator.execute();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse the command-line
  private static class ParsedCommandLine {
    public boolean executeUnitTests          = false; // --unit-tests
    public boolean generateJavaStdLibLoader  = false; // --generate-java-stdlib-loader
    public boolean launchingQuiet            = false; // -q --quiet
    public boolean loadingIntoREPL           = false; // -l --load
    public boolean importingIntoREPL         = false; // -i --import
    public String scriptName                 = null;  // <null> denotes no script
  }


  private static String getJavaBinPath() {
    return JvmPathPrefix.VALUE.substring(0,JvmPathPrefix.VALUE.length()-File.separator.length());
  }


  private static ParsedCommandLine parseCommandLine(String[] args) {
    ParsedCommandLine parsed = new ParsedCommandLine();
    for(int i = 0; i < args.length; ++i) {
      switch(args[i]) {
        case "--unit-tests": {
          parsed.executeUnitTests = true;
          return parsed;
        }
        case "--generate-java-stdlib-loader": {
          parsed.generateJavaStdLibLoader = true;
          return parsed;
        }
        case "-v": case "--version": {
          System.out.printf("Eerina's Scheme Version %s\n", SystemPrimitives.VERSION);
          System.out.printf("Target: %s %s\n", System.getProperty("os.name"),System.getProperty("os.version"));
          System.out.printf("InstalledDir: %s\n", EscmPath.VALUE);
          System.out.printf("JavaBin: %s\n", getJavaBinPath());
          System.exit(0);
        }
        case "-h": case "--help": {
          System.out.print(COMMAND_LINE_FLAGS);
          System.exit(0);
        }
        case "-q": case "--quiet": {
          parsed.launchingQuiet = true;
          break;
        }
        case "-l": case "--load": {
          parsed.loadingIntoREPL = true;
          if(i+1 == args.length) {
            System.err.printf("ESCM ERROR: No filename given to load into the REPL with \"%s\"!\n", args[i]);
            System.err.print(COMMAND_LINE_FLAGS);
            System.exit(1);
          }
          parsed.scriptName = args[i+1];
          GlobalState.setArgv(i+1,args);
          return parsed;
        }
        case "-i": case "--import": {
          parsed.importingIntoREPL = true;
          if(i+1 == args.length) {
            System.err.printf("ESCM ERROR: No filename given to import into the REPL with \"%s\"!\n", args[i]);
            System.err.print(COMMAND_LINE_FLAGS);
            System.exit(1);
          }
          parsed.scriptName = args[i+1];
          GlobalState.setArgv(i+1,args);
          return parsed;
        }
        default: {
          parsed.scriptName = args[i];
          GlobalState.setArgv(i,args);
          return parsed;
        }
      }
    }
    return parsed;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our Interpreter
  public static void main(String[] args) {
    escm.type.concurrent.Thread mainThread = new escm.type.concurrent.Thread(
      "escm-main",
      new Callable() {
        public String docstring() {
          return "EScheme's main thread runnable. Starts the EScheme VM.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(escm.type.concurrent.Thread.DEFAULT_RUNNABLE_NAME));
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          ParsedCommandLine parsedCmdLine = parseCommandLine(args);
          try {
            if(parsedCmdLine.generateJavaStdLibLoader == true) {
              generateJavaStdLibLoader();
            } else if(parsedCmdLine.executeUnitTests == true) {
              executeUnitTests();
            } else if(parsedCmdLine.scriptName == null) {
              GlobalState.inREPL = true; // trigger exit message to be printed
              launchRepl(parsedCmdLine.launchingQuiet,GlobalState.getDefaultEnvironment());
            } else {
              launchScript(parsedCmdLine);
            }
          } catch(Throwable t) {
            TopLevelError.report(t);
            System.exit(1);
          }
          return cont.run(escm.type.Void.VALUE);
        }
      }
    );
    mainThread.start();
  }
}