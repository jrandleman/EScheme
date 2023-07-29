// Author: Jordan Randleman - escm.vm.Main
// Purpose:
//    Main file to start the VM. Call via "launchESchemeSession()".

package escm.vm;
import java.util.ArrayList;
import java.util.Stack;
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.util.Pair;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.EscmCallStack;
import escm.vm.runtime.EscmThread;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.primitive.SystemPrimitives;
import escm.primitive.FilePrimitives;
import escm.primitive.SerializationPrimitives;

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
  private static void resetCurrentPorts() {
    InputPort.setCurrent(InputPort.STDIN);
    OutputPort.setCurrent(OutputPort.STDOUT);
  }


  private static int getLastUniqueStackFrameIndex(StackTraceElement[] stackTrace) {
    int i = stackTrace.length-1;
    while(i > 0 && stackTrace[i].equals(stackTrace[i-1])) --i;
    return i;
  }


  private static void printJavaStackTraceWithoutMessage(Throwable e) {
    StackTraceElement[] stackTrace = e.getStackTrace();
    if(stackTrace.length == 0) return;
    int n = getLastUniqueStackFrameIndex(stackTrace);
    System.err.printf(">> Java Call Stack: %s", stackTrace[0]);
    for(int i = 1; i <= n; ++i) {
      System.err.printf("\n                    %s", stackTrace[i]);
    }
    if(n < stackTrace.length-1) {
      System.err.print("\n                    ^");
      System.err.printf("\n                    %d more calls to \"%s\"", stackTrace.length-1-n, stackTrace[n]);
    }
  }


  public static void reportTopLevelError(Throwable t) {
    resetCurrentPorts();
    if(t instanceof Exception) {
      System.err.printf("\nESCM ERROR: %s\n", t.getMessage());
    } else if(t instanceof StackOverflowError) {
      System.err.println("\nESCM: JAVA STACK-OVERFLOW ERROR: Did you try printing/equating cyclical data?");
    } else {
      System.err.printf("\nESCM: JAVA ERROR: %s\n", t.getMessage());
    }
    EscmCallStack.print();
    System.err.printf(">> Java Throwable: %s\n", t.getClass().getName());
    printJavaStackTraceWithoutMessage(t);
    System.err.println("");
  }


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
    sb.append("      Copyright (c) Jordan Candide Randleman 2021-2023\n");
    sb.append(String.format("                Eerina's Scheme: Version %.1f\n",SystemPrimitives.VERSION));
    sb.append("            Type (help) for Help, (exit) to Exit\n");
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


  private static void launchRepl(ParsedCommandLine parsedCmdLine, Environment globalEnvironment) throws Exception {
    // Note that we DON'T set whether we're in the REPL here, as such risks
    //   causing a read/write race condition should a script loaded by "-l" 
    //   spawn a thread!
    if(parsedCmdLine.launchingQuiet == false) {
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
        reportTopLevelError(t);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our File Interpreter
  private static void loadScript(ParsedCommandLine parsedCmdLine) throws Exception {
    // Initialize the runtime, load the file, & launch REPL if found "-l" prior the filename
    Environment globalEnvironment = GlobalState.getDefaultEnvironment();
    Trampoline.Continuation replIfReplingContinuation = (value) -> () -> {
      if(parsedCmdLine.loadingIntoREPL) launchRepl(parsedCmdLine,globalEnvironment);
      return Trampoline.LAST_BOUNCE_SIGNAL;
    };
    // Note that we set whether we're in the REPL here, as setting in <launchRepl>
    //   could cause a read/write race condition should the loaded script spawn a 
    //   thread!
    if(parsedCmdLine.loadingIntoREPL) GlobalState.inREPL = true; // trigger exit message to be printed
    if(FilePrimitives.IsFileP.logic(Path.of(parsedCmdLine.scriptName)) == false)
      throw new Exceptionf("\"--load\": \"%s\" isn't a file!\n  %s", parsedCmdLine.scriptName, COMMAND_LINE_FLAGS.replaceAll("\n","\n  "));
    Trampoline.resolve(SystemPrimitives.Load.logic("load",parsedCmdLine.scriptName,globalEnvironment,replIfReplingContinuation));
  }


  private static void importScript(ParsedCommandLine parsedCmdLine) throws Exception {
    GlobalState.inREPL = true; // trigger exit message to be printed
    Environment globalEnvironment = GlobalState.getDefaultEnvironment();
    Symbol modulePath = new Symbol(parsedCmdLine.scriptName);
    Symbol moduleName = SystemPrimitives.EscmGetModuleName.logic(modulePath);
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(modulePath);
    Trampoline.resolve((new SystemPrimitives.EscmLoadModule()).callWith(args,(module) -> () -> {
      globalEnvironment.define(moduleName,module);
      launchRepl(parsedCmdLine,globalEnvironment);
      return Trampoline.LAST_BOUNCE_SIGNAL;
    }));
  }


  private static void launchScript(ParsedCommandLine parsedCmdLine) throws Exception {
    if(parsedCmdLine.importingIntoREPL) {
      importScript(parsedCmdLine);
    } else {
      loadScript(parsedCmdLine);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing the <stdlib.scm> Serializer
  private static void serializeStdLib() throws Exception {
    Environment globalEnvironment = GlobalState.getJavaPrimitiveEnvironment();
    String escmStdlibPath = EscmPath.VALUE+File.separator+"src"+File.separator+"stdlib.scm";
    String serStdlibPath = EscmPath.VALUE+File.separator+"bin"+File.separator+"stdlib.ser";
    Trampoline.Continuation terminalContinutation = (ignore) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;
    Trampoline.resolve(SerializationPrimitives.Serialize.logic("serialize",escmStdlibPath,serStdlibPath,globalEnvironment,terminalContinutation));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing the Unit Test Executor
  private static void executeUnitTests() throws Exception {
    ParsedCommandLine parsed = new ParsedCommandLine();
    parsed.scriptName = EscmPath.VALUE+File.separator+"src"+File.separator+"test"+File.separator+"main.scm";
    loadScript(parsed);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse the command-line
  private static class ParsedCommandLine {
    public boolean serializingStdLib = false; // --serialize-stdlib
    public boolean executeUnitTests  = false; // --unit-tests
    public boolean launchingQuiet    = false; // -q --quiet
    public boolean loadingIntoREPL   = false; // -l --load
    public boolean importingIntoREPL = false; // -i --import
    public String scriptName         = null;  // <null> denotes no script
  }


  private static void setArgv(int argvStart, String[] args) {
    Datum argvIterator = escm.type.Nil.VALUE;
    for(int i = args.length-1; i > argvStart; --i)
      argvIterator = new escm.type.Pair(new escm.type.String(args[i]),argvIterator);
    argvIterator = new escm.type.Pair(new escm.type.String(FilePrimitives.AbsolutePath.logic(args[argvStart])),argvIterator);
    GlobalState.setArgv(argvIterator);
  }


  private static ParsedCommandLine parseCommandLine(String[] args) {
    ParsedCommandLine parsed = new ParsedCommandLine();
    for(int i = 0; i < args.length; ++i) {
      switch(args[i]) {
        case "--serialize-stdlib": {
          parsed.serializingStdLib = true;
          return parsed;
        }
        case "--unit-tests": {
          parsed.executeUnitTests = true;
          return parsed;
        }
        case "-v": case "--version": {
          System.out.printf("Eerina's Scheme Version %s\n", SystemPrimitives.VERSION);
          System.out.printf("Target: %s %s\n", System.getProperty("os.name"),System.getProperty("os.version"));
          System.out.printf("InstalledDir: %s\n", EscmPath.VALUE);
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
          setArgv(i+1,args);
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
          setArgv(i+1,args);
          return parsed;
        }
        default: {
          parsed.scriptName = args[i];
          setArgv(i,args);
          return parsed;
        }
      }
    }
    return parsed;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our Interpreter
  public static void launchESchemeSession(String[] args) {
    escm.type.concurrent.Thread mainThread = new escm.type.concurrent.Thread(
      "escm-main",
      (params, cont) -> {
        ParsedCommandLine parsedCmdLine = parseCommandLine(args);
        try {
          if(parsedCmdLine.serializingStdLib == true) {
            serializeStdLib();
          } else if(parsedCmdLine.executeUnitTests == true) {
            executeUnitTests();
          } else if(parsedCmdLine.scriptName == null) {
            Environment globalEnvironment = GlobalState.getDefaultEnvironment();
            GlobalState.inREPL = true; // trigger exit message to be printed
            launchRepl(parsedCmdLine,globalEnvironment);
          } else {
            launchScript(parsedCmdLine);
          }
        } catch(Throwable t) {
          reportTopLevelError(t);
          System.exit(1);
        }
        return cont.run(escm.type.Void.VALUE);
      }
    );
    mainThread.start();
  }
}
