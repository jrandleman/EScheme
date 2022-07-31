// Author: Jordan Randleman - escm.vm.Main
// Purpose:
//    Main file to start the VM. Call via "launchESchemeSession()".

package escm.vm;
import java.util.ArrayList;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;
import escm.vm.runtime.EscmCallStack;
import escm.vm.runtime.EscmThread;
import escm.vm.runtime.GlobalState;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.primitive.SystemPrimitives;
import escm.primitive.FilePrimitives;

public class Main {
  ////////////////////////////////////////////////////////////////////////////
  // Command-line flag options
  public static final String COMMAND_LINE_FLAGS = 
    "Command-line flags may be used to modify EScheme's behavior:\n"+
    "  1. -v, --version                  | Print EScheme version information\n"+
    "  2. -h, --help                     | Print this information\n"+
    "  3. -q, --quiet                    | Launch the REPL without ASCII art\n"+
    "  4. -l, --load <script> <arg1> ... | Load <script> with <arg> ... as *argv* into the REPL\n"+
    "  5. <script> <arg1> ...            | Interpret <script> with <arg> ... as *argv*\n"+
    "  6. [no arguments]                 | Launch the REPL\n";


  ////////////////////////////////////////////////////////////////////////////
  // Evaluate an escm expression in the given environment
  public static void eval(Environment env, Datum d, Trampoline.Continuation continuation) throws Exception {
    Trampoline.resolve(Compiler.run(d,(compiled) -> () -> {
      return Interpreter.run(new ExecutionState(env,Assembler.run(compiled)),continuation);
    }));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our REPL
  private static void resetCurrentPorts() {
    InputPort.setCurrent(InputPort.STDIN);
    OutputPort.setCurrent(OutputPort.STDOUT);
  }


  private static void printJavaStackTraceWithoutMessage(Exception e) {
    StackTraceElement[] stackTrace = e.getStackTrace();
    if(stackTrace.length == 0) return;
    System.err.printf(">> Java Call Stack: %s", stackTrace[0]);
    for(int i = 1; i < stackTrace.length; ++i) {
      System.err.printf("\n                    %s", stackTrace[i]);
    }
  }


  public static void reportTopLevelException(Exception e) {
    resetCurrentPorts();
    System.err.printf("\nESCM ERROR: %s\n", e);
    EscmCallStack.print();
    printJavaStackTraceWithoutMessage(e);
    System.err.println("");
  }


  private static void printReplIntro() {
    StringBuilder sb = new StringBuilder();
    sb.append("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
    sb.append("J               _.._              _       @@@@@@@@@@@@@@@@          T\n");
    sb.append("E             .'@@'`` *          / *      @      __      @          U\n");
    sb.append("@            /@@/   *****        \\_/      @     /@@\\     @          @ EEEEEEEEEEEEEE   SSSSSSSSSSSSS      CCCCCCCMMMM             MMMM\n");
    sb.append("T     |      @@(     * *          |       @     \\@@/     @    |     E E::::::::::::E SS:::::::::::::S  CCC::::::CM:::M           M:::M\n");
    sb.append("'    /#\\     \\@@'.___.;          /#\\      @              @   /#\\    S E::::::::::::ES::::SSSSSS:::::S C:::::::::CM::::M         M::::M\n");
    sb.append("A   (###)     '.@@@@.'         /#####\\    @@@@@@@@@@@@@@@@  (###)   @ EE:::EEEEEE::ES::::S     SSSSSSC::::CCCC::CM:::::M       M:::::M\n");
    sb.append("D    | |    |    ``          /#########\\                |    | |    L   E::E    EEEES::::S          C:::C   CCCCCM::::::M     M::::::M\n");
    sb.append("O    |@|   /#\\|     /\\     /#############\\     /\\     |/#\\   |@|    '   E::E        S::::S          C:::C        M:::::::M   M:::::::M\n");
    sb.append("R    | |  (##/#\\   /##\\   (###############)   /##\\   /#\\##)  | |    A   E::E         S:::SSSS       C:::C        M:::M::::M M::: M:::M\n");
    sb.append("E    |@|   |(###) (####)  _\\#############/_  (####) (###)|   |@|    M   E:::EEEEEE    SS:::::SSSS   C:::C        M:::MM::::M::::MM:::M\n");
    sb.append("@    | |   |#| |___|/\\|__|_________________|__|/\\|___| |#|   | |    O   E::::::::E      SS:::::::SS C:::C        M:::M M:::::::M M:::M\n");
    sb.append("J    |@|   |#|@|###||||###|######/^\\######|###||||###|@|#|   |@|    U   E:::EEEEEE        SSSSSS:::SC:::C        M:::M  M:::::M  M:::M\n");
    sb.append("A    | |   |#| |###|##|#T#|####/     \\####|#1#|##|###| |#|   | |    R   E::E                   S::::S:::C        M:::M   M:::M   M:::M\n");
    sb.append("A    |@|   |#|@|###|/\\|#A#|##/         \\##|#0#|/\\|###|@|#|   |@|    @   E::E    EEEE           S::::SC:::C   CCCCM:::M    MMM    M:::M\n");
    sb.append("N    | |   |#| |###||||#S#|#(           )#|#2#||||###| |#|   | |    D EE:::EEEEEE::ESSSSSS     S::::SC::::CCCC::CM:::M     M     M:::M\n");
    sb.append("-    |@|   |#|@|###|##|#N#|##\\         /##|#2#|##|###|@|#|   |@|    E E::::::::::::ES:::::SSSSSS::::S C:::::::::CM:::M           M:::M\n");
    sb.append("M    | |   |#| |###|/\\|#I#|###|       |###|#2#|/\\|###| |#|   | |    @ E::::::::::::ES:::::::::::::SS   CCC::::::CM:::M           M:::M\n");
    sb.append("O    |@|   |#|@|###||||#M#|###|       |###|#1#||||###|@|#|   |@|    M EEEEEEEEEEEEEE SSSSSSSSSSSSS    @@@ CCCCCCCMMMMM           MMMMM\n");
    sb.append("N    | |   |#| |###||||###|###|       |###|###||||###| |#|   | |    A@@@@@@@@@@@@@@ @@@@@@@@@@@@@        @@@@@@@@@@@@           @@@@@\n");
    sb.append("I   _|_|___|#|_|###|##|###|###|_______|###|###|##|###|_|#|___|_|_   @\n");
    sb.append("E   |###|###################################################|###|   V        Copyright (c) Jordan Candide Randleman 2021-2022\n");
    sb.append(String.format("@   |###|###############/|=================|\\###############|###|   I                  Eerina's Scheme: Version %.1f\n",SystemPrimitives.VERSION));
    sb.append("@  /-------------------/ /=================\\ \\-------------------\\  E              Type (help) for Help, (exit) to Exit\n");
    sb.append("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
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


  private static void launchRepl(ParsedCommandLine parsedCmdLine) throws Exception {
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
        eval(GlobalState.globalEnvironment,readFullExpression(),printContinuation);
        EscmCallStack.clear(); // residue frames may reside after continuation nonsense
      } catch(Exception e) {
        reportTopLevelException(e);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our File Interpreter
  private static void launchScript(ParsedCommandLine parsedCmdLine) throws Exception {
    // Initialize the runtime, load the file, & launch REPL if found "-l" prior the filename
    GlobalState.initialize();
    Trampoline.Continuation replIfReplingContinuation = (value) -> () -> {
      if(parsedCmdLine.loadingIntoREPL) launchRepl(parsedCmdLine);
      return Trampoline.LAST_BOUNCE_SIGNAL;
    };
    // Note that we set whether we're in the REPL here, as setting in <launchRepl>
    //   could cause a read/write race condition should the loaded script spawn a 
    //   thread!
    if(parsedCmdLine.loadingIntoREPL) GlobalState.inREPL = true; // trigger exit message to be printed
    String buffer = null;
    try {
      buffer = FilePrimitives.FileRead.slurpFile(parsedCmdLine.scriptName,"escm-load-script");
    } catch(Exception e) {
      throw new Exceptionf("%s\n  %s", e, COMMAND_LINE_FLAGS.replaceAll("\n","\n  "));
    }
    ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(buffer);
    Trampoline.resolve(SystemPrimitives.Load.evalEachExpression(GlobalState.globalEnvironment,exprs,0,replIfReplingContinuation));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse the command-line
  private static class ParsedCommandLine {
    public boolean launchingQuiet  = false; // -q --quiet
    public boolean loadingIntoREPL = false; // -l --load
    public String scriptName = null;        // <null> denotes no script
  }


  private static void setArgv(int argvStart, String[] args) {
    Datum argvIterator = escm.type.Nil.VALUE;
    for(int i = args.length-1; i >= argvStart; --i)
      argvIterator = new escm.type.Pair(new escm.type.String(args[i]),argvIterator);
    GlobalState.setArgv(argvIterator);
  }


  private static ParsedCommandLine parseCommandLine(String[] args) {
    ParsedCommandLine parsed = new ParsedCommandLine();
    for(int i = 0; i < args.length; ++i) {
      switch(args[i]) {
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
          setArgv(i+2,args);
          return parsed;
        }
        default: {
          parsed.scriptName = args[i];
          setArgv(i+1,args);
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
          if(parsedCmdLine.scriptName == null) {
            GlobalState.initialize();
            GlobalState.inREPL = true; // trigger exit message to be printed
            launchRepl(parsedCmdLine);
          } else {
            launchScript(parsedCmdLine);
          }
        } catch(Exception e) {
          System.err.printf("Driver Loop Caught Error %s\n", e);
          EscmCallStack.print();
          printJavaStackTraceWithoutMessage(e);
        }
        return cont.run(escm.type.Void.VALUE);
      }
    );
    mainThread.start();
  }
}