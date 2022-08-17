// Author: Jordan Randleman - escm.vm.Main
// Purpose:
//    Main file to start the VM. Call via "launchESchemeSession()".

package escm.vm;
import java.util.ArrayList;
import java.util.Stack;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
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
    sb.append("                ,  ,\n");
    sb.append("             ,  |\\ |\\\n");
    sb.append("             |\\ '.`-.`-,                                 @@@@@@@@@@@@@@@@                     _.._\n");
    sb.append("              \\`'-:  `; \\                                @      __      @       _     |     .'@@'`` *    |\n");
    sb.append("               `-._'.  \\ \\                               @     /@@\\     @      / *   -O-   /@@/   ***** -O-\n");
    sb.append("              ,_.-=`,`.`` ~,     WEET                    @     \\@@/     @      \\_/    |    @@(     * *   | \n");
    sb.append("               '--,. `    _ \\    WOOT!            J |    @              @       |          \\@@'.___.;       | T\n");
    sb.append("                  /  ___ (O),\\   /               E /#\\   @@@@@@@@@@@@@@@@      /#\\       |  '.@@@@.'       /#\\ U\n");
    sb.append("                 |  /@@@\\  /_ \\                 @ (###)   |                  /#####\\    -O-    ``         (###) @\n");
    sb.append("                 |  \\@@@/  \\_\\|                  T | |    |                /#########\\   |            |    | | E\n");
    sb.append("                 |`'.     .(                     ' |@|   /#\\|     /\\     /#############\\     /\\     |/#\\   |@| S\n");
    sb.append("             __ / '. `````.'\\                    A | |  (##/#\\   /##\\   (###############)   /##\\   /#\\##)  | | @\n");
    sb.append("     /\\_    /,'`|   `````.-~\"~-.                 I |@|   |(###) (####)  _\\#############/_  (####) (###)|   |@| L\n");
    sb.append("     |`.\\_ |   /  ' ,   /      \\                 M | |   |#| |___|/\\|__|_________________|__|/\\|___| |#|   | | '\n");
    sb.append("   _/  `, \\|  ; , ,   .|.   ' . |                E |@|   |#|@|###||||###|######/^\\######|###||||###|@|#|   |@| A\n");
    sb.append("   \\   `,  |  |  ,  ,  | \\  ; : |                @ | |   |#| |###|##|#T#|####/     \\####|#1#|##|###| |#|   | | M\n");
    sb.append("   _\\  `,  \\  |.     , |  ; | | |                M |@|   |#|@|###|/\\|#A#|##/         \\##|#0#|/\\|###|@|#|   |@| O\n");
    sb.append("   \\`  `.   \\ |   '    |  |'|,'\\|                A | |   |#| |###||||#S#|#(           )#|#2#||||###| |#|   | | U\n");
    sb.append("   _\\   `,   `\\  '  . '|  | |  |                 @ |@|   |#|@|###|##|#N#|##\\         /##|#2#|##|###|@|#|   |@| R\n");
    sb.append("   \\     `,   | ,  '   |  |-|_/       __ ,-;`\\   B | |   |#| |###|/\\|#I#|###|       |###|#2#|/\\|###| |#|   | | @\n");
    sb.append("    \\    `,    \\ .  , '.| | | |     _/' ` _=`|   E |@|   |#|@|###||||#M#|###|       |###|#1#||||###|@|#|   |@| D\n");
    sb.append("     `\\    `,   \\     , | | | |\\ _/'   .=\"  /    L | |   |#| |###||||###|###|       |###|###||||###| |#|   | | E\n");
    sb.append("     \\`     `,   `\\     \\ | | ;/'   .=\"    |    L _|_|___|#|_|###|##|###|###|_______|###|###|##|###|_|#|___|_|_ @\n");
    sb.append("      \\      `,    `\\' . | ; /'    =\"    _/     E |###|###################################################|###| M\n");
    sb.append("       `\\     `,  .-\"\"-. : /'    =\"     /      @ |###|###############/|=================|\\###############|###| A\n");
    sb.append("        _`\\    ;_{  '  ; /'    =\"      /      J /-------------------/ /=================\\ \\-------------------\\ @ \n");
    sb.append("       _\\`-/__.~  `.8.'\"`~-. =\"     _,/      A /-------------------/ /===================\\ \\-------------------\\ V\n");
    sb.append("    __\\      {   '-.|.'--~'`}\"    _/` \\      A |------------------/ /=====================\\ \\------------------| I\n");
    sb.append("    \\    .=\"` }.-~\"'u'- '-..'  __/\\__, \\     N |-----------------/ /=======================\\ \\-----------------| E\n");
    sb.append("   _/  .\"    {  -'.~('-_,.'\\_,/ \\_ , \\_ \\\n");
    sb.append("  /  .\"    _/'`--; ;  `  ;        \\. ,\\_ \\              Copyright (c) Jordan Candide Randleman 2021-2022\n");
    sb.append(String.format(" /..=\"  _/'      `-..__-'           \\. ,\\ \\                       Eerina's Scheme: Version %.1f\n",SystemPrimitives.VERSION));
    sb.append("|;____/'                              \\.,`\\`                  Type (help) for Help, (exit) to Exit\n");
    sb.append("                                        \\.,\\\n");
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
        EscmCallStack.clear(); // residue frames may reside after call/cc nonsense
      } catch(Throwable t) {
        reportTopLevelError(t);
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
    ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(parsedCmdLine.scriptName,buffer);
    Trampoline.resolve(SystemPrimitives.Load.evalEachExpression(GlobalState.globalEnvironment,exprs,0,new Stack<Pair<String,SourceInformation>>(),replIfReplingContinuation));
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
    for(int i = args.length-1; i > argvStart; --i)
      argvIterator = new escm.type.Pair(new escm.type.String(args[i]),argvIterator);
    argvIterator = new escm.type.Pair(new escm.type.String(FilePrimitives.AbsolutePath.logic(args[argvStart])),argvIterator);
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
          if(parsedCmdLine.scriptName == null) {
            GlobalState.initialize();
            GlobalState.inREPL = true; // trigger exit message to be printed
            launchRepl(parsedCmdLine);
          } else {
            launchScript(parsedCmdLine);
          }
        } catch(Throwable t) {
          reportTopLevelError(t);
        }
        return cont.run(escm.type.Void.VALUE);
      }
    );
    mainThread.start();
  }
}
