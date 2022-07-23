// Author: Jordan Randleman - escm.vm.Main
// Purpose:
//    Main file to start the VM. Call via "launchESchemeSession()".

package escm.vm;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;
import escm.vm.runtime.CallStack;
import escm.vm.runtime.EscmThread;
import escm.vm.runtime.GlobalState;
import escm.primitive.SystemPrimitives;

public class Main {
  ////////////////////////////////////////////////////////////////////////////
  // Evaluate an escm expression in the given environment
  public static void eval(Environment env, Datum d, Trampoline.Continuation continuation) throws Exception {
    Trampoline.resolve(Compiler.run(d,(compiled) -> () -> {
      return Interpreter.run(new ExecutionState(env,Assembler.run(compiled)),continuation);
    }));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Read an expression from <br> (returns <null> if reads EOF)
  public static Datum read(BufferedReader br) throws Exception {
    StringBuilder sb = new StringBuilder();
    while(true) {
      try {
        String input = br.readLine();
        if(input == null) { // EOF detected
          if(sb.length() == 0) return null;
          sb = new StringBuilder();
          printReplPrompt();
          continue;
        }
        if(input.length() == 0) continue;
        sb.append(input);
        escm.util.Pair<Datum,Integer> result = Reader.read(sb.toString());
        if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(true); // from the newline input by the user's <enter>/<return> key stroke
        return result.first;
      } catch(Reader.IncompleteException e) {
        continue;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our REPL
  public static void reportTopLevelException(Exception e) {
    System.err.printf("\nESCM ERROR: %s\n", e.getMessage());
    CallStack.print();
    e.printStackTrace();
    System.err.println("");
  }


  private static void printReplIntro() {
    StringBuilder sb = new StringBuilder();
    sb.append("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
    sb.append("J               _.._              _                                 T\n");
    sb.append("E             .'@@'`` *          / *                                U\n");
    sb.append("@            /@@/   *****        \\_/                                @ EEEEEEEEEEEEEE   SSSSSSSSSSSSS      CCCCCCCMMMM             MMMM\n");
    sb.append("T     |      @@(     * *          |                           |     E E::::::::::::E SS:::::::::::::S  CCC::::::CM:::M           M:::M\n");
    sb.append("'    /#\\     \\@@'.___.;          /#\\                         /#\\    S E::::::::::::ES::::SSSSSS:::::S C:::::::::CM::::M         M::::M\n");
    sb.append("A   (###)     '.@@@@.'         /#####\\                      (###)   @ EE:::EEEEEE::ES::::S     SSSSSSC::::CCCC::CM:::::M       M:::::M\n");
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
    sb.append("@   |###|###############/|=================|\\###############|###|   I                  Eerina's Scheme: Version 5.0\n");
    sb.append("@  /-------------------/ /=================\\ \\-------------------\\  E              Type (help) for Help, (exit) to Exit\n");
    sb.append("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
    System.out.print(sb.toString());
  }


  private static void printReplPrompt() {
    if(!GlobalState.getLastPrintedANewline()) System.out.println("");
    System.out.print("> ");
    if(GlobalState.inREPL) GlobalState.setLastPrintedANewline(false); // from the newline input by the user's <enter>/<return> key stroke
  }


  private static Datum readFullExpression(BufferedReader br) {
    while(true) {
      try {
        printReplPrompt();
        Datum readDatum = read(br);
        // Account for EOF => triggers REPL termination!
        if(readDatum == null) {
          System.out.println('\n'+SystemPrimitives.EXIT_MESSAGE);
          System.exit(0);
        }
        return readDatum;
      } catch(Exception e) {
        System.err.printf("\n%s\n",e.getMessage());
      }
    }
  }


  private static void launchRepl() throws Exception {
    // Note that we DON'T set whether we're in the REPL here, as such risks
    //   causing a read/write race condition should a script loaded by "-l" 
    //   spawn a thread!
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    printReplIntro();
    Trampoline.Continuation printContinuation = (value) -> () -> {
      if(!(value instanceof escm.type.Void)) System.out.printf("%s\n", value.write());
      return Trampoline.LAST_BOUNCE_SIGNAL;
    };
    while(true) {
      try {
        eval(GlobalState.globalEnvironment,readFullExpression(br),printContinuation);
        CallStack.clear(); // residue frames may reside after continuation nonsense
      } catch(Exception e) {
        reportTopLevelException(e);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our File Interpreter
  private static void launchScript(String[] args) throws Exception {
    boolean loadingIntoREPL = args[0].equals("-l");
    int filenameIndex = loadingIntoREPL ? 1 : 0;
    if(filenameIndex >= args.length) {
      System.err.println("ERROR: No filename given to load into the REPL!");
      return;
    }
    // Populate *argv*
    String filename = args[filenameIndex];
    Datum argvIterator = escm.type.Nil.VALUE;
    for(int i = args.length-1; i > filenameIndex; --i)
      argvIterator = new escm.type.Pair(new escm.type.String(args[i]),argvIterator);
    GlobalState.setArgv(argvIterator);
    // Initialize the runtime, load the file, & launch REPL if found "-l" prior the filename
    GlobalState.initialize();
    Trampoline.Continuation replIfReplingContinuation = (value) -> () -> {
      if(loadingIntoREPL) launchRepl();
      return Trampoline.LAST_BOUNCE_SIGNAL;
    };
    // Note that we set whether we're in the REPL here, as setting in <launchRepl>
    //   could cause a read/write race condition should the loaded script spawn a 
    //   thread!
    if(loadingIntoREPL) GlobalState.inREPL = true; // trigger exit message to be printed
    Trampoline.resolve(SystemPrimitives.Load.loadFileInEnvironment(GlobalState.globalEnvironment,filename,replIfReplingContinuation));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing our Interpreter
  public static void launchESchemeSession(String[] args) {
    escm.type.Thread mainThread = new escm.type.Thread(
      "escm-main",
      (params, cont) -> {
        try {
          if(args.length == 0) {
            GlobalState.initialize();
            GlobalState.inREPL = true; // trigger exit message to be printed
            launchRepl();
          } else {
            launchScript(args);
          }
        } catch(Exception e) {
          System.err.printf("Driver Loop Caught Error %s\n", e);
          CallStack.print();
          e.printStackTrace();
        }
        return cont.run(escm.type.Void.VALUE); // never triggered
      }
    );
    mainThread.start();
  }
}