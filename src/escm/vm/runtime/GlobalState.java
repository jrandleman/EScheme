// Author: Jordan Randleman - escm.vm.runtime.State
// Purpose:
//    Class to encapsulate the global runtime state of the interpreter.

package escm.vm.runtime;
import java.util.Random;
import java.io.File;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.vm.util.Environment;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.vm.runtime.installerGenerated.JavaStdLibLoader;
import escm.vm.runtime.installerGenerated.EscmStdLibLoader;
import escm.primitive.FilePrimitives;

public class GlobalState {
  ////////////////////////////////////////////////////////////////////////////
  // static field to hold command-line args
  private static Datum argv = escm.type.Nil.VALUE;

  public static synchronized void setArgv(Datum d) { argv = d; }
  public static synchronized Datum getArgv() { return argv.copy(); }

  ////////////////////////////////////////////////////////////////////////////
  // static field to track whether REPL should print a newline
  private static boolean lastPrintedANewline = false;

  public static synchronized void setLastPrintedANewline(boolean b) { lastPrintedANewline = b; }
  public static synchronized boolean getLastPrintedANewline() { return lastPrintedANewline; }

  ////////////////////////////////////////////////////////////////////////////
  // Track if currently in a REPL session (determines if exit msg is printed)
  // => Only set by escm session launch, hence no synchronization required.
  public static boolean inREPL = false;

  ////////////////////////////////////////////////////////////////////////////
  // Global Random Number Generator
  private static Random prng = new Random();

  public static synchronized double getRandomDouble() { return prng.nextDouble(); }

  ////////////////////////////////////////////////////////////////////////////
  // Representing the Dynamic "Meta Thread" Environment
  public static Environment metaThreadDynamicEnvironment = new Environment();

  ////////////////////////////////////////////////////////////////////////////
  // Representing the Global Environment
  // => Initialized via <initialize> below
  public static Environment globalEnvironment = new Environment();

  ////////////////////////////////////////////////////////////////////////////
  // Initialize the global environment
  public static void initialize() throws Exception {
    globalEnvironment.define(new Symbol("*argv*"),getArgv());
    globalEnvironment.define(new Symbol("*file-separator*"),new escm.type.String(File.separator));
    globalEnvironment.define(new Symbol("*path-separator*"),new escm.type.String(File.pathSeparator));
    globalEnvironment.define(new Symbol("*os-name*"),new escm.type.String(System.getProperty("os.name")));
    globalEnvironment.define(new Symbol("*os-version*"),new escm.type.String(System.getProperty("os.version")));
    globalEnvironment.define(new Symbol("*os-architecture*"),new escm.type.String(System.getProperty("os.arch")));
    globalEnvironment.define(new Symbol("*escm-path*"),new escm.type.String(EscmPath.VALUE));
    globalEnvironment.define(new Symbol("*escm-execution-command*"),new escm.type.String(" java -classpath "+EscmPath.VALUE+File.separator+"bin Main "));
    globalEnvironment.define(new Symbol("*min-radix*"),new Exact(Number.MIN_RADIX));
    globalEnvironment.define(new Symbol("*max-radix*"),new Exact(Number.MAX_RADIX));
    JavaStdLibLoader.load();
    EscmStdLibLoader.load();
  }
};