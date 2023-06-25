// Author: Jordan Randleman - escm.vm.runtime.GlobalState
// Purpose:
//    Class to encapsulate the global runtime state of the interpreter.

package escm.vm.runtime;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.io.File;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.vm.util.Environment;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.vm.runtime.installerGenerated.JvmPathPrefix;
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
  // <SystemPrimitives> set of prior "load-once" args (sentinel ints)
  // => Note this isn't thread-local since files are globally loaded!
  public static ConcurrentHashMap<String,Integer> loadedOnceFiles = new ConcurrentHashMap<String,Integer>();

  ////////////////////////////////////////////////////////////////////////////
  // Representing the Dynamic "Meta Thread" Environment
  public static Environment metaThreadDynamicEnvironment = new Environment();

  ////////////////////////////////////////////////////////////////////////////
  // Get the default environment (only has EScheme's core primitives defined)
  public static void loadJavaPrimitives(Environment definitionEnvironment) throws Exception {
    definitionEnvironment.define(new Symbol("*argv*"),getArgv());
    definitionEnvironment.define(new Symbol("*file-separator*"),new escm.type.String(File.separator));
    definitionEnvironment.define(new Symbol("*path-separator*"),new escm.type.String(File.pathSeparator));
    definitionEnvironment.define(new Symbol("*os-name*"),new escm.type.String(System.getProperty("os.name")));
    definitionEnvironment.define(new Symbol("*os-version*"),new escm.type.String(System.getProperty("os.version")));
    definitionEnvironment.define(new Symbol("*os-architecture*"),new escm.type.String(System.getProperty("os.arch")));
    definitionEnvironment.define(new Symbol("*escm-path*"),new escm.type.String(EscmPath.VALUE));
    definitionEnvironment.define(new Symbol("*escm-execution-command*"),new escm.type.String(" "+JvmPathPrefix.VALUE+"java -classpath "+EscmPath.VALUE+File.separator+"bin Main "));
    definitionEnvironment.define(new Symbol("*min-radix*"),new Exact(Number.MIN_RADIX));
    definitionEnvironment.define(new Symbol("*max-radix*"),new Exact(Number.MAX_RADIX));
    JavaStdLibLoader.load(definitionEnvironment);
  }

  public static Environment getDefaultEnvironment() throws Exception {
    Environment definitionEnvironment = new Environment();
    loadJavaPrimitives(definitionEnvironment);
    EscmStdLibLoader.load(definitionEnvironment);
    return definitionEnvironment;
  }
};