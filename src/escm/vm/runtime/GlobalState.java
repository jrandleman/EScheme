// Author: Jordan Randleman - escm.vm.runtime.GlobalState
// Purpose:
//    Class to encapsulate the global runtime state of the interpreter.

package escm.vm.runtime;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.io.File;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Hashmap;
import escm.type.bool.Boolean;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.type.oo.EscmModule;
import escm.vm.util.Environment;
import escm.vm.runtime.installerGenerated.EscmPath;
import escm.vm.runtime.installerGenerated.JvmPathPrefix;
import escm.vm.runtime.installerGenerated.JavaStdLibLoader;
import escm.primitive.FilePrimitives;
import escm.primitive.SystemPrimitives;

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
  private static final Random prng = new Random();

  public static synchronized double getRandomDouble() { return prng.nextDouble(); }

  ////////////////////////////////////////////////////////////////////////////
  // <SystemPrimitives> set of imported modules: {absFilePath: moduleVar, ...}
  // => Note this isn't thread-local since files are globally imported!
  public static final ConcurrentHashMap<String,EscmModule> importedModules = new ConcurrentHashMap<String,EscmModule>();

  ////////////////////////////////////////////////////////////////////////////
  // Representing the Dynamic "Meta Thread" Environment
  public static final Environment metaThreadDynamicEnvironment = new Environment();

  ////////////////////////////////////////////////////////////////////////////
  // Representing the Parameter "Truly Global" Environment
  public static final Environment parameterEnvironment = new Environment();

  ////////////////////////////////////////////////////////////////////////////
  // Get the default environment (only has EScheme's core primitives defined)
  private static Datum getSystemProperty(String property) {
    try {
      return new escm.type.String(System.getProperty(property));
    } catch(Exception e) {
      return Boolean.FALSE;
    }
  }

  public static Environment getJavaPrimitiveEnvironment() throws Exception {
    Environment definitionEnvironment = new Environment(parameterEnvironment);
    definitionEnvironment.define(new Symbol("*argv*"),getArgv());
    definitionEnvironment.define(new Symbol("*file-separator*"),new escm.type.String(File.separator));
    definitionEnvironment.define(new Symbol("*path-separator*"),new escm.type.String(File.pathSeparator));
    definitionEnvironment.define(new Symbol("*os-name*"),getSystemProperty("os.name"));
    definitionEnvironment.define(new Symbol("*os-version*"),getSystemProperty("os.version"));
    definitionEnvironment.define(new Symbol("*os-architecture*"),getSystemProperty("os.arch"));
    definitionEnvironment.define(new Symbol("*user-home*"),getSystemProperty("user.home"));
    definitionEnvironment.define(new Symbol("*user-name*"),getSystemProperty("user.name"));
    definitionEnvironment.define(new Symbol("*escm-path*"),new escm.type.String(EscmPath.VALUE));
    definitionEnvironment.define(new Symbol("*escm-version*"),new escm.type.String(String.format("%.1f",SystemPrimitives.VERSION)));
    definitionEnvironment.define(new Symbol("*escm-execution-command*"),new escm.type.String(SystemPrimitives.ESCM_EXECUTION_COMMAND));
    definitionEnvironment.define(new Symbol("*min-radix*"),new Exact(Number.MIN_RADIX));
    definitionEnvironment.define(new Symbol("*max-radix*"),new Exact(Number.MAX_RADIX));
    definitionEnvironment.define(new Symbol("*min-priority*"),new Exact(Thread.MIN_PRIORITY));
    definitionEnvironment.define(new Symbol("*max-priority*"),new Exact(Thread.MAX_PRIORITY));
    definitionEnvironment.define(new Symbol("*import*"),Boolean.FALSE); // set <TRUE> in <SystemPrimitives>
    definitionEnvironment.define(new Symbol("*load-once-files*"),new Hashmap());
    JavaStdLibLoader.load(definitionEnvironment);
    return definitionEnvironment;
  }

  public static Environment getDefaultEnvironment() throws Exception {
    Environment definitionEnvironment = getJavaPrimitiveEnvironment();
    EscmStdLibLoader.load(definitionEnvironment);
    return definitionEnvironment;
  }
};