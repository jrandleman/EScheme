// Author: Jordan Randleman - escm.vm.runtime.GlobalState
// Purpose:
//    Class to manage/encapsulate the global runtime state of the interpreter.

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
import escm.type.concurrent.Mutex;
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
  // System-Dependant Invariants
  private static Datum getSystemProperty(String property) {
    try {
      return new escm.type.String(System.getProperty(property));
    } catch(Exception e) {
      return Boolean.FALSE;
    }
  }

  public static final escm.type.String FILE_SEPARATOR = new escm.type.String(File.separator);
  public static final escm.type.String PATH_SEPARATOR = new escm.type.String(File.pathSeparator);

  public static final Datum OS_NAME = getSystemProperty("os.name");
  public static final Datum OS_VERSION = getSystemProperty("os.version");
  public static final Datum OS_ARCHITECTURE = getSystemProperty("os.arch");

  public static final Datum USER_HOME = getSystemProperty("user.home");
  public static final Datum USER_NAME = getSystemProperty("user.name");

  public static final escm.type.String ESCM_PATH = new escm.type.String(EscmPath.VALUE);
  public static final escm.type.String ESCM_VERSION = new escm.type.String(String.format("%.1f",SystemPrimitives.VERSION));
  public static final escm.type.String ESCM_EXECUTION_COMMAND = new escm.type.String(SystemPrimitives.ESCM_EXECUTION_COMMAND);

  public static final Exact MIN_RADIX = new Exact(Number.MIN_RADIX);
  public static final Exact MAX_RADIX = new Exact(Number.MAX_RADIX);
  public static final Exact MIN_PRIORITY = new Exact(Thread.MIN_PRIORITY);
  public static final Exact MAX_PRIORITY = new Exact(Thread.MAX_PRIORITY);


  ////////////////////////////////////////////////////////////////////////////
  // Global Variable Names
  public static final Symbol GLOBAL_ARGV = new Symbol("*argv*");
  public static final Symbol GLOBAL_FILE_SEPARATOR = new Symbol("*file-separator*");
  public static final Symbol GLOBAL_PATH_SEPARATOR = new Symbol("*path-separator*");
  public static final Symbol GLOBAL_OS_NAME = new Symbol("*os-name*");
  public static final Symbol GLOBAL_OS_VERSION = new Symbol("*os-version*");
  public static final Symbol GLOBAL_OS_ARCHITECTURE = new Symbol("*os-architecture*");
  public static final Symbol GLOBAL_USER_HOME = new Symbol("*user-home*");
  public static final Symbol GLOBAL_USER_NAME = new Symbol("*user-name*");
  public static final Symbol GLOBAL_ESCM_PATH = new Symbol("*escm-path*");
  public static final Symbol GLOBAL_ESCM_VERSION = new Symbol("*escm-version*");
  public static final Symbol GLOBAL_ESCM_EXECUTION_COMMAND = new Symbol("*escm-execution-command*");
  public static final Symbol GLOBAL_MIN_RADIX = new Symbol("*min-radix*");
  public static final Symbol GLOBAL_MAX_RADIX = new Symbol("*max-radix*");
  public static final Symbol GLOBAL_MIN_PRIORITY = new Symbol("*min-priority*");
  public static final Symbol GLOBAL_MAX_PRIORITY = new Symbol("*max-priority*");
  public static final Symbol GLOBAL_IMPORT = new Symbol("*import*");
  public static final Symbol GLOBAL_LOAD_ONCE_FILES = new Symbol("*load-once-files*");
  public static final Symbol GLOBAL_DOSYNC_LOCK = new Symbol("*dosync-lock*");
  public static final Symbol GLOBAL_DOSYNC_MODULE_LOCK = new Symbol("*dosync-module-lock*");
  public static final Symbol GLOBAL_GENERATOR_COMPLETE = new Symbol("*generator-complete*");


  ////////////////////////////////////////////////////////////////////////////
  // Aliased Symbols
  private static final Symbol DEF = new Symbol("def");
  private static final Symbol DEFINE = new Symbol("define");

  private static final Symbol DEFP = new Symbol("def?");
  private static final Symbol DEFINEDP = new Symbol("defined?");

  private static final Symbol CALLCC = new Symbol("call/cc");
  private static final Symbol CALL_WITH_CURRENT_CONTINUATION = new Symbol("call-with-current-continuation");

  private static final Symbol PPRINT = new Symbol("pprint");
  private static final Symbol PRETTY_PRINT = new Symbol("pretty-print");

  private static final Symbol PPRINTF = new Symbol("pprintf");
  private static final Symbol PRETTY_PRINTF = new Symbol("pretty-printf");

  private static final Symbol PPRINT_TO_STRING = new Symbol("pprint-to-string");
  private static final Symbol PRETTY_PRINT_TO_STRING = new Symbol("pretty-print-to-string");

  private static final Symbol PRINT = new Symbol("print");
  private static final Symbol DISPLAY = new Symbol("display");

  private static final Symbol PRINTF = new Symbol("printf");
  private static final Symbol DISPLAYF = new Symbol("displayf");

  private static final Symbol PLUSPLUS = new Symbol("++");
  private static final Symbol APPEND = new Symbol("append");

  private static final Symbol STARSTAR = new Symbol("**");
  private static final Symbol EXPT = new Symbol("expt");

  private static final Symbol IS_ACP = new Symbol("ac?");
  private static final Symbol IS_ASSOCIATIVE_COLLECTIONP = new Symbol("associative-collection?");

  private static final Symbol IS_OCP = new Symbol("oc?");
  private static final Symbol IS_ORDERED_COLLECTIONP = new Symbol("ordered-collection?");


  ////////////////////////////////////////////////////////////////////////////
  // Get the default environment (only has EScheme's core primitives defined)
  public static Environment getDefaultEnvironment() throws Exception {
    Environment definitionEnvironment = new Environment(parameterEnvironment);
    definitionEnvironment.define(GLOBAL_ARGV,getArgv());
    definitionEnvironment.define(GLOBAL_FILE_SEPARATOR,FILE_SEPARATOR);
    definitionEnvironment.define(GLOBAL_PATH_SEPARATOR,PATH_SEPARATOR);
    definitionEnvironment.define(GLOBAL_OS_NAME,OS_NAME);
    definitionEnvironment.define(GLOBAL_OS_VERSION,OS_VERSION);
    definitionEnvironment.define(GLOBAL_OS_ARCHITECTURE,OS_ARCHITECTURE);
    definitionEnvironment.define(GLOBAL_USER_HOME,USER_HOME);
    definitionEnvironment.define(GLOBAL_USER_NAME,USER_NAME);
    definitionEnvironment.define(GLOBAL_ESCM_PATH,ESCM_PATH);
    definitionEnvironment.define(GLOBAL_ESCM_VERSION,ESCM_VERSION);
    definitionEnvironment.define(GLOBAL_ESCM_EXECUTION_COMMAND,ESCM_EXECUTION_COMMAND);
    definitionEnvironment.define(GLOBAL_MIN_RADIX,MIN_RADIX);
    definitionEnvironment.define(GLOBAL_MAX_RADIX,MAX_RADIX);
    definitionEnvironment.define(GLOBAL_MIN_PRIORITY,MIN_PRIORITY);
    definitionEnvironment.define(GLOBAL_MAX_PRIORITY,MAX_PRIORITY);
    definitionEnvironment.define(GLOBAL_IMPORT,Boolean.FALSE); // set <TRUE> in <SystemPrimitives>
    definitionEnvironment.define(GLOBAL_LOAD_ONCE_FILES,new Hashmap());
    parameterEnvironment.defineIfAbsent(GLOBAL_DOSYNC_LOCK,new Mutex(GLOBAL_DOSYNC_LOCK.value()));
    definitionEnvironment.define(GLOBAL_DOSYNC_MODULE_LOCK,new Mutex(GLOBAL_DOSYNC_MODULE_LOCK.value()));
    definitionEnvironment.define(GLOBAL_GENERATOR_COMPLETE,GLOBAL_GENERATOR_COMPLETE);
    JavaStdLibLoader.load(definitionEnvironment);
    definitionEnvironment.define(DEF,definitionEnvironment.get(DEFINE));
    definitionEnvironment.define(DEFP,definitionEnvironment.get(DEFINEDP));
    definitionEnvironment.define(CALLCC,definitionEnvironment.get(CALL_WITH_CURRENT_CONTINUATION));
    definitionEnvironment.define(PPRINT,definitionEnvironment.get(PRETTY_PRINT));
    definitionEnvironment.define(PPRINTF,definitionEnvironment.get(PRETTY_PRINTF));
    definitionEnvironment.define(PPRINT_TO_STRING,definitionEnvironment.get(PRETTY_PRINT_TO_STRING));
    definitionEnvironment.define(PRINT,definitionEnvironment.get(DISPLAY));
    definitionEnvironment.define(PRINTF,definitionEnvironment.get(DISPLAYF));
    definitionEnvironment.define(PLUSPLUS,definitionEnvironment.get(APPEND));
    definitionEnvironment.define(STARSTAR,definitionEnvironment.get(EXPT));
    definitionEnvironment.define(IS_ACP,definitionEnvironment.get(IS_ASSOCIATIVE_COLLECTIONP));
    definitionEnvironment.define(IS_OCP,definitionEnvironment.get(IS_ORDERED_COLLECTIONP));
    return definitionEnvironment;
  }
};