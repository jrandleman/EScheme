// Author: Jordan Randleman - Installer - MUST BE RUN WITHIN THE `EScheme/installer` DIRECTORY!
//   => Command-Line Options: 
//        -h, --help              (show these command-line options)
//        -v, --verbose           (print status updates)
//        -u, --unit-tests        (run EScheme's unit test suite after compilation)
//        -i, --interpret-stdlib  (don't serialize <../src/stdlib.scm>)
//        -j, --java-bin-path     (set the path to our JVM's <bin> directory)


/*
  Purpose: Compiles/Installs the EScheme runtime. This is done by accomplishing several tasks:
    
    1. Generate a file to store the path to our EScheme implementation.

    2. Generate a file to store the path prefix to our EScheme's JVM/JRE.
    
    3. Generate a loader file to interpret src/stdlib.scm upon EScheme's launch.

    4. Identify all of the "Outer Class" names in the <escm.primitive> package, and
       splice such as imports into a loader file that uses reflection to instantiate
       each Inner Class of the file (iff it implements 1 of the primitive function 
       object interfaces) to be defined within the global EScheme environment.

    5. Compile the EScheme <Main.java> file using Java11 via <Runtime.exec()>

    6. Serialize the EScheme Standard Library "src/stdlib.scm" by executing "escm --serialize-stdlib"

    7. Execute EScheme unit test suite (if requested)

    8. Output the <alias> string to stdout, for easy copying into "~/.zshrc" or "~/.bashrc"
*/


import java.nio.file.Path;
import java.nio.file.Files;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.File;
import java.util.ArrayList;

public class Installer {
  ////////////////////////////////////////////////////////////////////////////
  // Command-Line Parsing Setting Satus
  private static boolean VERBOSE_MODE = false;
  private static boolean INTERPRET_STDLIB = false;
  private static boolean EXECUTE_UNIT_TESTS = false;
  private static String JAVA_BIN_PATH = null;


  public static final String COMMAND_LINE_FLAGS = 
    "  1. -h, --help                 | Print this information\n"+
    "  2. -v, --verbose              | Print out installation progress messages\n"+
    "  3. -u, --unit-tests           | Run EScheme's unit test suite after compilation\n"+
    "  4. -i, --interpret-stdlib     | Don't serialize <../src/stdlib.scm>\n"+
    "  5. -j, --java-bin-path <path> | Set the path to our JVM's <bin> directory\n";


  private static void parseCommandLine(String[] args) {
    if(args.length == 0) return;
    for(int i = 0; i < args.length; ++i) {
      switch(args[i]) {
        case "-v": case "--verbose": {
          VERBOSE_MODE = true;
          break;
        }
        case "-i": case "--interpret-stdlib": {
          INTERPRET_STDLIB = true;
          break;
        }
        case "-u": case "--unit-tests": {
          EXECUTE_UNIT_TESTS = true;
          break;
        }
        case "-j": case "--java-bin-path": {
          if(i+1 == args.length) {
            System.err.printf("> [ FATAL ] ESCM INSTALLER ERROR: Must follow \"--java-bin-path\" with a file path!\n", args[i]);
            System.exit(1);
          }
          JAVA_BIN_PATH = args[++i];
          break;
        }
        case "-h": case "--help": {
          System.out.println("Supported Command-Line Flags Include:");
          System.out.print(COMMAND_LINE_FLAGS);
          System.out.flush();
          System.exit(0);
        }
        default: {
          System.err.printf("> [ FATAL ] ESCM INSTALLER ERROR: Invalid command-line argument \"%s\"! Use:\n", args[i]);
          System.err.print(COMMAND_LINE_FLAGS);
          System.exit(1);
        }
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // JVM/JRE Command Decoration
  private static String decorateJvmCmdPath(String cmd) {
    if(JAVA_BIN_PATH == null) return cmd;
    return JAVA_BIN_PATH + File.separator + cmd;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Terminal Command Execution
  private static class ExecuteCommandResult {
    public String out = null;
    public String err = null;
    public int exit = 0;
  };


  private static String getInputStreamLines(InputStream ins, boolean reportLive) throws Exception {
    String line = null;
    StringBuilder buffer = new StringBuilder();
    BufferedReader in = new BufferedReader(new InputStreamReader(ins));
    while((line = in.readLine()) != null) {
      if(reportLive) {
        System.out.print("  ");
        System.out.println(line);
        System.out.flush();
      }
      buffer.append('\n'+line);
    }
    if(buffer.length() == 0) return "";
    return buffer.substring(1);
  }


  private static ExecuteCommandResult executeCommand(String command, boolean reportLive) throws Exception {
    if(VERBOSE_MODE) {
      System.out.println("> Executing Command:");
      System.out.println("  \"" + command + "\"");
    }
    Process pro = Runtime.getRuntime().exec(command);
    ExecuteCommandResult res = new ExecuteCommandResult();
    res.out = getInputStreamLines(pro.getInputStream(),reportLive);
    res.err = getInputStreamLines(pro.getErrorStream(),false);
    pro.waitFor();
    res.exit = pro.exitValue();
    return res;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Retrieve EScheme Directory
  private static String getEscmDirectory() {
    String cwd = Path.of("").toAbsolutePath().toString();
    return cwd.substring(0,cwd.lastIndexOf(File.separator));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Create the Directory for our Generated Files
  private static String createInstallerNewFilesDirectory(String escmDir) {
    String generatedFilesDir = escmDir+File.separator+"src"+File.separator+"escm"+
                                       File.separator+"vm"+File.separator+"runtime"+
                                       File.separator+"installerGenerated";
    try {
      Path generatedFilesPath = Path.of(generatedFilesDir);
      if(Files.exists(generatedFilesPath) == true) {
        for(File entry : (new File(generatedFilesDir)).listFiles()) entry.delete();
        Files.deleteIfExists(generatedFilesPath);
      }
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't clear directory to store generated files: "+generatedFilesDir);
      System.err.println("  error: "+e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if((new File(generatedFilesDir)).mkdirs() == false) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't create directory for generated file: "+generatedFilesDir);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    return generatedFilesDir;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Generate File to Store the Path to our EScheme Implementation
  private static void generateEscmPath(String escmDir, String generatedFilesDir) {
    Path escmPathPath = Path.of(generatedFilesDir+File.separator+"EscmPath.java");
    StringBuilder escmPath = new StringBuilder();
    escmPath.append("// Generated by `installer"+File.separator+"Installer.java`\n");
    escmPath.append("// Purpose:\n");
    escmPath.append("//   Store the path to our EScheme implementation.\n");
    escmPath.append("\n");
    escmPath.append("package escm.vm.runtime.installerGenerated;\n");
    escmPath.append("\n");
    escmPath.append("public class EscmPath {\n");
    escmPath.append("  public static final java.lang.String VALUE = \"" + escmDir + "\";\n");
    escmPath.append("}\n");
    escmPath.append("\n");
    try {
      Files.deleteIfExists(escmPathPath);
      Files.writeString(escmPathPath,escmPath.toString());
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't create escm path file: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if(VERBOSE_MODE) {
      System.out.println("> Successfully generated the path file to our EScheme implementation!");
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Generate File to Store the Path Prefix to our JVM/JRE
  private static void generateJvmPathPrefix(String generatedFilesDir) {
    Path jvmPathPrefixPath = Path.of(generatedFilesDir+File.separator+"JvmPathPrefix.java");
    StringBuilder escmPath = new StringBuilder();
    escmPath.append("// Generated by `installer"+File.separator+"Installer.java`\n");
    escmPath.append("// Purpose:\n");
    escmPath.append("//   Store the path prefix to our JVM/JRE.\n");
    escmPath.append("\n");
    escmPath.append("package escm.vm.runtime.installerGenerated;\n");
    escmPath.append("\n");
    escmPath.append("public class JvmPathPrefix {\n");
    if(JAVA_BIN_PATH == null) {
      escmPath.append("  public static final java.lang.String VALUE = \"\";\n");
    } else {
      escmPath.append("  public static final java.lang.String VALUE = \"" + JAVA_BIN_PATH + File.separator + "\";\n");
    }
    escmPath.append("}\n");
    escmPath.append("\n");
    try {
      Files.deleteIfExists(jvmPathPrefixPath);
      Files.writeString(jvmPathPrefixPath,escmPath.toString());
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't create jvm/jre path prefix file: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if(VERBOSE_MODE) {
      System.out.println("> Successfully generated the path prefix file to our JVM/JRE!");
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Generate File to Retrieve <stdlib.scm>
  private static void generateEscmStdlibLoader(String escmDir, String generatedFilesDir) {
    StringBuilder escmStdlibLoader = new StringBuilder();
    if(INTERPRET_STDLIB) {
      String stdlibPath = escmDir+File.separator+"src"+File.separator+"stdlib.scm";
      escmStdlibLoader.append("// Generated by `installer"+File.separator+"Installer.java`\n");
      escmStdlibLoader.append("// Purpose:\n");
      escmStdlibLoader.append("//   Interpret the <src"+File.separator+"stdlib.scm> file.\n");
      escmStdlibLoader.append("\n");
      escmStdlibLoader.append("package escm.vm.runtime.installerGenerated;\n");
      escmStdlibLoader.append("import java.util.ArrayList;\n");
      escmStdlibLoader.append("import java.util.Stack;\n");
      escmStdlibLoader.append("import java.nio.file.Files;\n");
      escmStdlibLoader.append("import java.nio.file.Path;\n");
      escmStdlibLoader.append("import escm.type.Datum;\n");
      escmStdlibLoader.append("import escm.util.Pair;\n");
      escmStdlibLoader.append("import escm.util.Trampoline;\n");
      escmStdlibLoader.append("import escm.primitive.FilePrimitives;\n");
      escmStdlibLoader.append("import escm.primitive.SystemPrimitives;\n");
      escmStdlibLoader.append("import escm.vm.util.SourceInformation;\n");
      escmStdlibLoader.append("import escm.vm.util.Environment;\n");
      escmStdlibLoader.append("\n");
      escmStdlibLoader.append("public class EscmStdLibLoader {\n");
      escmStdlibLoader.append("  public static void load(Environment definitionEnvironment) throws Exception {\n");
      escmStdlibLoader.append("    String escmCode = Files.readString(Path.of(\""+stdlibPath+"\"));\n");
      escmStdlibLoader.append("    // Note that we know our stdlib don't store any continuations using call/cc\n");
      escmStdlibLoader.append("    //   upon loading, so we can afford evaluating it with a dummy continuation.\n");
      escmStdlibLoader.append("    Trampoline.Continuation terminalContinuation = (ignored) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;\n");
      escmStdlibLoader.append("    ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(\""+stdlibPath+"\",escmCode);\n");
      escmStdlibLoader.append("    Trampoline.resolve(SystemPrimitives.Load.evalEachExpression(exprs,0,new Stack<Pair<String,SourceInformation>>(),definitionEnvironment,terminalContinuation));\n");
      escmStdlibLoader.append("  }\n");
      escmStdlibLoader.append("}\n");
      escmStdlibLoader.append("\n");
    } else {
      String stdlibPath = escmDir+File.separator+"bin"+File.separator+"stdlib.ser";
      escmStdlibLoader.append("// Generated by `installer"+File.separator+"Installer.java`\n");
      escmStdlibLoader.append("// Purpose:\n");
      escmStdlibLoader.append("//   Interpret the <bin"+File.separator+"stdlib.ser> file.\n");
      escmStdlibLoader.append("\n");
      escmStdlibLoader.append("package escm.vm.runtime.installerGenerated;\n");
      escmStdlibLoader.append("import escm.primitive.SerializationPrimitives;\n");
      escmStdlibLoader.append("import escm.util.Trampoline;\n");
      escmStdlibLoader.append("import escm.vm.util.Environment;\n");
      escmStdlibLoader.append("\n");
      escmStdlibLoader.append("public class EscmStdLibLoader {\n");
      escmStdlibLoader.append("  public static void load(Environment definitionEnvironment) throws Exception {\n");
      escmStdlibLoader.append("    Trampoline.resolve(SerializationPrimitives.loadSerializedFile(\"load\",\""+stdlibPath+"\",definitionEnvironment,(ignored) -> () -> Trampoline.LAST_BOUNCE_SIGNAL));\n");
      escmStdlibLoader.append("  }\n");
      escmStdlibLoader.append("}\n");
      escmStdlibLoader.append("\n");
    }
    try {
      Path stdlibLoaderPath = Path.of(generatedFilesDir+File.separator+"EscmStdLibLoader.java");
      Files.deleteIfExists(stdlibLoaderPath);
      Files.writeString(stdlibLoaderPath,escmStdlibLoader.toString());
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't create escm stdlib loader file: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if(VERBOSE_MODE) {
      System.out.println("> Successfully generated the EScheme standard library loader!");
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Primitive Java Standard Library Files Being Used
  private static ArrayList<String> getPrimitiveFileNames(String escmDir) {
    ArrayList<String> prmFileNames = new ArrayList<String>();
    String primitiveDirectoryPath = escmDir+File.separator+"src"+File.separator+"escm"+File.separator+"primitive";
    File primitiveDirectory = new File(primitiveDirectoryPath);
    for(File entry : primitiveDirectory.listFiles()) {
      if(entry.isFile()) {
        String fileName = entry.getName();
        if(fileName.endsWith(".java")) {
          prmFileNames.add(fileName.substring(0,fileName.lastIndexOf(".java")));
        }
      }
    }
    return prmFileNames;
  }


  private static ArrayList<String> getPrimitiveFileImports(ArrayList<String> prmFileNames) {
    ArrayList<String> imports = new ArrayList<String>();
    for(String prmFileName : prmFileNames) {
      imports.add("import escm.primitive."+prmFileName+";");
    }
    return imports;
  }


  private static String getJavaStdlibLoaderContents(String escmDir) {
    ArrayList<String> prmFileNames = getPrimitiveFileNames(escmDir);
    ArrayList<String> imports = getPrimitiveFileImports(prmFileNames);
    StringBuilder javaStdlibLoader = new StringBuilder();
    javaStdlibLoader.append("// Generated by `installer"+File.separator+"Installer.java`\n");
    javaStdlibLoader.append("// Purpose:\n");
    javaStdlibLoader.append("//   Parse all of the \"Outer Classes\" in escm.type.primitive and define instances\n");
    javaStdlibLoader.append("//   of their inner static `Primitive` or `PrimitiveCallable` classes as EScheme\n");
    javaStdlibLoader.append("//   primitive functions available in the global environment.\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("package escm.vm.runtime.installerGenerated;\n");
    javaStdlibLoader.append("import java.lang.reflect.Constructor;\n");
    javaStdlibLoader.append("import escm.type.Symbol;\n");
    javaStdlibLoader.append("import escm.vm.type.Primitive;\n");
    javaStdlibLoader.append("import escm.vm.type.PrimitiveCallable;\n");
    javaStdlibLoader.append("import escm.vm.util.Environment;\n");
    for(String imprt : imports) {
      javaStdlibLoader.append(imprt+"\n");
    }
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("public class JavaStdLibLoader {\n");
    javaStdlibLoader.append("  // @PRECONDITION: o instanceof Primitive || o instanceof PrimitiveCallable\n");
    javaStdlibLoader.append("  private static String getPrimitiveName(Object o) {\n");
    javaStdlibLoader.append("    if(o instanceof Primitive) {\n");
    javaStdlibLoader.append("      return ((Primitive)o).escmName();\n");
    javaStdlibLoader.append("    } else { // o instanceof PrimitiveCallable\n");
    javaStdlibLoader.append("      return ((PrimitiveCallable)o).escmName();\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("  private static void definePrimitivesFromInnerClassesInClass(String className, Environment definitionEnvironment) {\n");
    javaStdlibLoader.append("    // Get the outer Class\n");
    javaStdlibLoader.append("    Class<?> outerClass = null;\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      outerClass = Class.forName(className);\n");
    javaStdlibLoader.append("    } catch(ClassNotFoundException e) {\n");
    javaStdlibLoader.append("      System.err.printf(\"ESCM JAVA-STDLIB-LOADER ERROR: Couldn't find class \\\"%s\\\": %s\\n\", className, e);\n");
    javaStdlibLoader.append("      return;\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("    // Get the inner Classes of the outer Class\n");
    javaStdlibLoader.append("    Class<?>[] innerClasses = null;\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      innerClasses = outerClass.getDeclaredClasses();\n");
    javaStdlibLoader.append("    } catch(SecurityException e) {\n");
    javaStdlibLoader.append("      System.err.printf(\"ESCM JAVA-STDLIB-LOADER ERROR: Security exception finding inner classes for class \\\"%s\\\": %s\\n\", className, e);\n");
    javaStdlibLoader.append("      return;\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("    // Define primitives in the global environment as object instances of the inner class\n");
    javaStdlibLoader.append("    for(Class<?> innerClass : innerClasses) {\n");
    javaStdlibLoader.append("      if(!innerClass.isInterface() && (Primitive.class.isAssignableFrom(innerClass) || PrimitiveCallable.class.isAssignableFrom(innerClass))) {\n");
    javaStdlibLoader.append("        try {\n");
    javaStdlibLoader.append("          Constructor ctor = innerClass.getDeclaredConstructor(new Class[]{});\n");
    javaStdlibLoader.append("          try {\n");
    javaStdlibLoader.append("            Object o = ctor.newInstance();\n");
    javaStdlibLoader.append("            String escmName = getPrimitiveName(o);\n");
    javaStdlibLoader.append("            if(o instanceof Primitive) {\n");
    javaStdlibLoader.append("              Primitive p = (Primitive)o;\n");
    javaStdlibLoader.append("              p.definitionEnvironment = definitionEnvironment;\n");
    javaStdlibLoader.append("              definitionEnvironment.define(new Symbol(escmName),p);\n");
    javaStdlibLoader.append("            } else { // o instanceof PrimitiveCallable\n");
    javaStdlibLoader.append("              PrimitiveCallable p = (PrimitiveCallable)o;\n");
    javaStdlibLoader.append("              p.definitionEnvironment = definitionEnvironment;\n");
    javaStdlibLoader.append("              definitionEnvironment.define(new Symbol(escmName),p);\n");
    javaStdlibLoader.append("            }\n");
    javaStdlibLoader.append("          } catch(Exception e) {\n");
    javaStdlibLoader.append("            System.err.printf(\"ESCM JAVA-STDLIB-LOADER ERROR: Can't invoke nullary Ctor for inner class \\\"%s\\\" in class \\\"%s\\\": %s\\n\", innerClass.getName(), className, e);\n");
    javaStdlibLoader.append("          }\n");
    javaStdlibLoader.append("        } catch(NoSuchMethodException e) {\n");
    javaStdlibLoader.append("          System.err.printf(\"ESCM JAVA-STDLIB-LOADER ERROR: Can't find nullary Ctor for inner class \\\"%s\\\" in class \\\"%s\\\": %s\\n\", innerClass.getName(), className, e);\n");
    javaStdlibLoader.append("        }\n");
    javaStdlibLoader.append("      }\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("  public static void load(Environment definitionEnvironment) {\n");
    for(String prmFileName : prmFileNames) {
      javaStdlibLoader.append("    definePrimitivesFromInnerClassesInClass(\"escm.primitive."+prmFileName+"\",definitionEnvironment);\n");
    }
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("}\n");
    javaStdlibLoader.append("\n");
    return javaStdlibLoader.toString();
  }


  private static void generateJavaStdlibLoader(String escmDir, String generatedFilesDir) {
    String contents = getJavaStdlibLoaderContents(escmDir);
    Path stdlibLoaderPath = Path.of(generatedFilesDir+File.separator+"JavaStdLibLoader.java");
    try {
      Files.deleteIfExists(stdlibLoaderPath);
      Files.writeString(stdlibLoaderPath,contents);
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't create java stdlib loader file: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if(VERBOSE_MODE) {
      System.out.println("> Successfully generated the Java standard library loader!");
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Compile EScheme
  private static void recursivelyDeleteDirectory(Path dir) throws Exception {
    if(Files.exists(dir) && Files.isDirectory(dir)) {
      for(File entry : (new File(dir.toString())).listFiles()) {
        Path entryPath = entry.toPath();
        if(Files.isDirectory(entryPath)) {
          recursivelyDeleteDirectory(entryPath);
        } else {
          entry.delete();
        }
      }
      Files.deleteIfExists(dir);
    }
  }

  private static void deleteBinIfExists(String binPath) {
    try {
      recursivelyDeleteDirectory(Path.of(binPath));
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't clear directory to store bin files: "+binPath);
      System.err.println("  error: "+e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }

  private static void compileEScheme(String escmDir) {
    String compileCmd = decorateJvmCmdPath("javac")+" -source 11 -target 11 -d "+escmDir+File.separator+"bin -classpath "+escmDir+File.separator+"src "+escmDir+File.separator+"src"+File.separator+"Main.java";
    deleteBinIfExists(escmDir+File.separator+"bin");
    try {
      ExecuteCommandResult res = executeCommand(compileCmd,false);
      if(res.exit != 0) {
        System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't compile: "+escmDir+File.separator+"src"+File.separator+"Main.java");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if(res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer SRC Compilation Warning(s) :\n  %s\n", res.err.replaceAll("\n","\n  "));
      }
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't compile: "+escmDir+File.separator+"src"+File.separator+"Main.java");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialize "src/stdlib.scm"
  private static void serializeESchemeStdLib(String escmDir) {
    String serializeCmd = decorateJvmCmdPath("java")+" -classpath "+escmDir+File.separator+"bin Main --serialize-stdlib";
    try {
      ExecuteCommandResult res = executeCommand(serializeCmd,false);
      if(res.exit != 0) {
        System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't serialize the EScheme Standard Library!");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if(res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer STDLIB Serialization Warning(s) :\n  %s\n", res.err.replaceAll("\n","\n  "));
      }
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Can't serialize the EScheme Standard Library!");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Execute EScheme's Unit Test Suite
  private static void executeUnitTests(String escmDir) {
    String unitTestCmd = decorateJvmCmdPath("java")+" -classpath "+escmDir+File.separator+"bin Main --unit-tests";
    try {
      ExecuteCommandResult res = executeCommand(unitTestCmd,true);
      if(res.exit != 0) {
        System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Error Executing EScheme Unit Tests!");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if(res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer EScheme Unit Test Warning(s) :\n  %s\n", res.err.replaceAll("\n","\n  "));
      }
    } catch(Exception e) {
      System.err.println("> [ FATAL ] ESCM INSTALLER ERROR: Error Executing EScheme Unit Tests!");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCM INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Generate the Shell Alias to Invoke EScheme
  private static void printEscmShellAliasString(String escmDir) {
    System.out.println("> [ OPTIONAL ] Alias for the REPL to put in `~/.bashrc` OR `~/.zshrc`:");
    System.out.println("  alias escm='"+decorateJvmCmdPath("java")+" -classpath "+escmDir+File.separator+"bin Main'");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main Dispatch
  public static void main(String[] args) {
    parseCommandLine(args);
    String escmDir = getEscmDirectory();
    String generatedFilesDir = createInstallerNewFilesDirectory(escmDir);
    generateEscmPath(escmDir,generatedFilesDir);
    generateJvmPathPrefix(generatedFilesDir);
    generateEscmStdlibLoader(escmDir,generatedFilesDir);
    generateJavaStdlibLoader(escmDir,generatedFilesDir);
    compileEScheme(escmDir);
    if(INTERPRET_STDLIB == false) serializeESchemeStdLib(escmDir);
    if(EXECUTE_UNIT_TESTS == true) executeUnitTests(escmDir);
    printEscmShellAliasString(escmDir);
    System.out.println("> Successfully installed EScheme! Happy Hacking :)");
  }
}