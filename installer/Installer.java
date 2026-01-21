// Author: Jordan Randleman - Installer - MUST BE RUN WITHIN THE `EScheme/installer` DIRECTORY!
//   => Command-Line Options: 
//        -h, --help       (show these command-line options)
//        -v, --verbose    (print status updates)
//        -u, --unit-tests (run EScheme's unit test suite after compilation)

/*
  Purpose: Compiles/Installs the EScheme runtime. This is done by accomplishing several tasks:
    
    1. Generate a file to store the path to our EScheme implementation.

    2. Generate a file to store the path prefix to our EScheme's JVM/JRE.

    3. Generate a sentinel file to load the Java stdlib (re-generated later in installation).
    
    4. Identify all of the "Outer Class" names in the <escm.primitive> package, and splice 
       such as imports into a Java-stdlib-loader-generator file that uses reflection to create 
       a Java-stdlib-loader that instantiates each Inner Class of the file (iff it implements 1 
       of the primitive function object interfaces) to be defined in EScheme's global environments.

    5. Compile the EScheme <Main.java> file using Java 21 via <Runtime.exec()>

    6. Execute `escm --generate-java-stdlib-loader` to re-generate the Java-stdlib-loader.

    7. Recompile EScheme to now use the newly generated Java-stdlib-loader.

    8. Execute EScheme unit test suite (if requested).

    9. Output the <alias> string to stdout, for easy copying into "~/.zshrc" or "~/.bashrc".
*/

import java.nio.file.Path;
import java.nio.file.Files;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.File;
import java.util.ArrayList;

public class Installer {
  ////////////////////////////////////////////////////////////////////////////
  // Java Runtime Bin Path (set on launch by <void main(String[])>)
  private static String JAVA_BIN_PATH = null;

  ////////////////////////////////////////////////////////////////////////////
  // Command-Line Parsing Setting Satus
  private static boolean VERBOSE_MODE = false;
  private static boolean EXECUTE_UNIT_TESTS = false;

  public static final String COMMAND_LINE_FLAGS = "  1. -h, --help       | Print this information\n" +
      "  2. -v, --verbose    | Print out installation progress messages\n" +
      "  3. -u, --unit-tests | Run EScheme's unit test suite after compilation\n";

  private static void parseCommandLine(String[] args) {
    if (args.length == 0)
      return;
    for (int i = 0; i < args.length; ++i) {
      switch (args[i]) {
        case "-v":
        case "--verbose": {
          VERBOSE_MODE = true;
          break;
        }
        case "-u":
        case "--unit-tests": {
          EXECUTE_UNIT_TESTS = true;
          break;
        }
        case "-h":
        case "--help": {
          System.out.println("Supported Command-Line Flags Include:");
          System.out.print(COMMAND_LINE_FLAGS);
          System.out.flush();
          System.exit(0);
        }
        default: {
          System.err.printf(
              "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Invalid command-line argument \"%s\"! Use:\n",
              args[i]);
          System.err.print(COMMAND_LINE_FLAGS);
          System.exit(1);
        }
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // JVM/JRE Command Decoration
  private static String decorateJvmCmdPath(String cmd) {
    return JAVA_BIN_PATH + cmd;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Command String Splitting
  // => Splits along whitespace while respecting quotes & escaped characters.
  public static String[] splitCommand(String command) throws Exception {
    ArrayList<String> tokens = new ArrayList<String>();
    StringBuilder current = new StringBuilder();
    boolean inSingleQuotes = false;
    boolean inDoubleQuotes = false;
    boolean escaping = false;
    for (int i = 0; i < command.length(); i++) {
      char c = command.charAt(i);
      if (escaping) {
        current.append(c); // Append the escaped character regardless of its type
        escaping = false;
      } else if (c == '\\') {
        escaping = true; // Next character is escaped
      } else if (c == '\'' && !inDoubleQuotes) {
        inSingleQuotes = !inSingleQuotes; // Toggle single quotes if not inside double quotes
      } else if (c == '"' && !inSingleQuotes) {
        inDoubleQuotes = !inDoubleQuotes; // Toggle double quotes if not inside single quotes
      } else if (Character.isWhitespace(c) && !inSingleQuotes && !inDoubleQuotes) {
        if (current.length() > 0) {
          tokens.add(current.toString()); // Token delimiter found outside quotes
          current.setLength(0);
        }
      } else {
        current.append(c); // Regular character
      }
    }
    // After processing all characters, check for unclosed quotes or escaping
    if (escaping) {
      throw new Exception(String.format("Bad <system> command; Incomplete escape sequence: %s", command));
    }
    if (inSingleQuotes || inDoubleQuotes) {
      throw new Exception(String.format("Bad <system> command; Mismatched quotes: %s", command));
    }
    if (current.length() > 0) {
      tokens.add(current.toString());
    }
    return tokens.toArray(new String[0]);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Terminal Command Execution
  private static class ExecuteCommandResult {
    public String out = null;
    public String err = null;
    public int exit = 0;
  };

  private static class ClosureString {
    public String value = "";
  };

  private static void readStream(StringBuilder buffer, BufferedReader in) throws Exception {
    String line = null;
    while ((line = in.readLine()) != null)
      buffer.append('\n' + line);
  }

  private static void readStreamAndReportLive(StringBuilder buffer, BufferedReader in) throws Exception {
    int last_ch = -1, ch = -1;
    while ((ch = in.read()) != -1) {
      boolean line_start = (char) last_ch == '\n' || last_ch == -1;
      if (line_start) {
        System.out.print("  ");
        buffer.append('\n');
      }
      System.out.print((char) ch);
      System.out.flush();
      buffer.append((char) ch);
      last_ch = ch;
    }
  }

  private static void getInputStreamLines(ExecuteCommandResult res, Process pro, boolean reportLive) throws Exception {
    ClosureString output = new ClosureString();
    ClosureString error = new ClosureString();
    Thread outputThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getInputStream()));
        StringBuilder buffer = new StringBuilder();
        if (reportLive) {
          readStreamAndReportLive(buffer, reader);
        } else {
          readStream(buffer, reader);
        }
        output.value = buffer.toString();
        pro.waitFor();
      } catch (Throwable e) {
        /* do nothing */ }
    });
    Thread errorThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getErrorStream()));
        StringBuilder buffer = new StringBuilder();
        readStream(buffer, reader);
        error.value = buffer.toString();
        pro.waitFor();
      } catch (Throwable e) {
        /* do nothing */ }
    });
    outputThread.start();
    errorThread.start();
    outputThread.join();
    errorThread.join();
    res.out = output.value.length() == 0 ? "" : output.value.substring(1);
    res.err = error.value.length() == 0 ? "" : error.value.substring(1);
  }

  private static ExecuteCommandResult executeCommand(String command, boolean reportLive) throws Exception {
    if (VERBOSE_MODE) {
      System.out.println("> Executing Command:");
      System.out.println("  \"" + command + "\"");
    }
    Process pro = Runtime.getRuntime().exec(splitCommand(command));
    ExecuteCommandResult res = new ExecuteCommandResult();
    getInputStreamLines(res, pro, reportLive);
    pro.waitFor();
    res.exit = pro.exitValue();
    return res;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Retrieve EScheme Directory
  private static String getEscmDirectory() {
    String cwd = Path.of("").toAbsolutePath().toString();
    return cwd.substring(0, cwd.lastIndexOf(File.separator));
  }

  ////////////////////////////////////////////////////////////////////////////
  // Create the Directory for our Generated Files
  private static String createInstallerNewFilesDirectory(String escmDir) {
    String generatedFilesDir = escmDir + File.separator + "src" + File.separator + "escm" +
        File.separator + "vm" + File.separator + "runtime" +
        File.separator + "installerGenerated";
    try {
      Path generatedFilesPath = Path.of(generatedFilesDir);
      if (Files.exists(generatedFilesPath) == true) {
        for (File entry : (new File(generatedFilesDir)).listFiles())
          entry.delete();
        Files.deleteIfExists(generatedFilesPath);
      }
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't clear directory to store generated files: "
              + generatedFilesDir);
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if ((new File(generatedFilesDir)).mkdirs() == false) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't create directory for generated file: "
              + generatedFilesDir);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    return generatedFilesDir;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Generate File to Store the Path to our EScheme Implementation
  private static void generateEscmPath(String escmDir, String generatedFilesDir) {
    Path escmPathPath = Path.of(generatedFilesDir + File.separator + "EscmPath.java");
    StringBuilder escmPath = new StringBuilder();
    escmPath.append("// Generated by `installer" + File.separator + "Installer.java`\n");
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
      Files.writeString(escmPathPath, escmPath.toString());
    } catch (Exception e) {
      System.err
          .println("> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't create escm path file: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if (VERBOSE_MODE) {
      System.out.println("> \u2705 Successfully generated the path file to our EScheme implementation!");
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Generate File to Store the Path Prefix to our JVM/JRE
  private static void generateJvmPathPrefix(String generatedFilesDir) {
    Path jvmPathPrefixPath = Path.of(generatedFilesDir + File.separator + "JvmPathPrefix.java");
    StringBuilder escmPath = new StringBuilder();
    escmPath.append("// Generated by `installer" + File.separator + "Installer.java`\n");
    escmPath.append("// Purpose:\n");
    escmPath.append("//   Store the path prefix to our JVM/JRE.\n");
    escmPath.append("\n");
    escmPath.append("package escm.vm.runtime.installerGenerated;\n");
    escmPath.append("\n");
    escmPath.append("public class JvmPathPrefix {\n");
    escmPath.append("  public static final java.lang.String VALUE = \"" + JAVA_BIN_PATH + "\";\n");
    escmPath.append("}\n");
    escmPath.append("\n");
    try {
      Files.deleteIfExists(jvmPathPrefixPath);
      Files.writeString(jvmPathPrefixPath, escmPath.toString());
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't create jvm/jre path prefix file: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if (VERBOSE_MODE) {
      System.out.println("> \u2705 Successfully generated the path prefix file to our JVM/JRE!");
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Generate Sentinel Java-stdlib-loader File
  // * Re-writted by the Java-stdlib-loader-generator (see below)
  // * Still required temporarily though for EScheme to compile at all
  private static void generateSentinelJavaStdLibLoader(String generatedFilesDir) {
    Path javaStdLibLoaderPath = Path.of(generatedFilesDir + File.separator + "JavaStdLibLoader.java");
    StringBuilder javaStdLibLoaderCode = new StringBuilder();
    javaStdLibLoaderCode.append("// Generated by `installer" + File.separator + "Installer.java`\n");
    javaStdLibLoaderCode.append("// Purpose:\n");
    javaStdLibLoaderCode.append("//   Bind all of EScheme's Java primitive functions to the given global\n");
    javaStdLibLoaderCode.append("//   environment.\n");
    javaStdLibLoaderCode.append("\n");
    javaStdLibLoaderCode.append("package escm.vm.runtime.installerGenerated;\n");
    javaStdLibLoaderCode.append("import escm.vm.util.Environment;\n");
    javaStdLibLoaderCode.append("\n");
    javaStdLibLoaderCode.append("public class JavaStdLibLoader {\n");
    javaStdLibLoaderCode.append("  public static void load(Environment definitionEnvironment) {\n");
    javaStdLibLoaderCode.append(
        "    System.err.println(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER ERROR \\ud83d\\udea8: THE JAVA STDLIB LOADER GENERATOR WAS NEVER EXECUTED (RE-RUN INSTALLER)!\");\n");
    javaStdLibLoaderCode.append("    System.exit(1);\n");
    javaStdLibLoaderCode.append("  }\n");
    javaStdLibLoaderCode.append("}\n");
    try {
      Files.deleteIfExists(javaStdLibLoaderPath);
      Files.writeString(javaStdLibLoaderPath, javaStdLibLoaderCode.toString());
    } catch (Exception e) {
      System.err
          .println(
              "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't create the sentinel Java-stdlib-loader file: "
                  + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if (VERBOSE_MODE) {
      System.out.println("> \u2705 Successfully generated the sentinel Java-stdlib-loader file!");
    }
  }

  private static String getPrimitivePackageName(boolean firstCall, String primitiveDirectoryPath) {
    return firstCall ? "" : Path.of(primitiveDirectoryPath).getFileName().toString() + ".";
  }

  private static ArrayList<String> getPrimitiveFileNames(boolean firstCall, String primitiveDirectoryPath) {
    String pkgName = getPrimitivePackageName(firstCall, primitiveDirectoryPath);
    File primitiveDirectory = new File(primitiveDirectoryPath);
    ArrayList<String> prmFileNames = new ArrayList<String>();
    for (File entry : primitiveDirectory.listFiles()) {
      if (entry.isFile()) {
        String fileName = entry.getName();
        if (fileName.endsWith(".java")) {
          prmFileNames.add(pkgName + fileName.substring(0, fileName.lastIndexOf(".java")));
        }
      } else if (entry.isDirectory()) {
        ArrayList<String> innerFileNames = getPrimitiveFileNames(false, entry.getPath());
        for (String innerFileName : innerFileNames) {
          prmFileNames.add(pkgName + innerFileName);
        }
      }
    }
    return prmFileNames;
  }

  private static String getJavaStdlibLoaderGeneratorContents(String escmDir, String generatedFilesDir) {
    ArrayList<String> prmFileNames = getPrimitiveFileNames(true,
        escmDir + File.separator + "src" + File.separator + "escm" + File.separator + "primitive");
    StringBuilder javaStdlibLoader = new StringBuilder();
    javaStdlibLoader.append("// Generated by `installer" + File.separator + "Installer.java`\n");
    javaStdlibLoader.append("// Purpose:\n");
    javaStdlibLoader.append("//    Class to regenerate <JavaStdLibLoader> after reflecting on the contents of\n");
    javaStdlibLoader
        .append("//    EScheme's Java primitive files. Launched by `escm --generate-java-stdlib-loader`,\n");
    javaStdlibLoader.append("//    which in turn is executed by the installer.\n");
    javaStdlibLoader.append("//\n");
    javaStdlibLoader.append("//    Reflection is accomplished by parsing all of the \"Outer Classes\" in\n");
    javaStdlibLoader.append("//    escm.type.primitive and extracting instances of their inner static `Primitive`,\n");
    javaStdlibLoader.append("//    `PrimitiveCallable`, `PrimitiveSyntax`, and `PrimitiveSyntaxCallable` classes.\n");
    javaStdlibLoader.append("//    These instances' names are then used to generate an efficient loader that\n");
    javaStdlibLoader.append("//    defines them as EScheme primitive function arguments in a given global scope.\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("package escm.vm.runtime.installerGenerated;\n");
    javaStdlibLoader.append("import java.nio.file.Files;\n");
    javaStdlibLoader.append("import java.nio.file.Path;\n");
    javaStdlibLoader.append("import escm.util.error.Exceptionf;\n");
    javaStdlibLoader.append("import escm.vm.type.primitive.Primitive;\n");
    javaStdlibLoader.append("import escm.vm.type.primitive.PrimitiveCallable;\n");
    javaStdlibLoader.append("import escm.vm.type.primitive.PrimitiveSyntax;\n");
    javaStdlibLoader.append("import escm.vm.type.primitive.PrimitiveSyntaxCallable;\n");
    for (String prmFileName : prmFileNames) {
      javaStdlibLoader.append("import escm.primitive." + prmFileName + ";\n");
    }
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("public class JavaStdLibLoaderGenerator {\n");
    javaStdlibLoader
        .append("  private static StringBuilder getPrimitiveDefinitionsFromInnerClassesInClass(String className) {\n");
    javaStdlibLoader.append("    // Get the outer Class\n");
    javaStdlibLoader.append("    Class<?> outerClass = null;\n");
    javaStdlibLoader.append("    StringBuilder sb = new StringBuilder();\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      outerClass = Class.forName(className);\n");
    javaStdlibLoader.append("    } catch(ClassNotFoundException e) {\n");
    javaStdlibLoader.append(
        "      System.err.printf(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER-GENERATOR ERROR \\ud83d\\udea8: Couldn't find class \\\"%s\\\": %s\\n\", className, e);\n");
    javaStdlibLoader.append("      return new StringBuilder();\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("    // Get the inner Classes of the outer Class\n");
    javaStdlibLoader.append("    Class<?>[] innerClasses = null;\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      innerClasses = outerClass.getDeclaredClasses();\n");
    javaStdlibLoader.append("    } catch(SecurityException e) {\n");
    javaStdlibLoader.append(
        "      System.err.printf(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER-GENERATOR ERROR \\ud83d\\udea8: Security exception finding inner classes for class \\\"%s\\\": %s\\n\", className, e);\n");
    javaStdlibLoader.append("      return new StringBuilder();\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader
        .append("    // Define primitives in the global environment as object instances of the inner class\n");
    javaStdlibLoader.append("    for(Class<?> innerClass : innerClasses) {\n");
    javaStdlibLoader.append(
        "      if(!innerClass.isInterface() && (Primitive.class.isAssignableFrom(innerClass) || PrimitiveCallable.class.isAssignableFrom(innerClass) || PrimitiveSyntax.class.isAssignableFrom(innerClass) || PrimitiveSyntaxCallable.class.isAssignableFrom(innerClass))) {\n");
    javaStdlibLoader.append("        String canonicalName = innerClass.getCanonicalName();\n");
    javaStdlibLoader.append("        if(canonicalName != null) {\n");
    javaStdlibLoader.append(
        "          sb.append(\"    definePrimitive(definitionEnvironment,new \"+innerClass.getCanonicalName()+\"());\\n\");\n");
    javaStdlibLoader.append("        }\n");
    javaStdlibLoader.append("      }\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("    return sb;\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("  private static StringBuilder getJavaStdLibLoaderContents() {\n");
    javaStdlibLoader.append("    StringBuilder sb = new StringBuilder();\n");
    javaStdlibLoader.append("    sb.append(\"// Generated by `JavaStdLibLoaderGenerator.java`\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"// Purpose:\\n\");\n");
    javaStdlibLoader
        .append("    sb.append(\"//   Bind all of EScheme's Java primitive functions to the given global\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"//   environment.\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"package escm.vm.runtime.installerGenerated;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.type.Symbol;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.vm.type.primitive.Primitive;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.vm.type.primitive.PrimitiveCallable;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.vm.type.primitive.PrimitiveSyntax;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.vm.type.primitive.PrimitiveSyntaxCallable;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"import escm.vm.util.Environment;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"public class JavaStdLibLoader {\\n\");\n");
    javaStdlibLoader.append(
        "    sb.append(\"  private static void definePrimitive(Environment definitionEnvironment, Primitive p) {\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    p.definitionEnvironment = definitionEnvironment;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    definitionEnvironment.define(new Symbol(p.escmName()),p);\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"  }\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append(
        "    sb.append(\"  private static void definePrimitive(Environment definitionEnvironment, PrimitiveCallable p) {\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    p.definitionEnvironment = definitionEnvironment;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    definitionEnvironment.define(new Symbol(p.escmName()),p);\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"  }\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append(
        "    sb.append(\"  private static void definePrimitive(Environment definitionEnvironment, PrimitiveSyntax p) {\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    p.definitionEnvironment = definitionEnvironment;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    definitionEnvironment.define(new Symbol(p.escmName()),p);\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"  }\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append(
        "    sb.append(\"  private static void definePrimitive(Environment definitionEnvironment, PrimitiveSyntaxCallable p) {\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    p.definitionEnvironment = definitionEnvironment;\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"    definitionEnvironment.define(new Symbol(p.escmName()),p);\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"  }\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"  public static void load(Environment definitionEnvironment) {\\n\");\n");
    for (String prmFileName : prmFileNames) {
      javaStdlibLoader.append(
          "    sb.append(getPrimitiveDefinitionsFromInnerClassesInClass(\"escm.primitive." + prmFileName + "\"));\n");
    }
    javaStdlibLoader.append("    sb.append(\"  }\\n\");\n");
    javaStdlibLoader.append("    sb.append(\"}\\n\");\n");
    javaStdlibLoader.append("    return sb;\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append(
        "  private static void deleteExistingSentinelJavaStdLibLoader(Path javaStdLibLoaderPath) throws Exception {\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      if(Files.deleteIfExists(javaStdLibLoaderPath) == false)\n");
    javaStdlibLoader.append(
        "        throw new Exceptionf(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER-GENERATOR ERROR \\ud83d\\udea8: Couldn't delete existing sentinel loader file: \\\"%s\\\"\", javaStdLibLoaderPath);\n");
    javaStdlibLoader.append("    } catch (Exception e) {\n");
    javaStdlibLoader.append(
        "      throw new Exceptionf(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER-GENERATOR ERROR \\ud83d\\udea8: Couldn't delete existing sentinel loader file: \\\"%s\\\"\", javaStdLibLoaderPath);\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append(
        "  private static void writeNewJavaStdLibLoader(Path javaStdLibLoaderPath, StringBuilder contents) throws Exception {\n");
    javaStdlibLoader.append("    try {\n");
    javaStdlibLoader.append("      Files.writeString(javaStdLibLoaderPath,contents);\n");
    javaStdlibLoader.append("    } catch (Exception e) {\n");
    javaStdlibLoader.append(
        "      throw new Exceptionf(\"\\ud83d\\udea8 ESCHEME JAVA-STDLIB-LOADER-GENERATOR ERROR \\ud83d\\udea8: Couldn't write new contents to loader file: \\\"%s\\\"\", javaStdLibLoaderPath);\n");
    javaStdlibLoader.append("    }\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("\n");
    javaStdlibLoader.append("  public static void execute() throws Exception {\n");
    javaStdlibLoader.append("    Path javaStdLibLoaderPath = Path.of(\"" + generatedFilesDir + "\"+\"" + File.separator
        + "\"+\"JavaStdLibLoader.java\");\n");
    javaStdlibLoader.append("    deleteExistingSentinelJavaStdLibLoader(javaStdLibLoaderPath);\n");
    javaStdlibLoader.append("    writeNewJavaStdLibLoader(javaStdLibLoaderPath,getJavaStdLibLoaderContents());\n");
    javaStdlibLoader.append("  }\n");
    javaStdlibLoader.append("}\n");
    return javaStdlibLoader.toString();
  }

  private static void generateJavaStdlibLoaderGenerator(String escmDir, String generatedFilesDir) {
    String contents = getJavaStdlibLoaderGeneratorContents(escmDir, generatedFilesDir);
    Path stdlibLoaderPath = Path.of(generatedFilesDir + File.separator + "JavaStdLibLoaderGenerator.java");
    try {
      Files.deleteIfExists(stdlibLoaderPath);
      Files.writeString(stdlibLoaderPath, contents);
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't create Java-stdlib-loader-generator file: "
              + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
    if (VERBOSE_MODE) {
      System.out.println("> \u2705 Successfully generated the Java-stdlib-loader-generator file!");
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Compile EScheme
  private static void recursivelyDeleteDirectory(Path dir) throws Exception {
    if (Files.exists(dir) && Files.isDirectory(dir)) {
      for (File entry : (new File(dir.toString())).listFiles()) {
        Path entryPath = entry.toPath();
        if (Files.isDirectory(entryPath)) {
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
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't clear directory to store bin files: "
              + binPath);
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }

  private static void compileEScheme(String escmDir) {
    String compileCmd = decorateJvmCmdPath("javac") + " -source 21 -target 21 -d \"" + escmDir + File.separator
        + "bin\" -classpath \"" + escmDir + File.separator + "src\" \"" + escmDir + File.separator + "src"
        + File.separator + "Main.java\"";
    deleteBinIfExists(escmDir + File.separator + "bin");
    try {
      ExecuteCommandResult res = executeCommand(compileCmd, false);
      if (res.exit != 0) {
        System.err.println("> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't compile: " + escmDir
            + File.separator + "src"
            + File.separator + "Main.java");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if (res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer SRC Compilation Warning(s) :\n  %s\n",
            res.err.replaceAll("\n", "\n  "));
      }
    } catch (Exception e) {
      System.err.println("> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Can't compile: " + escmDir
          + File.separator + "src"
          + File.separator + "Main.java");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Execute EScheme's Java-stdlib-loader-generator File
  private static void executeJavaStdLibLoaderGenerator(String escmDir) {
    String unitTestCmd = decorateJvmCmdPath("java") + " -classpath \"" + escmDir + File.separator
        + "bin\" Main --generate-java-stdlib-loader";
    try {
      ExecuteCommandResult res = executeCommand(unitTestCmd, true);
      if (res.exit != 0) {
        System.err
            .println(
                "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Error Executing EScheme's Java-stdlib-loader-generator!");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if (res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer Java-stdlib-loader-generator Warning(s) :\n  %s\n",
            res.err.replaceAll("\n", "\n  "));
      }
    } catch (Exception e) {
      System.err
          .println(
              "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Error Executing EScheme's Java-stdlib-loader-generator!");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Execute EScheme's Unit Test Suite
  private static void executeUnitTests(String escmDir) {
    String unitTestCmd = decorateJvmCmdPath("java") + " -classpath \"" + escmDir + File.separator
        + "bin\" Main --unit-tests";
    try {
      ExecuteCommandResult res = executeCommand(unitTestCmd, true);
      if (res.exit != 0) {
        System.err.println(
            "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Error Executing EScheme Unit Tests!");
        System.err.println("  exit: " + String.valueOf(res.exit));
        System.err.println("  error: " + res.err);
        System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
        System.exit(1);
      } else if (res.err.length() > 0 && VERBOSE_MODE) {
        System.err.printf("> [ NON FATAL ] EScheme Installer EScheme Unit Test Warning(s) :\n  %s\n",
            res.err.replaceAll("\n", "\n  "));
      }
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Error Executing EScheme Unit Tests!");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // High-level Executor to Generate an Efficient EScheme Java StdLib Loader
  private static void generateJavaStdlibLoader(String escmDir, String generatedFilesDir) {
    generateSentinelJavaStdLibLoader(generatedFilesDir);
    generateJavaStdlibLoaderGenerator(escmDir, generatedFilesDir);
    compileEScheme(escmDir); // to be able to execute the loader generator
    executeJavaStdLibLoaderGenerator(escmDir);
    compileEScheme(escmDir); // to be able to use the newly generated loader
  }

  ////////////////////////////////////////////////////////////////////////////
  // Generate the Shell Alias to Invoke EScheme
  private static void printEscmShellAliasString(String escmDir) {
    System.out.println("> [ OPTIONAL ] Alias for the REPL to put in `~/.bashrc` OR `~/.zshrc`:");
    System.out.println(
        "  alias escm='" + decorateJvmCmdPath("java") + " -classpath \"" + escmDir + File.separator + "bin\" Main'");
  }

  ////////////////////////////////////////////////////////////////////////////
  // Get Java Home Directory
  private static String getJavaHomeDirectory() {
    try {
      String homePath = System.getProperty("java.home");
      if (homePath.endsWith(File.separator) == false) {
        return homePath + File.separator + "bin" + File.separator;
      } else {
        return homePath + "bin" + File.separator;
      }
    } catch (Exception e) {
      System.err.println(
          "> [ FATAL ] \ud83d\udea8 ESCHEME INSTALLER ERROR \ud83d\udea8: Error Getting \"java.home\" System Property!");
      System.err.println("  error: " + e);
      System.err.println("> TERMINATING THE ESCHEME INSTALLER. RESOLVE AND RETRY.");
      System.exit(1);
      return null; // never reached: left in to satisfy <javac> type-checking
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Main Dispatch
  public static void main(String[] args) {
    JAVA_BIN_PATH = getJavaHomeDirectory();
    parseCommandLine(args);
    String escmDir = getEscmDirectory();
    String generatedFilesDir = createInstallerNewFilesDirectory(escmDir);
    generateEscmPath(escmDir, generatedFilesDir);
    generateJvmPathPrefix(generatedFilesDir);
    generateJavaStdlibLoader(escmDir, generatedFilesDir);
    if (EXECUTE_UNIT_TESTS == true)
      executeUnitTests(escmDir);
    printEscmShellAliasString(escmDir);
    System.out.println("> \u2705 Successfully installed EScheme! Happy Hacking :)");
  }
}