// Author: Jordan Randleman - escm.util.ExecuteSystemCommand
// Purpose:
//    Wrapper around <Runtime.getRuntime().exec()> to synchronously execute
//    cmd(s) in a seperate process. Returns the <stdout>, <stderr>, and exit 
//    code from the execution.

package escm.util;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.io.File;
import escm.util.error.Exceptionf;

public class ExecuteSystemCommand {
  ////////////////////////////////////////////////////////////////////////////
  // Result of executing a system command
  public static class Result {
    public String out = "";
    public String err = "";
    public int exit = 0;
  };

  ////////////////////////////////////////////////////////////////////////////
  // Timeout buffer to account for ESCHEME actions done while process executes
  // => e.g. stream reading & finished status setting
  private static long TIMEOUT_MS_BUFFER = (long) 100;

  ////////////////////////////////////////////////////////////////////////////
  // Split a command string into an array of arguments
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
      throw new Exceptionf("Bad <system> command; Incomplete escape sequence: %s", command);
    }
    if (inSingleQuotes || inDoubleQuotes) {
      throw new Exceptionf("Bad <system> command; Mismatched quotes: %s", command);
    }
    if (current.length() > 0) {
      tokens.add(current.toString());
    }
    return tokens.toArray(new String[0]);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Process main execution logic
  private static class ClosureBoolean {
    public boolean status = false;
  }

  private static class ClosureString {
    public String value = "";
  }

  private static long preprocessMillisecondTimeout(long millisecondTimeout) {
    if (millisecondTimeout < 0)
      millisecondTimeout = 0;
    if (Long.MAX_VALUE - TIMEOUT_MS_BUFFER <= millisecondTimeout)
      return Long.MAX_VALUE;
    return millisecondTimeout + TIMEOUT_MS_BUFFER;
  }

  private static void getInputStreamLines(Result res, Process pro) throws Exception {
    ClosureString output = new ClosureString();
    ClosureString error = new ClosureString();
    Thread outputThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getInputStream()));
        StringBuilder buffer = new StringBuilder();
        String line = null;
        while ((line = reader.readLine()) != null)
          buffer.append('\n' + line);
        output.value = buffer.toString();
        pro.waitFor();
      } catch (Throwable e) {
        /* do nothing */ }
    });
    Thread errorThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getErrorStream()));
        StringBuilder buffer = new StringBuilder();
        String line = null;
        while ((line = reader.readLine()) != null)
          buffer.append('\n' + line);
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

  public static Result run(long millisecondTimeout, String command, String[] environmentVariableSettings,
      File workingDirectory) throws Exception {
    long timeout = preprocessMillisecondTimeout(millisecondTimeout);
    Process pro = Runtime.getRuntime().exec(splitCommand(command), environmentVariableSettings, workingDirectory);
    Result res = new Result();
    ClosureBoolean finished = new ClosureBoolean();
    Thread processThread = new Thread(() -> {
      try {
        getInputStreamLines(res, pro);
        finished.status = true;
      } catch (Throwable e) {
      }
    });
    try {
      processThread.start();
      processThread.join(timeout);
    } catch (Throwable t) {
    }
    if (finished.status == false) {
      pro.destroyForcibly();
      if (res.err.length() != 0)
        res.err += '\n';
      res.err += String.format("ESCHEME: Process killed after exceeding %dms time limit.", millisecondTimeout);
      res.exit = -1;
    } else {
      res.exit = pro.exitValue();
    }
    return res;
  }

  public static Result run(String command, String[] environmentVariableSettings, File workingDirectory)
      throws Exception {
    Process pro = Runtime.getRuntime().exec(splitCommand(command), environmentVariableSettings, workingDirectory);
    Result res = new Result();
    getInputStreamLines(res, pro);
    res.exit = pro.exitValue();
    return res;
  }

  public static Result run(long millisecondTimeout, String command) throws Exception {
    return run(millisecondTimeout, command, null, null);
  }

  public static Result run(String command) throws Exception {
    return run(command, null, null);
  }
}