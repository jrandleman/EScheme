// Author: Jordan Randleman - escm.util.ExecuteSystemCommand
// Purpose:
//    Wrapper around <Runtime.getRuntime().exec()> to synchronously execute
//    cmd(s) in a seperate process. Returns the <stdout>, <stderr>, and exit 
//    value from the execution.

package escm.util;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.File;
import java.util.concurrent.TimeUnit;

public class ExecuteSystemCommand {
  ////////////////////////////////////////////////////////////////////////////
  // Result of executing a system command
  public static class Result {
    public String out = "";
    public String err = "";
    public int exit = 0;
  };


  ////////////////////////////////////////////////////////////////////////////
  // Timeout buffer to account for ESCM actions done while process executes
  // => e.g. stream reading & finished status setting
  private static long TIMEOUT_MS_BUFFER = (long)100;


  ////////////////////////////////////////////////////////////////////////////
  // Process main execution logic
  private static class ClosureBoolean {
    public boolean status = false;
  }

  private static class ClosureString {
    public String value = "";
  }

  private static long preprocessMillisecondTimeout(long millisecondTimeout) {
    if(millisecondTimeout < 0) millisecondTimeout = 0;
    if(Long.MAX_VALUE - TIMEOUT_MS_BUFFER <= millisecondTimeout) return Long.MAX_VALUE;
    return millisecondTimeout+TIMEOUT_MS_BUFFER;
  }

  private static void getInputStreamLines(Result res, Process pro) throws Exception {
    ClosureString output = new ClosureString();
    ClosureString error = new ClosureString();
    Thread outputThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getInputStream()));
        StringBuilder buffer = new StringBuilder();
        String line = null;
        while((line = reader.readLine()) != null) buffer.append('\n'+line);
        output.value = buffer.toString();
        pro.waitFor();
      } catch(Throwable e) { /* do nothing */ }
    });
    Thread errorThread = new Thread(() -> {
      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(pro.getErrorStream()));
        StringBuilder buffer = new StringBuilder();
        String line = null;
        while((line = reader.readLine()) != null) buffer.append('\n'+line);
        error.value = buffer.toString();
        pro.waitFor();
      } catch(Throwable e) { /* do nothing */ }
    });
    outputThread.start();
    errorThread.start();
    outputThread.join();
    errorThread.join();
    res.out = output.value.length() == 0 ? "" : output.value.substring(1);
    res.err = error.value.length() == 0 ? "" : error.value.substring(1);
  }

  public static Result run(long millisecondTimeout, String command, String[] environmentVariableSettings, File workingDirectory) throws Exception {
    long timeout = preprocessMillisecondTimeout(millisecondTimeout);
    Process pro = Runtime.getRuntime().exec(command,environmentVariableSettings,workingDirectory);
    Result res = new Result();
    ClosureBoolean finished = new ClosureBoolean();
    Thread processThread = new Thread(() -> {
      try {
        getInputStreamLines(res,pro);
        finished.status = true;
      } catch(Throwable e) {}
    });
    try {
      processThread.start();
      processThread.join(timeout);
    } catch(Throwable t) {}
    if(finished.status == false) {
      pro.destroyForcibly();
      if(res.err.length() != 0) res.err += '\n';
      res.err += String.format("ESCM: Process killed after exceeding %dms time limit.", millisecondTimeout);
      res.exit = -1;
    } else {
      res.exit = pro.exitValue();
    }
    return res;
  }

  public static Result run(String command, String[] environmentVariableSettings, File workingDirectory) throws Exception {
    Process pro = Runtime.getRuntime().exec(command,environmentVariableSettings,workingDirectory);
    Result res = new Result();
    getInputStreamLines(res,pro);
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