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

  private static long preprocessMillisecondTimeout(long millisecondTimeout) {
    if(millisecondTimeout < 0) millisecondTimeout = 0;
    if(Long.MAX_VALUE - TIMEOUT_MS_BUFFER <= millisecondTimeout) return Long.MAX_VALUE;
    return millisecondTimeout+TIMEOUT_MS_BUFFER;
  }

  private static String getInputStreamLines(InputStream ins) throws Exception {
    String line = null;
    StringBuilder buffer = new StringBuilder();
    BufferedReader in = new BufferedReader(new InputStreamReader(ins));
    while((line = in.readLine()) != null) buffer.append('\n'+line);
    if(buffer.length() == 0) return "";
    return buffer.substring(1);
  }

  public static Result run(long millisecondTimeout, String command, String[] environmentVariableSettings, File workingDirectory) throws Exception {
    long timeout = preprocessMillisecondTimeout(millisecondTimeout);
    Process pro = Runtime.getRuntime().exec(command,environmentVariableSettings,workingDirectory);
    Result res = new Result();
    ClosureBoolean finished = new ClosureBoolean();
    Thread processThread = new Thread(() -> {
      try {
        res.out = getInputStreamLines(pro.getInputStream());
        res.err = getInputStreamLines(pro.getErrorStream());
        pro.waitFor();
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
    res.out = getInputStreamLines(pro.getInputStream());
    res.err = getInputStreamLines(pro.getErrorStream());
    pro.waitFor();
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