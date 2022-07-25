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

public class ExecuteSystemCommand {
  public static class Result {
    public String out = null;
    public String err = null;
    public int exit = 0;
  };

  private static String getInputStreamLines(InputStream ins) throws Exception {
    String line = null;
    StringBuilder buffer = new StringBuilder();
    BufferedReader in = new BufferedReader(new InputStreamReader(ins));
    while((line = in.readLine()) != null) buffer.append('\n'+line);
    if(buffer.length() == 0) return "";
    return buffer.substring(1);
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

  public static Result run(String command, String[] environmentVariableSettings) throws Exception {
    return run(command, environmentVariableSettings, null);
  }

  public static Result run(String command) throws Exception {
    return run(command, null, null);
  }
}