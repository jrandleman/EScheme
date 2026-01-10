// Author: Jordan Randleman - escm.util.error.TopLevelError
// Purpose:
//    Logic to report a thread's top-level error. Includes printing out both
//      EScheme's & Java's callstacks.

package escm.util.error;

import escm.type.port.InputPort;
import escm.type.port.OutputPort;
import escm.vm.runtime.EscmCallStack;

public class TopLevelError {
  private static void resetCurrentPorts() {
    InputPort.setCurrent(InputPort.STDIN);
    OutputPort.setCurrent(OutputPort.STDOUT);
  }

  private static int getLastUniqueStackFrameIndex(StackTraceElement[] stackTrace) {
    int i = stackTrace.length - 1;
    while (i > 0 && stackTrace[i].equals(stackTrace[i - 1]))
      --i;
    return i;
  }

  private static void printJavaStackTraceWithoutMessage(Throwable e) {
    StackTraceElement[] stackTrace = e.getStackTrace();
    if (stackTrace.length == 0)
      return;
    int n = getLastUniqueStackFrameIndex(stackTrace);
    System.err.printf(">> Java Call Stack: %s", stackTrace[0]);
    for (int i = 1; i <= n; ++i) {
      System.err.printf("\n                    %s", stackTrace[i]);
    }
    if (n < stackTrace.length - 1) {
      System.err.print("\n                    ^");
      System.err.printf("\n                    %d more calls to \"%s\"", stackTrace.length - 1 - n, stackTrace[n]);
    }
  }

  public static void report(Throwable t) {
    resetCurrentPorts();
    if (t instanceof Exception) {
      System.err.printf("\nESCHEME ERROR: %s\n", t.getMessage());
    } else if (t instanceof StackOverflowError) {
      System.err.println("\nESCHEME: JAVA STACK-OVERFLOW ERROR: Did you try printing/equating cyclical data?");
    } else {
      System.err.printf("\nESCHEME: JAVA ERROR: %s\n", t.getMessage());
    }
    EscmCallStack.print();
    System.err.printf(">> Java Throwable: %s\n", t.getClass().getName());
    printJavaStackTraceWithoutMessage(t);
    System.err.println("");
  }
}