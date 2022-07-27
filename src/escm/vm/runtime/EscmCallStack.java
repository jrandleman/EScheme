// Author: Jordan Randleman - escm.vm.runtime.EscmCallStack
// Purpose:
//    Class with static methods to support the current thread's callstack 
//    operations (effectively making for a thread-local "EscmCallStack" 
//    variable!).

package escm.vm.runtime;
import java.util.Stack;

public class EscmCallStack {
  ////////////////////////////////////////////////////////////////////////////
  // Registering a call
  public static void push(String calledFunctionName) {
    ((EscmThread)Thread.currentThread()).callStack.push(calledFunctionName);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Deregistering a specific call (prevents double-pops when using call/cc)
  //   => NOTE: <nameToPop> uses SHALLOW EQUALITY (not string contents) !!!
  public static String pop(String nameToPop) {
    Stack<String> callStack = ((EscmThread)Thread.currentThread()).callStack;
    if(!callStack.empty() && callStack.peek() == nameToPop) return callStack.pop();
    return null;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Clearing the callstack (done by the repl after a top-level evaluation)
  public static void clear() {
    ((EscmThread)Thread.currentThread()).callStack.clear();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Printing (& clearing!) the current thread's callstack
  // @PRECONDITION: callStack.size() > 0
  private static String popReadableCallableName(Stack<String> callStack) {
    String name = callStack.pop();
    if(name.length()==0) return "#<ANONYMOUS-CALLABLE>";
    return name;
  }


  public static synchronized void print() {
    Stack<String> callStack = ((EscmThread)Thread.currentThread()).callStack;
    int n = callStack.size();
    if(n == 0) return;
    StringBuilder sb = new StringBuilder(">> Escm Call Stack: " + popReadableCallableName(callStack));
    for(int i = 1; i < n; ++i) {
      sb.append("\n                    ");
      sb.append(popReadableCallableName(callStack));
    }
    System.err.println(sb.toString());
  }
};