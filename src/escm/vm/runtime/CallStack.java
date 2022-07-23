// Author: Jordan Randleman - escm.vm.runtime.CallStack
// Purpose:
//    Class with static methods to support the current thread's callstack 
//    operations (effectively making for a thread-local "CallStack" variable!).

package escm.vm.runtime;
import java.util.Stack;

public class CallStack {
  ////////////////////////////////////////////////////////////////////////////
  // Registering a call
  public static void push(String calledFunctionName) {
    ((EscmThread)Thread.currentThread()).callStack.push(calledFunctionName);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Deregistering a specific call (prevents double-pops when using call/cc)
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
  public static synchronized void print() {
    Stack<String> callStack = ((EscmThread)Thread.currentThread()).callStack;
    int n = callStack.size();
    if(n == 0) return;
    StringBuilder sb = new StringBuilder(">> Escm Call Stack: " + callStack.pop());
    for(int i = 1; i < n; ++i) {
      sb.append("\n                    ");
      sb.append(callStack.pop());
    }
    System.out.println(sb.toString());
  }
};