// Author: Jordan Randleman - escm.vm.runtime.EscmCallStack
// Purpose:
//    Class with static methods to support the current thread's callstack 
//    operations (effectively making for a thread-local "EscmCallStack" 
//    variable!).

package escm.vm.runtime;
import java.util.Stack;
import escm.util.Pair;
import escm.vm.util.SourceInformation;

public class EscmCallStack {
  ////////////////////////////////////////////////////////////////////////////
  // Getting an empty callstack (used by <load>)
  public static Stack<Pair<String,SourceInformation>> newCallStack() {
    return new Stack<Pair<String,SourceInformation>>();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Registering a call
  public static void push(String calledFunctionName, SourceInformation invocationSource) {
    ((EscmThread)Thread.currentThread()).callStack.push(new Pair<String,SourceInformation>(calledFunctionName,invocationSource));
  }

  ////////////////////////////////////////////////////////////////////////////
  // Deregistering a specific call (prevents double-pops when using call/cc)
  //   => NOTE: <nameToPop> uses SHALLOW EQUALITY (not string contents) !!!
  public static void pop(String nameToPop) {
    Stack<Pair<String,SourceInformation>> callStack = ((EscmThread)Thread.currentThread()).callStack;
    if(!callStack.empty() && callStack.peek().first == nameToPop) callStack.pop();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Clearing the callstack (done by the repl after a top-level evaluation)
  public static void clear() {
    ((EscmThread)Thread.currentThread()).callStack.clear();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Copying the callstack (done by loader to preserve pre-load state)
  public static Stack<Pair<String,SourceInformation>> copy() {
    Stack<Pair<String,SourceInformation>> cloned = new Stack<Pair<String,SourceInformation>>();
    cloned.addAll(((EscmThread)Thread.currentThread()).callStack);
    return cloned;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Restoring the callstack (done by loader to restore pre-load state)
  public static void restore(Stack<Pair<String,SourceInformation>> restored) {
    Stack<Pair<String,SourceInformation>> callStack = ((EscmThread)Thread.currentThread()).callStack;
    callStack.clear();
    callStack.addAll(restored);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Printing (& clearing!) the current thread's callstack
  // @PRECONDITION: callStack.size() > 0
  private static String popReadableCallableName(Stack<Pair<String,SourceInformation>> callStack) {
    Pair<String,SourceInformation> nameSourcePair = callStack.pop();
    String name = null;
    if(nameSourcePair.first.length() == 0) { 
      name = "#<ANONYMOUS-CALLABLE>";
    } else {
      name = nameSourcePair.first;
    }
    if(nameSourcePair.second == null) return name;
    return name + " (" + nameSourcePair.second.toString() + ")";
  }


  public static void print() {
    Stack<Pair<String,SourceInformation>> callStack = ((EscmThread)Thread.currentThread()).callStack;
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