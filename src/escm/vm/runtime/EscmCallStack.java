// Author: Jordan Randleman - escm.vm.runtime.EscmCallStack
// Purpose:
//    Class with static methods to support the current thread's callstack 
//    operations (effectively making for a thread-local "EscmCallStack" 
//    variable!).

package escm.vm.runtime;
import java.util.ArrayDeque;
import escm.util.Pair;
import escm.vm.util.SourceInformation;

public class EscmCallStack {
  ////////////////////////////////////////////////////////////////////////////
  // Getting an empty callstack (used by <load>)
  public static ArrayDeque<Pair<String,SourceInformation>> newCallStack() {
    return new ArrayDeque<Pair<String,SourceInformation>>();
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
    ArrayDeque<Pair<String,SourceInformation>> callStack = ((EscmThread)Thread.currentThread()).callStack;
    if(callStack.size() > 0 && callStack.peek().first == nameToPop) callStack.pop();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Clearing the callstack (done by the repl after a top-level evaluation)
  public static void clear() {
    ((EscmThread)Thread.currentThread()).callStack.clear();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Copying the callstack (done by loader to preserve pre-load state)
  public static ArrayDeque<Pair<String,SourceInformation>> copy() {
    return (ArrayDeque<Pair<String,SourceInformation>>)((EscmThread)Thread.currentThread()).callStack.clone();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Restoring the callstack (done by loader to restore pre-load state)
  public static void restore(ArrayDeque<Pair<String,SourceInformation>> restored) {
    ((EscmThread)Thread.currentThread()).callStack = (ArrayDeque<Pair<String,SourceInformation>>)restored.clone();
  }

  ////////////////////////////////////////////////////////////////////////////
  // Printing (& clearing!) the current thread's callstack
  // @PRECONDITION: callStack.size() > 0
  private static String popReadableCallableName(ArrayDeque<Pair<String,SourceInformation>> callStack) {
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
    ArrayDeque<Pair<String,SourceInformation>> callStack = ((EscmThread)Thread.currentThread()).callStack;
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