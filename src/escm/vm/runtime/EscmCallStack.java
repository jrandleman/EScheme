// Author: Jordan Randleman - escm.vm.runtime.EscmCallStack
// Purpose:
//    Class with static methods to support the current thread's callstack 
//    operations (effectively making for a thread-local "EscmCallStack" 
//    variable!).

package escm.vm.runtime;
import java.util.ArrayDeque;
import escm.type.Pair;
import escm.type.Datum;
import escm.type.Nil;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.vm.util.SourceInformation;

public class EscmCallStack {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Entry class
  public static class Entry {
    final Entry parent;
    final String name;
    final SourceInformation source;
    public Entry(String name, SourceInformation source, Entry parent) {
      this.name = name;
      this.source = source;
      this.parent = parent;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Registering a call
  public static void push(String calledFunctionName, SourceInformation invocationSource) {
    EscmThread ct = (EscmThread)Thread.currentThread();
    ct.callStack = new Entry(calledFunctionName,invocationSource,ct.callStack);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Clearing the callstack (done by the repl after a top-level evaluation)
  public static void clear() {
    ((EscmThread)Thread.currentThread()).callStack = null;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Get the the callstack (done by loader to preserve pre-load state)
  public static Entry currentCallStack() {
    return ((EscmThread)Thread.currentThread()).callStack;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Restoring the callstack (done by loader to restore pre-load state)
  public static void restore(Entry restored) {
    ((EscmThread)Thread.currentThread()).callStack = restored;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Printing (& clearing!) the current thread's callstack
  private static boolean shouldSkipEntry(Entry callStack) {
    return callStack != null && callStack.parent == null && callStack.name.equals("runnable");
  }

  // @PRECONDITION: callStack != null
  private static Entry popReadableCallableName(StringBuilder sb, Entry callStack) {
    // ignore first "runnable" on the stack from the main thread.
    if(shouldSkipEntry(callStack)) return callStack.parent; 
    if(callStack.source == null) {
      sb.append(callStack.name);
    } else {
      sb.append(callStack.name + " (" + callStack.source.toString() + ")");
    }
    return callStack.parent;
  }

  public static void print() {
    EscmThread ct = (EscmThread)Thread.currentThread();
    if(ct.callStack == null) return;
    StringBuilder sb = new StringBuilder(">> Escm Call Stack: ");
    ct.callStack = popReadableCallableName(sb,ct.callStack);
    while(ct.callStack != null) {
      sb.append("\n                    ");
      ct.callStack = popReadableCallableName(sb,ct.callStack);
    }
    System.err.println(sb.toString());
  }

  ////////////////////////////////////////////////////////////////////////////
  // Getting the call-stack as an EScheme associative-list
  public static Datum toDatum() {
    Entry callStack = ((EscmThread)Thread.currentThread()).callStack;
    if(callStack == null) return Nil.VALUE;
    // ignore first "runnable" on the stack from the main thread.
    ArrayDeque<Entry> values = new ArrayDeque<Entry>();
    while(callStack.parent != null) {
      values.push(callStack);
      callStack = callStack.parent;
    }
    if(!shouldSkipEntry(callStack)) values.push(callStack);
    Datum alist = Nil.VALUE;
    while(values.size() > 0) {
      Entry value = values.pop();
      escm.type.String functionName = new escm.type.String(value.name);
      if(value.source != null) {
        Datum src = Pair.List(new escm.type.String(value.source.fileName()),
                              new Exact(value.source.lineNumber()),
                              new Exact(value.source.columnNumber()));
        alist = new Pair(Pair.List(functionName,src),alist);
      } else {
        alist = new Pair(Pair.List(functionName,Boolean.FALSE),alist);
      }
    }
    return alist;
  }
};