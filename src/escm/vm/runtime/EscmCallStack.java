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
  // Internal Frame class
  public static class Frame {
    final Frame parent;
    final String name;
    final SourceInformation source;
    Frame(String name, SourceInformation source, Frame parent) {
      this.name = name;
      this.source = source;
      this.parent = parent;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Registering a call
  public static void push(String calledFunctionName, SourceInformation invocationSource) {
    EscmThread ct = (EscmThread)Thread.currentThread();
    ct.callStack = new Frame(calledFunctionName,invocationSource,ct.callStack);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Clearing the callstack (done by the repl after a top-level evaluation)
  public static void clear() {
    ((EscmThread)Thread.currentThread()).callStack = null;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Get the the callstack (done by loader to preserve pre-load state)
  public static Frame currentStackFrame() {
    return ((EscmThread)Thread.currentThread()).callStack;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Restoring the callstack (done by loader to restore pre-load state)
  public static void restore(Frame restored) {
    ((EscmThread)Thread.currentThread()).callStack = restored;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Printing (& clearing!) the current thread's callstack
  private static boolean shouldSkipFrame(Frame callStack) {
    return callStack != null && callStack.parent == null && callStack.name.equals("runnable");
  }

  // @PRECONDITION: callStack != null
  private static Frame popReadableCallableName(StringBuilder sb, Frame callStack) {
    if(callStack.source == null) {
      sb.append(callStack.name + " ()");
    } else {
      sb.append(callStack.name + " (" + callStack.source.toString() + ")");
    }
    return callStack.parent;
  }

  public static void print() {
    EscmThread ct = (EscmThread)Thread.currentThread();
    if(ct.callStack == null || shouldSkipFrame(ct.callStack)) return;
    StringBuilder sb = new StringBuilder(">> Escm Call Stack: ");
    ct.callStack = popReadableCallableName(sb,ct.callStack);
    while(ct.callStack != null) {
      // ignore first "runnable" on the stack from the main thread.
      if(shouldSkipFrame(ct.callStack)) {
        ct.callStack = ct.callStack.parent;
      } else {
        sb.append("\n                    ");
        ct.callStack = popReadableCallableName(sb,ct.callStack);
      }
    }
    System.err.println(sb.toString());
  }

  ////////////////////////////////////////////////////////////////////////////
  // Getting the call-stack as an EScheme associative-list
  public static Datum toDatum() {
    Frame callStack = ((EscmThread)Thread.currentThread()).callStack;
    if(callStack == null) return Nil.VALUE;
    // ignore first "runnable" on the stack from the main thread.
    ArrayDeque<Frame> frames = new ArrayDeque<Frame>();
    while(callStack.parent != null) {
      frames.push(callStack);
      callStack = callStack.parent;
    }
    if(!shouldSkipFrame(callStack)) frames.push(callStack);
    Datum alist = Nil.VALUE;
    while(frames.size() > 0) {
      Frame frame = frames.pop();
      escm.type.String functionName = new escm.type.String(frame.name);
      if(frame.source != null) {
        Datum src = Pair.List(new escm.type.String(frame.source.fileName()),
                              new Exact(frame.source.lineNumber()),
                              new Exact(frame.source.columnNumber()));
        alist = new Pair(Pair.List(functionName,src),alist);
      } else {
        alist = new Pair(Pair.List(functionName,Boolean.FALSE),alist);
      }
    }
    return alist;
  }
};