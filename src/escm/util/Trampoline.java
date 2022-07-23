// Author: Jordan Randleman - escm.util.Trampoline
// Purpose:
//    Class to provide the escm VM's trampolining infrastructure.
//    We want escm to support continuations for "call/cc".
//    Two approaches come to mind:
//      1) Compile escm to emmit CPS (continuation passing style) code prior 
//         to bytecode conversion.
//         Pros:
//           A) This simplifies the escm VM written in Java.
//           B) Could have a "no CPS mode" for the compiler to process escm
//              expressions w/o the CPS transform in order to emmit more
//              efficient bytecode at the cost of not having continuations.
//                => Note that this gets tricky: mixing CPS & non-CPS code was
//                   one of the biggest challenges faced when implementing 
//                   Heist-Scheme !
//         Cons:
//           A) This significantly complicates the escm compiler written in Java.
//           B) First-class bytecode injection via the "bytecode" special form 
//              becomes MUCH more complex when trying to account for how the
//              bytecode of surrounding escm code will look after its CPS transform.
//           C) CPS transforming every escm expression will incur both a compile
//              and run time overhead. CPS transforms require both extensive 
//              analysis (increasing compile-time) and the addition of MANY
//              lambdas (increasing run-time).
//      2) Write the VM itself in CPS.
//         Pros:
//           A) This simplifies the escm compiler written in Java significantly.
//           B) Bytecode inlining is trivial given the direct escm->bytecode 
//              transform.
//           C) We get free tail-call optimization due to trampolining!
//         Cons:
//           A) All escm function calls incur the stack copying overhead of 
//              continuations. Note though that the escm VM has each function keep an 
//              individual stack, hence this copy tends to be cheap in practice.
//           B) Java primitives become more complex (by default) to implement since 
//              they also have to be aware of continuation management. Note that
//              this can be mitigated via wrapper objects to automate continuation
//              management for lower-order primitive functions.
//           C) Java doesn't have tail-call optimization, so we have to trampoline 
//              the call to continuations manually by creating extra objects that
//              trampoline our code upwards (the "Trampoline.Bounce" object).
// 
//    We opt for the second approach here. The allure of a simple compiler and
//      free tail call optimization from trampolining is too great!

package escm.util;
import escm.type.Datum;

public class Trampoline {
  //////////////////////////////////////////////////////////////////////////
  // Functional "Bounce" interface: the returned wrapper object containing 
  //   our trampolined code.
  public static interface Bounce {
    public Bounce bounce() throws Exception;
  }


  //////////////////////////////////////////////////////////////////////////
  // Functional "Continuation" interface: bouncing functions should accept
  //   a continuation as their last parameter. Continuation lambdas should
  //   also immediately return a thunk (containing their body) in order to 
  //   bounce properly on the trampoline.
  public static interface Continuation {
    public Bounce run(Datum result) throws Exception;
  }


  //////////////////////////////////////////////////////////////////////////
  // Static value to return in a "Bounce"ing procedure (typically a 
  //   Continuation) to denote that the trampoline should stop bouncing.
  public static final Bounce LAST_BOUNCE_SIGNAL = null;


  //////////////////////////////////////////////////////////////////////////
  // Trampolining logic: bounce <b> until <LAST_BOUNCE_SIGNAL> is detected
  public static void resolve(Bounce b) throws Exception {
    while(b != LAST_BOUNCE_SIGNAL) b = b.bounce();
  }
}