// Author: Jordan Randleman - escm.type.procedure.PrimitiveProcedure
// Purpose:
//    Java primitive procedure specialization of "escm.type.procedure.Procedure".
//    Wraps a "escm.vm.type.callable.Callable" object under the hood.

package escm.type.procedure;
import java.util.Objects;
import java.util.ArrayList;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.EscmCallStack;

public class PrimitiveProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal primitive procedure field
  private Callable prm;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public PrimitiveProcedure(java.lang.String name, Callable prm) {
    this.name = name;
    this.prm = prm;
  }

  public PrimitiveProcedure(java.lang.String name, Primitive prm) {
    this.name = name;
    this.prm = new Callable() {
      public String docstring() {
        return prm.docstring();
      }
      public Datum signature() {
        return prm.signature();
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
        return continuation.run(prm.callWith(parameters));
      }
    };
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public PrimitiveProcedure quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public PrimitiveProcedure loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  private PrimitiveProcedure(java.lang.String name, SourceInformation invocationSource, Callable prm) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.prm = prm;
  }


  public PrimitiveProcedure loadWithName(java.lang.String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new PrimitiveProcedure(name,invocationSource,prm);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public PrimitiveProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new PrimitiveProcedure(name,invocationSource,prm);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof PrimitiveProcedure && ((PrimitiveProcedure)o).prm == prm;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash("primitive-"+type(),prm);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public String docstring() {
    return prm.docstring();
  }

  private static boolean signatureShouldBindName(Datum name) {
    return name instanceof Symbol && ((Symbol)name).value().equals(Procedure.DEFAULT_NAME);
  }

  static Datum tagCallableSignature(Datum sig, String readableName) {
    if(!(sig instanceof Pair)) return sig;
    Pair psig = (Pair)sig;
    Datum head = psig.car();
    if(head instanceof Keyword) {
      return new Pair(head,tagCallableSignature(psig.cdr(),readableName));
    } else if(signatureShouldBindName(head)) {
      return new Pair(new Symbol(readableName),psig.cdr());
    } else if(head instanceof Pair) {
      Datum sigs = Nil.VALUE;
      while(sig instanceof Pair) {
        Pair sigp = (Pair)sig;
        Datum signature = sigp.car();
        Datum restSignatures = sigp.cdr();
        if(signature instanceof Keyword) {
          sigs = new Pair(signature,sigs);
          signature = restSignatures;
          if(signature instanceof Pair) {
            Pair psigClause = (Pair)signature;
            signature = psigClause.car();
            restSignatures = psigClause.cdr();
          }
        }
        if(signature instanceof Pair) {
          Pair psignature = (Pair)signature;
          if(signatureShouldBindName(psignature.car())) {
            sigs = new Pair(new Pair(new Symbol(readableName),psignature.cdr()),sigs);
          } else {
            sigs = new Pair(signature,sigs);
          }
        }
        sig = restSignatures;
      }
      if(sigs instanceof Pair) {
        return (Datum)((Pair)sigs).reverse();
      } else { // if(sigs instanceof Nil)
        return sigs;
      }
    } else {
      return sig;
    }
  }

  public Datum signature() {
    return tagCallableSignature(prm.signature(),readableName());
  }


  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> {
      EscmCallStack.Frame originalCallStack = EscmCallStack.currentStackFrame();
      EscmCallStack.push(readableName(),invocationSource);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        EscmCallStack.restore(originalCallStack);
        return continuation.run(value);
      };
      return prm.callWith(arguments,popContinuation);
    };
  }
}