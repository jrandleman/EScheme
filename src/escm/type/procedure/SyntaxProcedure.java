// Author: Jordan Randleman - escm.type.procedure.SyntaxProcedure
// Purpose:
//    EScheme macro procedure specialization of "escm.type.procedure.Procedure".
//    Wraps a "escm.vm.type.callable.Callable" object under the hood.

package escm.type.procedure;
import java.util.Objects;
import java.util.ArrayList;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.PrimitiveSyntax;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.EscmCallStack;

public class SyntaxProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal expansion procedure field
  private Callable macro;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public SyntaxProcedure(java.lang.String name, Callable macro) {
    this.name = name;
    if(macro instanceof Datum) {
      try {
        this.macro = (Callable)((Datum)macro).loadWithName(this.name);
      } catch (Exception e) { // catching the unlikely but possible (Callable) cast exception
        this.macro = macro;
      }
    } else {
      this.macro = macro;
    }
  }


  public SyntaxProcedure(java.lang.String name, PrimitiveSyntax prm) {
    this.name = name;
    this.macro = new Callable() {
      public Datum signature() {
        return prm.signature();
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
        return continuation.run(prm.callWith(parameters));
      }
    };
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public SyntaxProcedure loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  private SyntaxProcedure(java.lang.String name, SourceInformation invocationSource, Callable macro) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.macro = macro;
  }


  public SyntaxProcedure loadWithName(java.lang.String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    if(macro instanceof Datum) {
      try {
        return new SyntaxProcedure(name,invocationSource,(Callable)((Datum)macro).loadWithName(name));
      } catch(Exception e) { // catching the unlikely but possible (Callable) cast exception
        return new SyntaxProcedure(name,invocationSource,macro);
      }
    } else {
      return new SyntaxProcedure(name,invocationSource,macro);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public SyntaxProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new SyntaxProcedure(name,invocationSource,macro);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof SyntaxProcedure && ((SyntaxProcedure)o).macro == macro;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash("syntax-"+type(),macro);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "syntax";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<syntax " + readableName() + '>';
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  private boolean signatureShouldBindName(Datum name) {
    return name instanceof Symbol && ((Symbol)name).value().equals(Procedure.DEFAULT_NAME);
  }

  public Datum signature() {
    Datum sig = macro.signature();
    if(!(sig instanceof Pair)) return sig;
    Pair psig = (Pair)sig;
    Datum head = psig.car();
    if(signatureShouldBindName(head)) {
      return new Pair(new Symbol(readableName()),psig.cdr());
    } else if(head instanceof Pair) {
      Datum sigs = Nil.VALUE;
      while(sig instanceof Pair) {
        Pair sigp = (Pair)sig;
        Datum signature = sigp.car();
        if(signature instanceof Pair) {
          Pair psignature = (Pair)signature;
          if(signatureShouldBindName(psignature.car())) {
            sigs = new Pair(new Pair(new Symbol(readableName()),psignature.cdr()),sigs);
          } else {
            sigs = new Pair(signature,sigs);
          }
        } else {
          sigs = new Pair(signature,sigs);
        }
        sig = sigp.cdr();
      }
      if(sigs instanceof Pair) {
        return (Datum)((Pair)sigs).reverse();
      } else { // if(sigs instanceof Nil)
        return Nil.VALUE;
      }
    } else {
      return sig;
    }
  }

  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> {
      EscmCallStack.Frame originalCallStack = EscmCallStack.currentStackFrame();
      EscmCallStack.push(readableName(),invocationSource);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        EscmCallStack.restore(originalCallStack);
        return continuation.run(value);
      };
      return macro.callWith(arguments,popContinuation);
    };
  }
}