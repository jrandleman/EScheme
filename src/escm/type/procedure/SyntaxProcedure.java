// Author: Jordan Randleman - escm.type.procedure.SyntaxProcedure
// Purpose:
//    EScheme macro procedure specialization of "escm.type.procedure.Procedure".
//    Wraps a "escm.vm.type.Callable" object under the hood.

package escm.type.procedure;
import java.util.ArrayList;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.vm.type.Callable;
import escm.vm.type.PrimitiveSyntax;
import escm.vm.util.ExecutionState;
import escm.vm.util.SourceInformation;

public class SyntaxProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal primitive procedure field
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
    if(macro instanceof Procedure) {
      return new SyntaxProcedure(name,invocationSource,(Callable)((Procedure)macro).loadWithInvocationSource(invocationSource));
    } else {
      return new SyntaxProcedure(name,invocationSource,macro);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof SyntaxProcedure && ((SyntaxProcedure)o).macro == macro;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "syntax";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(name.equals(DEFAULT_NAME)) return "#<syntax>";
    return "#<syntax " + name + '>';
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> macro.callWith(arguments,continuation);
  }
}