// Author: Jordan Randleman - escm.type.procedure.CompoundProcedure
// Purpose:
//    EScheme compound procedure specialization of "escm.type.procedure.Procedure".
//    Compound procedures have 3 main components:
//      0) Definition Environment
//         => This is critical - since all EScheme compound procedures are closures,
//            we need a pointer to the enclosing environment in order to dynamically
//            find captured variables.
//      1) Parameter Lists
//         => Critical for obvious reasons, to define such as variables upon each
//            invocation of the procedure. Note that as an optimization technique we
//            parse such preemptively to also keep track of whether the parameters 
//            are variadic or not.
//      2) Bodies
//         => Sets of instructions from a compiled escm expression to be interpreted 
//            upon the application of the procedure, in a new environment extended with 
//            the parameters defined as variables with the values of the application 
//            arguments, whose parent environment is the procedure's definition environment.
//
//    Note that procedures have their name bound not upon construction (all escm
//    procedures are anonymous upon creation), but rather by the environment's
//    binding methods during "define" and "set!" invocations.
//
//    Further note that the definition environment is bound at runtime by the interpreter.
//
//    Lastly, observe that ALL escm procedures are multi-arity, supporting several parameter
//    signatures and associated function bodies.

package escm.type.procedure;
import java.util.ArrayList;
import java.io.Serializable;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.Nil;
import escm.vm.Interpreter;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.EscmCallStack;

public class CompoundProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal compound procedure fields
  protected static class CompileTime implements Serializable {
    public ArrayList<ArrayList<Symbol>> parametersList;
    public ArrayList<Symbol> variadicParameterList; // <null> indicates non-variadic
    public ArrayList<ArrayList<Instruction>> bodyList;
    public CompileTime(ArrayList<ArrayList<Symbol>> parametersList, ArrayList<Symbol> variadicParameterList, ArrayList<ArrayList<Instruction>> bodyList) {
      this.parametersList = parametersList;
      this.variadicParameterList = variadicParameterList;
      this.bodyList = bodyList;
    }
  };

  protected static class State implements Serializable {
    public Environment definitionEnvironment;
    public CompileTime compileTime;
    public State(CompileTime compileTime) {
      this.definitionEnvironment = null;
      this.compileTime = compileTime;
    }
    public State(Environment definitionEnvironment, CompileTime compileTime) {
      this.definitionEnvironment = definitionEnvironment;
      this.compileTime = compileTime;
    }
  };

  protected State state;
  

  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public CompoundProcedure(ArrayList<ArrayList<Symbol>> parametersList, ArrayList<Symbol> variadicParameterList, ArrayList<ArrayList<Instruction>> bodyList) {
    this.state = new State(new CompileTime(parametersList,variadicParameterList,bodyList));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  // => Cloning + super environment binding
  protected CompoundProcedure(String name, SourceInformation invocationSource, State state) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.state = state;
  }


  public CompoundProcedure loadWithState(ExecutionState exeState) {
    return new CompoundProcedure(name,invocationSource,new State(exeState.env,state.compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  public CompoundProcedure loadWithName(String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new CompoundProcedure(name,invocationSource,state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public CompoundProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new CompoundProcedure(name,invocationSource,state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return (o instanceof CompoundProcedure) && ((CompoundProcedure)o).state == state;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  protected String stringifyParameters(ArrayList<Symbol> params, Symbol variadic) {
    int n = params.size();
    if(n == 0 && variadic == null) return "expected 0 args";
    StringBuilder sb = new StringBuilder("(");
    for(int i = 0; i < n; ++i) {
      sb.append(params.get(i).value());
      if(i+1 < n) sb.append(" ");
    }
    if(variadic != null) {
      sb.append(" . " + variadic.value());
    }
    sb.append(")");
    return sb.toString();
  }


  protected String stringifyParameterSignatures() {
    StringBuilder sb = new StringBuilder("\n>> Available Signatures:");
    for(int i = 0, n = state.compileTime.parametersList.size(); i < n; ++i) {
      sb.append(String.format("\n   %2d) ", i+1) + stringifyParameters(state.compileTime.parametersList.get(i),state.compileTime.variadicParameterList.get(i)));
    }
    return sb.toString();
  }


  // @RETURN: the clause (param/body) index these args can be processed with
  protected int validateEnvironmentExtension(ArrayList<Datum> arguments) throws Exception {
    if(state.definitionEnvironment == null)
      throw new Exceptionf("Can't apply procedure %s with a null definition environment!", name);
    int totalArguments = arguments.size();
    for(int i = 0, n = state.compileTime.parametersList.size(); i < n; ++i) {
      int totalParameters = state.compileTime.parametersList.get(i).size();
      if(totalArguments == totalParameters) return i;
      if(totalArguments > totalParameters && state.compileTime.variadicParameterList.get(i) != null) return i;
    }
    throw new Exceptionf("Args (%s) don't match any signatures in procedure \"%s\":%s", Exceptionf.profileArgs(arguments), name, stringifyParameterSignatures());
  }


  protected Environment getExtendedEnvironment(int clauseNumber, ArrayList<Datum> arguments) throws Exception {
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    ArrayList<Symbol> parameters = state.compileTime.parametersList.get(clauseNumber);
    Symbol variadicParameter = state.compileTime.variadicParameterList.get(clauseNumber);
    int n = parameters.size();
    // register non-variadic arguments
    for(int i = 0; i < n; ++i)
      extendedEnvironment.define(parameters.get(i),arguments.get(i));
    // register variadic arguments
    if(variadicParameter != null) {
      Datum variadicArgumentList = Nil.VALUE;
      for(int i = arguments.size()-1; i >= n; --i)
        variadicArgumentList = new Pair(arguments.get(i),variadicArgumentList);
      extendedEnvironment.define(variadicParameter,variadicArgumentList);
    }
    return extendedEnvironment;
  }


  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> {
      int clauseNumber = validateEnvironmentExtension(arguments);
      Environment extendedEnvironment = getExtendedEnvironment(clauseNumber,arguments);
      EscmCallStack.push(name,invocationSource);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        EscmCallStack.pop(name);
        return continuation.run(value);
      };
      return Interpreter.run(new ExecutionState(extendedEnvironment,state.compileTime.bodyList.get(clauseNumber)),popContinuation);
    };
  }


  ////////////////////////////////////////////////////////////////////////////
  // Backdoors

}