// Author: Jordan Randleman - escm.type.CompoundProcedure
// Purpose:
//    EScheme compound procedure specialization of "escm.type.Procedure".
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

package escm.type;
import java.util.ArrayList;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.Interpreter;
import escm.vm.type.Instruction;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;
import escm.vm.runtime.CallStack;

public class CompoundProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal compound procedure fields
  protected static class CompileTime {
    public ArrayList<ArrayList<java.lang.String>> parametersList;
    public ArrayList<java.lang.String> variadicParameterList; // <null> indicates non-variadic
    public ArrayList<ArrayList<Instruction>> bodyList;
    public CompileTime(ArrayList<ArrayList<java.lang.String>> parametersList, ArrayList<java.lang.String> variadicParameterList, ArrayList<ArrayList<Instruction>> bodyList) {
      this.parametersList = parametersList;
      this.variadicParameterList = variadicParameterList;
      this.bodyList = bodyList;
    }
  };

  protected static class State {
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
  public CompoundProcedure(ArrayList<ArrayList<java.lang.String>> parametersList, ArrayList<java.lang.String> variadicParameterList, ArrayList<ArrayList<Instruction>> bodyList) {
    this.state = new State(new CompileTime(parametersList,variadicParameterList,bodyList));
  }
  

  ////////////////////////////////////////////////////////////////////////////
  // Thunk Querying
  public boolean isThunk() { // unary variadics are considered thunks!
    for(ArrayList<java.lang.String> params : this.state.compileTime.parametersList) {
      if(params.isEmpty()) return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  // => Cloning + super environment binding
  protected CompoundProcedure(java.lang.String name, State state) {
    this.name = name;
    this.state = state;
  }


  public CompoundProcedure loadWithState(ExecutionState exeState) {
    return new CompoundProcedure(name,new State(exeState.env,state.compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.type.Environment.java)
  public CompoundProcedure loadWithName(java.lang.String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new CompoundProcedure(name,state);
  }


  // (used by escm.type.MetaObject)
  public CompoundProcedure loadWithForcedName(java.lang.String name) {
    return new CompoundProcedure(name,state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return (o instanceof CompoundProcedure) && ((CompoundProcedure)o).state == state;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  protected java.lang.String stringifyParameters(ArrayList<java.lang.String> params, java.lang.String variadic) {
    int n = params.size();
    if(n == 0 && variadic == null) return "expected 0 args";
    StringBuilder sb = new StringBuilder("(");
    for(int i = 0; i < n; ++i) {
      sb.append(params.get(i));
      if(i+1 < n) sb.append(" ");
    }
    if(variadic != null) {
      sb.append(" . " + variadic);
    }
    sb.append(")");
    return sb.toString();
  }


  protected java.lang.String stringifyParameterSignatures() {
    StringBuilder sb = new StringBuilder("\n  Available Signatures:");
    for(int i = 0, n = state.compileTime.parametersList.size(); i < n; ++i) {
      sb.append("\n    > " + stringifyParameters(state.compileTime.parametersList.get(i),state.compileTime.variadicParameterList.get(i)));
    }
    return sb.toString();
  }


  // @RETURN: the clause (param/body) index these args can be processed with
  protected int validateEnvironmentExtension(ArrayList<Datum> arguments) throws Exception {
    if(state.definitionEnvironment == null)
      throw new Exceptionf("escm.type.CompoundProcedure can't apply procedure %s with a null definition environment!", name);
    int totalArguments = arguments.size();
    for(int i = 0, n = state.compileTime.parametersList.size(); i < n; ++i) {
      int totalParameters = state.compileTime.parametersList.get(i).size();
      if(totalArguments == totalParameters) return i;
      if(totalArguments > totalParameters && state.compileTime.variadicParameterList.get(i) != null) return i;
    }
    throw new Exceptionf("escm.type.CompoundProcedure args %s don't match any signatures in procedure \"%s\":%s", Exceptionf.profileArgs(arguments), name, stringifyParameterSignatures());
  }


  protected Environment getExtendedEnvironment(int clauseNumber, ArrayList<Datum> arguments) throws Exception {
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    ArrayList<java.lang.String> parameters = state.compileTime.parametersList.get(clauseNumber);
    java.lang.String variadicParameter = state.compileTime.variadicParameterList.get(clauseNumber);
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
      CallStack.push(name);
      Trampoline.Continuation popContinuation = (value) -> () -> {
        CallStack.pop(name);
        return continuation.run(value);
      };
      return Interpreter.run(new ExecutionState(extendedEnvironment,state.compileTime.bodyList.get(clauseNumber)),popContinuation);
    };
  }


  ////////////////////////////////////////////////////////////////////////////
  // <super> Binding (done updon object creation)
  public CompoundProcedure loadWithSuper(MetaObject supr) throws Exception {
    if(state.definitionEnvironment == null)
      throw new Exceptionf("escm.type.CompoundProcedure can't bind <super> %s to procedure %s with a null definition environment!", supr.profile(), name);
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    if(supr == null) { // note that <super> is <null> for interfaces!
      extendedEnvironment.define("super",Boolean.FALSE);
    } else {
      extendedEnvironment.define("super",supr);
    }
    return new CompoundProcedure(name,new State(extendedEnvironment,state.compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // <self> Binding (done updon method invocation)
  public CompoundProcedure loadWithSelf(MetaObject self) throws Exception {
    if(state.definitionEnvironment == null)
      throw new Exceptionf("escm.type.CompoundProcedure can't bind <self> %s to procedure %s with a null definition environment!", self.profile(), name);
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    extendedEnvironment.define("self",self);
    return new CompoundProcedure(name,new State(extendedEnvironment,state.compileTime));
  }
}