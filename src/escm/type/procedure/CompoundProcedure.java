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
import java.util.Objects;
import java.util.ArrayList;
import java.io.Serializable;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.Nil;
import escm.vm.Interpreter;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;
import escm.vm.runtime.EscmCallStack;
import escm.vm.type.callable.DocString;

public class CompoundProcedure extends Procedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal compound procedure fields
  protected static class CompileTime implements Serializable {
    public String docstring;
    // For <parameterTypeNames> & <parameterTypesList>:
    //   * "parameterTypeNames == null" => no bodies have types
    //   * "parameterTypeNames[i] != null" => ith body has no types
    //   * "parameterTypeNames[i][j] == null" => ith body's jth parameter is the ":any" type
    private ArrayList<ArrayList<String>> parameterTypeNames;
    private ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesList;
    public ArrayList<ArrayList<Symbol>> parametersList;
    public ArrayList<Symbol> variadicParameterList; // <null> indicates non-variadic
    public ArrayList<ArrayList<Instruction>> bodyList;

    public ArrayList<String> getParameterTypeName(int index) {
      if(parameterTypeNames == null) return null;
      return parameterTypeNames.get(index);
    }

    public ArrayList<TypeChecker.Predicate> getParameterTypes(int index) {
      if(parameterTypesList == null) return null;
      return parameterTypesList.get(index);
    }

    public CompileTime(String docstring, ArrayList<ArrayList<String>> parameterTypeNames, ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesList, ArrayList<ArrayList<Symbol>> parametersList, ArrayList<Symbol> variadicParameterList, ArrayList<ArrayList<Instruction>> bodyList) {
      this.parameterTypeNames = parameterTypeNames;
      this.docstring = docstring;
      this.parameterTypesList = parameterTypesList;
      this.parametersList = parametersList;
      this.variadicParameterList = variadicParameterList;
      this.bodyList = bodyList;
    }
  };

  protected Environment definitionEnvironment = null;
  protected CompileTime compileTime = null;
  

  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public CompoundProcedure(
    String docstring, 
    ArrayList<ArrayList<String>> parameterTypeNames,
    ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesList, 
    ArrayList<ArrayList<Symbol>> parametersList, 
    ArrayList<Symbol> variadicParameterList, 
    ArrayList<ArrayList<Instruction>> bodyList
  ) {
    this.compileTime = new CompileTime(
      DocString.format(docstring),
      parameterTypeNames,
      parameterTypesList,
      parametersList,
      variadicParameterList,
      bodyList
    );
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public CompoundProcedure quote(ExecutionState state) {
    return loadWithState(state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  // => Cloning + super environment binding
  protected CompoundProcedure(String name, SourceInformation invocationSource, Environment definitionEnvironment, CompileTime compileTime) {
    this.name = name;
    this.invocationSource = invocationSource;
    this.definitionEnvironment = definitionEnvironment;
    this.compileTime = compileTime;
  }


  public CompoundProcedure loadWithState(ExecutionState exeState) {
    if(definitionEnvironment != null) return this;
    return new CompoundProcedure(name,invocationSource,exeState.env,compileTime);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Name binding (used by escm.vm.util.Environment)
  public CompoundProcedure loadWithName(String name) {
    if(!this.name.equals(Procedure.DEFAULT_NAME)) return this;
    return new CompoundProcedure(name,invocationSource,definitionEnvironment,compileTime);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Invocation source binding (used by escm.type.Symbol)
  public CompoundProcedure loadWithInvocationSource(SourceInformation invocationSource) {
    return new CompoundProcedure(name,invocationSource,definitionEnvironment,compileTime);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(!(o instanceof CompoundProcedure)) return false;
    CompoundProcedure c = (CompoundProcedure)o;
    return c.definitionEnvironment == definitionEnvironment && c.compileTime == compileTime;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash("compound-"+type(),System.identityHashCode(definitionEnvironment),System.identityHashCode(compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    return compileTime.docstring;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Signature
  private static final Symbol DOT = new Symbol(".");

  // returns parameter clause with this procedure's name at the front
  private Datum clauseSignature(ArrayList<String> parameterTypeNames, ArrayList<Symbol> parameters, Symbol variadic) {
    Datum sig = variadic == null ? Nil.VALUE : Pair.List(DOT,variadic);
    for(int i = parameters.size()-1; i >= 0; --i) {
      sig = new Pair(parameters.get(i),sig);
      if(parameterTypeNames != null) {
        String name = parameterTypeNames.get(i);
        if(name != null) {
          sig = new Pair(new Keyword(name.substring(1)),sig);
        }
      }
    }
    return new Pair(new Symbol(readableName()),sig);
  }

  public Datum signature() {
    int n = compileTime.parametersList.size();
    if(n == 1) {
      return clauseSignature(compileTime.getParameterTypeName(0),compileTime.parametersList.get(0),compileTime.variadicParameterList.get(0));
    }
    Datum sigs = Nil.VALUE;
    for(int i = n-1; i >= 0; --i) {
      sigs = new Pair(clauseSignature(compileTime.getParameterTypeName(i),compileTime.parametersList.get(i),compileTime.variadicParameterList.get(i)),sigs);
    }
    return sigs;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  protected String stringifyParameters(ArrayList<String> parameterTypeNames, ArrayList<Symbol> params, Symbol variadic) {
    int n = params.size();
    if(n == 0 && variadic == null) return "expected 0 args";
    StringBuilder sb = new StringBuilder("(");
    for(int i = 0; i < n; ++i) {
      if(parameterTypeNames != null) {
        String name = parameterTypeNames.get(i);
        if(name != null) {
          sb.append(name);
          sb.append(" ");
        }
      }
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
    for(int i = 0, n = compileTime.parametersList.size(); i < n; ++i) {
      sb.append(String.format("\n   %2d) ", i+1) + stringifyParameters(compileTime.getParameterTypeName(i),compileTime.parametersList.get(i),compileTime.variadicParameterList.get(i)));
    }
    return sb.toString();
  }


  protected boolean validParameterArity(int i, int totalArguments) {
    int totalParameters = compileTime.parametersList.get(i).size();
    return totalArguments == totalParameters || 
           (totalArguments > totalParameters && compileTime.variadicParameterList.get(i) != null);
  }


  protected Environment getExtendedEnvironment(int clauseNumber, ArrayList<Datum> arguments) throws Exception {
    Environment extendedEnvironment = new Environment(definitionEnvironment);
    ArrayList<TypeChecker.Predicate> types = compileTime.getParameterTypes(clauseNumber);
    ArrayList<Symbol> parameters = compileTime.parametersList.get(clauseNumber);
    Symbol variadicParameter = compileTime.variadicParameterList.get(clauseNumber);
    int n = parameters.size();
    // register non-variadic arguments
    if(types == null) {
      for(int i = 0; i < n; ++i) {
        extendedEnvironment.define(parameters.get(i),arguments.get(i));
      }
    } else {
      for(int i = 0; i < n; ++i) {
        TypeChecker.Predicate type = types.get(i);
        Datum arg = arguments.get(i);
        if(type != null && !type.check(definitionEnvironment,arg))
          return null; // indicates type mismatch
        extendedEnvironment.define(parameters.get(i),arg);
      }
    }
    // register variadic arguments
    if(variadicParameter != null) {
      Datum variadicArgumentList = Nil.VALUE;
      for(int i = arguments.size()-1; i >= n; --i)
        variadicArgumentList = new Pair(arguments.get(i),variadicArgumentList);
      extendedEnvironment.define(variadicParameter,variadicArgumentList);
    }
    return extendedEnvironment;
  }


  protected Trampoline.Bounce callTheFunction(int i, Environment extendedEnvironment, Trampoline.Continuation continuation) throws Exception {
    EscmCallStack.Frame originalCallStack = EscmCallStack.currentStackFrame();
    EscmCallStack.push(readableName(),invocationSource);
    Trampoline.Continuation popContinuation = (value) -> () -> {
      EscmCallStack.restore(originalCallStack);
      return continuation.run(value);
    };
    return Interpreter.run(new ExecutionState(extendedEnvironment,compileTime.bodyList.get(i)),popContinuation);
  }


  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    return () -> {
      if(definitionEnvironment == null)
        throw new Exceptionf("Can't apply procedure %s with a null definition environment!", readableName());
      int totalArguments = arguments.size();
      for(int i = 0, n = compileTime.parametersList.size(); i < n; ++i) {
        if(!validParameterArity(i,totalArguments)) continue; // arity mismatch
        Environment extendedEnvironment = getExtendedEnvironment(i,arguments);
        if(extendedEnvironment == null) continue; // type mismatch
        return callTheFunction(i,extendedEnvironment,continuation);
      }
      throw new Exceptionf("Args (%s) don't match any signatures in procedure \"%s\":%s", Exceptionf.profileArgs(arguments), readableName(), stringifyParameterSignatures());
    };
  }
}