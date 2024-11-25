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
//    Lastly, observe that ALL escm procedures use multiple dispatch, supporting multiple
//    parameter signature arities and keyword types, with associated function bodies.

package escm.type.procedure;
import java.util.Objects;
import java.util.ArrayList;
import java.util.HashSet;
import java.io.Serializable;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.procedure.types.TypeChecker;
import escm.type.procedure.types.TypeEquality;
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
    // For <returnTypeNamesLists> & <returnTypeLists>:
    //   * "returnTypeNamesLists == null" => no bodies have return types
    //   * "returnTypeNamesLists[i] == null" => ith body has no return type
    private ArrayList<String> returnTypeNamesLists;
    private ArrayList<TypeChecker.Predicate> returnTypeLists;
    // For <parameterTypeNamesLists> & <parameterTypesLists>:
    //   * "parameterTypeNamesLists == null" => no bodies have types
    //   * "parameterTypeNamesLists[i] == null" => ith body has no types
    //   * "parameterTypeNamesLists[i][j] == null" => ith body's jth parameter has no type
    private ArrayList<ArrayList<String>> parameterTypeNamesLists;
    private ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesLists;
    public ArrayList<ArrayList<Symbol>> parametersList;
    public ArrayList<Symbol> variadicParameterList; // <null> indicates non-variadic
    public ArrayList<ArrayList<Instruction>> bodyList;

    public ArrayList<String> getParameterTypeNames(int index) {
      if(parameterTypeNamesLists == null) return null;
      return parameterTypeNamesLists.get(index);
    }

    public ArrayList<TypeChecker.Predicate> getParameterTypes(int index) {
      if(parameterTypesLists == null) return null;
      return parameterTypesLists.get(index);
    }

    public String getReturnTypeName(int index) {
      if(returnTypeNamesLists == null) return null;
      return returnTypeNamesLists.get(index);
    }

    public TypeChecker.Predicate getReturnType(int index) {
      if(returnTypeLists == null) return null;
      return returnTypeLists.get(index);
    }

    public CompileTime(
      String docstring, 
      ArrayList<String> returnTypeNames,
      ArrayList<TypeChecker.Predicate> returnTypeLists, 
      ArrayList<ArrayList<String>> parameterTypeNames, 
      ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesList, 
      ArrayList<ArrayList<Symbol>> parametersList, 
      ArrayList<Symbol> variadicParameterList, 
      ArrayList<ArrayList<Instruction>> bodyList
    ) {
      this.docstring = docstring;
      this.returnTypeNamesLists = returnTypeNames;
      this.returnTypeLists = returnTypeLists;
      this.parameterTypeNamesLists = parameterTypeNames;
      this.parameterTypesLists = parameterTypesList;
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
    ArrayList<String> returnTypeNames, 
    ArrayList<TypeChecker.Predicate> returnTypeLists, 
    ArrayList<ArrayList<String>> parameterTypeNames,
    ArrayList<ArrayList<TypeChecker.Predicate>> parameterTypesList, 
    ArrayList<ArrayList<Symbol>> parametersList, 
    ArrayList<Symbol> variadicParameterList, 
    ArrayList<ArrayList<Instruction>> bodyList
  ) {
    this.compileTime = new CompileTime(
      DocString.format(docstring),
      returnTypeNames,
      returnTypeLists,
      parameterTypeNames,
      parameterTypesList,
      parametersList,
      variadicParameterList,
      bodyList
    );
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type Signature Operations
  // Whether procedure only has, at most, `(load #void)` as its bodies
  public boolean isTypeSignature() {
    for(ArrayList<Instruction> body : compileTime.bodyList) {
      int n = body.size();
      if(n != 0) {
        if(n != 1) return false;
        Instruction inst = body.get(0);
        if(inst.operation != Instruction.LOAD) return false;
        if(!(inst.argument instanceof escm.type.Void)) return false;
      }
    }
    return true;
  }

  private static boolean mismatchedNulls(Object o1, Object o2) {
    return (o1 == null || o2 == null) && o1 != o2;
  }

  private static boolean differentTypes(String type1, Environment env1, String type2, Environment env2) throws Exception {
    if(mismatchedNulls(type1,type2)) return true;
    return type1 != null && TypeEquality.sameType(type1,env1,type2,env2) == false;
  }

  private static boolean sameSignature(
    Environment thisEnv, Environment thatEnv,
    ArrayList<Symbol> thisSig, Symbol thisVar, 
    ArrayList<String> thisTypeSig, String thisRetType, 
    ArrayList<Symbol> thatSig, Symbol thatVar,
    ArrayList<String> thatTypeSig, String thatRetType
  ) throws Exception {
    // Same variadic status?
    if(mismatchedNulls(thisVar,thatVar)) return false;
    // Same return type?
    if(differentTypes(thisRetType,thisEnv,thatRetType,thatEnv)) return false;
    // Same parameter counts/types?
    int n = thisSig.size();
    if(n != thatSig.size()) return false;
    if(mismatchedNulls(thisTypeSig,thatTypeSig)) return false;
    if(thisTypeSig != null) {
      for(int i = 0; i < n; ++i) {
        if(differentTypes(thisTypeSig.get(i),thisEnv,thatTypeSig.get(i),thatEnv)) {
          return false;
        }
      }
    }
    return true;
  }

  // Whether procedures share same parameter counts/types, and return types 
  // Assumes: this.isTypeSignature() && that.isTypeSignature()
  public boolean sameSignatures(CompoundProcedure that) throws Exception {
    HashSet<Integer> seen = new HashSet<Integer>();
    Environment thisEnv = this.definitionEnvironment;
    Environment thatEnv = that.definitionEnvironment;
    ArrayList<ArrayList<Symbol>> theseParams = this.compileTime.parametersList;
    ArrayList<ArrayList<Symbol>> thoseParams = that.compileTime.parametersList;
    ArrayList<Symbol> theseVars = this.compileTime.variadicParameterList;
    ArrayList<Symbol> thoseVars = that.compileTime.variadicParameterList;
    int n = theseParams.size();
    if(n != thoseParams.size()) return false;
    for(int i = 0; i < n; ++i) {
      ArrayList<Symbol> thisParams = theseParams.get(i);
      Symbol thisVar = theseVars.get(i);
      ArrayList<String> thisParamTypes = this.compileTime.getParameterTypeNames(i);
      String thisRetType = this.compileTime.getReturnTypeName(i);
      int j = 0;
      for(; j < n; ++j) {
        if(!seen.contains(j) && sameSignature(
              thisEnv,
              thatEnv,
              thisParams, 
              thisVar, 
              thisParamTypes, 
              thisRetType,
              thoseParams.get(j), 
              thoseVars.get(j), 
              that.compileTime.getParameterTypeNames(j), 
              that.compileTime.getReturnTypeName(j)
            )
          ) {
          seen.add(j);
          break;
        }
      }
      if(j == n) return false;
    }
    return true;
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
          sig = new Pair(new Keyword(name),sig);
        }
      }
    }
    return new Pair(new Symbol(readableName()),sig);
  }

  public Datum signature() {
    int n = compileTime.parametersList.size();
    if(n == 1) {
      String returnTypeName = compileTime.getReturnTypeName(0);
      Datum parameterClause = clauseSignature(compileTime.getParameterTypeNames(0),compileTime.parametersList.get(0),compileTime.variadicParameterList.get(0));
      if(returnTypeName == null) {
        return parameterClause;
      } else {
        return Pair.List(new Keyword(returnTypeName),parameterClause);
      }
    }
    Datum sigs = Nil.VALUE;
    for(int i = n-1; i >= 0; --i) {
      String returnTypeName = compileTime.getReturnTypeName(i);
      Datum parameterClause = clauseSignature(compileTime.getParameterTypeNames(i),compileTime.parametersList.get(i),compileTime.variadicParameterList.get(i));
      if(returnTypeName == null) {
        sigs = new Pair(parameterClause,sigs);
      } else {
        sigs = new Pair(new Keyword(returnTypeName),new Pair(parameterClause,sigs));
      }
    }
    return sigs;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Application Abstraction
  protected String stringifyReturnType(String returnTypeName) {
    if(returnTypeName == null) return "";
    return returnTypeName + " ";
  }

  protected String stringifyParameters(ArrayList<String> parameterTypeNames, ArrayList<Symbol> params, Symbol variadic) {
    int n = params.size();
    if(n == 0 && variadic == null) return "()";
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
      sb.append(String.format("\n   %2d) ", i+1));
      sb.append(stringifyReturnType(compileTime.getReturnTypeName(i)));
      sb.append(stringifyParameters(compileTime.getParameterTypeNames(i),compileTime.parametersList.get(i),compileTime.variadicParameterList.get(i)));
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
    TypeChecker.Predicate returnType = compileTime.getReturnType(i);
    if(returnType == null) {
      return Interpreter.run(new ExecutionState(extendedEnvironment,compileTime.bodyList.get(i)),(value) -> () -> {
        EscmCallStack.restore(originalCallStack);
        return continuation.run(value);
      });
    } else {
      return Interpreter.run(new ExecutionState(extendedEnvironment,compileTime.bodyList.get(i)),(value) -> () -> {
        if(!returnType.check(definitionEnvironment,value)) {
          throw new Exceptionf(
            "Procedure %s invalid return: type %s is not satisfied by %s", 
            readableName(), 
            compileTime.getReturnTypeName(i), 
            value.profile()
          );
        }
        EscmCallStack.restore(originalCallStack);
        return continuation.run(value);
      });
    }
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