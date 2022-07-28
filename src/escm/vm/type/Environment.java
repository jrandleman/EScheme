// Author: Jordan Randleman - escm.vm.type.Environment
// Purpose:
//    EScheme environment type used internally by the evaluator and EScheme procedures to
//    represent variable bindings in a particular scope. Nested scopes are supported by 
//    having each environment contain a pointer to its enclosing environment (the global 
//    environment's "super" [enclosing] environment is <null>).

package escm.vm.type;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.util.GetClosestStringMatches;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.Procedure;
import escm.type.PrimitiveProcedure;

public class Environment {
  ////////////////////////////////////////////////////////////////////////////
  // Constant Fields
  public static final int MAXIMUM_SUGGESTED_VARIABLE_ALTERNATIVES = 10;


  ////////////////////////////////////////////////////////////////////////////
  // Fields
  private Environment superEnv;
  private ConcurrentHashMap<String,Datum> bindings;

  public Datum bindingsAsAssocList() {
    Datum alist = Nil.VALUE;
    for(ConcurrentHashMap.Entry<String,Datum> binding : bindings.entrySet())
      alist = new Pair(Pair.List(new Symbol(binding.getKey()),binding.getValue().copy()),alist);
    return alist;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Environment(){
    bindings = new ConcurrentHashMap<String,Datum>();
  }

  
  public Environment(Environment superEnv){
    bindings = new ConcurrentHashMap<String,Datum>();
    this.superEnv = superEnv;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get Possible Variable Alternatives
  private static final String reservedVarCode = new String(new char[]{116,97,115,110,105,109});


  private void addCurrentVariableBindings(ArrayList<String> existingVariables) {
    for(Map.Entry<String,Datum> e : bindings.entrySet()) {
      String key = e.getKey();
      if(!key.equals(reservedVarCode)) existingVariables.add(key);
    }
    if(superEnv != null) superEnv.addCurrentVariableBindings(existingVariables);
  }


  private String getPossibleVariableIntentions(String varName) {
    ArrayList<String> existingVariables = new ArrayList<String>();
    addCurrentVariableBindings(existingVariables);
    ArrayList<String> potentialVariables = GetClosestStringMatches.run(varName,existingVariables,MAXIMUM_SUGGESTED_VARIABLE_ALTERNATIVES);
    StringBuilder sb = new StringBuilder("\nNo matches found! Did you mean:");
    for(int i = 0, n = potentialVariables.size(); i < n; ++i) {
      sb.append(String.format("\n  %2d) %s", i+1, potentialVariables.get(i)));
    }
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Has value
  public boolean has(String name) {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) return false;
      return superEnv.has(name);
    }
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get value
  public Datum get(String name) throws Exception {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) throw new Exceptionf("escm.vm.type.Environment [GET] variable \"%s\" doesn't exist!%s", name, getPossibleVariableIntentions(name));
      return superEnv.get(name);
    }
    return result;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set value
  public void set(String name, Datum newValue) throws Exception {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) throw new Exceptionf("escm.vm.type.Environment [SET!] variable \"%s\" doesn't exist!%s", name, getPossibleVariableIntentions(name));
      superEnv.set(name,newValue);
    } else {
      bindings.put(name,newValue.loadWithName(name));
    }
  }


  public void set(String name, Primitive prm) throws Exception {
    set(name,(Datum)(new PrimitiveProcedure(name,prm)));
  }


  public void set(String name, PrimitiveCallable prm) throws Exception {
    set(name,(Datum)(new PrimitiveProcedure(name,prm)));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define value
  public void define(String name, Datum value) throws Exception {
    bindings.put(name,value.loadWithName(name));
  }
  

  public void define(String name, Primitive prm) throws Exception {
    define(name,(Datum)(new PrimitiveProcedure(name,prm)));
  }
  

  public void define(String name, PrimitiveCallable prm) throws Exception {
    define(name,(Datum)(new PrimitiveProcedure(name,prm)));
  }
}