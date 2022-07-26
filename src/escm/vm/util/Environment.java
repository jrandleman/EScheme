// Author: Jordan Randleman - escm.vm.util.Environment
// Purpose:
//    EScheme environment type used internally by the evaluator and EScheme procedures to
//    represent variable bindings in a particular scope. Nested scopes are supported by 
//    having each environment contain a pointer to its enclosing environment (the global 
//    environment's "super" [enclosing] environment is <null>).

package escm.vm.util;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.io.Serializable;
import escm.util.Exceptionf;
import escm.util.GetClosestStringMatches;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class Environment implements Serializable {
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


  private String getPossibleVariableIntentions(Symbol varName) {
    ArrayList<String> existingVariables = new ArrayList<String>();
    addCurrentVariableBindings(existingVariables);
    ArrayList<String> potentialVariables = GetClosestStringMatches.run(varName.value(),existingVariables,MAXIMUM_SUGGESTED_VARIABLE_ALTERNATIVES);
    StringBuilder sb = new StringBuilder("\n>> Did you mean:");
    for(int i = 0, n = potentialVariables.size(); i < n; ++i) {
      sb.append(String.format("\n   %2d) %s", i+1, potentialVariables.get(i)));
    }
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Has value
  public boolean has(Symbol name) {
    Datum result = bindings.get(name.value());
    if(result == null) {
      if(superEnv == null) return false;
      return superEnv.has(name);
    }
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get value
  public Datum get(Symbol name) throws Exception {
    Datum result = bindings.get(name.value());
    if(result == null) {
      if(superEnv == null) {
        if(name.hasSourceInformation()) {
          throw new Exceptionf("VM [GET] variable \"%s\" doesn't exist!\n>> Location: %s%s", name.value(), name.source(), getPossibleVariableIntentions(name));
        } else {
          throw new Exceptionf("VM [GET] variable \"%s\" doesn't exist!%s", name.value(), getPossibleVariableIntentions(name));
        }
      }
      return superEnv.get(name);
    }
    return result;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set value
  public void set(Symbol name, Datum newValue) throws Exception {
    String nameString = name.value();
    Datum result = bindings.get(nameString);
    if(result == null) {
      if(superEnv == null) {
        if(name.hasSourceInformation()) {
          throw new Exceptionf("VM [SET!] variable \"%s\" doesn't exist!\n>> Location: %s%s", nameString, name.source(), getPossibleVariableIntentions(name));
        } else {
          throw new Exceptionf("VM [SET!] variable \"%s\" doesn't exist!%s", nameString, getPossibleVariableIntentions(name));
        }
      }
      superEnv.set(name,newValue);
    } else {
      bindings.put(nameString,newValue.loadWithName(nameString));
    }
  }


  public void set(Symbol name, Primitive prm) throws Exception {
    set(name,(Datum)(new PrimitiveProcedure(name.value(),prm)));
  }


  public void set(Symbol name, PrimitiveCallable prm) throws Exception {
    set(name,(Datum)(new PrimitiveProcedure(name.value(),prm)));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define value
  public void define(Symbol name, Datum value) {
    String nameString = name.value();
    bindings.put(nameString,value.loadWithName(nameString));
  }
  

  public void define(Symbol name, Primitive prm) {
    define(name,(Datum)(new PrimitiveProcedure(name.value(),prm)));
  }
  

  public void define(Symbol name, PrimitiveCallable prm) {
    define(name,(Datum)(new PrimitiveProcedure(name.value(),prm)));
  }
}