// Author: Jordan Randleman - escm.vm.type.Environment
// Purpose:
//    EScheme environment type used internally by the evaluator and EScheme procedures to
//    represent variable bindings in a particular scope. Nested scopes are supported by 
//    having each environment contain a pointer to its enclosing environment (the global 
//    environment's "super" [enclosing] environment is <null>).

package escm.vm.type;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.Procedure;
import escm.type.PrimitiveProcedure;

public class Environment {
  ////////////////////////////////////////////////////////////////////////////
  // Fields
  private Environment superEnv;
  private ConcurrentHashMap<String,Datum> bindings;

  public Datum bindingsAsAssocList() throws Exception {
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
  // Has value
  public boolean has(String name) throws Exception {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) return false;;
      return superEnv.has(name);
    }
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Get value
  public Datum get(String name) throws Exception {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) throw new Exceptionf("escm.vm.type.Environment variable %s doesn't exist!", name);
      return superEnv.get(name);
    }
    return result;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set value
  public void set(String name, Datum newValue) throws Exception {
    Datum result = bindings.get(name);
    if(result == null) {
      if(superEnv == null) throw new Exceptionf("escm.vm.type.Environment variable %s doesn't exist!", name);
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