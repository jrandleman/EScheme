// Author: Jordan Randleman - escm.type.procedure.MethodProcedure
// Purpose:
//    Extension to <CompoundProcedure> to support internal method-specific operations.

package escm.type.procedure;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.oo.MetaObject;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;

public class MethodProcedure extends CompoundProcedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Super Object (used by <loadWithDynamicSelf>)
  Datum superObj = Boolean.FALSE;


  ////////////////////////////////////////////////////////////////////////////
  // Internal Constructor
  private static final Symbol SELF_SYMBOL = new Symbol("self");
  private static final Symbol SUPER_SYMBOL = new Symbol("super");

  private static Datum superAsDatum(MetaObject superObj) {
    if(superObj == null) return Boolean.FALSE;
    return superObj;
  }

  private static State getMethodState(MetaObject selfObj, MetaObject superObj, State rootProcedureState) {
    Environment methodEnvironment = new Environment(rootProcedureState.definitionEnvironment);
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superAsDatum(superObj));
    return new State(methodEnvironment,rootProcedureState.compileTime);
  }

  public MethodProcedure(MetaObject selfObj, MetaObject superObj, CompoundProcedure rootProcedure) {
    super(rootProcedure.name,rootProcedure.invocationSource,getMethodState(selfObj,superObj,rootProcedure.state));
    if(superObj != null) this.superObj = superObj;
  }

  public MethodProcedure(MetaObject selfObj, MetaObject superObj, CompoundProcedure rootProcedure, String name) {
    super(name,rootProcedure.invocationSource,getMethodState(selfObj,superObj,rootProcedure.state));
    if(superObj != null) this.superObj = superObj;
  }
  

  ////////////////////////////////////////////////////////////////////////////
  // Thunk Querying (unary variadics are considered thunks!)
  public boolean isThunk() {
    for(ArrayList<Symbol> params : this.state.compileTime.parametersList) {
      if(params.isEmpty()) return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Forced Name binding (used by escm.type.oo.MetaObject)
  private MethodProcedure(String name, SourceInformation invocationSource, State state, Datum superObj) {
    super(name,invocationSource,state);
    this.superObj = superObj;
  }

  public MethodProcedure loadWithForcedName(String name) {
    return new MethodProcedure(name,invocationSource,state,superObj);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading with new object bindings (for <EscmObject.copy()>):
  //   => Note that <state.definitionEnvironment.superEnv()> points to the 
  //      object's class' original enclosing environment.
  public MethodProcedure loadWithNewSelfAndSuper(MetaObject selfObj, MetaObject superObj) {
    Datum superObjDatum = superAsDatum(superObj);
    Environment methodEnvironment = new Environment(state.definitionEnvironment.superEnv());
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superObjDatum);
    return new MethodProcedure(name,invocationSource,new State(methodEnvironment,state.compileTime),superObjDatum);
  }


  ////////////////////////////////////////////////////////////////////////////
  // <self> Binding (done updon method invocation)
  public MethodProcedure loadWithDynamicSelf(MetaObject selfObj) throws Exception {
    Environment methodEnvironment = new Environment(state.definitionEnvironment.superEnv());
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superObj);
    return new MethodProcedure(name,invocationSource,new State(methodEnvironment,state.compileTime),superObj);
  }
}