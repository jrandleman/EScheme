// Author: Jordan Randleman - escm.type.procedure.MethodProcedure
// Purpose:
//    Extension to <CompoundProcedure> to support internal method-specific operations.

package escm.type.procedure;
import java.util.ArrayList;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.oo.MetaObject;
import escm.vm.util.Environment;
import escm.vm.util.SourceInformation;

public class MethodProcedure extends CompoundProcedure {
  ////////////////////////////////////////////////////////////////////////////
  // Internal Constructor
  private static final Symbol SUPER_SYMBOL = new Symbol("super");

  private static Environment getSuperBoundEnvironment(CompoundProcedure rootProcedure, MetaObject supr) throws Exception {
    Environment extendedEnvironment = new Environment(rootProcedure.state.definitionEnvironment);
    if(supr == null) { // note that <super> is <null> for interfaces!
      extendedEnvironment.define(SUPER_SYMBOL,Boolean.FALSE);
    } else {
      extendedEnvironment.define(SUPER_SYMBOL,supr);
    }
    return extendedEnvironment;
  }

  // With <super> unbound!
  public MethodProcedure(CompoundProcedure rootProcedure) {
    super(rootProcedure.name,rootProcedure.invocationSource,rootProcedure.state);
  }

  // With <super> bound!
  public MethodProcedure(CompoundProcedure rootProcedure, MetaObject supr) throws Exception {
    super(rootProcedure.name,rootProcedure.invocationSource,new CompoundProcedure.State(getSuperBoundEnvironment(rootProcedure,supr),rootProcedure.state.compileTime));
  }

  // With <super> bound!
  public MethodProcedure(CompoundProcedure rootProcedure, MetaObject supr, String name) throws Exception {
    super(name,rootProcedure.invocationSource,new CompoundProcedure.State(getSuperBoundEnvironment(rootProcedure,supr),rootProcedure.state.compileTime));
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
  private MethodProcedure(String name, SourceInformation invocationSource, State state) {
    super(name,invocationSource,state);
  }

  public MethodProcedure loadWithForcedName(String name) {
    return new MethodProcedure(name,invocationSource,state);
  }


  ////////////////////////////////////////////////////////////////////////////
  // <super> Binding (done updon object creation)
  public MethodProcedure loadWithSuper(MetaObject supr) {
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    if(supr == null) { // note that <super> is <null> for interfaces!
      extendedEnvironment.define(SUPER_SYMBOL,Boolean.FALSE);
    } else {
      extendedEnvironment.define(SUPER_SYMBOL,supr);
    }
    return new MethodProcedure(name,invocationSource,new State(extendedEnvironment,state.compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // <self> Binding (done updon method invocation)
  private static final Symbol SELF_SYMBOL = new Symbol("self");

  public MethodProcedure loadWithSelf(MetaObject self) {
    Environment extendedEnvironment = new Environment(state.definitionEnvironment);
    extendedEnvironment.define(SELF_SYMBOL,self);
    return new MethodProcedure(name,invocationSource,new State(extendedEnvironment,state.compileTime));
  }
}