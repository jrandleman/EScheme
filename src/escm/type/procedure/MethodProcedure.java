// Author: Jordan Randleman - escm.type.procedure.MethodProcedure
// Purpose:
//    Extension to <CompoundProcedure> to support internal method-specific operations.

package escm.type.procedure;
import java.util.Objects;
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

  private static Environment getMethodEnvironment(MetaObject selfObj, MetaObject superObj, Environment rootProcedureEnvironment) {
    Environment methodEnvironment = new Environment(rootProcedureEnvironment);
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superAsDatum(superObj));
    return methodEnvironment;
  }

  public MethodProcedure(MetaObject selfObj, MetaObject superObj, CompoundProcedure rootProcedure) {
    super(rootProcedure.name,rootProcedure.invocationSource,getMethodEnvironment(selfObj,superObj,rootProcedure.definitionEnvironment),rootProcedure.compileTime);
    if(superObj != null) this.superObj = superObj;
  }

  public MethodProcedure(MetaObject selfObj, MetaObject superObj, CompoundProcedure rootProcedure, String name) {
    super(name,rootProcedure.invocationSource,getMethodEnvironment(selfObj,superObj,rootProcedure.definitionEnvironment),rootProcedure.compileTime);
    if(superObj != null) this.superObj = superObj;
  }
  

  ////////////////////////////////////////////////////////////////////////////
  // Thunk Querying (unary variadics are considered thunks!)
  public boolean isThunk() {
    for(ArrayList<Symbol> params : this.compileTime.parametersList) {
      if(params.isEmpty()) return true;
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash("method-"+type(),System.identityHashCode(definitionEnvironment),System.identityHashCode(compileTime));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Forced Name binding (used by escm.type.oo.MetaObject)
  private MethodProcedure(String name, SourceInformation invocationSource, Environment definitionEnvironment, CompileTime compileTime, Datum superObj) {
    super(name,invocationSource,definitionEnvironment,compileTime);
    this.superObj = superObj;
  }

  public MethodProcedure loadWithForcedName(String name) {
    return new MethodProcedure(name,invocationSource,definitionEnvironment,compileTime,superObj);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading with new object bindings (for <EscmObject.copy()>):
  //   => Note that <definitionEnvironment.superEnv()> points to the 
  //      object's class' original enclosing environment.
  public MethodProcedure loadWithNewSelfAndSuper(MetaObject selfObj, MetaObject superObj) {
    Datum superObjDatum = superAsDatum(superObj);
    Environment methodEnvironment = new Environment(definitionEnvironment.superEnv());
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superObjDatum);
    return new MethodProcedure(name,invocationSource,methodEnvironment,compileTime,superObjDatum);
  }


  ////////////////////////////////////////////////////////////////////////////
  // <self> Binding (done updon dynamic method invocation)
  public MethodProcedure loadWithDynamicSelf(MetaObject selfObj) throws Exception {
    Environment methodEnvironment = new Environment(definitionEnvironment.superEnv());
    methodEnvironment.define(SELF_SYMBOL,selfObj);
    methodEnvironment.define(SUPER_SYMBOL,superObj);
    return new MethodProcedure(name,invocationSource,methodEnvironment,compileTime,superObj);
  }
}