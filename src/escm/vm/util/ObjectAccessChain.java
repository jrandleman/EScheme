// Author: Jordan Randleman - escm.vm.util.ObjectAccessChain
// Purpose:
//    Special datum generated by the assembler to hold object access chains
//    (e.g. "obj.prop1.prop2") once they've been split up. This makes it such 
//    that the interpreter only need evaluate object accesses at runtime, 
//    rather than having to re-parse each reference at runtime.
//
//    Note that this datatype is IMPOSSIBLE to generate organically via EScheme
//    code, it is ONLY intended for internal use by the VM.

package escm.vm.util;
import java.util.Objects;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.oo.Dottable;
import escm.type.procedure.Procedure;
import escm.util.error.Exceptionf;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;

public class ObjectAccessChain extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Implementing Object Access Chain Parsing:
  //   'obj.prop1.prop2 => ['obj, 'prop1, 'prop2]
  private static long updatedColumnNumber(String[] accessChainStrings, int currentProperty, long originalColumnNumber) {
    for(int i = 0; i < currentProperty; ++i)
      originalColumnNumber += accessChainStrings[i].length()+1;
    return originalColumnNumber;
  }


  private static Symbol generatePropertySymbol(String[] accessChainStrings, int i, SourceInformation originalSource) {
    long propColumn = updatedColumnNumber(accessChainStrings,i,originalSource.columnNumber());
    SourceInformation newSource = new SourceInformation(originalSource.fileName(),originalSource.lineNumber(),propColumn);
    return new Symbol(accessChainStrings[i],newSource);
  }


  // @PRECONDITION: ObjectAccessChain.is(accessChainSymbol)
  public static Symbol[] parse(Symbol accessChainSymbol) throws Exception {
    String[] accessChainStrings = accessChainSymbol.value().split("\\.");
    Symbol[] accessChain = new Symbol[accessChainStrings.length];
    if(accessChainSymbol.hasSourceInformation()) {
      SourceInformation originalSource = accessChainSymbol.source();
      for(int i = 0; i < accessChainStrings.length; ++i)
        accessChain[i] = generatePropertySymbol(accessChainStrings,i,originalSource);
    } else {
      for(int i = 0; i < accessChainStrings.length; ++i)
        accessChain[i] = new Symbol(accessChainStrings[i]);
    }
    return accessChain;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing Object Access Chain Checking 
  //   => Symbol contains a "." and isn't entirely made of "."s
  public static boolean is(String accessChain) {
    int i = 0, n = accessChain.length();
    while(i < n && accessChain.charAt(i) != '.') ++i;
    if(i == 0 || i == n) return false;
    while(i < n) {
      if(accessChain.charAt(i) == '.' && (i == n-1 || accessChain.charAt(i+1) == '.')) return false;
      ++i;
    }
    return true;
  }

  public static boolean is(Symbol accessChain) {
    return is(accessChain.value());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Chain Field
  private Symbol[] value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Chain Getter
  public Symbol[] value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  // @PRECONDITION: ObjectAccessChain.is(accessChainSymbol)
  public ObjectAccessChain(Symbol s) throws Exception {
    value = parse(s);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "object-access-chain";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof ObjectAccessChain && ((ObjectAccessChain)o).value.equals(value);
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  private String serializeValue() {
    StringBuilder sb = new StringBuilder("[");
    for(int i = 0; i < value.length; ++i) {
      sb.append(value[i].value());
      if(i+1<value.length) sb.append(' ');
    }
    sb.append("]");
    return sb.toString();
  }

  public String display() {
    return "#<object-access-chain "+serializeValue()+">";
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define a new value in the object property chain
  private String invalidObjectAccessChainMessage(String opName, int idx, Datum val) throws Exception {
    if(value[idx].hasSourceInformation()) {
      return String.format("ObjectAccessChain: <%s> property #%d \"%s\" (%s) isn't <dottable>: %s\n>> Location: %s", opName, idx+1, value[idx], val.profile(), serializeValue(), value[idx].source());
    } else {
      return String.format("ObjectAccessChain: <%s> property #%d \"%s\" (%s) isn't <dottable>: %s", opName, idx+1, value[idx], val.profile(), serializeValue());
    }
  }


  public void define(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value[0]);
    if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("define",0,result));
    int n = value.length-1;
    for(int i = 1; i < n; ++i) {
      result = ((Dottable)result).get(value[i]);
      if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("define",i,result));
    }
    ((Dottable)result).define(value[n],newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set an existing value in the object property chain
  public void set(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value[0]);
    if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("set!",0,result));
    int n = value.length-1;
    for(int i = 1; i < n; ++i) {
      result = ((Dottable)result).get(value[i]);
      if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("set!",i,result));
    }
    ((Dottable)result).set(value[n],newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Check if an object property chain is valid
  public boolean has(ExecutionState state) throws Exception {
    Datum result = state.env.get(value[0]);
    if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("has?",0,result));
    int n = value.length-1;
    for(int i = 1; i < n; ++i) {
      result = ((Dottable)result).get(value[i]);
      if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("has?",i,result));
    }
    return ((Dottable)result).has(value[n]);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter (effectively "get")
  public Datum loadWithState(Environment env) throws Exception {
    Datum result = env.get(value[0]);
    if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("get",0,result));
    for(int i = 1, n = value.length; i < n; ++i) {
      result = ((Dottable)result).get(value[i]);
      if(i+1 < n) {
        if(!(result instanceof Dottable)) throw new Exceptionf(invalidObjectAccessChainMessage("get",i,result));
      } else if(result instanceof Procedure && value[i].hasSourceInformation()) {
        result = ((Procedure)result).loadWithInvocationSource(value[i].source());
      }
    }
    return result;
  }

  public Datum loadWithState(ExecutionState state) throws Exception {
    return loadWithState(state.env);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public ObjectAccessChain loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public ObjectAccessChain copy() {
    return this;
  }
}