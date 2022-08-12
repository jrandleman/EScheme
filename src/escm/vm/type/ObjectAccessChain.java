// Author: Jordan Randleman - escm.type.ObjectAccessChain
// Purpose:
//    Special datum generated by the assembler to hold object access chains
//    (e.g. "obj.prop1.prop2") once they've been split up. This makes it such 
//    that the interpreter only need evaluate object accesses at runtime, 
//    rather than having to re-parse each reference at runtime.
//
//    Note that this datatype is IMPOSSIBLE to generate organically via EScheme
//    code, it is ONLY intended for internal use by the VM.

package escm.vm.type;
import java.util.Objects;
import java.util.ArrayList;
import java.util.Arrays;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.oo.MetaObject;
import escm.type.procedure.Procedure;
import escm.util.Exceptionf;
import escm.vm.util.SourceInformation;

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


  public static ArrayList<Symbol> parse(Symbol accessChainSymbol) throws Exception {
    String[] accessChainStrings = accessChainSymbol.value().split("\\.");
    ArrayList<Symbol> accessChain = new ArrayList<Symbol>(accessChainStrings.length);
    if(accessChainSymbol.hasSourceInformation()) {
      SourceInformation originalSource = accessChainSymbol.source();
      for(int i = 0; i < accessChainStrings.length; ++i)
        accessChain.add(generatePropertySymbol(accessChainStrings,i,originalSource));
    } else {
      for(int i = 0; i < accessChainStrings.length; ++i)
        accessChain.add(new Symbol(accessChainStrings[i]));
    }
    validateObjectAccessChain(accessChain,accessChainSymbol);
    return accessChain;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing Object Access Chain Checking 
  //   => Symbol contains a "." and isn't entirely made of "."s
  public static boolean is(String accessChainString) {
    if(accessChainString.contains(".")) {
      for(int i = 0, n = accessChainString.length(); i < n; ++i) {
        if(accessChainString.charAt(i) != '.') return true;
      }
    }
    return false;
  }

  public static boolean is(Symbol accessChainString) {
    return is(accessChainString.value());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Chain Field
  private ArrayList<Symbol> value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Chain Getter
  public ArrayList<Symbol> value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  private static void validateObjectAccessChain(ArrayList<Symbol> accessChain, Symbol originalSymbol) throws Exception {
    if(accessChain.size() < 2) {
      if(originalSymbol.hasSourceInformation()) {
        throw new Exceptionf("ObjectAccessChain: Invalid object access (%s) can't start or end with a period!\n>> Location: %s", originalSymbol.value(), originalSymbol.source());
      } else {
        throw new Exceptionf("ObjectAccessChain: Invalid object access (%s) can't start or end with a period!", originalSymbol.value());
      }
    }
    for(Symbol access : accessChain) {
      if(access.value().length() == 0) {
        if(originalSymbol.hasSourceInformation()) {
          throw new Exceptionf("ObjectAccessChain: Invalid object access (%s) can't contain sequential periods!\n>> Location: %s", originalSymbol.value(), originalSymbol.source());
        } else {
          throw new Exceptionf("ObjectAccessChain: Invalid object access (%s) can't contain sequential periods!", originalSymbol.value());
        }
      }
    }
  }

  public ObjectAccessChain(Symbol s) throws Exception {
    value = parse(s);
    validateObjectAccessChain(value,s);
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
  public String display() {
    return String.format("#<object-access-chain %s>", value);
  }

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Define a new value in the object property chain
  public void define(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject)) {
      if(value.get(0).hasSourceInformation()) {
        throw new Exceptionf("ObjectAccessChain: <define> foremost item \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(0), result.profile(), value, value.get(0).source());
      } else {
        throw new Exceptionf("ObjectAccessChain: <define> foremost item \"%s\" (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
      }
    }
    for(int i = 1, n = value.size()-1; i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(!(result instanceof MetaObject)) {
        if(value.get(i).hasSourceInformation()) {
          throw new Exceptionf("ObjectAccessChain: <define> property \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(i), result.profile(), value, value.get(i).source());
        } else {
          throw new Exceptionf("ObjectAccessChain: <define> property \"%s\" (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
        }
      }
    }
    ((MetaObject)result).define(value.get(value.size()-1),newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Set an existing value in the object property chain
  public void set(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject)) {
      if(value.get(0).hasSourceInformation()) {
        throw new Exceptionf("ObjectAccessChain: <set!> foremost item \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(0), result.profile(), value, value.get(0).source());
      } else {
        throw new Exceptionf("ObjectAccessChain: <set!> foremost item \"%s\" (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
      }
    }
    for(int i = 1, n = value.size()-1; i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(!(result instanceof MetaObject)) {
        if(value.get(i).hasSourceInformation()) {
          throw new Exceptionf("ObjectAccessChain: <set!> property \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(i), result.profile(), value, value.get(i).source());
        } else {
          throw new Exceptionf("ObjectAccessChain: <set!> property \"%s\" (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
        }
      }
    }
    ((MetaObject)result).set(value.get(value.size()-1),newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Check if an object property chain is valid
  public boolean has(ExecutionState state) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject)) {
      if(value.get(0).hasSourceInformation()) {
        throw new Exceptionf("ObjectAccessChain: <has?> foremost item \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(0), result.profile(), value, value.get(0).source());
      } else {
        throw new Exceptionf("ObjectAccessChain: <has?> foremost item \"%s\" (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
      }
    }
    for(int i = 1, n = value.size()-1; i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(!(result instanceof MetaObject)) {
        if(value.get(i).hasSourceInformation()) {
          throw new Exceptionf("ObjectAccessChain: <has?> property \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(i), result.profile(), value, value.get(i).source());
        } else {
          throw new Exceptionf("ObjectAccessChain: <has?> property \"%s\" (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
        }
      }
    }
    return ((MetaObject)result).has(value.get(value.size()-1));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter (effectively "get")
  public Datum loadWithState(ExecutionState state) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject)) {
      if(value.get(0).hasSourceInformation()) {
        throw new Exceptionf("ObjectAccessChain: <get> foremost item \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", value.get(0), result.profile(), value, value.get(0).source());
      } else {
        throw new Exceptionf("ObjectAccessChain: <get> foremost item \"%s\" (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
      }
    }
    for(int i = 1, n = value.size(); i < n; ++i) {
      Symbol prop = value.get(i);
      result = ((MetaObject)result).get(prop);
      if(i+1 < n && !(result instanceof MetaObject)) {
        if(prop.hasSourceInformation()) {
          throw new Exceptionf("ObjectAccessChain: <get> property \"%s\" (%s) isn't a meta-object: %s\n>> Location: %s", prop, result.profile(), value, prop.source());
        } else {
          throw new Exceptionf("ObjectAccessChain: <get> property \"%s\" (%s) isn't a meta-object: %s", prop, result.profile(), value);
        }
      } else if(i+1 == n && result instanceof Procedure && prop.hasSourceInformation()) {
        result = ((Procedure)result).loadWithInvocationSource(prop.source());
      }
    }
    return result;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public ObjectAccessChain loadWithName(String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public ObjectAccessChain copy() {
    return this;
  }
}