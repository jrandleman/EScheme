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
import escm.type.MetaObject;
import escm.util.Exceptionf;

public class ObjectAccessChain extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Implementing Object Access Chain Parsing:
  //   "obj.prop1.prop2" => ["obj", "prop1", "prop2"]
  public static ArrayList<String> parse(String accessChainString) throws Exception {
    ArrayList<String> accessChain = new ArrayList<String>(Arrays.asList(accessChainString.split("\\.")));
    validateObjectAccessChain(accessChain,accessChainString);
    return accessChain;
  }

  public static ArrayList<String> parse(Symbol accessChainString) throws Exception {
    return parse(accessChainString.value());
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
  private ArrayList<String> value = null;


  ////////////////////////////////////////////////////////////////////////////
  // Chain Getter
  public ArrayList<String> value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  private static void validateObjectAccessChain(ArrayList<String> accessChain, String originalString) throws Exception {
    if(accessChain.size() < 2)
      throw new Exceptionf("'ObjectAccessChain invalid object access (%s) can't start or end with a period!", originalString);
    for(String access : accessChain) {
      if(access.length() == 0)
        throw new Exceptionf("'ObjectAccessChain invalid object access (%s) can't contain sequential periods!", originalString);
    }
  }

  public ObjectAccessChain(String s) throws Exception {
    value = parse(s);
    validateObjectAccessChain(value,s);
  }

  public ObjectAccessChain(Symbol s) throws Exception {
    value = parse(s);
    validateObjectAccessChain(value,s.value());
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
    return o instanceof ObjectAccessChain && ((Symbol)o).value().equals(value);
  }

  public boolean equals(Object o) {
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
  // Loading-into-memory semantics for the VM's interpreter
  public void define(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject))
      throw new Exceptionf("'ObjectAccessChain 'define foremost item %s (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
    for(int i = 1, n = value.size()-1; i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(!(result instanceof MetaObject))
        throw new Exceptionf("'ObjectAccessChain 'define property %s (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
    }
    ((MetaObject)result).define(value.get(value.size()-1),newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public void set(ExecutionState state, Datum newPropValue) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject))
      throw new Exceptionf("'ObjectAccessChain 'set! foremost item %s (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
    for(int i = 1, n = value.size()-1; i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(!(result instanceof MetaObject))
        throw new Exceptionf("'ObjectAccessChain 'set! property %s (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
    }
    ((MetaObject)result).set(value.get(value.size()-1),newPropValue);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Datum loadWithState(ExecutionState state) throws Exception {
    Datum result = state.env.get(value.get(0));
    if(!(result instanceof MetaObject))
      throw new Exceptionf("'ObjectAccessChain 'load foremost item %s (%s) isn't a meta-object: %s", value.get(0), result.profile(), value);
    for(int i = 1, n = value.size(); i < n; ++i) {
      result = ((MetaObject)result).get(value.get(i));
      if(i+1 < n && !(result instanceof MetaObject))
        throw new Exceptionf("'ObjectAccessChain 'load property %s (%s) isn't a meta-object: %s", value.get(i), result.profile(), value);
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