// Author: Jordan Randleman - escm.type.procedure.types.TypeAlias
// Purpose:
//    Type Alias primitive type, the value returned by "define-type".
//    Used to reference a preexisting type during procedural dispatch.

package escm.type.procedure.types;
import java.util.Objects;
import escm.type.Datum;
import escm.type.Keyword;
import escm.vm.util.Environment;
import escm.vm.util.ExecutionState;

public class TypeAlias extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private final Environment definitionEnvironment;
  private final Keyword type;
  private final TypeChecker.Predicate predicate;

  public Environment definitionEnvironment() {
    return definitionEnvironment;
  }

  public Keyword typeKeyword() {
    return type;
  }

  public boolean check(Datum value) throws Exception {
    return predicate.check(definitionEnvironment,value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public static boolean isCyclic(Environment definitionEnvironment, Keyword type) throws Exception {
    try {
      TypeEquality.Node typeEqualityNode = TypeEquality.tree(type,definitionEnvironment);
      typeEqualityNode.equals(typeEqualityNode); // overflows if alias is cyclic
      return false;
    } catch(StackOverflowError e) {
      return true;
    }
  }


  public TypeAlias(Environment definitionEnvironment, Keyword type) throws Exception {
    this.definitionEnvironment = definitionEnvironment;
    this.type = type;
    this.predicate = TypeChecker.getPredicate(type);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "type-alias";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof TypeAlias && ((TypeAlias)o).predicate == predicate;
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public java.lang.String docstring() {
    return "Type alias for \""+type.value()+"\". Created by <define-type>.";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),predicate);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return "#<type-alias "+type.value()+">";
  }

  public java.lang.String write() {
    return display();
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  public TypeAlias quote(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public TypeAlias loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public TypeAlias loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public TypeAlias shallowCopy() {
    return this;
  }
}