// Author: Jordan Randleman - escm.type.Pair
// Purpose:
//    Pair primitive type.

package escm.type;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Pair extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Car/Cdr Fields
  private Datum car;
  private Datum cdr;


  ////////////////////////////////////////////////////////////////////////////
  // Public Car/Cdr Getters
  public Datum car() {
    return car;
  }
  
  public Datum cdr() {
    return cdr;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public Pair(Datum car, Datum cdr) {
    this.car = car;
    this.cdr = cdr;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Generator
  public static Datum List(Datum ... listContents) {
    Datum d = Nil.VALUE;
    for(int i = listContents.length-1; i >= 0; --i)
      d = new Pair(listContents[i],d);
    return d;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Predicate
  public static boolean isList(Datum d) {
    if(d instanceof Nil) return true;
    if(!(d instanceof Pair)) return false;
    Datum iterator = d;
    while(iterator instanceof Pair) iterator = ((Pair)iterator).cdr;
    return iterator instanceof Nil;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "pair";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Pair && ((Pair)o).car.eq(car) && ((Pair)o).cdr.eq(cdr);
  }

  public boolean equals(Object o) {
    return o instanceof Pair && ((Pair)o).car.equals(car) && ((Pair)o).cdr.equals(cdr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),car,cdr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    if(!(cdr instanceof Pair) && !(cdr instanceof Nil)) { // printing non-list pair
      return "(" + car.display() + " . " + cdr.display() + ")";
    } else { // printing a list
      Datum iterator = this;
      boolean addSpace = false;
      StringBuilder list = new StringBuilder("(");
      while(iterator instanceof Pair) {
        if(addSpace) {
          list.append(' ');
        } else {
          addSpace = true;
        }
        list.append(((Pair)iterator).car.display());
        iterator = ((Pair)iterator).cdr;
      }
      if(!(iterator instanceof Nil)) { // dotted list
        list.append(" . ");
        list.append(iterator.display());
      }
      list.append(')');
      return list.toString();
    }
  }

  public java.lang.String write() {
    if(!(cdr instanceof Pair) && !(cdr instanceof Nil)) { // printing non-list pair
      return "(" + car.write() + " . " + cdr.write() + ")";
    } else { // printing a list
      Datum iterator = this;
      boolean addSpace = false;
      StringBuilder list = new StringBuilder("(");
      while(iterator instanceof Pair) {
        if(addSpace) {
          list.append(' ');
        } else {
          addSpace = true;
        }
        list.append(((Pair)iterator).car.write());
        iterator = ((Pair)iterator).cdr;
      }
      if(!(iterator instanceof Nil)) { // dotted list
        list.append(" . ");
        list.append(iterator.write());
      }
      list.append(')');
      return list.toString();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Pair loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Pair loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}