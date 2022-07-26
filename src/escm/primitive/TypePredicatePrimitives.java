// Author: Jordan Randleman - escm.primitive.TypePredicatePrimitives
// Purpose:
//    Java primitives for type predicates.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Boolean;
import escm.type.Eof;
import escm.type.Void;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class TypePredicatePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // typeof
  public static class Typeof implements Primitive {
    public java.lang.String escmName() {
      return "typeof";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(typeof <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new Symbol(parameters.get(0).type());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // void?
  public static class IsVoid implements Primitive {
    public java.lang.String escmName() {
      return "void?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(void? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Void);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // boolean?
  public static class IsBoolean implements Primitive {
    public java.lang.String escmName() {
      return "boolean?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(boolean? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Boolean);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // eof?
  public static class IsEof implements Primitive {
    public java.lang.String escmName() {
      return "eof?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eof? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Eof);
    }
  }
}