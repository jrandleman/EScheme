// Author: Jordan Randleman - escm.primitive.CharacterPrimitives
// Purpose:
//    Java primitives for char operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class CharacterPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // char?
  public static class IsChar implements Primitive {
    public java.lang.String escmName() {
      return "char?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(char? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Character);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-alphabetic?
  public static class IsCharAlphabetic implements Primitive {
    public java.lang.String escmName() {
      return "char-alphabetic?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-alphabetic? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isAlphabetic(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-numeric?
  public static class IsCharNumeric implements Primitive {
    public java.lang.String escmName() {
      return "char-numeric?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-numeric? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isDigit(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-whitespace?
  public static class IsCharWhitespace implements Primitive {
    public java.lang.String escmName() {
      return "char-whitespace?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-whitespace? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isWhitespace(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-upper-case?
  public static class IsCharUpperCase implements Primitive {
    public java.lang.String escmName() {
      return "char-upper-case?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-upper-case? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isUpperCase(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-lower-case?
  public static class IsCharLowerCase implements Primitive {
    public java.lang.String escmName() {
      return "char-lower-case?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-lower-case? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isLowerCase(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-alphanumeric?
  public static class IsCharAlphanumeric implements Primitive {
    public java.lang.String escmName() {
      return "char-alphanumeric?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-alphanumeric? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      int ch = ((escm.type.Character)parameters.get(0)).value();
      return Boolean.valueOf(Character.isAlphabetic(ch) || Character.isDigit(ch));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-control?
  public static class IsCharControl implements Primitive {
    public java.lang.String escmName() {
      return "char-control?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-control? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isISOControl(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-punctuation?
  public static class IsCharPunctuation implements Primitive {
    public java.lang.String escmName() {
      return "char-punctuation?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-punctuation? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0).display().matches("\\p{Punct}"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-graph?
  public static class IsCharGraph implements Primitive {
    public java.lang.String escmName() {
      return "char-graph?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-graph? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      int ch = ((escm.type.Character)parameters.get(0)).value();
      return Boolean.valueOf(Character.isAlphabetic(ch) || Character.isDigit(ch) || Character.toString(ch).matches("\\p{Punct}"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-print?
  public static class IsCharPrint implements Primitive {
    public java.lang.String escmName() {
      return "char-print?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-print? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      int ch = ((escm.type.Character)parameters.get(0)).value();
      return Boolean.valueOf(Character.isAlphabetic(ch) || Character.isDigit(ch) || ch == ' ' || Character.toString(ch).matches("\\p{Punct}"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-xdigit?
  public static class IsCharXdigit implements Primitive {
    public java.lang.String escmName() {
      return "char-xdigit?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-xdigit? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      int ch = ((escm.type.Character)parameters.get(0)).value();
      return Boolean.valueOf((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'));
    }
  }
}