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


  ////////////////////////////////////////////////////////////////////////////
  // char-upcase
  public static class CharUpcase implements Primitive {
    public java.lang.String escmName() {
      return "char-upcase";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-upcase <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Character(Character.toUpperCase(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-downcase
  public static class CharDowncase implements Primitive {
    public java.lang.String escmName() {
      return "char-downcase";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-downcase <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Character(Character.toLowerCase(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char=?
  public static class IsCharEqual implements Primitive {
    public java.lang.String escmName() {
      return "char=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int mainValue = ((escm.type.Character)parameters.get(0)).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        if(mainValue != ((escm.type.Character)p).value()) return Boolean.FALSE;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char<?
  public static class IsCharLessThan implements Primitive {
    public java.lang.String escmName() {
      return "char<?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char<? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = ((escm.type.Character)parameters.get(0)).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char<? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = ((escm.type.Character)p).value();
        if(lastValue >= currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char>?
  public static class IsCharGreaterThan implements Primitive {
    public java.lang.String escmName() {
      return "char>?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char>? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = ((escm.type.Character)parameters.get(0)).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char>? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = ((escm.type.Character)p).value();
        if(lastValue <= currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char<=?
  public static class IsCharLessThanOrEqual implements Primitive {
    public java.lang.String escmName() {
      return "char<=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char<=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = ((escm.type.Character)parameters.get(0)).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char<=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = ((escm.type.Character)p).value();
        if(lastValue > currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char>=?
  public static class IsCharGreaterThanOrEqual implements Primitive {
    public java.lang.String escmName() {
      return "char>=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char>=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = ((escm.type.Character)parameters.get(0)).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char>=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = ((escm.type.Character)p).value();
        if(lastValue < currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-ci=?
  public static class IsCharEqualCI implements Primitive {
    public java.lang.String escmName() {
      return "char-ci=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-ci=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int mainValue = Character.toUpperCase(((escm.type.Character)parameters.get(0)).value());
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char-ci=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        if(mainValue != Character.toUpperCase(((escm.type.Character)p).value())) return Boolean.FALSE;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-ci<?
  public static class IsCharLessThanCI implements Primitive {
    public java.lang.String escmName() {
      return "char-ci<?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-ci<? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = Character.toUpperCase(((escm.type.Character)parameters.get(0)).value());
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char-ci<? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = Character.toUpperCase(((escm.type.Character)p).value());
        if(lastValue >= currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-ci>?
  public static class IsCharGreaterThanCI implements Primitive {
    public java.lang.String escmName() {
      return "char-ci>?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-ci>? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = Character.toUpperCase(((escm.type.Character)parameters.get(0)).value());
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char-ci>? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = Character.toUpperCase(((escm.type.Character)p).value());
        if(lastValue <= currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-ci<=?
  public static class IsCharLessThanOrEqualCI implements Primitive {
    public java.lang.String escmName() {
      return "char-ci<=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-ci<=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = Character.toUpperCase(((escm.type.Character)parameters.get(0)).value());
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char-ci<=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = Character.toUpperCase(((escm.type.Character)p).value());
        if(lastValue > currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-ci>=?
  public static class IsCharGreaterThanOrEqualCI implements Primitive {
    public java.lang.String escmName() {
      return "char-ci>=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-ci>=? <char> ...) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int lastValue = Character.toUpperCase(((escm.type.Character)parameters.get(0)).value());
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Character))
          throw new Exceptionf("'(char-ci>=? <char> ...) arg %s isn't a char: %s", p.profile(), Exceptionf.profileArgs(parameters));
        int currValue = Character.toUpperCase(((escm.type.Character)p).value());
        if(lastValue < currValue) return Boolean.FALSE;
        lastValue = currValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-pair?
  public static class IsCharPair implements Primitive {
    public java.lang.String escmName() {
      return "char-pair?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-pair? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Character)parameters.get(0)).isSurrogateCharPair());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // java-char?
  public static class IsJavaChar implements Primitive {
    public java.lang.String escmName() {
      return "java-char?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(java-char? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Character)parameters.get(0)).isJavaChar());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ascii-char?
  public static class IsAsciiChar implements Primitive {
    public java.lang.String escmName() {
      return "ascii-char?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(ascii-char? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Character)parameters.get(0)).isAsciiChar());
    }
  }
}