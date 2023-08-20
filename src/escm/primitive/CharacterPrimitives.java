// Author: Jordan Randleman - escm.primitive.CharacterPrimitives
// Purpose:
//    Java primitives for char operations.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.util.error.Exceptionf;
import escm.vm.type.Primitive;

public class CharacterPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // char?
  public static class IsChar extends Primitive {
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
  public static class IsCharAlphabetic extends Primitive {
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
  public static class IsCharNumeric extends Primitive {
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
  public static class IsCharWhitespace extends Primitive {
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
  public static class IsCharUpperCase extends Primitive {
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
  public static class IsCharLowerCase extends Primitive {
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
  public static class IsCharAlphanumeric extends Primitive {
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
  public static class IsCharControl extends Primitive {
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
  public static class IsCharPunctuation extends Primitive {
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
  public static class IsCharGraph extends Primitive {
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
  public static class IsCharPrint extends Primitive {
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
  public static class IsCharXdigit extends Primitive {
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
  public static class CharUpcase extends Primitive {
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
  public static class CharDowncase extends Primitive {
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
  public static class IsCharEqual extends Primitive {
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
  public static class IsCharLessThan extends Primitive {
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
  public static class IsCharGreaterThan extends Primitive {
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
  public static class IsCharLessThanOrEqual extends Primitive {
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
  public static class IsCharGreaterThanOrEqual extends Primitive {
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
  public static class IsCharEqualCI extends Primitive {
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
  public static class IsCharLessThanCI extends Primitive {
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
  public static class IsCharGreaterThanCI extends Primitive {
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
  public static class IsCharLessThanOrEqualCI extends Primitive {
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
  public static class IsCharGreaterThanOrEqualCI extends Primitive {
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
  public static class IsCharPair extends Primitive {
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
  public static class IsJavaChar extends Primitive {
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
  public static class IsAsciiChar extends Primitive {
    public java.lang.String escmName() {
      return "ascii-char?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(ascii-char? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Character)parameters.get(0)).isAsciiChar());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-count
  public static class CharCount extends Primitive {
    public java.lang.String escmName() {
      return "char-count";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-count <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return new Exact(Character.charCount(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-digit
  public static class CharDigit extends Primitive {
    public java.lang.String escmName() {
      return "char-digit";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 2 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-digit <char> <optional-radix>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int radix = 36;
      if(parameters.size() == 2) {
        Datum radixDatum = parameters.get(1);
        if(!ListPrimitives.isValidSize(radixDatum))
          throw new Exceptionf("'(char-digit <char> <optional-radix>) invalid radix (only 2-36): %s", Exceptionf.profileArgs(parameters));
        radix = ((Real)radixDatum).intValue();
        if(radix < 2 || radix > 36) return Boolean.FALSE;
      }
      int digit = Character.digit(((escm.type.Character)parameters.get(0)).value(),radix);
      if(digit == -1) return Boolean.FALSE;
      return new Exact(digit);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-for-digit
  public static class CharForDigit extends Primitive {
    public java.lang.String escmName() {
      return "char-for-digit";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 2 || !ListPrimitives.isValidSize(parameters.get(0))) 
        throw new Exceptionf("'(char-for-digit <non-negative-integer> <optional-radix>) invalid arg signature: %s", Exceptionf.profileArgs(parameters));
      int radix = 36;
      if(parameters.size() == 2) {
        Datum radixDatum = parameters.get(1);
        if(!ListPrimitives.isValidSize(radixDatum))
          throw new Exceptionf("'(char-for-digit <char> <optional-radix>) invalid radix (only 2-36): %s", Exceptionf.profileArgs(parameters));
        radix = ((Real)radixDatum).intValue();
        if(radix < 2 || radix > 36) return Boolean.FALSE;
      }
      char ch = Character.forDigit(((Real)parameters.get(0)).intValue(),radix);
      if(ch == '\0') return Boolean.FALSE;
      return new escm.type.Character(ch);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-name
  public static class CharName extends Primitive {
    public java.lang.String escmName() {
      return "char-name";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-name <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      String name = Character.getName(((escm.type.Character)parameters.get(0)).value());
      if(name == null) return Boolean.FALSE;
      return new escm.type.String(name);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-defined?
  public static class IsCharDefined extends Primitive {
    public java.lang.String escmName() {
      return "char-defined?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-defined? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Character.isDefined(((escm.type.Character)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-high?
  public static class IsCharHigh extends Primitive {
    public java.lang.String escmName() {
      return "char-high?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-high? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      escm.type.Character ch = (escm.type.Character)parameters.get(0);
      return Boolean.valueOf(ch.isJavaChar() && Character.isHighSurrogate((char)ch.value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-low?
  public static class IsCharLow extends Primitive {
    public java.lang.String escmName() {
      return "char-low?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-low? <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      escm.type.Character ch = (escm.type.Character)parameters.get(0);
      return Boolean.valueOf(ch.isJavaChar() && Character.isLowSurrogate((char)ch.value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-high
  public static class CharHigh extends Primitive {
    public java.lang.String escmName() {
      return "char-high";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-high <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      escm.type.Character ch = (escm.type.Character)parameters.get(0);
      if(ch.isJavaChar()) return Boolean.FALSE;
      return new escm.type.Character(Character.highSurrogate(ch.value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-low
  public static class CharLow extends Primitive {
    public java.lang.String escmName() {
      return "char-low";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-low <char>) expects exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      escm.type.Character ch = (escm.type.Character)parameters.get(0);
      if(ch.isJavaChar()) return Boolean.FALSE;
      return new escm.type.Character(Character.lowSurrogate(ch.value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char-codepoint
  public static class CharCodepoint extends Primitive {
    public java.lang.String escmName() {
      return "char-codepoint";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.Character) || !(parameters.get(1) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char-codepoint <high-char> <low-char>) expects exactly 2 chars: %s", Exceptionf.profileArgs(parameters));
      escm.type.Character high = (escm.type.Character)parameters.get(0);
      escm.type.Character low = (escm.type.Character)parameters.get(1);
      if(!high.isJavaChar() || !Character.isHighSurrogate((char)high.value())) return Boolean.FALSE;
      if(!low.isJavaChar() || !Character.isLowSurrogate((char)low.value())) return Boolean.FALSE;
      return new escm.type.Character(Character.toCodePoint((char)high.value(),(char)low.value()));
    }
  }
}