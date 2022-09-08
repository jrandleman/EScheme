// Author: Jordan Randleman - escm.primitive.StringPrimitives
// Purpose:
//    Java primitives for string procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.regex.Matcher;
import escm.type.Datum;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.util.StringParser;
import escm.vm.type.Primitive;

public class StringPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // string
  public static class String implements Primitive {
    public java.lang.String escmName() {
      return "string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) sb.append(p.display());
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-length
  public static class StringLength implements Primitive {
    public java.lang.String escmName() {
      return "string-length";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-length <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.String)parameters.get(0)).codePointLength());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-java-length
  public static class StringJavaLength implements Primitive {
    public java.lang.String escmName() {
      return "string-java-length";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-java-length <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.String)parameters.get(0)).value().length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-empty?
  public static class IsStringEmpty implements Primitive {
    public java.lang.String escmName() {
      return "string-empty?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-empty? <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.String)parameters.get(0)).value().length() == 0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-reverse
  public static class StringReverse implements Primitive {
    public java.lang.String escmName() {
      return "string-reverse";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-reverse <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      int[] codepoints = ((escm.type.String)parameters.get(0)).toCodePoints();
      StringBuilder sb = new StringBuilder();
      for(int i = codepoints.length-1; i >= 0; --i)
        sb.append(java.lang.Character.toString(codepoints[i]));
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-append
  public static class StringAppend implements Primitive {
    public java.lang.String escmName() {
      return "string-append";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) {
        if(!(p instanceof escm.type.String))
          throw new Exceptionf("'(string-append <string> ...) received a non-string arg: %s", Exceptionf.profileArgs(parameters));
        sb.append(((escm.type.String)p).value());
      }
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ref
  public static class StringRef implements Primitive {
    public java.lang.String escmName() {
      return "string-ref";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(string-ref <string> <index>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      Datum index = parameters.get(1);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(string-ref <string> <index>) 1st arg %s isn't a string!", str.profile());
      if(!ListPrimitives.isValidSize(index))
        throw new Exceptionf("'(string-ref <string> <index>) 2nd arg %s isn't a non-negative integer!", index.profile());
      int indexValue = ((Real)index).intValue();
      escm.type.String strValue = (escm.type.String)str;
      if(indexValue >= strValue.codePointLength())
        throw new Exceptionf("'(string-ref <string> <index>) index %d exceeds length of string %s", indexValue, str.write());
      return new escm.type.Character(strValue.codePointAt(indexValue));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // substring
  public static class Substring implements Primitive {
    public java.lang.String escmName() {
      return "substring";
    }
    
    private static double getSubstringLength(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 2) return Double.POSITIVE_INFINITY; // defaults till the end of the list
      Datum endIndex = parameters.get(2);
      if(!ListPrimitives.isValidSize(endIndex)) 
        throw new Exceptionf("'(substring <string> <start-index> <optional-length>) 3rd arg %s isn't a non-negative integer!", endIndex.profile());
      return ((Real)endIndex).doubleValue();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 && parameters.size() != 3)
        throw new Exceptionf("'(substring <string> <start-index> <optional-length>) didn't receive 2 or 3 args: %s", Exceptionf.profileArgs(parameters));
      double substringLength = getSubstringLength(parameters);
      Datum str = parameters.get(0);
      Datum startIndex = parameters.get(1);
      if(!(str instanceof escm.type.String)) 
        throw new Exceptionf("'(substring <string> <start-index> <optional-length>) 1st arg %s isn't a string!", str.profile());
      if(!ListPrimitives.isValidSize(startIndex)) 
        throw new Exceptionf("'(substring <string> <start-index> <optional-length>) 2nd %s arg isn't a non-negative integer!", startIndex.profile());
      double startIndexValue = ((Real)startIndex).doubleValue();
      escm.type.String strValue = (escm.type.String)str;
      if(startIndexValue >= strValue.codePointLength() || substringLength == 0) 
        return new escm.type.String("");
      int[] codepoints = strValue.toCodePoints();
      StringBuilder sb = new StringBuilder();
      for(int i = (int)startIndexValue, n = Math.min((int)(substringLength+startIndexValue),codepoints.length); i < n; ++i) {
        sb.append(java.lang.Character.toString(codepoints[i]));
      }
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-upcase
  public static class StringUpcase implements Primitive {
    public java.lang.String escmName() {
      return "string-upcase";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-upcase <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().toUpperCase());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-downcase
  public static class StringDowncase implements Primitive {
    public java.lang.String escmName() {
      return "string-downcase";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-downcase <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().toLowerCase());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-escape
  public static class StringEscape implements Primitive {
    public java.lang.String escmName() {
      return "string-escape";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-escape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.escapeWithCustomUnicodeEscape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-java-escape
  public static class StringJavaEscape implements Primitive {
    public java.lang.String escmName() {
      return "string-java-escape";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-java-escape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.escape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-unescape
  public static class StringUnescape implements Primitive {
    public java.lang.String escmName() {
      return "string-unescape";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-unescape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.unescape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-replace
  public static class StringReplace implements Primitive {
    public java.lang.String escmName() {
      return "string-replace";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3 || !(parameters.get(0) instanceof escm.type.String) || 
                                   !(parameters.get(1) instanceof escm.type.String) || 
                                   !(parameters.get(2) instanceof escm.type.String)) {
        throw new Exceptionf("'(string-replace <string> <regex-string> <replacement-string>) didn't receive exactly 3 strings: %s", Exceptionf.profileArgs(parameters));
      }
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().replaceAll(((escm.type.String)parameters.get(1)).value(),
                                                                                Matcher.quoteReplacement(((escm.type.String)parameters.get(2)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-trim
  public static class StringTrim implements Primitive {
    public java.lang.String escmName() {
      return "string-trim";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-trim <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().trim());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-contains
  public static class StringContains implements Primitive {
    public java.lang.String escmName() {
      return "string-contains";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-contains <string> <substring>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      int result = ((escm.type.String)parameters.get(0)).value().indexOf(((escm.type.String)parameters.get(1)).value());
      if(result < 0) return Boolean.FALSE;
      return new Exact(result);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-contains-right
  public static class StringContainsRight implements Primitive {
    public java.lang.String escmName() {
      return "string-contains-right";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-contains-right <string> <substring>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      int result = ((escm.type.String)parameters.get(0)).value().lastIndexOf(((escm.type.String)parameters.get(1)).value());
      if(result < 0) return Boolean.FALSE;
      return new Exact(result);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-join
  public static class StringJoin implements Primitive {
    public java.lang.String escmName() {
      return "string-join";
    }
    
    private static java.lang.String getJoinerString(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 1) return "";
      Datum joiner = parameters.get(1);
      if(!(joiner instanceof escm.type.String))
        throw new Exceptionf("'(string-join <string-list> <optional-string>) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.String)joiner).value();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(string-join <string-list> <optional-string>) didn't receive exactly 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      StringBuilder sb = new StringBuilder();
      java.lang.String joiner = getJoinerString(parameters);
      Datum iterator = parameters.get(0);
      if(!escm.type.Pair.isList(iterator))
        throw new Exceptionf("'(string-join <string-list> <optional-string>) 1st arg %s isn't a string list!", parameters.get(0).profile());
      while(iterator instanceof escm.type.Pair) {
        escm.type.Pair iteratorPair = (escm.type.Pair)iterator;
        if(!(iteratorPair.car() instanceof escm.type.String))
          throw new Exceptionf("'(string-join <string-list> <optional-string>) 1st arg %s isn't a string list!", parameters.get(0).profile());
        sb.append(((escm.type.String)iteratorPair.car()).value());
        if(!(iteratorPair.cdr() instanceof escm.type.Nil))
          sb.append(joiner);
        iterator = iteratorPair.cdr();
      }
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-split
  public static class StringSplit implements Primitive {
    public java.lang.String escmName() {
      return "string-split";
    }
    
    private static java.lang.String getSplitterString(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 1) return "";
      Datum splitter = parameters.get(1);
      if(!(splitter instanceof escm.type.String))
        throw new Exceptionf("'(string-split <string> <optional-string>) 2nd arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.String)splitter).value();
    }

    private static boolean canUnifyStrings(java.lang.String lhs, java.lang.String rhs) {
      return lhs.length() == 1 && rhs.length() == 1 && java.lang.Character.isHighSurrogate(lhs.charAt(0)) && java.lang.Character.isLowSurrogate(rhs.charAt(0));
    }

    // We want the result of (string-split <str>), if <str> has surrogate pairs, to split
    // the string with those surrogate char pairs preserved in the same string together.
    private static ArrayList<java.lang.String> unifySurrogatePairsIntoOneString(java.lang.String[] splitStrs) {
      ArrayList<java.lang.String> unified = new ArrayList<java.lang.String>();
      for(int i = 0; i < splitStrs.length-1; ++i) {
        if(canUnifyStrings(splitStrs[i],splitStrs[i+1])) {
          unified.add(java.lang.Character.toString(java.lang.Character.toCodePoint(splitStrs[i].charAt(0),splitStrs[i+1].charAt(0))));
          ++i;
        } else {
          unified.add(splitStrs[i]);
        }
      }
      return unified;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if((parameters.size() != 1 && parameters.size() != 2) || !(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(string-split <string> <optional-string>) didn't receive exactly 1 or 2 strings: %s", Exceptionf.profileArgs(parameters));
      ArrayList<java.lang.String> strArray = unifySurrogatePairsIntoOneString(((escm.type.String)parameters.get(0)).value().split(getSplitterString(parameters)));
      Datum strList = escm.type.Nil.VALUE;
      for(int i = strArray.size()-1; i >= 0; --i)
        strList = new escm.type.Pair(new escm.type.String(strArray.get(i)),strList);
      return strList;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string=?
  public static class StringEquals implements Primitive {
    public java.lang.String escmName() {
      return "string=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) != 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string<?
  public static class StringLessThan implements Primitive {
    public java.lang.String escmName() {
      return "string<?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string<? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string<? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string<? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) >= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string>?
  public static class StringGreaterThan implements Primitive {
    public java.lang.String escmName() {
      return "string>?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string>? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string>? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string>? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) <= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string<=?
  public static class StringLessThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return "string<=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string<=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string<=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string<=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) > 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string>=?
  public static class StringGreaterThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return "string>=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string>=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string>=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string>=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) < 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci=?
  public static class StringCiEquals implements Primitive {
    public java.lang.String escmName() {
      return "string-ci=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string-ci=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) != 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci<?
  public static class StringCiLessThan implements Primitive {
    public java.lang.String escmName() {
      return "string-ci<?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string-ci<? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci<? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci<? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) >= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci>?
  public static class StringCiGreaterThan implements Primitive {
    public java.lang.String escmName() {
      return "string-ci>?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string-ci>? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci>? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci>? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) <= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci<=?
  public static class StringCiLessThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return "string-ci<=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string-ci<=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci<=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci<=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) > 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci>=?
  public static class StringCiGreaterThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return "string-ci>=?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(string-ci>=? <string1> <string2> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci>=? <string1> <string2> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci>=? <string1> <string2> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) < 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string?
  public static class IsString implements Primitive {
    public java.lang.String escmName() {
      return "string?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(string? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.String);
    }
  }
}