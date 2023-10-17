// Author: Jordan Randleman - escm.primitive.StringPrimitives
// Purpose:
//    Java primitives for string procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.regex.Matcher;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.util.error.Exceptionf;
import escm.util.string.StringParser;
import escm.util.Trampoline;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.callable.Callable;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.callable.Signature;

public class StringPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // string
  public static class String extends Primitive {
    public java.lang.String escmName() {
      return "string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nCreate a new string by appending each displayed argument together.\n\nRepresents a Java <string> under the hood (hence immutable).\nLiterals are denoted via double-quotes.\n\nStrings support the following control characters:\n  1) \"\\t\": tab,             represented as a char by #\\tab\n  2) \"\\n\": newline,         represented as a char by #\\newline\n  3) \"\\f\": form feed,       represented as a char by #\\page\n  4) \"\\r\": carriage return, represented as a char by #\\return\n  5) \"\\b\": backspace,       represented as a char by #\\backspace\n\nOctal literals may be used by prefixing up to 6 octal digits with \"\\\", ranging from\n\\0-\\177777 (0-65535 in decimal). This range ensures that each value fits neatly\nwithin a single 16bit Java char internally.\n  => Note this extends Java's octals, which only support \\0-\\377 (0-255 in decimal).\n\nJava 16bit unicode literals may be used by prefixing up to 4 hex digits with \"\\u\".\n  => Adjacent unicode literals may be used to create \"surrogate pairs\" that render\n     as a single unicode image for unicode values that require 32bit encoding.\n\nEScheme also extends Java unicode literals with syntax for 32bit unicode values.\nPrefixing up to 8 hex digits with \"\\U\" compiles to 2 seperate \"\\u\" instances.\n  => For example, both \"\\U1f608\" and \"\\ud83d\\ude08\" create the same string, but the\n     former is easier to write out after referencing the \"U+\" code from the internet.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(Datum p : parameters) sb.append(p.display());
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-java-length
  public static class StringJavaLength extends Primitive {
    public java.lang.String escmName() {
      return "string-java-length";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-java-length"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns the length of <string>, with surrogate pairs counting as 2 chars.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-java-length <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.String)parameters.get(0)).value().length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-upcase
  public static class StringUpcase extends Primitive {
    public java.lang.String escmName() {
      return "string-upcase";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-upcase"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns <string> entirely upper-cased.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-upcase <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().toUpperCase());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-downcase
  public static class StringDowncase extends Primitive {
    public java.lang.String escmName() {
      return "string-downcase";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-downcase"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns <string> entirely lower-cased.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-downcase <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().toLowerCase());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-escape
  public static class StringEscape extends Primitive {
    public java.lang.String escmName() {
      return "string-escape";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-escape"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns <string> with special characters escaped (like when printing via <write>).\nNote that this escapes surrogate pairs using EScheme's custom \"\\U\" syntax.\n<string-java-escape> should be used to escape such with 2 \"\\u\" instances.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-escape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.escapeWithCustomUnicodeEscape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-java-escape
  public static class StringJavaEscape extends Primitive {
    public java.lang.String escmName() {
      return "string-java-escape";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-java-escape"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns <string> with special characters escaped (like when printing via <write>).\nNote that this escapes surrogate pairs using 2 \"\\u\" instances.\n<string-escape> should be used to escape such with EScheme's custom \"\\U\" syntax.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-java-escape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.escape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-unescape
  public static class StringUnescape extends Primitive {
    public java.lang.String escmName() {
      return "string-unescape";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-unescape"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns <string> with special characters unescaped (like when printing via\n<display>). Note that this also unescapes EScheme's custom \"\\U\" syntax.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-unescape <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(StringParser.unescape(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-replace
  public static class StringReplace extends Primitive {
    public java.lang.String escmName() {
      return "string-replace";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-replace"),new Symbol("<string>"),new Symbol("<regex-string>"),new Symbol("<replacement-string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReplaces all instances of <regex-string> in <string> with <replacement-string>.";
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
  public static class StringTrim extends Primitive {
    public java.lang.String escmName() {
      return "string-trim";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-trim"),new Symbol("<string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns a string with the whitespace removed from both ends of <string>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-trim <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.String)parameters.get(0)).value().trim());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-contains
  public static class StringContains extends Primitive {
    public java.lang.String escmName() {
      return "string-contains";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-contains"),new Symbol("<string>"),new Symbol("<substring>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns the first index of <substring> in <string> if present, or #f if it isn't.";
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
  public static class StringContainsRight extends Primitive {
    public java.lang.String escmName() {
      return "string-contains-right";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-contains-right"),new Symbol("<string>"),new Symbol("<substring>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns the last index of <substring> in <string> if present, or #f if it isn't.";
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
  // string-prefix?
  public static class StringPrefixP extends Primitive {
    public java.lang.String escmName() {
      return "string-prefix?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-prefix?"),new Symbol("<string>"),new Symbol("<prefix-string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether <string> starts with <prefix-string>. Also see <string-suffix?>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-prefix? <string> <prefix-string>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.String)parameters.get(0)).value().startsWith(((escm.type.String)parameters.get(1)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-suffix?
  public static class StringSuffixP extends Primitive {
    public java.lang.String escmName() {
      return "string-suffix?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-suffix?"),new Symbol("<string>"),new Symbol("<suffix-string>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether <string> ends with <suffix-string>. Also see <string-prefix?>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(string-suffix? <string> <suffix-string>) didn't receive exactly 2 strings: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.String)parameters.get(0)).value().endsWith(((escm.type.String)parameters.get(1)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-join
  public static class StringJoin extends Primitive {
    public java.lang.String escmName() {
      return "string-join";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("string-join"),new Symbol("<string-list>")),
        Pair.List(new Symbol("string-join"),new Symbol("<string-list>"),new Symbol("<joiner-string>")));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns a string made from joining the strings in <string-list> by splicing\n<joiner-string> (defaults to \"\") between each item.";
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
  public static class StringSplit extends Primitive {
    public java.lang.String escmName() {
      return "string-split";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("string-split"),new Symbol("<string>")),
        Pair.List(new Symbol("string-split"),new Symbol("<string>"),new Symbol("<splitter-regex-string>")));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns a list of strings made from splitting <string> at each\n<splitter-regex> instance. Defaults to splitting into characters.";
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

    // We want the result of (string-split <string>), if <string> has surrogate pairs, to split
    // the string with those surrogate char pairs preserved in the same string together.
    private static ArrayList<java.lang.String> unifySurrogatePairsIntoOneString(java.lang.String[] splitStrs) {
      ArrayList<java.lang.String> unified = new ArrayList<java.lang.String>();
      for(int i = 0; i < splitStrs.length; ++i) {
        if(i+1 < splitStrs.length && canUnifyStrings(splitStrs[i],splitStrs[i+1])) {
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
  // string-unfold
  public static class StringUnfold extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "string-unfold";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-unfold"),new Symbol("<break?-callable>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nUnfolds a string from left to right, starting with <seed>. <break?-condition>\ndetermines when unfolding stops, <mapper-callable> maps the <seed> to a value\nin the unfolded string, and <update-callable> increments <seed> for the\nnext round of unfolding.\n\nNote that the result of <mapper-callable> must always be a character.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(string-unfold <break-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(string-unfold <break-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(string-unfold <break-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(string-unfold <break-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return ListPrimitives.Unfold.logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),(resultList) -> () -> {
        return continuation.run(((AssociativeCollection)resultList).toACString());
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-unfold-right
  public static class StringUnfoldRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "string-unfold-right";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-unfold"),new Symbol("<break?-callable>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nUnfolds a string from right to left, starting with <seed>. <break?-condition>\ndetermines when unfolding stops, <mapper-callable> maps the <seed> to a value\nin the unfolded string, and <update-callable> increments <seed> for the\nnext round of unfolding.\n\nNote that the result of <mapper-callable> must always be a character.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(string-unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return ListPrimitives.UnfoldRight.logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),(resultList) -> () -> {
        return continuation.run(((AssociativeCollection)resultList).toACString());
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string=?
  public static class StringEquals extends Primitive {
    public java.lang.String escmName() {
      return "string=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are equal to one another (case-sensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) != 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string<?
  public static class StringLessThan extends Primitive {
    public java.lang.String escmName() {
      return "string<?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string<?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are < one another (case-sensitive).";
    }

    // @PRECONDITION: <parameters.size() >= 1>
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string<? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string<? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) >= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string<? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string>?
  public static class StringGreaterThan extends Primitive {
    public java.lang.String escmName() {
      return "string>?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string>?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are > one another (case-sensitive).";
    }

    // @PRECONDITION: <parameters.size() >= 1>
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string>? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string>? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) <= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string>? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string<=?
  public static class StringLessThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return "string<=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string<=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are <= one another (case-sensitive).";
    }

    // @PRECONDITION: <parameters.size() >= 1>
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string<=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string<=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) > 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string<=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string>=?
  public static class StringGreaterThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return "string>=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string>=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are >= one another (case-sensitive).";
    }

    // @PRECONDITION: <parameters.size() >= 1>
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string>=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string>=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareTo(strValue) < 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string>=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci=?
  public static class StringCiEquals extends Primitive {
    public java.lang.String escmName() {
      return "string-ci=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-ci=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are equal to one another (case-insensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string-ci=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) != 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci<?
  public static class StringCiLessThan extends Primitive {
    public java.lang.String escmName() {
      return "string-ci<?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-ci<?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are < one another (case-insensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string-ci<? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci<? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci<? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) >= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci>?
  public static class StringCiGreaterThan extends Primitive {
    public java.lang.String escmName() {
      return "string-ci>?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-ci>?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are > one another (case-insensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string-ci>? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci>? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci>? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) <= 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci<=?
  public static class StringCiLessThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return "string-ci<=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-ci<=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are <= one another (case-insensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string-ci<=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci<=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci<=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) > 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string-ci>=?
  public static class StringCiGreaterThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return "string-ci>=?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string-ci>=?"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether \"<string> <string> ...\" are >= one another (case-insensitive).";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(string-ci>=? <string> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum p = parameters.get(0);
      if(!(p instanceof escm.type.String))
        throw new Exceptionf("'(string-ci>=? <string> ...) invalid non-string arg %s recieved!", p.profile());
      java.lang.String lastValue = ((escm.type.String)p).value();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum str = parameters.get(i);
        if(!(str instanceof escm.type.String))
          throw new Exceptionf("'(string-ci>=? <string> ...) invalid non-string arg %s recieved!", str.profile());
        java.lang.String strValue = ((escm.type.String)str).value();
        if(lastValue.compareToIgnoreCase(strValue) < 0) return Boolean.FALSE;
        lastValue = strValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string?
  public static class IsString extends Primitive {
    public java.lang.String escmName() {
      return "string?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string?"),new Symbol("<obj>"));
    }

    public java.lang.String docstring() {
      return "@help:Procedures:Strings\nReturns whether <obj> is a string.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(string? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.String);
    }
  }
}