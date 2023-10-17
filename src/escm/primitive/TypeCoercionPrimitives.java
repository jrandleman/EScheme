// Author: Jordan Randleman - escm.primitive.TypeCoercionPrimitives
// Purpose:
//    Java primitives for type coercion procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.Nil;
import escm.type.number.Number;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class TypeCoercionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // string->number
  public static class StringToNumber extends Primitive {
    public java.lang.String escmName() {
      return "string->number";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("string->number"),new Symbol("<string>")),
        Pair.List(new Symbol("string->number"),new Symbol("<string>"),new Symbol("<radix>")));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a string to a number in base <radix> (defaults to 10).\n<radix> must be between <*min-radix*> and <*max-radix*>.\nReturns #f if failed coercion.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 2) 
        throw new Exceptionf("'(string->number <string> <optional-radix>) didn't receive 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum str = parameters.get(0);
      if(!(str instanceof escm.type.String))
        throw new Exceptionf("'(string->number <string> <optional-radix>) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      int radix = -1;
      if(parameters.size() == 2) {
        Datum radixDatum = parameters.get(1);
        if(!(radixDatum instanceof Real) || !((Real)radixDatum).isInteger())
          throw new Exceptionf("'(string->number <string> <optional-radix>) invalid radix (only %d-%d): %s", Number.MIN_RADIX, Number.MAX_RADIX, Exceptionf.profileArgs(parameters));
        radix = ((Real)radixDatum).intValue();
        if(radix < Number.MIN_RADIX || radix > Number.MAX_RADIX)
          throw new Exceptionf("'(string->number <string> <optional-radix>) invalid radix (only %d-%d): %s", Number.MIN_RADIX, Number.MAX_RADIX, Exceptionf.profileArgs(parameters));
      }
      try {
        if(radix == -1) {
          return Number.valueOf(((escm.type.String)str).value());
        } else {
          return Number.valueOf(((escm.type.String)str).value(),radix);
        }
      } catch(Exception e) {
        return escm.type.bool.Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // number->string
  public static class NumberToString extends Primitive {
    public java.lang.String escmName() {
      return "number->string";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("number->string"),new Symbol("<number>")),
        Pair.List(new Symbol("number->string"),new Symbol("<number>"),new Symbol("<radix>")));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a number to a string in base <radix> (defaults to 10).\n<radix> must be between <*min-radix*> and <*max-radix*>.\nReturns #f if failed coercion.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 2) 
        throw new Exceptionf("'(number->string <number> <optional-radix>) didn't receive 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum num = parameters.get(0);
      if(!(num instanceof Number))
        throw new Exceptionf("'(number->string <number> <optional-radix>) 1st arg isn't a number: %s", Exceptionf.profileArgs(parameters));
      int radix = -1;
      if(parameters.size() == 2) {
        Datum radixDatum = parameters.get(1);
        if(!(radixDatum instanceof Real) || !((Real)radixDatum).isInteger())
          throw new Exceptionf("'(number->string <number> <optional-radix>) invalid radix (only %d-%d): %s", Number.MIN_RADIX, Number.MAX_RADIX, Exceptionf.profileArgs(parameters));
        radix = ((Real)radixDatum).intValue();
        if(radix < Number.MIN_RADIX || radix > Number.MAX_RADIX)
          throw new Exceptionf("'(number->string <number> <optional-radix>) invalid radix (only %d-%d): %s", Number.MIN_RADIX, Number.MAX_RADIX, Exceptionf.profileArgs(parameters));
      }
      try {
        if(radix == -1) {
          return new escm.type.String(((Number)num).toString());
        } else {
          return new escm.type.String(((Number)num).toString(radix));
        }
      } catch(Exception e) {
        return escm.type.bool.Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // keyword->symbol
  public static class KeywordToSymbol extends Primitive {
    public java.lang.String escmName() {
      return "keyword->symbol";
    }

    public Datum signature() {
      return Pair.List(new Symbol("keyword->symbol"),new Symbol("<keyword>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a keyword to a symbol.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Keyword)) 
        throw new Exceptionf("'(keyword->symbol <keyword>) expects exactly 1 keyword arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Symbol(((escm.type.Keyword)parameters.get(0)).value().substring(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symbol->keyword
  public static class SymbolToKeyword extends Primitive {
    public java.lang.String escmName() {
      return "symbol->keyword";
    }

    public Datum signature() {
      return Pair.List(new Symbol("symbol->keyword"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a symbol to a keyword.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Symbol)) 
        throw new Exceptionf("'(symbol->keyword <symbol>) expects exactly 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Keyword(((escm.type.Symbol)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->symbol
  public static class StringToSymbol extends Primitive {
    public java.lang.String escmName() {
      return "string->symbol";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string->symbol"),new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a string to a symbol.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->symbol <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Symbol(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symbol->string
  public static class SymbolToString extends Primitive {
    public java.lang.String escmName() {
      return "symbol->string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("symbol->string"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a symbol to a string.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Symbol)) 
        throw new Exceptionf("'(symbol->string <symbol>) expects exactly 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.Symbol)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->keyword
  public static class StringToKeyword extends Primitive {
    public java.lang.String escmName() {
      return "string->keyword";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string->keyword"),new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a string to a keyword.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->keyword <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Keyword(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // keyword->string
  public static class KeywordToString extends Primitive {
    public java.lang.String escmName() {
      return "keyword->string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("keyword->string"),new Symbol("<keyword>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a keyword to a string.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Keyword)) 
        throw new Exceptionf("'(keyword->string <keyword>) expects exactly 1 keyword arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.Keyword)parameters.get(0)).value().substring(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // write-to-string
  public static class WriteToString extends Primitive {
    public java.lang.String escmName() {
      return "write-to-string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("write-to-string"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nWrite <obj> to a string in machine-readable form.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(write-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).write());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // display-to-string
  public static class DisplayToString extends Primitive {
    public java.lang.String escmName() {
      return "display-to-string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("display-to-string"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nWrite <obj> to a string in human-readable form.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(display-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).display());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // pretty-print-to-string
  public static class PrettyPrintToString extends Primitive {
    public java.lang.String escmName() {
      return "pretty-print-to-string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("pretty-print-to-string"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nWrite <obj> to a string in indented, machine-readable form.\nAliased by <pprint-to-string>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(pretty-print-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).pprint());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector->list
  public static class VectorToList extends Primitive {
    public java.lang.String escmName() {
      return "vector->list";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector->list"),new Symbol("<vector>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a vector to a list.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector)) 
        throw new Exceptionf("'(vector->list <vector>) expects exactly 1 vector arg: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).toList();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list->vector
  public static class ListToVector extends Primitive {
    public java.lang.String escmName() {
      return "list->vector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list->vector"),new Symbol("<list>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a list to a vector.";
    }

    public static escm.type.Vector logic(Datum l) {
      escm.type.Vector v = new escm.type.Vector();
      while(l instanceof escm.type.Pair) {
        escm.type.Pair p = (escm.type.Pair)l;
        v.push(p.car());
        l = p.cdr();
      }
      return v;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || (!(parameters.get(0) instanceof escm.type.Pair) && !(parameters.get(0) instanceof Nil))) 
        throw new Exceptionf("'(list->vector <list>) expects exactly 1 list arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // hashmap->list
  public static class HashmapToList extends Primitive {
    public java.lang.String escmName() {
      return "hashmap->list";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap->list"),new Symbol("<hashmap>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a hashmap to a list of keys & values.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Hashmap)) 
        throw new Exceptionf("'(hashmap->list <hashmap>) expects exactly 1 hashmap arg: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Hashmap)parameters.get(0)).toList();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // hashmap->vector
  public static class HashmapToVector extends Primitive {
    public java.lang.String escmName() {
      return "hashmap->vector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap->vector"),new Symbol("<hashmap>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a hashmap to a vector of keys & values.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Hashmap)) 
        throw new Exceptionf("'(hashmap->vector <hashmap>) expects exactly 1 hashmap arg: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Hashmap)parameters.get(0)).toVector();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list->hashmap
  public static class ListToHashmap extends Primitive {
    public java.lang.String escmName() {
      return "list->hashmap";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list->hashmap"),new Symbol("<list>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a list of keys & values to a hashmap.";
    }

    private static escm.type.Hashmap logic(Datum lst, ArrayList<Datum> parameters) throws Exception {
      escm.type.Hashmap h = new escm.type.Hashmap();
      while(lst instanceof escm.type.Pair) {
        escm.type.Pair p = (escm.type.Pair)lst;
        if(!(p.cdr() instanceof escm.type.Pair))
          throw new Exceptionf("'(list->hashmap <list>) <list> doesn't have an even number of items: %s", Exceptionf.profileArgs(parameters));
        escm.type.Pair cdr = (escm.type.Pair)p.cdr();
        h.set(p.car(),cdr.car());
        lst = cdr.cdr();
      }
      return h;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || (!(parameters.get(0) instanceof escm.type.Pair) && !(parameters.get(0) instanceof Nil))) 
        throw new Exceptionf("'(list->hashmap <list>) expects exactly 1 list arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector->hashmap
  public static class VectorToHashmap extends Primitive {
    public java.lang.String escmName() {
      return "vector->hashmap";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector->hashmap"),new Symbol("<vector>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a vector of keys & values to a hashmap.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector)) 
        throw new Exceptionf("'(vector->hashmap <vector>) expects exactly 1 vector arg: %s", Exceptionf.profileArgs(parameters));
      escm.type.Vector v = (escm.type.Vector)parameters.get(0);
      if(v.size() % 2 != 0)
        throw new Exceptionf("'(vector->hashmap <vector>) <vector> doesn't have an even number of items: %s", Exceptionf.profileArgs(parameters));
      return v.toHashmap();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // char->integer
  public static class CharToInteger extends Primitive {
    public java.lang.String escmName() {
      return "char->integer";
    }

    public Datum signature() {
      return Pair.List(new Symbol("char->integer"),new Symbol("<character>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a character to an integer codepoint value.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Character)) 
        throw new Exceptionf("'(char->integer <char>) didn't receive exactly 1 char: %s", Exceptionf.profileArgs(parameters));
      return new Exact(((escm.type.Character)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // integer->char
  public static class IntegerToChar extends Primitive {
    public java.lang.String escmName() {
      return "integer->char";
    }

    public Datum signature() {
      return Pair.List(new Symbol("integer->char"),new Symbol("<integer>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert an integer codepoint value to a character.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(integer->char <integer>) didn't receive exactly 1 integer: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real) || !((Real)n).isInteger())
        throw new Exceptionf("'(integer->char <integer>) didn't receive exactly 1 integer: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Character(((Real)n).intValue());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list->string
  public static class ListToString extends Primitive {
    public java.lang.String escmName() {
      return "list->string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list->string"),new Symbol("<list>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a list of chars to a string.";
    }

    private escm.type.String logic(Datum lis, ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      while(lis instanceof escm.type.Pair) {
        escm.type.Pair p = (escm.type.Pair)lis;
        Datum item = p.car();
        if(!(item instanceof escm.type.Character))
          throw new Exceptionf("'(list->string <list>) list item %s isn't a character: %s", item.profile(), Exceptionf.profileArgs(parameters));
        sb.append(item.display());
        lis = p.cdr();
      }
      return new escm.type.String(sb.toString());
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || (!(parameters.get(0) instanceof escm.type.Pair) && !(parameters.get(0) instanceof Nil))) 
        throw new Exceptionf("'(list->string <list>) didn't receive exactly 1 list: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->list
  public static class StringToList extends Primitive {
    public java.lang.String escmName() {
      return "string->list";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string->list"),new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a string to a list of chars.";
    }

    private Datum logic(escm.type.String str, ArrayList<Datum> parameters) throws Exception {
      Datum lis = Nil.VALUE;
      escm.type.Character[] chars = str.toChars();
      for(int i = chars.length-1; i >= 0; --i) {
        lis = new escm.type.Pair(chars[i],lis);
      }
      return lis;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->list <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return logic((escm.type.String)parameters.get(0),parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector->string
  public static class VectorToString extends Primitive {
    public java.lang.String escmName() {
      return "vector->string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("vector->string"),new Symbol("<vector>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a vector of chars to a string.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector)) 
        throw new Exceptionf("'(vector->string <vector>) didn't receive exactly 1 vector: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Vector)parameters.get(0)).toEscmString();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->vector
  public static class StringToVector extends Primitive {
    public java.lang.String escmName() {
      return "string->vector";
    }

    public Datum signature() {
      return Pair.List(new Symbol("string->vector"),new Symbol("<string>"));
    }

    public String docstring() {
      return "@help:Procedures:Type-Coercions\nConvert a string to a vector of chars.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->vector <string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Vector(((escm.type.String)parameters.get(0)).toChars());
    }
  }
}