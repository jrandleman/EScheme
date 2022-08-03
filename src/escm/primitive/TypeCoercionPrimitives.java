// Author: Jordan Randleman - escm.primitive.TypeCoercionPrimitives
// Purpose:
//    Java primitives for type coercion procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Nil;
import escm.type.number.Number;
import escm.type.number.Real;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class TypeCoercionPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // string->number
  public static class StringToNumber implements Primitive {
    public java.lang.String escmName() {
      return "string->number";
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
        return escm.type.Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // number->string
  public static class NumberToString implements Primitive {
    public java.lang.String escmName() {
      return "number->string";
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
        return escm.type.Boolean.FALSE;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // keyword->symbol
  public static class KeywordToSymbol implements Primitive {
    public java.lang.String escmName() {
      return "keyword->symbol";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Keyword)) 
        throw new Exceptionf("'(keyword->symbol <keyword>) expects exactly 1 keyword arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Symbol(((escm.type.Keyword)parameters.get(0)).value().substring(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symbol->keyword
  public static class SymbolToKeyword implements Primitive {
    public java.lang.String escmName() {
      return "symbol->keyword";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Symbol)) 
        throw new Exceptionf("'(symbol->keyword <symbol>) expects exactly 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Keyword(((escm.type.Symbol)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->symbol
  public static class StringToSymbol implements Primitive {
    public java.lang.String escmName() {
      return "string->symbol";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->symbol <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Symbol(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // symbol->string
  public static class SymbolToString implements Primitive {
    public java.lang.String escmName() {
      return "symbol->string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Symbol)) 
        throw new Exceptionf("'(symbol->string <symbol>) expects exactly 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.Symbol)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // string->keyword
  public static class StringToKeyword implements Primitive {
    public java.lang.String escmName() {
      return "string->keyword";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(string->keyword <string>) expects exactly 1 string arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.Keyword(((escm.type.String)parameters.get(0)).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // keyword->string
  public static class KeywordToString implements Primitive {
    public java.lang.String escmName() {
      return "keyword->string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Keyword)) 
        throw new Exceptionf("'(keyword->string <keyword>) expects exactly 1 keyword arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(((escm.type.Keyword)parameters.get(0)).value().substring(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // write-to-string
  public static class WriteToString implements Primitive {
    public java.lang.String escmName() {
      return "write-to-string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(write-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).write());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // display-to-string
  public static class DisplayToString implements Primitive {
    public java.lang.String escmName() {
      return "display-to-string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(display-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).display());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // pretty-print-to-string
  public static class PrettyPrintToString implements Primitive {
    public java.lang.String escmName() {
      return "pretty-print-to-string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(pretty-print-to-string <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(parameters.get(0).pprint());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // vector->list
  public static class VectorToList implements Primitive {
    public java.lang.String escmName() {
      return "vector->list";
    }

    public static Datum logic(escm.type.Vector v) {
      synchronized(v) {
        try {
          Datum lis = Nil.VALUE;
          for(int i = v.size()-1; i >= 0; --i)
            lis = new escm.type.Pair(v.get(i),lis);
          return lis;  
        } catch(Exception e) {
          return Nil.VALUE; // never triggered since we know <v.get(i)> won't throw.
        }
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Vector)) 
        throw new Exceptionf("'(vector->list <vector>) expects exactly 1 vector arg: %s", Exceptionf.profileArgs(parameters));
      return logic((escm.type.Vector)parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list->vector
  public static class ListToVector implements Primitive {
    public java.lang.String escmName() {
      return "list->vector";
    }

    public static escm.type.Vector logic(Datum l) {
      ArrayList<Datum> v = new ArrayList<Datum>();
      while(l instanceof escm.type.Pair) {
        escm.type.Pair p = (escm.type.Pair)l;
        v.add(p.car());
        l = p.cdr();
      }
      return new escm.type.Vector(v);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || (!(parameters.get(0) instanceof escm.type.Pair) && !(parameters.get(0) instanceof Nil))) 
        throw new Exceptionf("'(list->vector <list>) expects exactly 1 list arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }
}