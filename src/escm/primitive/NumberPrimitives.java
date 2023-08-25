// Author: Jordan Randleman - escm.primitive.NumberPrimitives
// Purpose:
//    Java primitives for number procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.Random;
import escm.type.Datum;
import escm.type.Nil;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Number;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.number.Inexact;
import escm.type.number.Complex;
import escm.util.error.Exceptionf;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.callable.Signature;
import escm.vm.runtime.GlobalState;
import escm.primitive.FunctionalPrimitives;
import escm.primitive.AssociativeCollectionPrimitives;

public class NumberPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // + (aliases <append> and <bind> too)
  public static class Plus extends Primitive {
    public java.lang.String escmName() {
      return "+";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("+")),
        Pair.List(new Symbol("+"),new Symbol("<obj>")),
        Pair.List(new Symbol("+"),new Symbol("<symbol>"),Signature.VARIADIC),
        Pair.List(new Symbol("+"),new Symbol("<keyword>"),Signature.VARIADIC),
        Pair.List(new Symbol("+"),new Symbol("<associative-collection>"),Signature.VARIADIC),
        Pair.List(new Symbol("+"),new Symbol("<callable>"),new Symbol("<arg>"),Signature.VARIADIC),
        Pair.List(new Symbol("+"),new Symbol("<number>"),Signature.VARIADIC));
    }

    private static Datum logic(ArrayList<Datum> parameters) throws Exception {
      Number sum = new Exact();
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(+ <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        sum = sum.add((Number)p);
      }
      return sum;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      // alias <append> for no/single args
      int n = parameters.size();
      if(n < 2) return n == 0 ? Nil.VALUE : parameters.get(0);
      // Add numbers
      Datum firstparam = parameters.get(0);
      if(firstparam instanceof Number) 
        return logic(parameters);
      // Append values
      if(AssociativeCollectionPrimitives.Append.isAppendable(firstparam))
        return AssociativeCollectionPrimitives.Append.logic(parameters);
      // Bind procedures
      if(firstparam instanceof Callable) 
        return FunctionalPrimitives.Bind.logic(parameters);
      throw new Exceptionf("'(+ <number> ...) invalid non-numeric arg %s recieved!", firstparam.profile());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // -
  public static class Minus extends Primitive {
    public java.lang.String escmName() {
      return "-";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("-"),new Symbol("<number>")),
        Pair.List(new Symbol("-"),new Symbol("<number>"),new Symbol("<number>"),Signature.VARIADIC));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(- <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum firstparam = parameters.get(0);
      if(!(firstparam instanceof Number))
        throw new Exceptionf("'(- <number> ...) invalid non-numeric arg %s recieved!", firstparam.profile());
      if(parameters.size() == 1) {
        if(firstparam instanceof Real) return ((Real)firstparam).negate();
        return (new Complex()).sub((Complex)firstparam);
      }
      Number diff = (Number)firstparam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(- <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        diff = diff.sub((Number)p);
      }
      return diff;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // *
  public static class Multiply extends Primitive {
    public java.lang.String escmName() {
      return "*";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("*"),new Symbol("<callable>"),Signature.VARIADIC),
        Pair.List(new Symbol("*"),new Symbol("<number>"),Signature.VARIADIC));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(* <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.get(0) instanceof Callable) return FunctionalPrimitives.Compose.logic(parameters);
      Number product = new Exact(1);
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(* <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        product = product.mul((Number)p);
      }
      return product;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // /
  public static class Divide extends Primitive {
    public java.lang.String escmName() {
      return "/";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("/"),new Symbol("<number>")),
        Pair.List(new Symbol("/"),new Symbol("<number>"),new Symbol("<number>"),Signature.VARIADIC));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(/ <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum firstparam = parameters.get(0);
      if(!(firstparam instanceof Number))
        throw new Exceptionf("'(/ <number> ...) invalid non-numeric arg %s recieved!", firstparam.profile());
      if(parameters.size() == 1)
        return (new Exact(1)).div((Number)firstparam);
      Number div = (Number)firstparam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(/ <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        div = div.div((Number)p);
      }
      return div;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // =
  public static class Equals extends Primitive {
    public java.lang.String escmName() {
      return "=";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("="),new Symbol("<number>"),new Symbol("<number>"),Signature.VARIADIC),
        Pair.List(new Symbol("="),new Symbol("<obj>"),new Symbol("<obj>"),Signature.VARIADIC));
    }

    public static Datum logic(Number firstValue, ArrayList<Datum> parameters) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(= <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        if(firstValue.eqs((Number)p) == false) return Boolean.FALSE;
      }
      return Boolean.TRUE;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(= <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(firstParam instanceof Number) return logic((Number)firstParam,parameters);
      return EqualityPrimitives.IsEq.logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // <
  public static class LessThan extends Primitive {
    public java.lang.String escmName() {
      return "<";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("<"),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC),
        Pair.List(new Symbol("<"),new Symbol("<string>"),new Symbol("<string>"),Signature.VARIADIC),
        Pair.List(new Symbol("<"),new Symbol("<char>"),new Symbol("<char>"),Signature.VARIADIC));
    }

    public static Datum logic(Real lastValue, ArrayList<Datum> parameters) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(< <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(!lastValue.lt(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(< <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(firstParam instanceof Real) {
        return logic((Real)firstParam,parameters);
      } else if(firstParam instanceof escm.type.String) {
        return StringPrimitives.StringLessThan.logic(parameters);
      } else if(firstParam instanceof escm.type.Character) {
        return CharacterPrimitives.IsCharLessThan.logic(parameters);
      } else {
        throw new Exceptionf("'(< <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // >
  public static class GreaterThan extends Primitive {
    public java.lang.String escmName() {
      return ">";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol(">"),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC),
        Pair.List(new Symbol(">"),new Symbol("<string>"),new Symbol("<string>"),Signature.VARIADIC),
        Pair.List(new Symbol(">"),new Symbol("<char>"),new Symbol("<char>"),Signature.VARIADIC));
    }

    public static Datum logic(Real lastValue, ArrayList<Datum> parameters) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(> <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(!lastValue.gt(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(> <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(firstParam instanceof Real) {
        return logic((Real)firstParam,parameters);
      } else if(firstParam instanceof escm.type.String) {
        return StringPrimitives.StringGreaterThan.logic(parameters);
      } else if(firstParam instanceof escm.type.Character) {
        return CharacterPrimitives.IsCharGreaterThan.logic(parameters);
      } else {
        throw new Exceptionf("'(> <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // <=
  public static class LessThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return "<=";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("<="),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC),
        Pair.List(new Symbol("<="),new Symbol("<string>"),new Symbol("<string>"),Signature.VARIADIC),
        Pair.List(new Symbol("<="),new Symbol("<char>"),new Symbol("<char>"),Signature.VARIADIC));
    }

    public static Datum logic(Real lastValue, ArrayList<Datum> parameters) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(<= <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(!lastValue.lte(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(<= <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(firstParam instanceof Real) {
        return logic((Real)firstParam,parameters);
      } else if(firstParam instanceof escm.type.String) {
        return StringPrimitives.StringLessThanOrEqualTo.logic(parameters);
      } else if(firstParam instanceof escm.type.Character) {
        return CharacterPrimitives.IsCharLessThanOrEqual.logic(parameters);
      } else {
        throw new Exceptionf("'(<= <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // >=
  public static class GreaterThanOrEqualTo extends Primitive {
    public java.lang.String escmName() {
      return ">=";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol(">="),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC),
        Pair.List(new Symbol(">="),new Symbol("<string>"),new Symbol("<string>"),Signature.VARIADIC),
        Pair.List(new Symbol(">="),new Symbol("<char>"),new Symbol("<char>"),Signature.VARIADIC));
    }

    public static Datum logic(Real lastValue, ArrayList<Datum> parameters) throws Exception {
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(>= <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(!lastValue.gte(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(>= <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(firstParam instanceof Real) {
        return logic((Real)firstParam,parameters);
      } else if(firstParam instanceof escm.type.String) {
        return StringPrimitives.StringGreaterThanOrEqualTo.logic(parameters);
      } else if(firstParam instanceof escm.type.Character) {
        return CharacterPrimitives.IsCharGreaterThanOrEqual.logic(parameters);
      } else {
        throw new Exceptionf("'(>= <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // expt
  public static class Expt extends Primitive {
    public java.lang.String escmName() {
      return "expt";
    }

    public Datum signature() {
      return Pair.List(new Symbol("expt"),new Symbol("<number>"),new Symbol("<number>"),Signature.VARIADIC);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(expt <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstparam = parameters.get(parameters.size()-1);
      if(!(firstparam instanceof Number))
        throw new Exceptionf("'(expt <number> <number> ...) invalid non-numeric arg %s recieved!", firstparam.profile());
      Number powVal = (Number)firstparam;
      for(int i = parameters.size()-2; i >= 0; --i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(expt <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        powVal = ((Number)p).expt(powVal);
      }
      return powVal;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // exp
  public static class Exp extends Primitive {
    public java.lang.String escmName() {
      return "exp";
    }

    public Datum signature() {
      return Pair.List(new Symbol("exp"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(exp <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(exp <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).exp();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // log
  public static class Log extends Primitive {
    public java.lang.String escmName() {
      return "log";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("log"),new Symbol("<number>")),
        Pair.List(new Symbol("log"),new Symbol("<number>"),new Symbol("<log-base-number>")));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1 || parameters.size() > 2) 
        throw new Exceptionf("'(log <number> <optional-base>) expects 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(log <number> <optional-base>) invalid non-numeric arg %s recieved!", n.profile());
      if(parameters.size() == 1) return ((Number)n).log();
      Datum base = parameters.get(1);
      if(!(base instanceof Number))
        throw new Exceptionf("'(log <number> <optional-base>) invalid non-numeric arg %s recieved!", base.profile());
      if(base instanceof Real && ((Real)base).isZero()) return Inexact.NAN;
      return ((Number)n).log().div(((Number)base).log());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sqrt
  public static class Sqrt extends Primitive {
    public java.lang.String escmName() {
      return "sqrt";
    }

    public Datum signature() {
      return Pair.List(new Symbol("sqrt"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(sqrt <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(sqrt <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).sqrt();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // abs
  public static class Abs extends Primitive {
    public java.lang.String escmName() {
      return "abs";
    }

    public Datum signature() {
      return Pair.List(new Symbol("abs"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(abs <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(abs <real>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Real)n).abs();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // expt-mod
  public static class ExptMod extends Primitive {
    public java.lang.String escmName() {
      return "expt-mod";
    }

    public Datum signature() {
      return Pair.List(new Symbol("expt-mod"),new Symbol("<base-real>"),new Symbol("<power-real>"),new Symbol("<mod-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3) 
        throw new Exceptionf("'(expt-mod <base-real> <power-real> <mod-real>) expects exactly 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum base = parameters.get(0);
      Datum power = parameters.get(1);
      Datum mod = parameters.get(2);
      if(!(base instanceof Real))
        throw new Exceptionf("'(expt-mod <base-real> <power-real> <mod-real>) invalid non-real <base>: %s", Exceptionf.profileArgs(parameters));
      if(!(power instanceof Real))
        throw new Exceptionf("'(expt-mod <base-real> <power-real> <mod-real>) invalid non-real <power>: %s", Exceptionf.profileArgs(parameters));
      if(!(mod instanceof Real))
        throw new Exceptionf("'(expt-mod <base-real> <power-real> <mod-real>) invalid non-real <mod>: %s", Exceptionf.profileArgs(parameters));
      return ((Real)base).exptMod((Real)power,(Real)mod);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // min
  public static class Min extends Primitive {
    public java.lang.String escmName() {
      return "min";
    }

    public Datum signature() {
      return Pair.List(new Symbol("min"),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(min <real> <real> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Real min = Inexact.POSITIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Real))
          throw new Exceptionf("'(min <real> <real> ...) invalid non-numeric arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(pValue.isNaN() || (!min.isNaN() && min.gt(pValue))) min = pValue;
      }
      return min;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // max
  public static class Max extends Primitive {
    public java.lang.String escmName() {
      return "max";
    }

    public Datum signature() {
      return Pair.List(new Symbol("max"),new Symbol("<real>"),new Symbol("<real>"),Signature.VARIADIC);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(max <real> <real> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Real max = Inexact.NEGATIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Real))
          throw new Exceptionf("'(max <real> <real> ...) invalid non-numeric arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(pValue.isNaN() || (!max.isNaN() && max.lt(pValue))) max = pValue;
      }
      return max;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // exact->inexact
  public static class ExactToInexact extends Primitive {
    public java.lang.String escmName() {
      return "exact->inexact";
    }

    public Datum signature() {
      return Pair.List(new Symbol("exact->inexact"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(exact->inexact <number>) expects exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      return ((Number)parameters.get(0)).toInexact();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // inexact->exact
  public static class InexactToExact extends Primitive {
    public java.lang.String escmName() {
      return "inexact->exact";
    }

    public Datum signature() {
      return Pair.List(new Symbol("inexact->exact"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(inexact->exact <number>) expects exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      return ((Number)parameters.get(0)).toExact();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // numerator
  public static class Numerator extends Primitive {
    public java.lang.String escmName() {
      return "numerator";
    }

    public Datum signature() {
      return Pair.List(new Symbol("numerator"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Real)) 
        throw new Exceptionf("'(numerator <real>) expects exactly 1 real: %s", Exceptionf.profileArgs(parameters));
      return ((Real)parameters.get(0)).numerator();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // denominator
  public static class Denominator extends Primitive {
    public java.lang.String escmName() {
      return "denominator";
    }

    public Datum signature() {
      return Pair.List(new Symbol("denominator"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Real)) 
        throw new Exceptionf("'(denominator <real>) expects exactly 1 real: %s", Exceptionf.profileArgs(parameters));
      return ((Real)parameters.get(0)).denominator();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // quotient
  public static class Quotient extends Primitive {
    public java.lang.String escmName() {
      return "quotient";
    }

    public Datum signature() {
      return Pair.List(new Symbol("quotient"),new Symbol("<dividend-real>"),new Symbol("<divisor-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(quotient <dividend> <divisor>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(quotient <dividend> <divisor>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(quotient <dividend> <divisor>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      return ((Real)lhs).quotient((Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // remainder
  public static class Remainder extends Primitive {
    public java.lang.String escmName() {
      return "remainder";
    }

    public Datum signature() {
      return Pair.List(new Symbol("remainder"),new Symbol("<dividend-real>"),new Symbol("<divisor-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(remainder <dividend> <divisor>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(remainder <dividend> <divisor>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(remainder <dividend> <divisor>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      return ((Real)lhs).remainder((Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // modulo
  public static class Modulo extends Primitive {
    public java.lang.String escmName() {
      return "modulo";
    }

    public Datum signature() {
      return Pair.List(new Symbol("modulo"),new Symbol("<dividend-real>"),new Symbol("<divisor-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(modulo <real1> <real2>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(modulo <real1> <real2>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(modulo <real1> <real2>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      return ((Real)lhs).modulo((Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // divrem
  public static class Divrem extends Primitive {
    public java.lang.String escmName() {
      return "divrem";
    }

    public Datum signature() {
      return Pair.List(new Symbol("divrem"),new Symbol("<dividend-real>"),new Symbol("<divisor-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(divrem <dividend> <divisor>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(divrem <dividend> <divisor>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(divrem <dividend> <divisor>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      Real[] pair = ((Real)lhs).divrem((Real)rhs);
      return new escm.type.Pair(pair[0],pair[1]);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // modf
  public static class Modf extends Primitive {
    public java.lang.String escmName() {
      return "modf";
    }

    public Datum signature() {
      return Pair.List(new Symbol("modf"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(modf <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(modf <real>) invalid non-real arg: %s", Exceptionf.profileArgs(parameters));
      Real[] pair = ((Real)lhs).modf();
      return new escm.type.Pair(pair[0],pair[1]);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // integral
  public static class Integral extends Primitive {
    public java.lang.String escmName() {
      return "integral";
    }

    public Datum signature() {
      return Pair.List(new Symbol("integral"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Real)) 
        throw new Exceptionf("'(integral <real>) expects exactly 1 real: %s", Exceptionf.profileArgs(parameters));
      return ((Real)parameters.get(0)).integral();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // fractional
  public static class Fractional extends Primitive {
    public java.lang.String escmName() {
      return "fractional";
    }

    public Datum signature() {
      return Pair.List(new Symbol("fractional"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Real)) 
        throw new Exceptionf("'(fractional <real>) expects exactly 1 real: %s", Exceptionf.profileArgs(parameters));
      return ((Real)parameters.get(0)).fractional();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // gcd
  public static class Gcd extends Primitive {
    public java.lang.String escmName() {
      return "gcd";
    }

    public Datum signature() {
      return Pair.List(new Symbol("gcd"),new Symbol("<integer>"),new Symbol("<integer>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(gcd <integer1> <integer2>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real) || !((Real)lhs).isInteger() || ((Real)lhs).isNegative())
        throw new Exceptionf("'(gcd <integer1> <integer2>) invalid not non-negative-integer 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real) || !((Real)rhs).isInteger() || ((Real)rhs).isNegative())
        throw new Exceptionf("'(gcd <integer1> <integer2>) invalid not non-negative-integer 2nd arg %s recieved!", rhs.profile());
      return ((Real)lhs).gcd((Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // lcm
  public static class Lcm extends Primitive {
    public java.lang.String escmName() {
      return "lcm";
    }

    public Datum signature() {
      return Pair.List(new Symbol("lcm"),new Symbol("<integer>"),new Symbol("<integer>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(lcm <integer1> <integer2>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real) || !((Real)lhs).isInteger() || ((Real)lhs).isNegative())
        throw new Exceptionf("'(lcm <integer1> <integer2>) invalid not non-negative-integer 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real) || !((Real)rhs).isInteger() || ((Real)rhs).isNegative())
        throw new Exceptionf("'(lcm <integer1> <integer2>) invalid not non-negative-integer 2nd arg %s recieved!", rhs.profile());
      return ((Real)lhs).lcm((Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // round
  public static class Round extends Primitive {
    public java.lang.String escmName() {
      return "round";
    }

    public Datum signature() {
      return Pair.List(new Symbol("round"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(round <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(round <real>) invalid non-real arg %s recieved!", n.profile());
      return ((Real)n).round();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // floor
  public static class Floor extends Primitive {
    public java.lang.String escmName() {
      return "floor";
    }

    public Datum signature() {
      return Pair.List(new Symbol("floor"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(floor <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(floor <real>) invalid non-real arg %s recieved!", n.profile());
      return ((Real)n).floor();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ceiling
  public static class Ceiling extends Primitive {
    public java.lang.String escmName() {
      return "ceiling";
    }

    public Datum signature() {
      return Pair.List(new Symbol("ceiling"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(ceiling <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(ceiling <real>) invalid non-real arg %s recieved!", n.profile());
      return ((Real)n).ceil();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // truncate
  public static class Truncate extends Primitive {
    public java.lang.String escmName() {
      return "truncate";
    }

    public Datum signature() {
      return Pair.List(new Symbol("truncate"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(truncate <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(truncate <real>) invalid non-real arg %s recieved!", n.profile());
      return ((Real)n).trunc();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // number?
  public static class IsNumber extends Primitive {
    public java.lang.String escmName() {
      return "number?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("number?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(number? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // complex?
  public static class IsComplex extends Primitive {
    public java.lang.String escmName() {
      return "complex?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("complex?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(complex? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // real?
  public static class IsReal extends Primitive {
    public java.lang.String escmName() {
      return "real?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("real?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(real? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Real);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // inexact?
  public static class IsInexact extends Primitive {
    public java.lang.String escmName() {
      return "inexact?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("inexact?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(inexact? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number && ((Number)parameters.get(0)).isInexact());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // exact?
  public static class IsExact extends Primitive {
    public java.lang.String escmName() {
      return "exact?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("exact?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(exact? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number && ((Number)parameters.get(0)).isExact());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // integer?
  public static class IsInteger extends Primitive {
    public java.lang.String escmName() {
      return "integer?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("integer?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(integer? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isInteger());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // finite?
  public static class IsFinite extends Primitive {
    public java.lang.String escmName() {
      return "finite?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("finite?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(finite? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isFinite());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // infinite?
  public static class IsInfinite extends Primitive {
    public java.lang.String escmName() {
      return "infinite?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("infinite?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(infinite? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isInfinite());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // nan?
  public static class IsNaN extends Primitive {
    public java.lang.String escmName() {
      return "nan?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("nan?"),new Symbol("<obj>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(nan? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isNaN());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // odd?
  public static class IsOdd extends Primitive {
    public java.lang.String escmName() {
      return "odd?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("odd?"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(odd? <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(odd? <real>) invalid non-real arg %s recieved!", n.profile());
      return Boolean.valueOf(((Real)n).isOdd());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // even?
  public static class IsEven extends Primitive {
    public java.lang.String escmName() {
      return "even?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("even?"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(even? <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(even? <real>) invalid non-real arg %s recieved!", n.profile());
      return Boolean.valueOf(((Real)n).isEven());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // positive?
  public static class IsPositive extends Primitive {
    public java.lang.String escmName() {
      return "positive?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("positive?"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(positive? <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(positive? <real>) invalid non-real arg %s recieved!", n.profile());
      return Boolean.valueOf(((Real)n).isPositive());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // negative?
  public static class IsNegative extends Primitive {
    public java.lang.String escmName() {
      return "negative?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("negative?"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(negative? <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(negative? <real>) invalid non-real arg %s recieved!", n.profile());
      return Boolean.valueOf(((Real)n).isNegative());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // zero?
  public static class IsZero extends Primitive {
    public java.lang.String escmName() {
      return "zero?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("zero?"),new Symbol("<real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(zero? <real>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Real))
        throw new Exceptionf("'(zero? <real>) invalid non-real arg %s recieved!", n.profile());
      return Boolean.valueOf(((Real)n).isZero());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sin
  public static class Sin extends Primitive {
    public java.lang.String escmName() {
      return "sin";
    }

    public Datum signature() {
      return Pair.List(new Symbol("sin"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(sin <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(sin <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).sin();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cos
  public static class Cos extends Primitive {
    public java.lang.String escmName() {
      return "cos";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cos"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(cos <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(cos <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).cos();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // tan
  public static class Tan extends Primitive {
    public java.lang.String escmName() {
      return "tan";
    }

    public Datum signature() {
      return Pair.List(new Symbol("tan"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(tan <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(tan <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).tan();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // asin
  public static class Asin extends Primitive {
    public java.lang.String escmName() {
      return "asin";
    }

    public Datum signature() {
      return Pair.List(new Symbol("asin"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(asin <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(asin <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).asin();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // acos
  public static class Acos extends Primitive {
    public java.lang.String escmName() {
      return "acos";
    }

    public Datum signature() {
      return Pair.List(new Symbol("acos"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(acos <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(acos <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).acos();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // atan
  public static class Atan extends Primitive {
    public java.lang.String escmName() {
      return "atan";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("atan"),new Symbol("<number>")),
        Pair.List(new Symbol("atan"),new Symbol("<real>"),new Symbol("<real>")));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 && parameters.size() != 2) 
        throw new Exceptionf("'(atan <number>) '(atan <real> <real>) expects exactly 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(atan <number>) '(atan <real> <real>) invalid non-numeric arg %s recieved!", n.profile());
      if(parameters.size() == 1) return ((Number)n).atan();
      if(!(n instanceof Real))
        throw new Exceptionf("'(atan <real> <real>) invalid non-real arg %s recieved!", n.profile());
      Datum n2 = parameters.get(1);
      if(!(n2 instanceof Real))
        throw new Exceptionf("'(atan <number>) '(atan <real> <real>) invalid non-real 2nd arg %s recieved!", n2.profile());
      return ((Real)n).atan((Real)n2);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sinh
  public static class Sinh extends Primitive {
    public java.lang.String escmName() {
      return "sinh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("sinh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(sinh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(sinh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).sinh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cosh
  public static class Cosh extends Primitive {
    public java.lang.String escmName() {
      return "cosh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cosh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(cosh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(cosh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).cosh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // tanh
  public static class Tanh extends Primitive {
    public java.lang.String escmName() {
      return "tanh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("tanh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(tanh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(tanh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).tanh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // asinh
  public static class Asinh extends Primitive {
    public java.lang.String escmName() {
      return "asinh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("asinh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(asinh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(asinh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).asinh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // acosh
  public static class Acosh extends Primitive {
    public java.lang.String escmName() {
      return "acosh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("acosh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(acosh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(acosh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).acosh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // atanh
  public static class Atanh extends Primitive {
    public java.lang.String escmName() {
      return "atanh";
    }

    public Datum signature() {
      return Pair.List(new Symbol("atanh"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(atanh <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(atanh <number>) invalid non-numeric arg %s recieved!", n.profile());
      return ((Number)n).atanh();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // npr
  public static class Npr extends Primitive {
    public java.lang.String escmName() {
      return "npr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("npr"),new Symbol("<n-integer>"),new Symbol("<r-integer>"));
    }

    private static final Exact ONE = new Exact(1);

    // @PRECONDITION: <n> is a non-negative integer
    public static Real factorial(Real n) {
      Real p = ONE;
      while(n.gt(ONE)) {
        p = (Real)p.mul(n);
        n = (Real)n.sub(ONE);
      }
      return p;
    }

    // @PRECONDITION: <n> & <r> are non-negative integers
    public static Real logic(Real n, Real r) {
      return (Real)factorial(n).div(factorial((Real)n.sub(r)));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(npr <n-integer> <r-integer>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      Datum r = parameters.get(1);
      if(!(n instanceof Real) || !((Real)n).isInteger() || ((Real)n).isNegative())
        throw new Exceptionf("'(npr <n-integer> <r-integer>) invalid not non-negative-integer <n> recieved: %s", Exceptionf.profileArgs(parameters));
      if(!(r instanceof Real) || !((Real)r).isInteger() || ((Real)r).isNegative())
        throw new Exceptionf("'(npr <n-integer> <r-integer>) invalid not non-negative-integer <r> recieved: %s", Exceptionf.profileArgs(parameters));
      return logic((Real)n,(Real)r);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ncr
  public static class Ncr extends Primitive {
    public java.lang.String escmName() {
      return "ncr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("ncr"),new Symbol("<n-integer>"),new Symbol("<r-integer>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(ncr <n-integer> <r-integer>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      Datum r = parameters.get(1);
      if(!(n instanceof Real) || !((Real)n).isInteger() || ((Real)n).isNegative())
        throw new Exceptionf("'(ncr <n-integer> <r-integer>) invalid not non-negative-integer <n> recieved: %s", Exceptionf.profileArgs(parameters));
      if(!(r instanceof Real) || !((Real)r).isInteger() || ((Real)r).isNegative())
        throw new Exceptionf("'(ncr <n-integer> <r-integer>) invalid not non-negative-integer <r> recieved: %s", Exceptionf.profileArgs(parameters));
      return Npr.logic((Real)n,(Real)r).div(Npr.factorial((Real)r));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // random
  public static class Random extends Primitive {
    public java.lang.String escmName() {
      return "random";
    }

    public Datum signature() {
      return Pair.List(new Symbol("random"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) throw new Exceptionf("'(random) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new Inexact(GlobalState.getRandomDouble());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-rectangular
  public static class MakeRectangular extends Primitive {
    public java.lang.String escmName() {
      return "make-rectangular";
    }

    public Datum signature() {
      return Pair.List(new Symbol("make-rectangular"),new Symbol("<real-real>"),new Symbol("<imag-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(make-rectangular <real-real> <imag-real>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(make-rectangular <real-real> <imag-real>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(make-rectangular <real-real> <imag-real>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      return Complex.makeRectangular((Real)lhs,(Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-polar
  public static class MakePolar extends Primitive {
    public java.lang.String escmName() {
      return "make-polar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("make-polar"),new Symbol("<magnitude-real>"),new Symbol("<angle-real>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(make-polar <magnitude-real> <angle-real>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lhs = parameters.get(0);
      if(!(lhs instanceof Real))
        throw new Exceptionf("'(make-polar <magnitude-real> <angle-real>) invalid non-real 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Real))
        throw new Exceptionf("'(make-polar <magnitude-real> <angle-real>) invalid non-real 2nd arg %s recieved!", rhs.profile());
      return Complex.makePolar((Real)lhs,(Real)rhs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // real-part
  public static class RealPart extends Primitive {
    public java.lang.String escmName() {
      return "real-part";
    }

    public Datum signature() {
      return Pair.List(new Symbol("real-part"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(real-part <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Real) return n;
      return ((Complex)n).realPart();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // imag-part
  public static class ImagPart extends Primitive {
    public java.lang.String escmName() {
      return "imag-part";
    }

    public Datum signature() {
      return Pair.List(new Symbol("imag-part"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(imag-part <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Exact) return new Exact();
      if(n instanceof Inexact) return new Inexact();
      return ((Complex)n).imagPart();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // magnitude
  public static class Magnitude extends Primitive {
    public java.lang.String escmName() {
      return "magnitude";
    }

    public Datum signature() {
      return Pair.List(new Symbol("magnitude"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(magnitude <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Real) return ((Real)n).abs();
      return ((Complex)n).magnitude();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // angle
  public static class Angle extends Primitive {
    public java.lang.String escmName() {
      return "angle";
    }

    public Datum signature() {
      return Pair.List(new Symbol("angle"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(angle <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Exact) return (new Exact()).atan((Exact)n);
      if(n instanceof Inexact) return (new Inexact()).atan((Inexact)n);
      return ((Complex)n).angle();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // conjugate
  public static class Conjugate extends Primitive {
    public java.lang.String escmName() {
      return "conjugate";
    }

    public Datum signature() {
      return Pair.List(new Symbol("conjugate"),new Symbol("<number>"));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(conjugate <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Real) return n;
      return ((Complex)n).conjugate();
    }
  }
}