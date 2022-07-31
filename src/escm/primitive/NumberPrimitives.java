// Author: Jordan Randleman - escm.primitive.NumberPrimitives
// Purpose:
//    Java primitives for number procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.Random;
import escm.type.Datum;
import escm.type.Boolean;
import escm.type.number.Number;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.number.Inexact;
import escm.type.number.Complex;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;
import escm.vm.runtime.GlobalState;

public class NumberPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // +
  public static class Plus implements Primitive {
    public java.lang.String escmName() {
      return "+";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(+ <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Number sum = new Exact();
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(+ <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        sum = sum.add((Number)p);
      }
      return sum;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // -
  public static class Minus implements Primitive {
    public java.lang.String escmName() {
      return "-";
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
  public static class Multiply implements Primitive {
    public java.lang.String escmName() {
      return "*";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(* <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
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
  public static class Divide implements Primitive {
    public java.lang.String escmName() {
      return "/";
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
  public static class Equals implements Primitive {
    public java.lang.String escmName() {
      return "=";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(= <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Number))
        throw new Exceptionf("'(= <number> <number> ...) invalid non-numeric arg %s recieved!", firstParam.profile());
      Number firstValue = (Number)firstParam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(= <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        if(firstValue.eqs((Number)p) == false) return Boolean.FALSE;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // <
  public static class LessThan implements Primitive {
    public java.lang.String escmName() {
      return "<";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(< <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Real))
        throw new Exceptionf("'(< <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      Real lastValue = (Real)firstParam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(< <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(lastValue.gte(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // >
  public static class GreaterThan implements Primitive {
    public java.lang.String escmName() {
      return ">";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(> <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Real))
        throw new Exceptionf("'(> <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      Real lastValue = (Real)firstParam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(> <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(lastValue.lte(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // <=
  public static class LessThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return "<=";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(<= <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Real))
        throw new Exceptionf("'(<= <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      Real lastValue = (Real)firstParam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(<= <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(lastValue.gt(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // >=
  public static class GreaterThanOrEqualTo implements Primitive {
    public java.lang.String escmName() {
      return ">=";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) throw new Exceptionf("'(>= <real> <real> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Real))
        throw new Exceptionf("'(>= <real> <real> ...) invalid non-real arg %s recieved!", firstParam.profile());
      Real lastValue = (Real)firstParam;
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Real))
          throw new Exceptionf("'(>= <real> <real> ...) invalid non-real arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(lastValue.lt(pValue)) return Boolean.FALSE;
        lastValue = pValue;
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // expt
  public static class Expt implements Primitive {
    public java.lang.String escmName() {
      return "expt";
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
  public static class Exp implements Primitive {
    public java.lang.String escmName() {
      return "exp";
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
  public static class Log implements Primitive {
    public java.lang.String escmName() {
      return "log";
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
      return ((Number)n).log().div(((Number)base).log());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sqrt
  public static class Sqrt implements Primitive {
    public java.lang.String escmName() {
      return "sqrt";
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
  public static class Abs implements Primitive {
    public java.lang.String escmName() {
      return "abs";
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
  // mod-expt
  public static class ModExpt implements Primitive {
    public java.lang.String escmName() {
      return "mod-expt";
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
  public static class Min implements Primitive {
    public java.lang.String escmName() {
      return "min";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(min <real> <real> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Real min = Inexact.POSITIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Real))
          throw new Exceptionf("'(min <real> <real> ...) invalid non-numeric arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(min.gt(pValue)) min = pValue;
      }
      return min;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // max
  public static class Max implements Primitive {
    public java.lang.String escmName() {
      return "max";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(max <real> <real> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Real max = Inexact.NEGATIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Real))
          throw new Exceptionf("'(max <real> <real> ...) invalid non-numeric arg %s recieved!", p.profile());
        Real pValue = (Real)p;
        if(max.lt(pValue)) max = pValue;
      }
      return max;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // quotient
  public static class Quotient implements Primitive {
    public java.lang.String escmName() {
      return "quotient";
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
  public static class Remainder implements Primitive {
    public java.lang.String escmName() {
      return "remainder";
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
  public static class Modulo implements Primitive {
    public java.lang.String escmName() {
      return "modulo";
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
  public static class Divrem implements Primitive {
    public java.lang.String escmName() {
      return "divrem";
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
  public static class Modf implements Primitive {
    public java.lang.String escmName() {
      return "modf";
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
  // gcd
  public static class Gcd implements Primitive {
    public java.lang.String escmName() {
      return "gcd";
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
  public static class Lcm implements Primitive {
    public java.lang.String escmName() {
      return "lcm";
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
  public static class Round implements Primitive {
    public java.lang.String escmName() {
      return "round";
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
  public static class Floor implements Primitive {
    public java.lang.String escmName() {
      return "floor";
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
  // ceil
  public static class Ceiling implements Primitive {
    public java.lang.String escmName() {
      return "ceil";
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
  public static class Truncate implements Primitive {
    public java.lang.String escmName() {
      return "truncate";
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
  public static class IsNumber implements Primitive {
    public java.lang.String escmName() {
      return "number?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(number? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // complex?
  public static class IsComplex implements Primitive {
    public java.lang.String escmName() {
      return "complex?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(complex? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // real?
  public static class IsReal implements Primitive {
    public java.lang.String escmName() {
      return "real?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(real? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Real);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // inexact?
  public static class IsInexact implements Primitive {
    public java.lang.String escmName() {
      return "inexact?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(inexact? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number && ((Number)parameters.get(0)).isInexact());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // exact?
  public static class IsExact implements Primitive {
    public java.lang.String escmName() {
      return "exact?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(exact? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Number && ((Number)parameters.get(0)).isExact());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // integer?
  public static class IsInteger implements Primitive {
    public java.lang.String escmName() {
      return "integer?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(integer? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isInteger());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // finite?
  public static class IsFinite implements Primitive {
    public java.lang.String escmName() {
      return "finite?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(finite? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isFinite());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // infinite?
  public static class IsInfinite implements Primitive {
    public java.lang.String escmName() {
      return "infinite?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(infinite? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isInfinite());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // nan?
  public static class IsNaN implements Primitive {
    public java.lang.String escmName() {
      return "nan?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(nan? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      return Boolean.valueOf(n instanceof Real && ((Real)n).isNaN());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // odd?
  public static class IsOdd implements Primitive {
    public java.lang.String escmName() {
      return "odd?";
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
  public static class IsEven implements Primitive {
    public java.lang.String escmName() {
      return "even?";
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
  public static class IsPositive implements Primitive {
    public java.lang.String escmName() {
      return "positive?";
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
  public static class IsNegative implements Primitive {
    public java.lang.String escmName() {
      return "negative?";
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
  public static class IsZero implements Primitive {
    public java.lang.String escmName() {
      return "zero?";
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
  public static class Sin implements Primitive {
    public java.lang.String escmName() {
      return "sin";
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
  public static class Cos implements Primitive {
    public java.lang.String escmName() {
      return "cos";
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
  public static class Tan implements Primitive {
    public java.lang.String escmName() {
      return "tan";
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
  public static class Asin implements Primitive {
    public java.lang.String escmName() {
      return "asin";
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
  public static class Acos implements Primitive {
    public java.lang.String escmName() {
      return "acos";
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
  public static class Atan implements Primitive {
    public java.lang.String escmName() {
      return "atan";
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
  public static class Sinh implements Primitive {
    public java.lang.String escmName() {
      return "sinh";
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
  public static class Cosh implements Primitive {
    public java.lang.String escmName() {
      return "cosh";
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
  public static class Tanh implements Primitive {
    public java.lang.String escmName() {
      return "tanh";
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
  public static class Asinh implements Primitive {
    public java.lang.String escmName() {
      return "asinh";
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
  public static class Acosh implements Primitive {
    public java.lang.String escmName() {
      return "acosh";
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
  public static class Atanh implements Primitive {
    public java.lang.String escmName() {
      return "atanh";
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
  public static class Npr implements Primitive {
    public java.lang.String escmName() {
      return "npr";
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
  public static class Ncr implements Primitive {
    public java.lang.String escmName() {
      return "ncr";
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
  public static class Random implements Primitive {
    public java.lang.String escmName() {
      return "random";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) throw new Exceptionf("'(random) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new Inexact(GlobalState.getRandomDouble());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-rectangular
  public static class MakeRectangular implements Primitive {
    public java.lang.String escmName() {
      return "make-rectangular";
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
  public static class MakePolar implements Primitive {
    public java.lang.String escmName() {
      return "make-polar";
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
  public static class RealPart implements Primitive {
    public java.lang.String escmName() {
      return "real-part";
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
  public static class ImagPart implements Primitive {
    public java.lang.String escmName() {
      return "imag-part";
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
  public static class Magnitude implements Primitive {
    public java.lang.String escmName() {
      return "magnitude";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Number)) 
        throw new Exceptionf("'(magnitude <number>) not given exactly 1 number: %s", Exceptionf.profileArgs(parameters));
      Number n = (Number)parameters.get(0);
      if(n instanceof Real) return n;
      return ((Complex)n).magnitude();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // angle
  public static class Angle implements Primitive {
    public java.lang.String escmName() {
      return "angle";
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
  public static class Conjugate implements Primitive {
    public java.lang.String escmName() {
      return "conjugate";
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