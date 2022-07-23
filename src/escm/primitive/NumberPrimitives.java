// Author: Jordan Randleman - escm.primitive.NumberPrimitives
// Purpose:
//    Java primitives for number procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.Random;
import escm.type.Datum;
import escm.type.Boolean;
import escm.type.Number;
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
      double sum = 0.0;
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(+ <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        sum += ((Number)p).doubleValue();
      }
      return new Number(sum);
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
      if(parameters.size() == 1) 
        return new Number(-((Number)firstparam).doubleValue());
      double diff = ((Number)firstparam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(- <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        diff -= ((Number)p).doubleValue();
      }
      return new Number(diff);
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
      double product = 1.0;
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(* <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        product *= ((Number)p).doubleValue();
      }
      return new Number(product);
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
        return new Number(1.0/((Number)firstparam).doubleValue());
      double div = ((Number)firstparam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(/ <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        div /= ((Number)p).doubleValue();
      }
      return new Number(div);
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
      double lastValue = ((Number)firstParam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(= <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(lastValue != pValue) return Boolean.FALSE;
        lastValue = pValue;
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
      if(parameters.size() < 2) throw new Exceptionf("'(< <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Number))
        throw new Exceptionf("'(< <number> <number> ...) invalid non-numeric arg %s recieved!", firstParam.profile());
      double lastValue = ((Number)firstParam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(< <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(lastValue >= pValue) return Boolean.FALSE;
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
      if(parameters.size() < 2) throw new Exceptionf("'(> <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Number))
        throw new Exceptionf("'(> <number> <number> ...) invalid non-numeric arg %s recieved!", firstParam.profile());
      double lastValue = ((Number)firstParam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(> <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(lastValue <= pValue) return Boolean.FALSE;
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
      if(parameters.size() < 2) throw new Exceptionf("'(<= <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Number))
        throw new Exceptionf("'(<= <number> <number> ...) invalid non-numeric arg %s recieved!", firstParam.profile());
      double lastValue = ((Number)firstParam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(<= <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(lastValue > pValue) return Boolean.FALSE;
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
      if(parameters.size() < 2) throw new Exceptionf("'(>= <number> <number> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum firstParam = parameters.get(0);
      if(!(firstParam instanceof Number))
        throw new Exceptionf("'(>= <number> <number> ...) invalid non-numeric arg %s recieved!", firstParam.profile());
      double lastValue = ((Number)firstParam).doubleValue();
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(>= <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(lastValue < pValue) return Boolean.FALSE;
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
      double powVal = ((Number)firstparam).doubleValue();
      for(int i = parameters.size()-2; i >= 0; --i) {
        Datum p = parameters.get(i);
        if(!(p instanceof Number))
          throw new Exceptionf("'(expt <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        powVal = Math.pow(((Number)p).doubleValue(),powVal);
      }
      return new Number(powVal);
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
      return new Number(Math.exp(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // log
  public static class Log implements Primitive {
    public java.lang.String escmName() {
      return "log";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(log <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(log <number>) invalid non-numeric arg %s recieved!", n.profile());
      return new Number(Math.log(((Number)n).doubleValue()));
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
      return new Number(Math.sqrt(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // abs
  public static class Abs implements Primitive {
    public java.lang.String escmName() {
      return "abs";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(abs <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(abs <number>) invalid non-numeric arg %s recieved!", n.profile());
      return new Number(Math.abs(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // min
  public static class Min implements Primitive {
    public java.lang.String escmName() {
      return "min";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(min <number> <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      double min = Double.POSITIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(min <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(min > pValue) min = pValue;
      }
      return new Number(min);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // max
  public static class Max implements Primitive {
    public java.lang.String escmName() {
      return "max";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) throw new Exceptionf("'(max <number> <number> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      double max = Double.NEGATIVE_INFINITY;
      for(Datum p : parameters) {
        if(!(p instanceof Number))
          throw new Exceptionf("'(max <number> <number> ...) invalid non-numeric arg %s recieved!", p.profile());
        double pValue = ((Number)p).doubleValue();
        if(max < pValue) max = pValue;
      }
      return new Number(max);
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
      if(!(lhs instanceof Number))
        throw new Exceptionf("'(quotient <dividend> <divisor>) invalid non-numeric 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Number))
        throw new Exceptionf("'(quotient <dividend> <divisor>) invalid non-numeric 2nd arg %s recieved!", rhs.profile());
      double quo = ((Number)lhs).doubleValue()/((Number)rhs).doubleValue();
      if(quo < 0) return new Number(Math.ceil(quo));
      return new Number(Math.floor(quo));
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
      if(!(lhs instanceof Number))
        throw new Exceptionf("'(remainder <dividend> <divisor>) invalid non-numeric 1st arg %s recieved!", lhs.profile());
      Datum rhs = parameters.get(1);
      if(!(rhs instanceof Number))
        throw new Exceptionf("'(remainder <dividend> <divisor>) invalid non-numeric 2nd arg %s recieved!", rhs.profile());
      double lhsValue = ((Number)lhs).doubleValue();
      double rhsValue = ((Number)rhs).doubleValue();
      double quo = lhsValue/rhsValue;
      if(quo < 0) return new Number(lhsValue - Math.ceil(quo) * rhsValue);
      return new Number(lhsValue - Math.floor(quo) * rhsValue);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // round
  public static class Round implements Primitive {
    public java.lang.String escmName() {
      return "round";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(round <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(round <number>) invalid non-numeric arg %s recieved!", n.profile());
      return new Number(Math.round(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // floor
  public static class Floor implements Primitive {
    public java.lang.String escmName() {
      return "floor";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(floor <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(floor <number>) invalid non-numeric arg %s recieved!", n.profile());
      return new Number(Math.floor(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ceil
  public static class Ceiling implements Primitive {
    public java.lang.String escmName() {
      return "ceil";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(ceiling <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(ceiling <number>) invalid non-numeric arg %s recieved!", n.profile());
      return new Number(Math.ceil(((Number)n).doubleValue()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // truncate
  public static class Truncate implements Primitive {
    public java.lang.String escmName() {
      return "truncate";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(truncate <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(truncate <number>) invalid non-numeric arg %s recieved!", n.profile());
      double nValue = ((Number)n).doubleValue();
      if(nValue < 0) return new Number(Math.ceil(nValue));
      return new Number(Math.floor(nValue));
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
  // integer?
  public static class IsInteger implements Primitive {
    public java.lang.String escmName() {
      return "integer?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(integer? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(integer? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(Math.abs(((Number)n).doubleValue()) % 1 == 0.0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // finite?
  public static class IsFinite implements Primitive {
    public java.lang.String escmName() {
      return "finite?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(finite? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(finite? <number>) invalid non-numeric arg %s recieved!", n.profile());
      double nValue = ((Number)n).doubleValue();
      return Boolean.valueOf(nValue != Double.POSITIVE_INFINITY && 
                             nValue != Double.NEGATIVE_INFINITY && 
                             nValue == nValue); // x == x checks against x being NaN
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // infinite?
  public static class IsInfinite implements Primitive {
    public java.lang.String escmName() {
      return "infinite?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(infinite? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(infinite? <number>) invalid non-numeric arg %s recieved!", n.profile());
      double nValue = ((Number)n).doubleValue();
      return Boolean.valueOf(nValue == Double.POSITIVE_INFINITY || nValue == Double.NEGATIVE_INFINITY);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // nan?
  public static class IsNaN implements Primitive {
    public java.lang.String escmName() {
      return "nan?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(nan? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(nan? <number>) invalid non-numeric arg %s recieved!", n.profile());
      double nValue = ((Number)n).doubleValue();
      return Boolean.valueOf(nValue != nValue); // x != x is ONLY true if x is NaN
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // odd?
  public static class IsOdd implements Primitive {
    public java.lang.String escmName() {
      return "odd?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(odd? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(odd? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(Math.abs(((Number)n).doubleValue()) % 2.0 == 1.0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // even?
  public static class IsEven implements Primitive {
    public java.lang.String escmName() {
      return "even?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(even? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(even? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(Math.abs(((Number)n).doubleValue()) % 2.0 == 0.0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // positive?
  public static class IsPositive implements Primitive {
    public java.lang.String escmName() {
      return "positive?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(positive? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(positive? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(((Number)n).doubleValue() > 0.0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // negative?
  public static class IsNegative implements Primitive {
    public java.lang.String escmName() {
      return "negative?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(negative? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(negative? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(((Number)n).doubleValue() < 0.0);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // zero?
  public static class IsZero implements Primitive {
    public java.lang.String escmName() {
      return "zero?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(zero? <number>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(zero? <number>) invalid non-numeric arg %s recieved!", n.profile());
      return Boolean.valueOf(((Number)n).doubleValue() == 0.0);
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
      return new Number(Math.sin(((Number)n).doubleValue()));
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
      return new Number(Math.cos(((Number)n).doubleValue()));
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
      return new Number(Math.tan(((Number)n).doubleValue()));
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
      return new Number(Math.asin(((Number)n).doubleValue()));
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
      return new Number(Math.acos(((Number)n).doubleValue()));
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
        throw new Exceptionf("'(atan <number> <optional-number>) expects exactly 1 or 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum n = parameters.get(0);
      if(!(n instanceof Number))
        throw new Exceptionf("'(atan <number> <optional-number>) invalid non-numeric arg %s recieved!", n.profile());
      if(parameters.size() == 1) return new Number(Math.atan(((Number)n).doubleValue()));
      Datum n2 = parameters.get(1);
      if(!(n2 instanceof Number))
        throw new Exceptionf("'(atan <number> <optional-number>) invalid non-numeric 2nd arg %s recieved!", n2.profile());
      return new Number(Math.atan2(((Number)n).doubleValue(),((Number)n2).doubleValue()));
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
      return new Number(Math.sinh(((Number)n).doubleValue()));
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
      return new Number(Math.cosh(((Number)n).doubleValue()));
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
      return new Number(Math.tanh(((Number)n).doubleValue()));
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
      double nValue = ((Number)n).doubleValue();
      return new Number(Math.log(nValue + Math.sqrt(Math.pow(nValue,2) + 1)));
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
      double nValue = ((Number)n).doubleValue();
      return new Number(Math.log(nValue + Math.sqrt(Math.pow(nValue,2) - 1)));
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
      double nValue = ((Number)n).doubleValue();
      return new Number(0.5 * Math.log((1 + nValue) / (1 - nValue)));
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
      return new Number(GlobalState.getRandomDouble());
    }
  }
}