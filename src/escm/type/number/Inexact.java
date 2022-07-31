// Author: Jordan Randleman - escm.type.number.Inexact
// Purpose:
//    Inexact class: holds a Java <double>.

package escm.type.number;
import java.util.Objects;
import escm.util.Exceptionf;

public class Inexact extends Real {
  ////////////////////////////////////////////////////////////////////////////
  // Constant Fields
  public static final Inexact NAN = new Inexact(Double.NaN);

  public static final Inexact POSITIVE_INFINITY = new Inexact(Double.POSITIVE_INFINITY);
  
  public static final Inexact NEGATIVE_INFINITY = new Inexact(Double.NEGATIVE_INFINITY);


  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private double value = 0.0;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Inexact() {}

  public Inexact(int n) {
    value = (double)n;
  }

  public Inexact(long n) {
    value = (double)n;
  }

  public Inexact(double n) {
    value = n;
  }

  public Inexact(Exact n) {
    value = n.doubleValue();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Value Coercion
  public int intValue() {
    return (int)value;
  }

  public long longValue() {
    return (long)value;
  }

  public double doubleValue() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Comparators
  public boolean eqs(Number n) {
    if(n instanceof Complex)
      return (new Complex(value)).eqs(n);
    return value == ((Real)n).doubleValue();
  }

  public boolean lt(Real n) {
    return value < n.doubleValue();
  }

  public boolean gt(Real n) {
    return value > n.doubleValue();
  }

  public boolean lte(Real n) {
    return value <= n.doubleValue();
  }

  public boolean gte(Real n) {
    return value >= n.doubleValue();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Negation
  public Inexact negate() {
    return new Inexact(-value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Expt-modulo: this^n mod m
  public Inexact exptMod(Real p, Real m) {
    return (new Inexact(Math.pow(value,p.doubleValue()))).modulo(m);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Arithmetic
  public Number add(Number n) {
    if(n instanceof Real) {
      return new Inexact(value + ((Real)n).doubleValue());
    } else {
      return (new Complex(value)).add(n);
    }
  }

  public Number sub(Number n) {
    if(n instanceof Real) {
      return new Inexact(value - ((Real)n).doubleValue());
    } else {
      return (new Complex(value)).sub(n);
    }
  }

  public Number mul(Number n) {
    if(n instanceof Real) {
      return new Inexact(value * ((Real)n).doubleValue());
    } else {
      return (new Complex(value)).mul(n);
    }
  }

  public Number div(Number n) {
    if(n instanceof Real) {
      return new Inexact(value / ((Real)n).doubleValue());
    } else {
      return (new Complex(value)).div(n);
    }
  }


  public Number expt(Number n) {
    if(n instanceof Complex || value < 0.0) 
      return (new Complex(value)).expt(n);
    return new Inexact(Math.pow(value,((Real)n).doubleValue()));
  }

  public Inexact exp() {
    return new Inexact(Math.exp(value));
  }

  public Inexact log() {
    return new Inexact(Math.log(value));
  }

  public Number sqrt() {
    if(value < 0.0)
      return new Complex(new Inexact(),new Inexact(-Math.sqrt(-value)));
    return new Inexact(Math.sqrt(value));
  }

  public Inexact abs() {
    return new Inexact(Math.abs(value));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Division Details
  public Inexact quotient(Real n) {
    double quo = value / n.doubleValue();
    if(quo < 0) return new Inexact(Math.ceil(quo));
    return new Inexact(Math.floor(quo));
  }

  public Inexact remainder(Real n) {
    double rhs = n.doubleValue();
    double quo = value / rhs;
    if(quo < 0) return new Inexact(value - Math.ceil(quo) * rhs);
    return new Inexact(value - Math.floor(quo) * rhs);
  }

  public Inexact[] divrem(Real n) {
    double rhs = n.doubleValue();
    double quo = value / rhs;
    if(quo < 0) return new Inexact[]{new Inexact(value / rhs), new Inexact(value - Math.ceil(quo) * rhs)};
    return new Inexact[]{new Inexact(value / rhs), new Inexact(value - Math.floor(quo) * rhs)};
  }

  // Credit for this Algorithm (modE) goes to Daan Leijen of the University of Utrecht. 
  // Proof (see page 5):
  // "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf"
  public Inexact modulo(Real n) {
    double rhs = n.doubleValue();
    double r = value % rhs;
    if(r < 0.0) {
      if(value > 0.0) {
        r += value;
      } {
        r -= value;
      }
    }
    return new Inexact(r);
  }

  public Inexact numerator() throws Exception { // NaN, Infinity, -Infinity
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <numerator> of %f", value);
    return (new Inexact(value)).numerator();
  }

  public Inexact denominator() throws Exception { // NaN, Infinity, -Infinity
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <denominator> of %f", value);
    return (new Inexact(value)).denominator();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Modf 
  // @return: [integral, fractional]
  public Real[] modf() {
    double fractional = value % 1;
    return new Real[]{new Inexact(value-fractional),new Inexact(fractional)};
  }


  ////////////////////////////////////////////////////////////////////////////
  // Common Denominator & Multiple Computations
  private void guarenteeGivenIntegers(String opName, double b) throws Exception {
    if(Math.round(value) != value) 
      throw new Exceptionf("Can't perform (%s %f %f) with non-integer %f", opName, value, b, value);
    if(Math.round(b) != value) 
      throw new Exceptionf("Can't perform (%s %f %f) with non-integer %f", opName, value, b, b);
    if(value < 0.0) 
      throw new Exceptionf("Can't perform (%s %f %f) with negative %f", opName, value, b, value);
    if(b < 0.0) 
      throw new Exceptionf("Can't perform (%s %f %f) with non-integer %f", opName, value, b, b);
  }


  public Real gcd(Real r) throws Exception {
    double b = r.doubleValue();
    guarenteeGivenIntegers("gcd",b);
    double a = value;
    while(a != b) {
      if(a > b) {
        a -= b;
      } else {
        b -= a;
      }
    }
    return new Inexact(a);
  }


  public Real lcm(Real r) throws Exception {
    double b = r.doubleValue();
    guarenteeGivenIntegers("lcm",b);
    double originalA = value;
    double originalB = b;
    double a = value;
    while(a != b) {
      if(a > b) {
        a -= b;
      } else {
        b -= a;
      }
    }
    return new Inexact((originalA * originalB) / a);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Approximations
  static double truncate(double d) {
    if(d < 0) return Math.ceil(d);
    return Math.floor(d);
  }


  public Inexact round() {
    return new Inexact(Math.round(value));
  }

  public Inexact floor() {
    return new Inexact(Math.floor(value));
  }

  public Inexact ceil() {
    return new Inexact(Math.ceil(value));
  }

  public Inexact trunc() {
    return new Inexact(truncate(value));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Predicates
  public boolean isInteger() {
    return Math.round(value) == value;
  }

  public boolean isInfinite() {
    return Double.isInfinite(value);
  }

  public boolean isFinite() {
    return Double.isFinite(value);
  }

  public boolean isNaN() {
    return Double.isNaN(value);
  }

  public boolean isOdd() {
    return Math.abs(value) % 2.0 == 1.0;
  }

  public boolean isEven() {
    return Math.abs(value) % 2.0 == 0.0;
  }

  public boolean isPositive() {
    return value > 0.0;
  }

  public boolean isNegative() {
    return value < 0.0;
  }

  public boolean isZero() {
    return value == 0.0;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  public Inexact sin() {
    return new Inexact(Math.sin(value));
  }

  public Inexact cos() {
    return new Inexact(Math.cos(value));
  }

  public Inexact tan() {
    return new Inexact(Math.tan(value));
  }


  public Inexact asin() {
    return new Inexact(Math.asin(value));
  }

  public Inexact acos() {
    return new Inexact(Math.acos(value));
  }

  public Inexact atan() {
    return new Inexact(Math.atan(value));
  }

  public Real atan(Real n) {
    return new Inexact(Math.atan2(value,n.doubleValue()));
  }


  public Inexact sinh() {
    return new Inexact(Math.sinh(value));
  }

  public Inexact cosh() {
    return new Inexact(Math.cosh(value));
  }

  public Inexact tanh() {
    return new Inexact(Math.tanh(value));
  }


  public Inexact asinh() {
    return new Inexact(Math.log(value + Math.sqrt(Math.pow(value,2) + 1)));
  }

  public Inexact acosh() {
    return new Inexact(Math.log(value + Math.sqrt(Math.pow(value,2) - 1)));
  }

  public Inexact atanh() {
    return new Inexact(0.5 * Math.log((1 + value) / (1 - value)));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Check
  public boolean isExact() {
    return false;
  }

  public boolean isInexact() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "inexact";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(o instanceof Real) {
      if(Double.isNaN(value)) return Double.isNaN(((Real)o).doubleValue());
      return value == ((Real)o).doubleValue();
    } else if(o instanceof Complex) {
      return (new Complex(value)).eq(o);
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    return Double.toString(value);
  }

  public String toString(int radix) throws Exception {
    if(radix != Number.DEC_RADIX)
      throw new Exceptionf("Can't print number %s with invalid radix %d (only %d)", display(), radix, Number.DEC_RADIX);
    return Double.toString(value);
  }
}