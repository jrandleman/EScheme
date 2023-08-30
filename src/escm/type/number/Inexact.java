// Author: Jordan Randleman - escm.type.number.Inexact
// Purpose:
//    Inexact class: holds a Java <double>.

package escm.type.number;
import java.util.Objects;
import escm.util.error.Exceptionf;

public class Inexact extends Real {
  ////////////////////////////////////////////////////////////////////////////
  // Constant Fields
  public static final Inexact NAN = new Inexact(Double.NaN);

  public static final Inexact POSITIVE_INFINITY = new Inexact(Double.POSITIVE_INFINITY);
  
  public static final Inexact NEGATIVE_INFINITY = new Inexact(Double.NEGATIVE_INFINITY);


  ////////////////////////////////////////////////////////////////////////////
  // Escm Inexact Multiplication
  // => In Java, 0*Inf = 0*-Inf = 0*NaN = NaN
  // => However, this behavior is problematic in EScheme. Complex numbers
  //    mean that multiplications by 0.0 can creep in from all over the place,
  //    and hence we end up with random NaNs everywhere. 
  // => As such, EScheme defines 0.0*N=0.0 for all Inexact <N>. 
  //    * Note that, while violating IEEE 754 (defines Java's behavior), it both:
  //        (a) does not violate math itself, since 0*inf is undefined.
  //        (b) follows in Chez-Scheme's footsteps: an excellent implementation.
  //      Both of these points, combined with the fact that I (JCR) find it more
  //      intuitive and useful to preserve scalar values where we can, explains
  //      why EScheme multiplies 0s the way it does.
  public static double doubleMultiply(double a, double b) {
    if(a == 0.0 || b == 0.0) return 0.0;
    return a*b;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Escm Inexact Exponentiation
  // => Ensures complex numbers are created as needed
  static final Inexact ZERO = new Inexact(0.0);
  static final Inexact ONE = new Inexact(1.0);

  private static boolean powYieldsComplex(double base, double pow) {
    return base < 0.0 && Double.isFinite(pow) && Math.round(pow) != pow;    
  }

  // n^0 = 1
  // 0^+n = 1
  // 0^-n = Infinity
  // [+-]1^[+-]Infinity = 1
  public static Number doublePow(double base, double pow) {
    if(pow == 0.0) return ONE;
    if(base == 0.0) {
      if(pow < 0.0) return POSITIVE_INFINITY;
      return ZERO;
    }
    if(Math.abs(base) == 1.0 && Double.isInfinite(pow)) return ONE;
    if(powYieldsComplex(base,pow))
      return (new Complex(new Inexact(base))).expt(new Inexact(pow));
    return new Inexact(Math.pow(base,pow));
  }


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
  public Real exptMod(Real p, Real m) throws Exception {
    Number expt = doublePow(value,p.doubleValue());
    if(expt instanceof Complex)
      throw new Exceptionf("Invalid Complex expt-mod result: base=%s, power=%s, mod=%s", display(), p.display(), m.display());
    return ((Real)expt).modulo(m);
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
      return new Inexact(doubleMultiply(value,((Real)n).doubleValue()));
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
    if(n instanceof Complex) 
      return (new Complex(value)).expt(n);
    return doublePow(value,((Real)n).doubleValue());
  }

  public Inexact exp() {
    return new Inexact(Math.exp(value));
  }

  public Number log() {
    if(value < 0.0) 
      return (new Complex(this)).log();
    return new Inexact(Math.log(value));
  }

  public Number sqrt() {
    if(value < 0.0) 
      return (new Complex(new Inexact(),(Real)doublePow(-value,0.5))).unifyComponentExactness();
    return doublePow(value,0.5);
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
    if(n.isZero()) return NAN;
    double rhs = n.doubleValue();
    double quo = value / rhs;
    if(quo < 0) return new Inexact(value - doubleMultiply(Math.ceil(quo),rhs));
    return new Inexact(value - doubleMultiply(Math.floor(quo),rhs));
  }

  public Inexact[] divrem(Real n) {
    return new Inexact[]{quotient(n),remainder(n)};
  }

  // Credit for this Algorithm (modF) goes to Daan Leijen of the University of Utrecht. 
  // Proof (see page 5): https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
  public Inexact modulo(Real n) {
    double d = n.doubleValue();
    double r = value % d;
    if((r > 0 && d < 0) || (r < 0 && d > 0)) r += d;
    return new Inexact(r);
  }

  public Exact numerator() throws Exception { // NaN, Infinity, -Infinity
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <numerator> of %f", value);
    return toExact().numerator();
  }

  public Exact denominator() throws Exception { // NaN, Infinity, -Infinity
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <denominator> of %f", value);
    return toExact().denominator();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Modf 
  // @return: [integral, fractional]
  public Real[] modf() {
    if(Double.isInfinite(value)) return new Real[]{this,ZERO};
    double fractional = value % 1;
    return new Real[]{new Inexact(value-fractional),new Inexact(fractional)};
  }


  ////////////////////////////////////////////////////////////////////////////
  // Integral & Fractional 
  // @return: integral
  public Exact integral() throws Exception {
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <integral> of %f", value);
    double fractional = value % 1;
    return new Exact(value-fractional);
  }

  // @return: fractional (as an integer)
  public Exact fractional() throws Exception {
    if(Double.isFinite(value) == false)
      throw new Exceptionf("<Inexact>: can't get <fractional> of %f", value);
    double fractional = value % 1;
    while(fractional % 1 != 0.0) fractional = doubleMultiply(fractional,10.0);
    return new Exact(fractional);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Common Denominator & Multiple Computations
  private void guarenteeGivenIntegers(String opName, double b) throws Exception {
    if(Math.round(value) != value) 
      throw new Exceptionf("Can't perform (%s %f %f) with non-integer %f", opName, value, b, value);
    if(Math.round(b) != b) 
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
    if(a == 0.0) return new Inexact(b);
    if(b == 0.0) return new Inexact(a);
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
    if(a == 0.0 || b == 0.0) return ZERO;
    while(a != b) {
      if(a > b) {
        a -= b;
      } else {
        b -= a;
      }
    }
    return new Inexact(doubleMultiply(originalA,originalB)/a);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Approximations
  static double truncate(double d) {
    if(d < 0) return Math.ceil(d);
    return Math.floor(d);
  }


  public Real round() {
    if(!Double.isFinite(value)) return this;
    return new Inexact(Math.round(value));
  }

  public Real floor() {
    if(!Double.isFinite(value)) return this;
    return new Inexact(Math.floor(value));
  }

  public Real ceil() {
    if(!Double.isFinite(value)) return this;
    return new Inexact(Math.ceil(value));
  }

  public Real trunc() {
    if(!Double.isFinite(value)) return this;
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


  public Number asin() {
    double result = Math.asin(value);
    if(!Double.isNaN(result)) return new Inexact(result);
    return (new Complex(this,ZERO)).asin();
  }

  public Number acos() {
    double result = Math.acos(value);
    if(!Double.isNaN(result)) return new Inexact(result);
    return (new Complex(this,ZERO)).acos();
  }

  public Inexact atan() {
    return new Inexact(Math.atan(value));
  }

  public Inexact atan(Real n) {
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
    return new Inexact(Math.log(value+Math.sqrt(Math.pow(value,2)+1)));
  }

  public Number acosh() {
    double result = Math.log(value+Math.sqrt(Math.pow(value,2)-1));
    if(!Double.isNaN(result)) return new Inexact(result);
    return (new Complex(this,ZERO)).acosh();
  }

  public Number atanh() {
    double result = doubleMultiply(0.5,Math.log((1+value)/(1-value)));
    if(!Double.isNaN(result)) return new Inexact(result);
    return (new Complex(this,ZERO)).atanh();
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
  // Exactness Conversion
  public Exact toExact() throws Exception {
    return new Exact(this);
  }

  public Inexact toInexact() throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "inexact";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(o instanceof Inexact) {
      Inexact i = (Inexact)o;
      if(Double.isNaN(value)) return Double.isNaN(i.value);
      return value == i.value;
    } else if(o instanceof Complex) {
      Complex c = (Complex)o;
      Real r = c.realPart();
      return r instanceof Inexact && c.imagPart().isZero() && eq(r);
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public String docstring() {
    return "Inexact value: "+display();
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