// Author: Jordan Randleman - escm.type.number.Exact
// Purpose:
//    Exact class: holds 2 Java <BigInteger>s that create a fraction.

package escm.type.number;
import java.util.Objects;
import java.math.BigInteger;
import java.math.BigDecimal;
import escm.util.error.Exceptionf;

public class Exact extends Real {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private BigInteger num = BigInteger.ZERO; // numerator
  private BigInteger den = BigInteger.ONE;  // denominator


  ////////////////////////////////////////////////////////////////////////////
  // Initialization Via Double
  //   15 digits of guarenteed precision for Java's 64-bit doubles.
  //   Multiplying a fractional by 10^14 accounts for the "0" in "0.XYZ".
  //     => ABC.XYZ = ABC+0.XYZ = [(ABC*10^14)+(0.XYZ*10^14)]/10^14
  private void initWithDouble(double n) throws Exception {
    if(Double.isFinite(n) == false)
      throw new Exceptionf("Coercion: Can't convert <Inexact> %f to an <Exact> number", n);
    if(Math.round(n) == n) {
      num = BigDecimal.valueOf(n).toBigInteger();
    } else {
      double multiplier = Math.pow(10,14);
      double fractional = n%1;
      // Cast doubles to a big integer (don't care about losing their fractional components here)
      BigInteger bigMultiplier = BigDecimal.valueOf(multiplier).toBigInteger();
      BigInteger bigMultipliedFractional = BigDecimal.valueOf(Inexact.doubleMultiply(fractional,multiplier)).toBigInteger();
      BigInteger bigIntegral = BigDecimal.valueOf(n-fractional).toBigInteger();
      // Reduce the resulting fraction to its simplest form
      BigInteger numerator = bigIntegral.multiply(bigMultiplier).add(bigMultipliedFractional);
      BigInteger gcd = numerator.gcd(bigMultiplier);
      try {
        num = numerator.divide(gcd);
        den = bigMultiplier.divide(gcd);
      } catch(Exception e) {
        throw new Exceptionf("Coercion: Can't convert <Inexact> %f to an <Exact> number", n);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Exact() {}

  public Exact(int n) {
    num = BigInteger.valueOf((long)n);
  }

  public Exact(long n) {
    num = BigInteger.valueOf(n);
  }

  public Exact(double n) throws Exception {
    initWithDouble(n);
  }

  public Exact(Inexact n) throws Exception {
    initWithDouble(n.doubleValue());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Reduction to Simplest Form
  Exact(BigInteger n, BigInteger d) {
    num = n;
    den = d;
  }

  Real reduceToSimplestForm() {
    if(den.equals(BigInteger.ZERO)) return new Inexact(num.signum()/0.0);
    if(num.equals(BigInteger.ZERO)) return new Exact();
    BigInteger gcd = num.gcd(den);
    if(gcd.equals(BigInteger.ONE)) return this;
    try {
      return new Exact(num.divide(gcd),den.divide(gcd));
    } catch(Exception e) {
      return Inexact.NAN;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Value Coercion
  public int intValue() {
    if(isInteger()) return num.intValue();
    return (int)(num.longValue() / den.longValue());
  }

  public long longValue() {
    if(isInteger()) return num.longValue();
    return num.longValue() / den.longValue();
  }

  public double doubleValue() {
    if(isInteger()) return num.doubleValue();
    return num.doubleValue() / den.doubleValue();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Comparators
  public boolean eqs(Number n) {
    if(n instanceof Exact) {
      Exact e = (Exact)n;
      return num.multiply(e.den).equals(e.num.multiply(den));
    } else if(n instanceof Inexact) {
      return doubleValue() == ((Inexact)n).doubleValue();
    } else { // if(n instanceof Complex)
      return (new Complex(this)).eqs(n);
    }
  }

  public boolean lt(Real n) {
    if(n instanceof Exact) {
      Exact e = (Exact)n;
      return num.multiply(e.den).compareTo(e.num.multiply(den)) == -1;
    } else { //  if(n instanceof Inexact)
      return doubleValue() < n.doubleValue();
    }
  }

  public boolean gt(Real n) {
    if(n instanceof Exact) {
      Exact e = (Exact)n;
      return num.multiply(e.den).compareTo(e.num.multiply(den)) == 1;
    } else { //  if(n instanceof Inexact)
      return doubleValue() > n.doubleValue();
    }
  }

  public boolean lte(Real n) {
    if(n instanceof Exact) {
      Exact e = (Exact)n;
      return num.multiply(e.den).compareTo(e.num.multiply(den)) != 1;
    } else { //  if(n instanceof Inexact)
      return doubleValue() <= n.doubleValue();
    }
  }

  public boolean gte(Real n) {
    if(n instanceof Exact) {
      Exact e = (Exact)n;
      return num.multiply(e.den).compareTo(e.num.multiply(den)) != -1;
    } else { //  if(n instanceof Inexact)
      return doubleValue() >= n.doubleValue();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Negation
  public Exact negate() {
    return new Exact(num.negate(),den);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Expt-modulo: this^n mod m
  public Real exptMod(Real p, Real m) throws Exception {
    if(!p.isNegative() && m.isPositive() && isInteger() && p instanceof Exact && p.isInteger() && m instanceof Exact && m.isInteger())
      return new Exact(num.modPow(((Exact)p).num,((Exact)m).num),BigInteger.ONE);
    Number n = expt(p);
    if(n instanceof Complex)
      throw new Exceptionf("Invalid Complex expt-mod result: base=%s, power=%s, mod=%s", display(), p.display(), m.display());
    return ((Real)n).modulo(m);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Arithmetic <expt> Helper(s)
  private static final BigInteger MAX_INTEGER = BigInteger.valueOf((long)Integer.MAX_VALUE);

  // From: https://stackoverflow.com/questions/4582277/biginteger-powbiginteger
  private static BigInteger repeatedSquares(BigInteger base, BigInteger exponent) {
    BigInteger result = BigInteger.ONE;
    while(exponent.signum() > 0) {
      if(exponent.testBit(0)) result = result.multiply(base);
      base = base.multiply(base);
      exponent = exponent.shiftRight(1);
    }
    return result;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Arithmetic
  static final Exact ZERO = new Exact(BigInteger.ZERO,BigInteger.ONE);
  static final Exact ONE = new Exact(BigInteger.ONE,BigInteger.ONE);
  static final Exact HALF = new Exact(BigInteger.ONE,BigInteger.ONE.add(BigInteger.ONE));
  static final BigInteger BIGINT_TWO = BigInteger.ONE.add(BigInteger.ONE);


  public Number add(Number n) {
    if(n instanceof Complex) {
      return (new Complex(this)).add(n);
    } else if(n instanceof Inexact) {
      return new Inexact(doubleValue()+((Inexact)n).doubleValue());
    } else {
      Exact e = (Exact)n;
      if(isInteger() && e.isInteger()) 
        return new Exact(num.add(e.num),BigInteger.ONE);
      BigInteger denom = den.multiply(e.den);
      return (new Exact(num.multiply(e.den).add(e.num.multiply(den)),denom)).reduceToSimplestForm();
    }
  }

  public Number sub(Number n) {
    if(n instanceof Complex) {
      return (new Complex(this)).sub(n);
    } else if(n instanceof Inexact) {
      return new Inexact(doubleValue()-((Inexact)n).doubleValue());
    } else {
      Exact e = (Exact)n;
      if(isInteger() && e.isInteger()) 
        return new Exact(num.subtract(e.num),BigInteger.ONE);
      BigInteger denom = den.multiply(e.den);
      return (new Exact(num.multiply(e.den).subtract(e.num.multiply(den)),denom)).reduceToSimplestForm(); 
    }
  }

  public Number mul(Number n) {
    if(n instanceof Complex) {
      return (new Complex(this)).mul(n);
    } else if(n instanceof Inexact) {
      return new Inexact(Inexact.doubleMultiply(doubleValue(),((Inexact)n).doubleValue()));
    } else {
      Exact e = (Exact)n;
      if(isInteger() && e.isInteger()) 
        return new Exact(num.multiply(e.num),BigInteger.ONE);
      return (new Exact(num.multiply(e.num),den.multiply(e.den))).reduceToSimplestForm();
    }
  }

  public Number div(Number n) {
    if(n instanceof Complex) {
      return (new Complex(this)).div(n);
    } else if(n instanceof Inexact) {
      return new Inexact(doubleValue()/((Inexact)n).doubleValue());
    } else {
      Exact e = (Exact)n;
      if(isInteger() && e.isInteger()) 
        return new Exact(num,e.num).reduceToSimplestForm();
      return (new Exact(num.multiply(e.den),den.multiply(e.num))).reduceToSimplestForm();   
    }
  }

  private boolean powYieldsComplex(Real pow) {
    return isNegative() && pow.isFinite() && !pow.isInteger();
  }

  // n^0 = 1
  // 0^+n = 1
  // 0^-n = Infinity
  // [+-]1^[+-]Infinity = 1
  public Number expt(Number n) {
    boolean nIsReal = n instanceof Real;
    if(nIsReal && ((Real)n).isZero()) {
      if(n instanceof Exact) return ONE;
      return Inexact.ONE;
    }
    if(isZero()) {
      if(nIsReal) {
        if(((Real)n).isNegative()) return Inexact.POSITIVE_INFINITY;
        if(n instanceof Exact) return ZERO;
        return Inexact.ZERO;
      }
      return ZERO;
    }
    if(abs().eqs(ONE) && nIsReal && ((Real)n).isInfinite()) return ONE;
    if(n instanceof Complex) return (new Complex(this)).expt(n);
    Real real_n = (Real)n; // guarenteed success since !(n instanceof Complex)
    if(powYieldsComplex(real_n))
      return (new Complex(this)).expt(n);
    if(real_n.isNegative())
      return ONE.div(expt(real_n.negate()));
    if(n instanceof Inexact) {
      return Inexact.doublePow(doubleValue(),((Inexact)n).doubleValue());
    } else { // if(n instanceof Exact)
      Exact power = (Exact)n;
      if(power.isInteger()) {
        if(isInteger()) {
          if(power.num.compareTo(MAX_INTEGER) != 1) {
            return new Exact(num.pow(power.num.intValue()),BigInteger.ONE);
          } else {
            return new Exact(repeatedSquares(num,power.num),BigInteger.ONE);
          }
        } else {
          if(power.num.compareTo(MAX_INTEGER) != 1) {
            BigInteger numer = num.pow(power.num.intValue());
            BigInteger denom = den.pow(power.num.intValue());
            return new Exact(numer,denom);
          } else {
            BigInteger numer = repeatedSquares(num,power.num);
            BigInteger denom = repeatedSquares(den,power.num);
            return new Exact(numer,denom);
          }
        }
      } else {
        double powerValue = power.doubleValue();
        Number numResult = Inexact.doublePow(num.doubleValue(),powerValue);
        if(numResult instanceof Complex) return numResult;
        double numResultDouble = ((Real)numResult).doubleValue();
        if(isInteger()) {
          if(Math.round(numResultDouble) == numResultDouble) {
            return new Exact(BigDecimal.valueOf(numResultDouble).toBigInteger(),BigInteger.ONE);
          } else {
            return numResult;
          }
        } else {
          Number denResult = Inexact.doublePow(den.doubleValue(),powerValue);
          if(denResult instanceof Complex) return numResult.div(denResult);
          double denResultDouble = ((Real)denResult).doubleValue();
          if(Math.round(numResultDouble) == numResultDouble && Math.round(denResultDouble) == denResultDouble) {
            return new Exact(BigDecimal.valueOf(numResultDouble).toBigInteger(),BigDecimal.valueOf(denResultDouble).toBigInteger());
          } else {
            return new Inexact(numResultDouble/denResultDouble);
          }
        }
      }
    }
  }

  public Inexact exp() {
    return new Inexact(Math.exp(doubleValue()));
  }

  public Number log() {
    if(isNegative())
      return (new Complex(this)).log();
    return new Inexact(Math.log(doubleValue()));
  }

  public Number sqrt() {
    if(isNegative()) 
      return (new Complex(new Exact(),(Real)negate().sqrt())).unifyComponentExactness();
    return expt(HALF);
  }

  public Exact abs() {
    return new Exact(num.abs(),den.abs());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Division Details (recall: dividend = divisor × quotient + remainder)
  public Real quotient(Real n) {
    if(n.isZero()) return new Inexact(doubleValue()/0.0);
    try {
      if(isInteger() && n instanceof Exact && n.isInteger())
        return new Exact(num.divide(((Exact)n).num),BigInteger.ONE);
      Number result = div(n);
      if(result instanceof Real) {
        Real truncd = ((Real)result).trunc();
        if(truncd instanceof Exact) return truncd;
        return new Exact((Inexact)truncd);
      }
      return Inexact.NAN;
    } catch(Exception e) {
      return new Inexact(doubleValue()/0.0);
    }
  }

  public Real remainder(Real n) {
    if(n.isZero()) return Inexact.NAN;
    try {
      if(isInteger() && n instanceof Exact && n.isInteger())
        return new Exact(num.remainder(((Exact)n).num),BigInteger.ONE);
      Number result = sub(n.mul(quotient(n)));
      if(result instanceof Real) return (Real)result;
      return Inexact.NAN;
    } catch(Exception e) {
      return new Inexact(doubleValue()/0.0);
    }
  }

  public Real[] divrem(Real n) {
    if(!n.isZero() && isInteger() && n instanceof Exact && n.isInteger()) {
      BigInteger[] results = num.divideAndRemainder(((Exact)n).num);
      return new Exact[]{new Exact(results[0],BigInteger.ONE),new Exact(results[1],BigInteger.ONE)};
    } else {
      return new Real[]{quotient(n),remainder(n)};
    }
  }

  // Credit for this Algorithm (modF) goes to Daan Leijen of the University of Utrecht. 
  // Proof (see page 5): https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
  public Real modulo(Real n) throws Exception {
    if(n instanceof Inexact || !isInteger() || !n.isInteger())
      return (new Inexact(doubleValue())).modulo(n);
    Exact d = (Exact)n;
    Real r = remainder(n);
    if((r.isPositive() && d.isNegative()) || (r.isNegative() && d.isPositive())) r = (Real)r.add(d);
    return r;
  }

  public Exact numerator() {
    return new Exact(num,BigInteger.ONE);
  }

  public Exact denominator() {
    return new Exact(den,BigInteger.ONE);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Modf 
  // @return: [integral, fractional]
  public Real[] modf() {
    return (new Inexact(doubleValue())).modf();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Integral & Fractional 
  // @return: integral
  public Exact integral() throws Exception {
    return (new Inexact(doubleValue())).integral();
  }

  // @return: fractional (as an integer)
  public Exact fractional() throws Exception {
    return (new Inexact(doubleValue())).fractional();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Common Denominator & Multiple Computations
  private void guarenteeGivenIntegers(String opName, Real r) throws Exception {
    if(isInteger() == false) 
      throw new Exceptionf("Can't perform (%s %s %s) with non-integer %s", opName, display(), r.display(), display());
    if(r.isInteger() == false) 
      throw new Exceptionf("Can't perform (%s %s %s) with non-integer %s", opName, display(), r.display(), r.display());
    if(isNegative()) 
      throw new Exceptionf("Can't perform (%s %s %s) with negative %s", opName, display(), r.display(), display());
    if(r.isNegative()) 
      throw new Exceptionf("Can't perform (%s %s %s) with non-integer %s", opName, display(), r.display(), r.display());
  }


  public Real gcd(Real r) throws Exception {
    guarenteeGivenIntegers("gcd",r);
    if(r instanceof Exact) {
      return new Exact(num.gcd(((Exact)r).num),BigInteger.ONE);
    } else {
      return r.gcd(this);
    }
  }


  public Real lcm(Real r) throws Exception {
    guarenteeGivenIntegers("lcm",r);
    Real gcd = null;
    if(r instanceof Exact) {
      gcd = new Exact(num.gcd(((Exact)r).num),BigInteger.ONE);
    } else {
      gcd = r.gcd(this);
    }
    Number result = mul(r).div(gcd);
    if(result instanceof Real) return (Real)result;
    throw new Exceptionf("Invalid Complex (lcm %s %s) result: %s", display(), r.display(), result.display());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Approximations
  public Real round() throws Exception {
    if(den.equals(BigInteger.ONE)) return this;
    return (new Inexact(doubleValue())).round();
  }

  public Real floor() throws Exception {
    if(den.equals(BigInteger.ONE)) return this;
    return (new Inexact(doubleValue())).floor();
  }

  public Real ceil() throws Exception {
    if(den.equals(BigInteger.ONE)) return this;
    return (new Inexact(doubleValue())).ceil();
  }

  public Real trunc() throws Exception {
    if(den.equals(BigInteger.ONE)) return this;
    return (new Inexact(doubleValue())).trunc();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Predicates
  public boolean isInteger() {
    return den.equals(BigInteger.ONE);
  }

  public boolean isInfinite() {
    return false;
  }

  public boolean isFinite() {
    return true;
  }

  public boolean isNaN() {
    return false;
  }

  public boolean isOdd() {
    try {
      if(den.equals(BigInteger.ONE)) return num.abs().remainder(BIGINT_TWO).equals(BigInteger.ONE);
    } catch(Exception e) {}
    return false;
  }

  public boolean isEven() {
    try {
      if(den.equals(BigInteger.ONE)) return num.remainder(BIGINT_TWO).equals(BigInteger.ZERO);
    } catch(Exception e) {}
    return false;
  }

  public boolean isPositive() {
    return num.signum() * den.signum() == 1;
  }

  public boolean isNegative() {
    return num.signum() * den.signum() == -1;
  }

  public boolean isZero() {
    return num.signum() == 0;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  public Inexact sin() {
    return (new Inexact(doubleValue())).sin();
  }

  public Inexact cos() {
    return (new Inexact(doubleValue())).cos();
  }

  public Inexact tan() {
    return (new Inexact(doubleValue())).tan();
  }


  public Number asin() {
    return (new Inexact(doubleValue())).asin();
  }

  public Number acos() {
    return (new Inexact(doubleValue())).acos();
  }

  public Inexact atan() {
    return (new Inexact(doubleValue())).atan();
  }

  public Inexact atan(Real n) {
    return (new Inexact(doubleValue())).atan(n);
  }


  public Inexact sinh() {
    return (new Inexact(doubleValue())).sinh();
  }

  public Inexact cosh() {
    return (new Inexact(doubleValue())).cosh();
  }

  public Inexact tanh() {
    return (new Inexact(doubleValue())).tanh();
  }


  public Inexact asinh() {
    return (new Inexact(doubleValue())).asinh();
  }

  public Number acosh() {
    return (new Inexact(doubleValue())).acosh();
  }

  public Number atanh() {
    return (new Inexact(doubleValue())).atanh();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Check
  public boolean isExact() {
    return true;
  }

  public boolean isInexact() {
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Conversion
  public Exact toExact() throws Exception {
    return this;
  }

  public Inexact toInexact() throws Exception {
    return new Inexact(this);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "exact";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(o instanceof Exact) {
      Exact e = (Exact)o;
      return num.multiply(e.den).equals(e.num.multiply(den));
    } else if(o instanceof Complex) {
      Complex c = (Complex)o;
      Real r = c.realPart();
      return r instanceof Exact && c.imagPart().isZero() && eq(r);
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),num,den);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    if(num.signum() == 0) return "0";
    BigInteger numer = null;
    BigInteger denom = null;
    if(den.signum() == -1) {
      numer = num.negate();
      denom = den.negate();
    } else {
      numer = num;
      denom = den;
    }
    if(denom.equals(BigInteger.ONE)) {
      return numer.toString();
    } else {
      return String.format("%s/%s", numer.toString(), denom.toString());
    }
  }


  public String toString(int radix) throws Exception {
    if(radix < Number.MIN_RADIX || radix > Number.MAX_RADIX)
      throw new Exceptionf("Can't print number %s with invalid radix %d (only %d-%d)", display(), radix, Number.MIN_RADIX, Number.MAX_RADIX);
    if(num.signum() == 0) return "0";
    BigInteger numer = null;
    BigInteger denom = null;
    if(den.signum() == -1) {
      numer = num.negate();
      denom = den.negate();
    } else {
      numer = num;
      denom = den;
    }
    if(denom.equals(BigInteger.ONE)) {
      return numer.toString(radix);
    } else {
      return String.format("%s/%s", numer.toString(radix), denom.toString(radix));
    }
  }
}