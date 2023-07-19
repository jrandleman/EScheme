// Author: Jordan Randleman - escm.type.number.Real
// Purpose:
//    Real abstract class: base for <Exact> and <Inexact>.
//
// Guarentees:
//   - int intValue()
//   - long longValue()
//   - double doubleValue()
//
//   - boolean lt(Real n)
//   - boolean gt(Real n)
//   - boolean lte(Real n)
//   - boolean gte(Real n)
//
//   - Real negate()
//
//   - Real exptMod(Real p, Real m)
//
//   - Real abs()
//
//   - Real quotient(Real n)
//   - Real remainder(Real n)
//   - Real modulo(Real n)
//   - Real[] divrem(Real n)
//   - Real numerator()
//   - Real denominator()
//
//   - Real[] modf()
//
//   - Real gcd(Real r)
//   - Real lcm(Real r)
//
//   - Real round()
//   - Real floor()
//   - Real ceil()
//   - Real trunc()
//
//   - boolean isInteger()
//   - boolean isInfinite()
//   - boolean isFinite()
//   - boolean isNaN()
//   - boolean isOdd()
//   - boolean isEven()
//   - boolean isPositive()
//   - boolean isNegative()
//   - boolean isZero()
//
//   - Inexact sin()
//   - Inexact cos()
//   - Inexact tan()
//
//   - Number asin()
//   - Number acos()
//   - Inexact atan()
//
//   - Inexact sinh()
//   - Inexact cosh()
//   - Inexact tanh()
//
//   - Inexact asinh()
//   - Number acosh()
//   - Number atanh()
//
//   - Inexact atan(Real n)

package escm.type.number;

public abstract class Real extends Number {
  ////////////////////////////////////////////////////////////////////////////
  // Value Coercion
  public abstract int intValue();
  public abstract long longValue();
  public abstract double doubleValue();


  ////////////////////////////////////////////////////////////////////////////
  // Comparators
  public abstract boolean lt(Real n);
  public abstract boolean gt(Real n);
  public abstract boolean lte(Real n);
  public abstract boolean gte(Real n);


  ////////////////////////////////////////////////////////////////////////////
  // Negation
  public abstract Real negate();


  ////////////////////////////////////////////////////////////////////////////
  // Expt-modulo: this^n mod m
  public abstract Real exptMod(Real p, Real m) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Absolute Value
  public abstract Real abs();


  ////////////////////////////////////////////////////////////////////////////
  // Division Details
  public abstract Real quotient(Real n);
  public abstract Real remainder(Real n);
  public abstract Real modulo(Real n) throws Exception;
  
  public abstract Real[] divrem(Real n); // [quotient, remainder]

  public abstract Exact numerator() throws Exception;   // NaN, Infinity, -Infinity
  public abstract Exact denominator() throws Exception; // NaN, Infinity, -Infinity


  ////////////////////////////////////////////////////////////////////////////
  // Modf 
  // @return: [integral, 0.fractional]
  public abstract Real[] modf();


  ////////////////////////////////////////////////////////////////////////////
  // Fractional 
  // @return: [integral, fractional]
  public abstract Exact integral() throws Exception;
  public abstract Exact fractional() throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Common Denominator & Multiple Computations
  public abstract Real gcd(Real r) throws Exception;

  public abstract Real lcm(Real r) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Approximations
  public abstract Real round() throws Exception;
  public abstract Real floor() throws Exception;
  public abstract Real ceil()  throws Exception;
  public abstract Real trunc() throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Predicates
  public abstract boolean isInteger();
  public abstract boolean isInfinite();
  public abstract boolean isFinite();
  public abstract boolean isNaN();
  public abstract boolean isOdd();
  public abstract boolean isEven();
  public abstract boolean isPositive();
  public abstract boolean isNegative();
  public abstract boolean isZero();


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  public abstract Real sin();
  public abstract Real cos();
  public abstract Real tan();

  public abstract Number asin();
  public abstract Number acos();
  public abstract Real atan();

  public abstract Real sinh();
  public abstract Real cosh();
  public abstract Real tanh();

  public abstract Real asinh();
  public abstract Number acosh();
  public abstract Number atanh();

  public abstract Real atan(Real n);
}