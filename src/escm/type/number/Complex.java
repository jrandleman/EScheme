// Author: Jordan Randleman - escm.type.number.Complex
// Purpose:
//    Complex class: holds 2 <escm.type.number.Real>s, must be BOTH exact or inexact.
//
// Guarentees:
//   - static Number makeRectangular(Real real, Real imaginary)
//   - static Number makePolar(Real magnitude, Real angle)
//
//   - Real realPart()
//   - Real imagPart()
//
//   - Real magnitude()
//   - Real angle()
//
//   - Complex conjugate()
//
//   - Number asExact()   // return <this> with both components as Exact
//   - Number asInexact() // return <this> with both components as Inexact

package escm.type.number;
import java.util.Objects;
import java.math.BigInteger;
import escm.util.Exceptionf;

public class Complex extends Number {
  ////////////////////////////////////////////////////////////////////////////
  // Static Factories
  static public Number makeRectangular(Real real, Real imaginary) {
    if(real instanceof Inexact) {
      if(imaginary instanceof Inexact) {
        return (new Complex(real,imaginary)).unifyComponentExactness();
      } else {
        return (new Complex(real,new Inexact((Exact)imaginary))).unifyComponentExactness();
      }
    } else {
      if(imaginary instanceof Inexact) {
        return (new Complex(new Inexact((Exact)real),imaginary)).unifyComponentExactness();
      } else {
        return (new Complex(real,imaginary)).unifyComponentExactness();
      }
    }
  }

  static public Number makePolar(Real magnitude, Real angle) {
    return (new Complex((Real)magnitude.mul(angle.cos()),(Real)magnitude.mul(angle.sin()))).unifyComponentExactness();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Static Constants
  private static final Exact EXACT_TWO = new Exact(2);


  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private Real real = Exact.ZERO;
  private Real imag = Exact.ZERO;


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Complex() {}

  public Complex(int n) {
    real = new Exact(n);
  }

  public Complex(long n) {
    real = new Exact(n);
  }

  public Complex(double n) {
    real = new Inexact(n);
    imag = new Inexact();
  }

  public Complex(Exact n) {
    real = n;
  }

  public Complex(Inexact n) {
    real = n;
    imag = new Inexact();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Getters
  Complex(Real real, Real imag) {
    this.real = real;
    this.imag = imag;
  }


  public Real realPart() {
    return real;
  }

  public Real imagPart() {
    return imag;
  }

  public Real magnitude() {
    return (Real)real.mul(real).add(imag.mul(imag)).sqrt();
  }

  public Real angle() {
    return imag.atan(real);
  }

  public Complex conjugate() {
    return new Complex(real,imag.negate());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Coercion
  public Number asExact() throws Exception {
    if(real instanceof Exact) return this;
    return (new Complex(new Exact((Inexact)real), new Exact((Inexact)imag))).unifyComponentExactness();
  }

  public Number asInexact() throws Exception {
    if(real instanceof Inexact) return this;
    return (new Complex(new Inexact((Exact)real), new Inexact((Exact)imag))).unifyComponentExactness();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Reduction to Simplest Form
  Number unifyComponentExactness() {
    if(real.isNaN() || imag.isNaN()) return Inexact.NAN;
    if(imag.isZero()) return real;
    if(real instanceof Inexact) {
      if(imag instanceof Inexact) return this;
      return new Complex(real,new Inexact((Exact)imag));
    } else if(imag instanceof Inexact) {
      return new Complex(new Inexact((Exact)real),imag);
    } else {
      return this;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Comparator(s)
  public boolean eqs(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      return real.eqs(c.real) && imag.eqs(c.imag);
    } else if(n instanceof Inexact) {
      return real.eqs((Inexact)n) && imag.isZero();
    } else { //  if(n instanceof Exact)
      return real.eqs((Exact)n) && imag.isZero();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Arithmetic
  public Number add(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      return (new Complex((Real)real.add(c.real),(Real)imag.add(c.imag))).unifyComponentExactness();
    } else {
      return (new Complex((Real)real.add(n),imag)).unifyComponentExactness();
    }
  }

  public Number sub(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      return (new Complex((Real)real.sub(c.real),(Real)imag.sub(c.imag))).unifyComponentExactness();
    } else {
      return (new Complex((Real)real.sub(n),imag)).unifyComponentExactness();
    }
  }

  // (x+yi)(u+vi) = (xu-yv)+(xv+yu)i
  public Number mul(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      Real resultReal = (Real)real.mul(c.real).sub(imag.mul(c.imag));
      Real resultImag = (Real)real.mul(c.imag).add(imag.mul(c.real));
      return (new Complex(resultReal,resultImag)).unifyComponentExactness();
    } else {
      return (new Complex((Real)real.mul(n),(Real)imag.mul(n))).unifyComponentExactness();
    }
  }
  // (a+bi)/(c+di) = [(ac+bd)/(cc+dd)]+[(bc-ad)/(cc+dd)]i
  // (a+bi)/(c+0i) = (a/c)+(b/c)i
  public Number div(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      Real denom = (Real)c.real.mul(c.real).add(c.imag.mul(c.imag));
      Real resultReal = (Real)real.mul(c.real).add(imag.mul(c.imag)).div(denom);
      Real resultImag = (Real)imag.mul(c.real).sub(real.mul(c.imag)).div(denom);
      return (new Complex(resultReal,resultImag)).unifyComponentExactness();
    } else {
      return (new Complex((Real)real.div(n),(Real)imag.div(n))).unifyComponentExactness();
    }
  }

  // w**z = exp(z*log(w))
  // From: https://math.stackexchange.com/questions/476968/complex-power-of-a-complex-number
  // n^0 = 1
  // 0^+n = 1
  // 0^-n = Infinity
  // [+-]1^[+-]Infinity = 1
  public Number expt(Number n) {
    boolean nIsReal = n instanceof Real;
    if(nIsReal && ((Real)n).isZero()) {
      if (n instanceof Exact) return Exact.ONE;
      return Inexact.ONE;
    }
    if(real.isZero() && imag.isZero()) {
      if(nIsReal) {
        if(((Real)n).isNegative()) return Inexact.POSITIVE_INFINITY;
        if(n instanceof Exact) return Exact.ZERO;
        return Inexact.ZERO;
      }
      if(real instanceof Exact) return Exact.ZERO;
      return Inexact.ZERO;
    }
    Real absReal = real.abs();
    if(absReal.eqs(Inexact.ONE) && imag.isZero() && nIsReal && ((Real)n).isInfinite()) return absReal;
    return n.mul(log()).exp();
  }

  // e^(a+bi) = (e^a)*(cos(b)+sin(b)i)
  // From: https://math.stackexchange.com/questions/142610/expressing-ez-where-z-abi-in-polar-form
  public Number exp() {
    return real.exp().mul((new Complex(imag.cos(),imag.sin())).unifyComponentExactness());
  }

  // ln(z) = ln(r)+(theta)i
  // => z = rL(theta)
  // From: https://en.wikipedia.org/wiki/Complex_logarithm
  public Number log() {
    return (new Complex((Real)magnitude().log(),angle())).unifyComponentExactness();
  }

  private static boolean isPositiveInfinity(Real n) {
    return n.isInfinite() && n.isPositive();
  }

  private static boolean isNegativeInfinity(Real n) {
    return n.isInfinite() && n.isNegative();
  }

  private static Number complexSqrtAdd(Real a, Real b) {
    if(isPositiveInfinity(a) && isNegativeInfinity(b)) return Exact.ZERO;
    if(isNegativeInfinity(a) && isPositiveInfinity(b)) return Exact.ZERO;
    return a.add(b);
  }

  private static Number complexSqrtSub(Real a, Real b) {
    if(isPositiveInfinity(a) && isPositiveInfinity(b)) return Exact.ZERO;
    if(isNegativeInfinity(a) && isNegativeInfinity(b)) return Exact.ZERO;
    return a.sub(b);
  }

  // sqrt(z) = sqrt((mag(z)+a)/2)+(b/abs(b))*sqrt((mag(z)-a)/2)i
  // => z = a+bi
  // From: https://www.cuemath.com/algebra/square-root-of-complex-number/
  public Number sqrt() {
    if(real.isInfinite() && imag.isInfinite()) return Inexact.NAN;
    Real mag = magnitude();
    Real sqrtReal = (Real)complexSqrtAdd(mag,real).div(EXACT_TWO).sqrt();
    Real sqrtImag = (Real)complexSqrtSub(mag,real).div(EXACT_TWO).sqrt();
    if(imag.isNegative()) sqrtImag = sqrtImag.negate();
    return (new Complex(sqrtReal,sqrtImag)).unifyComponentExactness();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  // sin(a+bi) = sin(a)cosh(b) + cos(a)sinh(b)i
  public Number sin() {
    return (new Complex((Real)real.sin().mul(imag.cosh()),(Real)real.cos().mul(imag.sinh()))).unifyComponentExactness();
  }
  
  // cos(a+bi) = cos(a)cosh(b) - sin(a)sinh(b)i
  public Number cos() {
    return (new Complex((Real)real.cos().mul(imag.cosh()),((Real)real.sin().mul(imag.sinh())).negate())).unifyComponentExactness();
  }
  
  // tan(a+bi) = [sin(2a) + sinh(2b)i] / [cos(2a) + cosh(2b)]
  public Number tan() {
    Real realX2 = (Real)real.mul(EXACT_TWO);
    Real imagX2 = (Real)imag.mul(EXACT_TWO);
    return (new Complex((Real)realX2.sin(),(Real)imagX2.sinh())).unifyComponentExactness().div(realX2.cos().add(imagX2.cosh()));
  }


  // asin(z) = -i * ln(sqrt(1-z^2) + (z*i))
  private static final Complex NEGATIVE_I = new Complex(Exact.ZERO,Exact.ONE.negate());
  private static final Complex POSITIVE_I = new Complex(Exact.ZERO,Exact.ONE);
  public Number asin() {
    return NEGATIVE_I.mul(Exact.ONE.sub(mul(this)).sqrt().add(mul(POSITIVE_I)).log());
  }
  
  // acos(z) = (1/2)pi - asin(z)
  private static final Inexact HALF_PI = new Inexact(Math.acos(0));
  public Number acos() {
    return HALF_PI.sub(asin());
  }
  
  // atan(z) = (1/(2i))ln((i-z)/(i+z))
  private static final Number INVERSE_2PI = Exact.ONE.div(new Complex(Exact.ZERO,EXACT_TWO));
  public Number atan() {
    return INVERSE_2PI.mul(POSITIVE_I.sub(this).div(POSITIVE_I.add(this)).log());
  }


  // sinh(a+bi) = sinh(a)cos(b) + cosh(a)sin(b)i
  public Number sinh() {
    return (new Complex((Real)real.sinh().mul(imag.cos()),(Real)real.cosh().mul(imag.sin()))).unifyComponentExactness();
  }
  
  // cosh(a+bi) = cosh(a)cos(b) + sinh(a)sin(b)i
  public Number cosh() {
    return (new Complex((Real)real.cosh().mul(imag.cos()),(Real)real.sinh().mul(imag.sin()))).unifyComponentExactness();
  }
  
  // tanh(a+bi) = [sinh(2a) + sin(2b)i] / [cosh(2a) + cos(2b)]
  public Number tanh() {
    Real realX2 = (Real)real.mul(EXACT_TWO);
    Real imagX2 = (Real)imag.mul(EXACT_TWO);
    return (new Complex((Real)realX2.sinh(),(Real)imagX2.sin())).unifyComponentExactness().div(realX2.cosh().add(imagX2.cos()));
  }


  // asinh(z) = ln(z + sqrt(z^2 + 1))
  public Number asinh() {
    return mul(this).add(Exact.ONE).sqrt().add(this).log();
  }
  
  // acosh(z) = ln(z + sqrt(z + 1) * sqrt(z - 1))
  public Number acosh() {
    return add(add(Exact.ONE).sqrt().mul(sub(Exact.ONE).sqrt())).log();
  }
  
  // atanh(z) = (1/2) * ln((1+z)/(1-z))
  public Number atanh() {
    return Exact.HALF.mul(add(Exact.ONE).div(Exact.ONE.sub(this)).log());
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Check
  public boolean isExact() {
    return real instanceof Exact;
  }

  public boolean isInexact() {
    return real instanceof Inexact;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Conversion
  public Number toExact() throws Exception {
    return asExact();
  }

  public Number toInexact() throws Exception {
    return asInexact();
  }
  

  ////////////////////////////////////////////////////////////////////////////
  // Type
  public String type() {
    return "complex";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    if(o instanceof Complex) {
      Complex c = (Complex)o;
      return real.eq(c.real) && imag.eq(c.imag);
    } else if(o instanceof Inexact) {
      return real.eq((Inexact)o) && imag.isZero();
    } else if(o instanceof Exact) {
      return real.eq((Exact)o) && imag.isZero();
    }
    return false;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),real,imag);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String display() {
    if(imag.isNegative() == false) {
      return real.display()+"+"+imag.display()+"i";
    } else {
      return real.display()+imag.display()+"i";
    }
  }


  public String toString(int radix) throws Exception {
    if(radix < Number.MIN_RADIX || radix > Number.MAX_RADIX)
      throw new Exceptionf("Can't print number %s with invalid radix %d (only %d-%d)", display(), radix, Number.MIN_RADIX, Number.MAX_RADIX);
    if(imag.isNegative() == false) {
      return real.toString(radix)+"+"+imag.toString(radix)+"i";
    } else {
      return real.toString(radix)+imag.toString(radix)+"i";
    }
  }
}