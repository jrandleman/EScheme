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
  private static final Exact ZERO = new Exact();
  private static final Exact ONE  = new Exact(1);
  private static final Exact TWO  = new Exact(2);
  private static final Exact HALF = new Exact(BigInteger.ONE,Exact.TWO);


  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private Real real = ZERO;
  private Real imag = ZERO;


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
    imag = new Inexact(0.0);
  }

  public Complex(Exact n) {
    real = n;
  }

  public Complex(Inexact n) {
    real = n;
    imag = new Inexact(0.0);
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
    return (Real)real.expt(TWO).add(imag.expt(TWO)).sqrt();
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
      Real resultImag = (Real)real.mul(c.imag).sub(imag.mul(c.real));
      return (new Complex(resultReal,resultImag)).unifyComponentExactness();
    } else {
      return (new Complex((Real)real.mul(n),((Real)imag.mul(n)).negate())).unifyComponentExactness();
    }
  }

  // (a+bi)/(c+di) = [(ac+bd)/(cc+dd)]+[(bc-ad)/(cc+dd)]i
  public Number div(Number n) {
    if(n instanceof Complex) {
      Complex c = (Complex)n;
      Real denom = (Real)c.real.mul(c.real).add(c.imag.mul(c.imag));
      Real resultReal = (Real)real.mul(c.real).add(imag.mul(c.imag)).div(denom);
      Real resultImag = (Real)imag.mul(c.real).sub(real.mul(c.imag)).div(denom);
      return (new Complex(resultReal,resultImag)).unifyComponentExactness();
    } else {
      Real denom = (Real)n.mul(n);
      return (new Complex((Real)real.mul(n).div(denom),(Real)imag.mul(n).div(denom))).unifyComponentExactness();
    }
  }


  // w**z = [cos(d*ln(r)+c*theta) + isin(d*ln(r)+c*theta)] * (r^c) * exp(-d * theta)
  // => w = rL(theta), z = c+di
  public Number expt(Number n) {
    Real r = magnitude();
    Real t = angle();
    Real c = null;
    Real d = null;
    if(n instanceof Complex) {
      Complex rhs = (Complex)n;
      c = rhs.real;
      d = rhs.imag;
    } else {
      c = (Real)n;
      if(n instanceof Inexact) {
        d = new Inexact();
      } else {
        d = ZERO;
      }
    }
    Real trigArg = (Real)d.mul(r.log()).add(c.mul(t));
    return (new Complex((Real)trigArg.cos(),(Real)trigArg.sin())).unifyComponentExactness().mul(r.expt(c).mul(d.negate().mul(t).exp()));
  }

  public Number exp() {
    return (new Complex(Math.exp(1.0))).expt(this);
  }

  // ln(z) = (1/2)ln(a^2 + b^2) + atan2(b,a)i
  // => z = a+bi
  public Number log() {
    Real newReal = (Real)HALF.mul(real.expt(TWO).add(imag.expt(TWO)).log());
    Real newImag = (Real)imag.atan(real);
    return (new Complex(newReal,newImag)).unifyComponentExactness();
  }

  public Number sqrt() {
    return expt(HALF);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  // sin(a+bi) = sin(a)cosh(b) + cos(a)sinh(b)i
  public Number sin() {
    return (new Complex((Real)real.sin().mul(imag.cosh()),(Real)real.cos().mul(imag.sinh()))).unifyComponentExactness();
  }
  
  // cos(a+bi) = cos(a)cosh(b) + sin(a)sinh(b)i
  public Number cos() {
    return (new Complex((Real)real.cos().mul(imag.cosh()),(Real)real.sin().mul(imag.sinh()))).unifyComponentExactness();
  }
  
  // tan(a+bi) = [sin(2a) + sinh(2b)i] / [cos(2a) + cosh(2b)]
  public Number tan() {
    Real realX2 = (Real)real.mul(TWO);
    Real imagX2 = (Real)imag.mul(TWO);
    return (new Complex((Real)realX2.sin(),(Real)imagX2.sinh())).unifyComponentExactness().div(realX2.cos().add(imagX2.cosh()));
  }


  // asin(z) = -i * ln(sqrt(1-z^2) + (z*i))
  private static final Complex NEGATIVE_I = new Complex(ZERO,ONE.negate());
  private static final Complex POSITIVE_I = new Complex(ZERO,ONE);
  public Number asin() {
    return NEGATIVE_I.mul(ONE.sub(expt(TWO)).sqrt().add(mul(POSITIVE_I)).log());
  }
  
  // acos(z) = (1/2)pi - asin(z)
  private static final Inexact HALF_PI = new Inexact(Math.acos(0));
  public Number acos() {
    return HALF_PI.sub(asin());
  }
  
  // atan(z) = (1/(2i))ln((i-z)/(i+z))
  private static final Number INVERSE_2PI = ONE.div(new Complex(ZERO,TWO));
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
    Real realX2 = (Real)real.mul(TWO);
    Real imagX2 = (Real)imag.mul(TWO);
    return (new Complex((Real)realX2.sinh(),(Real)imagX2.sin())).unifyComponentExactness().div(realX2.cosh().add(imagX2.cos()));
  }


  // asinh(z) = ln(z + sqrt(z^2 + 1))
  public Number asinh() {
    return add(expt(TWO).add(ONE).sqrt()).log();
  }
  
  // acosh(z) = ln(z + sqrt(z + 1) * sqrt(z - 1))
  public Number acosh() {
    return add(add(ONE).sqrt().mul(sub(ONE).sqrt())).log();
  }
  
  // atanh(z) = (1/2) * ln((1+z)/(1-z))
  public Number atanh() {
    return HALF.mul(add(ONE).div(ONE.sub(this)).log());
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
    if(radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
      throw new Exceptionf("Can't print number %s with invalid radix %d (only %d-%d)", display(), radix, Character.MIN_RADIX, Character.MAX_RADIX);
    if(imag.isNegative() == false) {
      return real.toString(radix)+"+"+imag.toString(radix)+"i";
    } else {
      return real.toString(radix)+imag.toString(radix)+"i";
    }
  }
}