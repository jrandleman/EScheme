// Author: Jordan Randleman - escm.type.number.Number
// Purpose:
//    Number abstract class: base for <Real> and <Complex>.
//
// Guarentees:
//
//   - static Number valueOf(CharSequence s, int radix) // Assumes no EScheme prefixes.
//   - static Number valueOf(CharSequence s)            // Also parses EScheme prefixes.
//     * EScheme prefixes: #e, #i, #b, #o, #x, #NNr
//       - In order: to-exact, to-inexact, binary, octal, hexadecimal, radices 2-36
//
//   - boolean eqs(Number n)
// 
//   - Number add(Number n)
//   - Number sub(Number n)
//   - Number mul(Number n)
//   - Number div(Number n)
//
//   - Number expt(Number n)
//   - Number exp()
//   - Number log()
//   - Number sqrt()
//
//   - Number sin()
//   - Number cos()
//   - Number tan()
//
//   - Number asin()
//   - Number acos()
//   - Number atan()
//
//   - Number sinh()
//   - Number cosh()
//   - Number tanh()
//
//   - Number asinh()
//   - Number acosh()
//   - Number atanh()
//
//   - boolean isExact()
//   - boolean isInexact()
//
//   - String toString()
//   - String toString(int radix)

package escm.type.number;
import java.math.BigInteger;
import java.nio.CharBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.vm.util.ExecutionState;

public abstract class Number extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Base Constants
  public static final int MIN_RADIX = Character.MIN_RADIX;
  public static final int BIN_RADIX = 2;
  public static final int OCT_RADIX = 8;
  public static final int DEC_RADIX = 10;
  public static final int HEX_RADIX = 16;
  public static final int MAX_RADIX = Character.MAX_RADIX;
  

  ////////////////////////////////////////////////////////////////////////////
  // Exact Patterns Array
  //   => Generate an array where idx <i> correlates to the pattern for an 
  //      exact real number of radix <i>
  private static final Pattern[] EXACT_PATTERNS = init_EXACT_PATTERNS();

  private static Pattern[] init_EXACT_PATTERNS() {
    Pattern[] pats = new Pattern[MAX_RADIX+1];
    for(int i = MIN_RADIX; i <= MAX_RADIX; ++i) {
      if(i <= DEC_RADIX) {
        char c = Character.forDigit(i-1,MAX_RADIX);
        pats[i] = Pattern.compile(String.format("([-+]?([0-%c]+(/[0-%c]+)?))", c, c));
      } else {
        char lower = Character.forDigit(i-1,MAX_RADIX);
        char upper = Character.toUpperCase(lower);
        pats[i] = Pattern.compile(String.format("([-+]?([0-9a-%cA-%c]+(/[0-9a-%cA-%c]+)?))", lower, upper, lower, upper));
      }
    }
    return pats;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Exact Complex Patterns Array
  //   => Generate an array where idx <i> correlates to the pattern for an 
  //      exact complex number of radix <i>
  private static final int EXACT_COMPLEX_REAL_GROUP_IDX = 2;
  
  private static final int EXACT_COMPLEX_IMAG_GROUP_IDX = 5;

  private static final Pattern[] EXACT_COMPLEX_PATTERNS = init_EXACT_COMPLEX_PATTERNS();

  private static Pattern[] init_EXACT_COMPLEX_PATTERNS() {
    Pattern[] pats = new Pattern[MAX_RADIX+1];
    for(int i = MIN_RADIX; i <= MAX_RADIX; ++i) {
      if(i <= DEC_RADIX) {
        char c = Character.forDigit(i-1,MAX_RADIX);
        pats[i] = Pattern.compile(String.format("(([-+]?([0-%c]+(/[0-%c]+)?))?([-+]([0-%c]+(/[0-%c]+)?)?)i)", c, c, c, c));
      } else {
        char lower = Character.forDigit(i-1,MAX_RADIX);
        char upper = Character.toUpperCase(lower);
        pats[i] = Pattern.compile(String.format("(([-+]?([0-9a-%cA-%c]+(/[0-9a-%cA-%c]+)?))?([-+]([0-9a-%cA-%c]+(/[0-9a-%cA-%c]+)?)?)i)", lower, upper, lower, upper, lower, upper, lower, upper));
      }
    }
    return pats;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Inexact Pattern (ONLY FOR RADIX 10!)
  private static final Pattern INEXACT_PATTERN = Pattern.compile("(([-+]?(((([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)|(Infinity)))|(NaN))");


  ////////////////////////////////////////////////////////////////////////////
  // Inexact Complex Pattern (ONLY FOR RADIX 10!)
  
  // Group idxs to parse: <inexact>?<inexact>i
  private static final int INEXACT_COMPLEX_II_REAL_GROUP_IDX = 4;
  private static final int INEXACT_COMPLEX_II_IMAG_GROUP_IDX = 13;

  // Group idxs to parse: <exact><inexact>i
  private static final int INEXACT_COMPLEX_EI_REAL_GROUP_IDX = 23;
  private static final int INEXACT_COMPLEX_EI_IMAG_GROUP_IDX = 26;

  // Group idxs to parse: <inexact><exact>i
  private static final int INEXACT_COMPLEX_IE_REAL_GROUP_IDX = 36;
  private static final int INEXACT_COMPLEX_IE_IMAG_GROUP_IDX = 45;

  private static final Pattern INEXACT_COMPLEX_PATTERN = Pattern.compile(
    "(("+ // <inexact>?<inexact>i
      "(([-+]?(((([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)|(Infinity)))?([-+](((([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)|(Infinity))))"+
      "|"+ // <exact><inexact>i
      "(([-+]?([0-9]+(/[0-9]+)?))([-+](((([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)|(Infinity))))"+
      "|"+ // <inexact><exact>i
      "(([-+]?(((([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+))([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+)|(Infinity)))([-+]([0-9]+(/[0-9]+)?)))"+
    ")i)"
  );


  ////////////////////////////////////////////////////////////////////////////
  // Number Type Identification
  public static boolean isValidNumberMatch(Matcher m, int n) {
    return m.find() && m.start() == 0 && m.end() == n;
  }


  // Returns <s> as a String back if so, <null> if not
  private static String isInexact(CharSequence s, int radix) {
    if(radix != DEC_RADIX) return null;
    Matcher m = INEXACT_PATTERN.matcher(s);
    if(isValidNumberMatch(m,s.length())) return s.toString();
    return null;
  }


  // Returns <s> as a String back if so, <null> if not
  private static String isExact(CharSequence s, int radix) {
    Matcher m = EXACT_PATTERNS[radix].matcher(s);
    if(isValidNumberMatch(m,s.length())) return s.toString();
    return null;
  }


  // Returns {realPart, imagPart} back if so, <null> if not
  private static escm.util.Pair<String,String> isInexactComplex(CharSequence s, int radix) {
    if(radix != DEC_RADIX) return null;
    Matcher m = INEXACT_COMPLEX_PATTERN.matcher(s);
    if(isValidNumberMatch(m,s.length())) {
      if(m.group(INEXACT_COMPLEX_II_IMAG_GROUP_IDX) != null) {
        return new escm.util.Pair<String,String>(m.group(INEXACT_COMPLEX_II_REAL_GROUP_IDX),m.group(INEXACT_COMPLEX_II_IMAG_GROUP_IDX));
      } else if(m.group(INEXACT_COMPLEX_EI_IMAG_GROUP_IDX) != null) {
        return new escm.util.Pair<String,String>(m.group(INEXACT_COMPLEX_EI_REAL_GROUP_IDX),m.group(INEXACT_COMPLEX_EI_IMAG_GROUP_IDX));
      } else {
        return new escm.util.Pair<String,String>(m.group(INEXACT_COMPLEX_IE_REAL_GROUP_IDX),m.group(INEXACT_COMPLEX_IE_IMAG_GROUP_IDX));
      }
    }
    return null;
  }


  // Returns {realPart, imagPart} back if so, <null> if not
  private static escm.util.Pair<String,String> isExactComplex(CharSequence s, int radix) {
    Matcher m = EXACT_COMPLEX_PATTERNS[radix].matcher(s);
    if(isValidNumberMatch(m,s.length())) 
      return new escm.util.Pair<String,String>(m.group(EXACT_COMPLEX_REAL_GROUP_IDX),m.group(EXACT_COMPLEX_IMAG_GROUP_IDX));
    return null;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Number Parsers
  private static final BigInteger NEGATIVE_ONE = BigInteger.ONE.negate();


  private static Inexact parseInexact(String s) throws Exception {
    if(s == null) return new Inexact();
    return new Inexact(Double.parseDouble(s));
  }


  private static Real parseExact(String s, int radix) throws Exception {
    if(s == null) return new Exact();
    if(s.contains("/")) {
      String[] fractionComponents = s.split("/");
      return (new Exact(new BigInteger(fractionComponents[0],radix),new BigInteger(fractionComponents[1],radix))).reduceToSimplestForm();
    } else {
      if(s.equals("+")) {
        return new Exact(BigInteger.ONE,BigInteger.ONE);
      } else if(s.equals("-")) {
        return new Exact(NEGATIVE_ONE,BigInteger.ONE);
      } else {
        return new Exact(new BigInteger(s,radix),BigInteger.ONE);
      }
    }
  }


  private static Real parseReal(String s) throws Exception {
    if(s == null) return new Exact();
    if(s.equals("Infinity") || s.equals("-Infinity"))
      return new Inexact(Double.parseDouble(s));
    for(int i = 0, n = s.length(); i < n; ++i) {
      char c = s.charAt(i);
      if(c=='.'||c=='e'||c=='E') {
        return new Inexact(Double.parseDouble(s));
      }
    }
    return parseExact(s,DEC_RADIX);
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Prefix Parsing (#e, #i, #b, #o, #x, #NNr)
  private static class EschemePrefix {
    public static final int DONT_COERCE    = 0;
    public static final int COERCE_EXACT   = 1;
    public static final int COERCE_INEXACT = 2;

    public int coerceCode       = DONT_COERCE;
    public int radix            = DEC_RADIX;
    public int indexAfterPrefix = 0;

    public EschemePrefix() {}
  }


  private static void parsePrefixInstance(EschemePrefix prefix, CharSequence s, int i, int n) throws Exception {
    if(i+2 >= n || s.charAt(i) != '#') return;
    char c = s.charAt(i+1);
    if(c=='e' || c=='E') {
      prefix.coerceCode = EschemePrefix.COERCE_EXACT;
      prefix.indexAfterPrefix = i+2;
    } else if(c=='i' || c=='I') {
      prefix.coerceCode = EschemePrefix.COERCE_INEXACT;
      prefix.indexAfterPrefix = i+2;
    } else if(c=='b' || c=='B') {
      prefix.radix = BIN_RADIX;
      prefix.indexAfterPrefix = i+2;
    } else if(c=='o' || c=='O') {
      prefix.radix = OCT_RADIX;
      prefix.indexAfterPrefix = i+2;
    } else if(c=='x' || c=='X') {
      prefix.radix = HEX_RADIX;
      prefix.indexAfterPrefix = i+2;
    } else if(c >= '1' && c <= '9') {
      char c2 = s.charAt(i+2);
      if(i+3 < n && c2=='r') {
        prefix.radix = c-'0';
        prefix.indexAfterPrefix = i+3;
      } else if(i+4 < n && c2 >= '0' && c2 <= '9' && s.charAt(i+3)=='r') {
        prefix.radix = ((c-'0')*10) + (c2-'0');
        prefix.indexAfterPrefix = i+4;
      }
    }
    if(prefix.indexAfterPrefix >= n)
      throw new Exceptionf("Can't convert \"%s\" to a radix-%d number!", s, prefix.radix);
  }


  private static EschemePrefix parsePrefixes(CharSequence s) throws Exception {
    EschemePrefix prefix = new EschemePrefix();
    int n = s.length();
    parsePrefixInstance(prefix,s,0,n);
    if(prefix.indexAfterPrefix == 0) return prefix;
    parsePrefixInstance(prefix,s,prefix.indexAfterPrefix,n);
    return prefix;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main Standard Parser Dispatch
  public static Number valueOf(CharSequence s, int radix) throws Exception {
    if(s == null) 
      throw new Exceptionf("Can't parse null string!");
    
    if(radix < MIN_RADIX || radix > MAX_RADIX) 
      throw new Exceptionf("Invalid radix %d (only supports %d-%d)!", radix, MIN_RADIX, MAX_RADIX);
    
    String inexactResult = isInexact(s,radix);
    if(inexactResult != null) return parseInexact(inexactResult);

    String exactResult = isExact(s,radix);
    if(exactResult != null) return parseExact(exactResult,radix);

    escm.util.Pair<String,String> inexactComplexResult = isInexactComplex(s,radix);
    if(inexactComplexResult != null)
      return (new Complex(parseReal(inexactComplexResult.first), parseReal(inexactComplexResult.second))).unifyComponentExactness();

    escm.util.Pair<String,String> exactComplexResult = isExactComplex(s,radix);
    if(exactComplexResult != null)
      return (new Complex(parseExact(exactComplexResult.first,radix), parseExact(exactComplexResult.second,radix))).unifyComponentExactness();
    
    throw new Exceptionf("Can't convert \"%s\" to a radix-%d number!", s, radix);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main EScheme Parser Dispatch
  public static Number valueOf(CharSequence s) throws Exception {
    if(s == null) throw new Exceptionf("Can't parse null string!");
    if(s.length() == 0) throw new Exceptionf("Can't parse empty string!");
    EschemePrefix prefix = parsePrefixes(s);
    Number result = valueOf(CharBuffer.wrap(s,prefix.indexAfterPrefix,s.length()),prefix.radix);
    switch(prefix.coerceCode) {
      case EschemePrefix.COERCE_EXACT: {
        if(result instanceof Exact) {
          return result;
        } else if(result instanceof Inexact) {
          return new Exact((Inexact)result);
        } else { // if(result instanceof Complex)
          return ((Complex)result).asExact();
        }
      }
      case EschemePrefix.COERCE_INEXACT: {
        if(result instanceof Exact) {
          return new Inexact((Exact)result);
        } else if(result instanceof Inexact) {
          return result;
        } else { // if(result instanceof Complex)
          return ((Complex)result).asInexact();
        }
      } 
      default: {
        return result;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Comparator(s)
  public abstract boolean eqs(Number n);


  ////////////////////////////////////////////////////////////////////////////
  // Arithmetic
  public abstract Number add(Number n);
  public abstract Number sub(Number n);
  public abstract Number mul(Number n);
  public abstract Number div(Number n);

  public abstract Number expt(Number n);
  public abstract Number exp();
  public abstract Number log();
  public abstract Number sqrt();


  ////////////////////////////////////////////////////////////////////////////
  // Trigonometry
  public abstract Number sin();
  public abstract Number cos();
  public abstract Number tan();

  public abstract Number asin();
  public abstract Number acos();
  public abstract Number atan();

  public abstract Number sinh();
  public abstract Number cosh();
  public abstract Number tanh();

  public abstract Number asinh();
  public abstract Number acosh();
  public abstract Number atanh();


  ////////////////////////////////////////////////////////////////////////////
  // Exactness Check
  public abstract boolean isExact();

  public abstract boolean isInexact();


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public abstract String type();


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean eq(Object o);

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public abstract int hashCode();


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract String display();

  public String write() {
    return display();
  }

  public String pprint() {
    return write();
  }

  public abstract String toString(int radix) throws Exception;


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Number loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Number loadWithName(String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Number copy() {
    return this;
  }
}