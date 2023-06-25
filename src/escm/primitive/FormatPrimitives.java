// Author: Jordan Randleman - escm.primitive.FormatPrimitives
// Purpose:
//    Java <stringf> primitive to create formatted strings.

package escm.primitive;
import java.util.ArrayList;
import java.nio.CharBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Vector;
import escm.type.Hashmap;
import escm.type.number.Complex;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.number.Inexact;
import escm.type.number.Number;
import escm.util.Exceptionf;
import escm.vm.type.Primitive;

public class FormatPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // stringf
  public static class Stringf extends Primitive {
    public java.lang.String escmName() {
      return "stringf";
    }


    private static final Pattern STRING_PATTERN = Pattern.compile("-?\\d((\\d)|(\\d\\d))?s");

    // Sign, Commas, E-Coercion, Radix, Padding (SCERP)
    private static final Pattern SCERP_NUMBER_PATTERN = Pattern.compile("\\+?,?(E|e)?((\\d|\\d\\d)(r|R))?(\\d|\\d\\d)?n");

    // Sign, Commas, I-Coercion, Padding, Precision (SCIPP)
    private static final Pattern SCIPP_NUMBER_PATTERN = Pattern.compile("\\+?,?(I|i)?(\\d|\\d\\d)?(\\.(\\d|\\d\\d))?n");

    public static final String FORMAT_OPTIONS =  "\n>> <format-string> is like Java's printf with unique formatting patterns:\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %a = display anything\n"+
                                                  "   %wa = write anything\n"+
                                                  "   %pa = pretty-print anything\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %... = display unpacked list/vector/hashmap\n"+
                                                  "   %w... = write unpacked list/vector/hashmap\n"+
                                                  "   %p... = pretty-print unpacked list/vector/hashmap\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %n = number\n"+
                                                  "   %+n = number (show sign if positive too)\n"+
                                                  "   %,n = number with commas\n"+
                                                  "   %En = %en = number (coerced to exact)\n"+
                                                  "   %In = %in = number (coerced to inexact)\n"+
                                                  "   %#rn = %#Rn = number (in radix <#> from 2 to 36)\n"+
                                                  "   %#n = number (left-padded with 0s to a width of <#> characters)\n"+
                                                  "   %.#n = number (with <#> digits of precision)\n"+
                                                  "   -> IE \"%+e2rn\": make exact in binary with sign\n"+
                                                  "   -> NOTE: 1) 0-padding & precision MUST be of 2 digits or less!\n"+
                                                  "            2) Can't have radix with I-coercion or precision!\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %$ = display real finite as a dollar value\n"+
                                                  "   %,$ = display real finite as a dollar value seperated by commas\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %s = display string\n"+
                                                  "   %#s = display string & pad left with # spaces\n"+
                                                  "   %-#s = display string & pad right with # spaces\n"+
                                                  "   %ws = write string\n"+
                                                  "   -> NOTE: padding MUST be of 3 digits or less (ie from -999 to 999)\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %b  = bool\n"+
                                                  "   %wb = write \"true\" or \"false\" instead of \"#t\" or \"#f\"\n"+
                                                  "   ----------------------------------------------------------------------\n"+
                                                  "   %%  = \"%\" (escapes a \"%\")\n"+
                                                  "   ----------------------------------------------------------------------";


    public static class StringfException extends Exception {
      public StringfException(String fmt, Object ... args) {
        super(String.format(fmt,args)+FORMAT_OPTIONS);
      }
    }


    private static String profileFormat(String fmtStr, ArrayList<Datum> args) {
      ArrayList<Datum> parameters = new ArrayList<Datum>();
      parameters.add(new escm.type.String(fmtStr));
      parameters.addAll(args);
      return Exceptionf.profileArgs(parameters);
    }


    private static String addIntegerCommas(String number) {
      int n = number.length();
      StringBuilder sb = new StringBuilder();
      for(int i = 0; i < n; ++i) {
        if((n-i) < n && (n-i) % 3 == 0) sb.append(',');
        sb.append(number.charAt(i));
      }
      return sb.toString();
    }


    private static String addRealNumberCommas(String number) {
      if(number.indexOf('.') != -1) {
        String[] components = number.split("\\.");
        return addIntegerCommas(components[0])+"."+components[1];
      } else if(number.indexOf('/') != -1) {
        String[] components = number.split("/");
        return addIntegerCommas(components[0])+"/"+addIntegerCommas(components[1]);
      } else {
        return addIntegerCommas(number);
      }
    }


    private static String paddZeros(String number, int padding) {
      int n = number.length();
      if(n >= padding) return number;
      StringBuilder sb = new StringBuilder();
      for(int i = padding-n; i > 0; --i) sb.append('0');
      sb.append(number);
      return sb.toString();
    }


    private static String serializeNumberWithRadix(Real r, int radix) {
      try {
        return r.toString(radix);
      } catch(Exception e) {
        return r.toString();
      }
    }


    private static String serializeNumberWithPrecision(Real r, int precision) {
      if(precision == -1) return r.toString();
      return String.format("%."+String.valueOf(precision)+"f",((Real)r).doubleValue());
    }


    private static boolean isSpacePaddedString(String fmtStr, int i, int n) {
      if(i+5 >= n) {
        Matcher m = STRING_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,n));
        return m.find() && m.start() == 0;
      } else {
        Matcher m = STRING_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,i+5));
        return m.find() && m.start() == 0;
      }
    }


    // Sign, Commas, E-Coercion, Radix, Padding (SCERP)
    private static boolean isScerpNumber(String fmtStr, int i, int n) {
      if(i+9 >= n) {
        Matcher m = SCERP_NUMBER_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,n));
        return m.find() && m.start() == 0;
      } else {
        Matcher m = SCERP_NUMBER_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,i+9));
        return m.find() && m.start() == 0;
      }
    }


    // Sign, Commas, I-Coercion, Padding, Precision (SCIPP)
    private static boolean isScippNumber(String fmtStr, int i, int n) {
      if(i+9 >= n) {
        Matcher m = SCIPP_NUMBER_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,n));
        return m.find() && m.start() == 0;
      } else {
        Matcher m = SCIPP_NUMBER_PATTERN.matcher(CharBuffer.wrap(fmtStr,i,i+9));
        return m.find() && m.start() == 0;
      }
    }


    // Parses %-?##?#?s
    private static int displayPaddedStringToken(String fmtStr, int i, StringBuilder sb, String fmtArg) throws Exception {
      boolean paddingRight = fmtStr.charAt(i) == '-';
      if(paddingRight) ++i;
      StringBuilder padding = new StringBuilder();
      while(Character.isDigit(fmtStr.charAt(i))) padding.append(fmtStr.charAt(i++));
      String paddingStr = padding.toString();
      if(Integer.parseInt(paddingStr) == 0) {
        sb.append(fmtArg);
      } else if(paddingRight) {
        sb.append(String.format("%1$-"+paddingStr+"s",fmtArg));
      } else {
        sb.append(String.format("%1$"+paddingStr+"s",fmtArg));
      }
      return i;
    }


    // Sign, Commas, E-Coercion, Radix, Padding (SCERP)
    private static int displayScerpNumberToken(String fmtStr, int i, StringBuilder sb, Number fmtArg, ArrayList<Datum> args, String signature) throws Exception {
      // Parse the prefix
      int start = i;
      boolean signs = fmtStr.charAt(i) == '+';
      if(signs) ++i;
      boolean commas = fmtStr.charAt(i) == ',';
      if(commas) ++i;
      boolean coerceExact = fmtStr.charAt(i) == 'E' || fmtStr.charAt(i) == 'e';
      if(coerceExact) ++i;
      int radix = Number.DEC_RADIX;
      if((Character.isDigit(fmtStr.charAt(i)) && (fmtStr.charAt(i+1) == 'r' || fmtStr.charAt(i+1) == 'R')) || 
         (Character.isDigit(fmtStr.charAt(i)) && Character.isDigit(fmtStr.charAt(i+1)) && (fmtStr.charAt(i+2) == 'r' || fmtStr.charAt(i+2) == 'R'))) {
        if(Character.isDigit(fmtStr.charAt(i+1))) {
          radix = (10 * (fmtStr.charAt(i)-'0')) + (fmtStr.charAt(i+1)-'0');
          i += 3;
        } else {
          radix = fmtStr.charAt(i)-'0';
          i += 2;
        }
        if(radix < Number.MIN_RADIX || radix > Number.MAX_RADIX) {
          StringBuilder token = new StringBuilder();
          while(fmtStr.charAt(start) != 'n') token.append(fmtStr.charAt(start++));
          throw new StringfException("'%s Invalid radix in token %%%sn (only supports %d-%d): %s", signature, token, Number.MIN_RADIX, Number.MAX_RADIX, profileFormat(fmtStr,args));
        }
      }
      int leftPadding = 0;
      if(Character.isDigit(fmtStr.charAt(i))) {
        if(Character.isDigit(fmtStr.charAt(i+1))) {
          leftPadding = (10 * (fmtStr.charAt(i)-'0')) + (fmtStr.charAt(i+1)-'0');
          i += 2;
        } else {
          leftPadding = fmtStr.charAt(i++)-'0';
        }
      }
      // Display the number
      if(signs && ((fmtArg instanceof Complex && ((Complex)fmtArg).realPart().isPositive()) || (fmtArg instanceof Real && ((Real)fmtArg).isPositive()))) {
        sb.append('+');
      }
      if(coerceExact && !(fmtArg instanceof Exact)) {
        if(fmtArg instanceof Inexact) {
          fmtArg = new Exact((Inexact)fmtArg);
        } else if(((Complex)fmtArg).realPart() instanceof Inexact) {
          fmtArg = ((Complex)fmtArg).asExact();
        }
      }
      if(fmtArg instanceof Complex) {
        StringBuilder complexBuffer = new StringBuilder();
        Complex c = (Complex)fmtArg;
        Real real = c.realPart();
        Real imag = c.imagPart();
        if(commas) {
          complexBuffer.append(addRealNumberCommas(serializeNumberWithRadix(real,radix)));
          if(imag.isPositive()) complexBuffer.append('+');
          complexBuffer.append(addRealNumberCommas(serializeNumberWithRadix(imag,radix)));
        } else {
          complexBuffer.append(serializeNumberWithRadix(real,radix));
          if(imag.isPositive()) complexBuffer.append('+');
          complexBuffer.append(serializeNumberWithRadix(imag,radix));
        }
        complexBuffer.append('i');
        sb.append(paddZeros(complexBuffer.toString(),leftPadding));
      } else {
        if(commas) {
          sb.append(paddZeros(addRealNumberCommas(serializeNumberWithRadix((Real)fmtArg,radix)),leftPadding));
        } else {
          sb.append(paddZeros(serializeNumberWithRadix((Real)fmtArg,radix),leftPadding));
        }
      }
      return i;
    }


    // Sign, Commas, I-Coercion, Padding, Precision (SCIPP)
    private static int displayScippNumberToken(String fmtStr, int i, StringBuilder sb, Number fmtArg, ArrayList<Datum> args) throws Exception {
      // Parse the prefix
      int start = i;
      boolean signs = fmtStr.charAt(i) == '+';
      if(signs) ++i;
      boolean commas = fmtStr.charAt(i) == ',';
      if(commas) ++i;
      boolean coerceInxact = fmtStr.charAt(i) == 'I' || fmtStr.charAt(i) == 'i';
      if(coerceInxact) ++i;
      int leftPadding = 0;
      if(Character.isDigit(fmtStr.charAt(i))) {
        if(Character.isDigit(fmtStr.charAt(i+1))) {
          leftPadding = (10 * (fmtStr.charAt(i)-'0')) + (fmtStr.charAt(i+1)-'0');
          i += 2;
        } else {
          leftPadding = fmtStr.charAt(i++)-'0';
        }
      }
      int precision = -1; // -1 denotes to use the default precision
      if(fmtStr.charAt(i) == '.') {
        ++i;
        if(Character.isDigit(fmtStr.charAt(i+1))) {
          precision = (10 * (fmtStr.charAt(i)-'0')) + (fmtStr.charAt(i+1)-'0');
          i += 2;
        } else {
          precision = fmtStr.charAt(i++)-'0';
        }
      }
      // Display the number
      if(signs && ((fmtArg instanceof Complex && ((Complex)fmtArg).realPart().isPositive()) || (fmtArg instanceof Real && ((Real)fmtArg).isPositive()))) {
        sb.append('+');
      }
      if(coerceInxact && !(fmtArg instanceof Inexact)) {
        if(fmtArg instanceof Exact) {
          fmtArg = new Inexact((Exact)fmtArg);
        } else if(((Complex)fmtArg).realPart() instanceof Exact) {
          fmtArg = ((Complex)fmtArg).asInexact();
        }
      }
      if(fmtArg instanceof Complex) {
        StringBuilder complexBuffer = new StringBuilder();
        Complex c = (Complex)fmtArg;
        Real real = c.realPart();
        Real imag = c.imagPart();
        if(commas) {
          complexBuffer.append(addRealNumberCommas(serializeNumberWithPrecision(real,precision)));
          if(imag.isPositive()) complexBuffer.append('+');
          complexBuffer.append(addRealNumberCommas(serializeNumberWithPrecision(imag,precision)));
        } else {
          complexBuffer.append(serializeNumberWithPrecision(real,precision));
          if(imag.isPositive()) complexBuffer.append('+');
          complexBuffer.append(serializeNumberWithPrecision(imag,precision));
        }
        complexBuffer.append('i');
        sb.append(paddZeros(complexBuffer.toString(),leftPadding));
      } else {
        if(commas) {
          sb.append(paddZeros(addRealNumberCommas(serializeNumberWithPrecision((Real)fmtArg,precision)),leftPadding));
        } else {
          sb.append(paddZeros(serializeNumberWithPrecision((Real)fmtArg,precision),leftPadding));
        }
      }
      return i;
    }


    public static String logic(String fmtStr, ArrayList<Datum> args, String signature) throws Exception {
      StringBuilder sb = new StringBuilder();
      int totalArgs = args.size();
      int argIndex = 0;
      for(int i = 0, n = fmtStr.length(); i < n; ++i) {
        char percentChar = fmtStr.charAt(i);
        // Non-token
        if(percentChar != '%') {
          sb.append(percentChar);
          continue;
        }
        // Incomplete token
        if(++i == n) {
          throw new StringfException("'%s can't terminate string with '%': %s", signature, profileFormat(fmtStr,args));
        }
        // '%' literal
        if(fmtStr.charAt(i) == '%') {
          sb.append('%');
          continue;
        }
        // Parsing an argument
        if(argIndex == totalArgs) {
          throw new StringfException("'%s insufficient <arg>s for <format-string>: %s", signature, profileFormat(fmtStr,args));
        }
        Datum fmtArg = args.get(argIndex++);
        // Scrape the largest possible token: "+,E36r99n" or "+,I99.99n"
        String token = null;
        if(i+9 >= n) {
          token = fmtStr.substring(i,n);
        } else {
          token = fmtStr.substring(i,i+9);
        }
        // Parse <a>
        if(token.startsWith("a")) {
          sb.append(fmtArg.display());
          continue;
        }
        if(token.startsWith("wa")) {
          sb.append(fmtArg.write());
          ++i;
          continue;
        }
        if(token.startsWith("pa")) {
          sb.append(fmtArg.pprint());
          ++i;
          continue;
        }
        // Parse <...>
        if(token.startsWith("...")) {
          if(!Pair.isList(fmtArg) && !(fmtArg instanceof Vector) && !(fmtArg instanceof Hashmap)) {
            throw new StringfException("'%s arg %s isn't a list/vector/hashmap (required by %%...): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          String unpacked = fmtArg.display();
          sb.append(unpacked.substring(1,unpacked.length()-1));
          i += 2;
          continue;
        }
        if(token.startsWith("w...")) {
          if(!Pair.isList(fmtArg) && !(fmtArg instanceof Vector) && !(fmtArg instanceof Hashmap)) {
            throw new StringfException("'%s arg %s isn't a list/vector/hashmap (required by %%w...): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          String unpacked = fmtArg.write();
          sb.append(unpacked.substring(1,unpacked.length()-1));
          i += 3;
          continue;
        }
        if(token.startsWith("p...")) {
          if(!Pair.isList(fmtArg) && !(fmtArg instanceof Vector) && !(fmtArg instanceof Hashmap)) {
            throw new StringfException("'%s arg %s isn't a list/vector/hashmap (required by %%p...): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          String unpacked = fmtArg.pprint();
          sb.append(unpacked.substring(1,unpacked.length()-1));
          i += 3;
          continue;
        }
        // Parse <b>
        if(token.startsWith("b")) {
          if(fmtArg.isTruthy()) {
            sb.append("#t");
          } else {
            sb.append("#f");
          }
          continue;
        }
        if(token.startsWith("wb")) {
          if(fmtArg.isTruthy()) {
            sb.append("true");
          } else {
            sb.append("false");
          }
          ++i;
          continue;
        }
        // Parse <$>
        if(token.startsWith("$")) {
          if(!(fmtArg instanceof Real)) {
            throw new StringfException("'%s arg %s isn't a real number (required by %%$): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          sb.append(String.format("%.2f",((Real)fmtArg).doubleValue()));
          continue;
        }
        if(token.startsWith(",$")) {
          if(!(fmtArg instanceof Real)) {
            throw new StringfException("'%s arg %s isn't a real number (required by %%,$): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          String[] components = String.format("%.2f",((Real)fmtArg).doubleValue()).split("\\.");
          sb.append(addRealNumberCommas(components[0])+"."+components[1]);
          ++i;
          continue;
        }
        // Parse <s>
        if(token.startsWith("s")) {
          if(!(fmtArg instanceof escm.type.String)) {
            throw new StringfException("'%s arg %s isn't a string (required by %%s): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          sb.append(fmtArg.display());
          continue;
        }
        if(token.startsWith("ws")) {
          if(!(fmtArg instanceof escm.type.String)) {
            throw new StringfException("'%s arg %s isn't a string (required by %%ws): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          sb.append(fmtArg.write());
          ++i;
          continue;
        }
        if(isSpacePaddedString(fmtStr,i,n)) {
          if(!(fmtArg instanceof escm.type.String)) {
            StringBuilder strToken = new StringBuilder();
            while(fmtStr.charAt(i) != 's') strToken.append(fmtStr.charAt(i++));
            throw new StringfException("'%s arg %s isn't a string (required by %%%ss): %s", signature, fmtArg.profile(), strToken, profileFormat(fmtStr,args));
          }
          i = displayPaddedStringToken(fmtStr,i,sb,((escm.type.String)fmtArg).value());
          continue;
        }
        // Parse <n>
        if(token.startsWith("n")) {
          if(!(fmtArg instanceof Number)) {
            throw new StringfException("'%s arg %s isn't a number (required by %%n): %s", signature, fmtArg.profile(), profileFormat(fmtStr,args));
          }
          sb.append(fmtArg.display());
          continue;
        }
        if(isScerpNumber(fmtStr,i,n)) {
          if(!(fmtArg instanceof Number)) {
            StringBuilder numToken = new StringBuilder();
            while(fmtStr.charAt(i) != 'n') numToken.append(fmtStr.charAt(i++));
            throw new StringfException("'%s arg %s isn't a number (required by %%%sn): %s", signature, fmtArg.profile(), numToken, profileFormat(fmtStr,args));
          }
          i = displayScerpNumberToken(fmtStr,i,sb,(Number)fmtArg,args,signature);
          continue;
        }
        if(isScippNumber(fmtStr,i,n)) {
          if(!(fmtArg instanceof Number)) {
            StringBuilder numToken = new StringBuilder();
            while(fmtStr.charAt(i) != 'n') numToken.append(fmtStr.charAt(i++));
            throw new StringfException("'%s arg %s isn't a number (required by %%%sn): %s", signature, fmtArg.profile(), numToken, profileFormat(fmtStr,args));
          }
          i = displayScippNumberToken(fmtStr,i,sb,(Number)fmtArg,args);
          continue;
        }
        // Invalid token!
        throw new StringfException("'%s unknown token %%%c: %s", fmtStr.charAt(i), profileFormat(fmtStr,args));
      }
      return sb.toString();
    }


    public static String logic(String fmtStr, ArrayList<Datum> args) throws Exception {
      return logic(fmtStr,args,"(stringf <format-string> <arg> ...)");
    }


    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) 
        throw new StringfException("'(stringf <format-string> <arg> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof escm.type.String))
        throw new StringfException("'(stringf <format-string> <arg> ...) 1st arg %s isn't a string: %s", parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
      String fmtStr = ((escm.type.String)parameters.get(0)).value();
      ArrayList<Datum> args = new ArrayList<Datum>(parameters);
      args.remove(0);
      return new escm.type.String(logic(fmtStr,args));
    }
  }
}