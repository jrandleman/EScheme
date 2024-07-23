// Author: Jordan Randleman - escm.util.string.StringParser
// Purpose:
//    String parsing utility to provide string escaping and unescaping via
//    "StringParser.escape" & "StringParser.unescape" when reading/printing 
//    strings in human and machine readable formats.
//
//    Also provides "StringParser.escapeWithCustomUnicodeEscape" to escape 
//    surrogate char pairs using EScheme's custom "U" literal syntax.

package escm.util.string;
import escm.util.Pair;

public class StringParser {
  ////////////////////////////////////////////////////////////////////////////
  // Implementing String Unescaping: "\\n" => "\n"
  private static boolean isEscapeableChar(char c) {
    return c=='\''|| c=='"' || c=='?' || c=='\\' || 
           c=='b' || c=='f' || c=='n' || c=='r' || 
           c=='t';
  }


  private static boolean isUnicodeDigit(char c) {
    return (c>='0' && c<='9')||(c>='A' && c<='F')||(c>='a' && c<='f');
  }


  private static boolean isUnicodeEscape(char c1, char c2) {
    return c1=='u' && isUnicodeDigit(c2);
  }


  private static boolean isUnicodeSurrogatePairEscape(char c1, char c2) {
    return c1=='U' && isUnicodeDigit(c2);
  }


  private static boolean isOctEscape(char c) {
    return c>='0' && c<='7';
  }


  private static char specialCharVariant(char c) {
    switch(c) {
      case 'b': return '\b'; 
      case 'f': return '\f'; 
      case 'n': return '\n';
      case 'r': return '\r'; 
      case 't': return '\t';
      default:  return c;
    }
  }


  // Returns length of the escaped unicode sequence
  private static int insertUnicodeUnescapedChar(StringBuilder sb, String str, int i, int n) {
    StringBuilder numSb = new StringBuilder();
    int count = 0;
    int start = i;
    while(i < n && count < 4 && isUnicodeDigit(str.charAt(i))) {
      numSb.append(str.charAt(i++));
      ++count;
    }
    sb.append((char)(int)Integer.valueOf(numSb.toString(),16));
    return i-start;
  }


  // Returns length of the escaped unicode sequence
  private static int insertUnicodeSurrogatePairUnescapedChar(StringBuilder sb, String str, int i, int n) {
    StringBuilder numSb = new StringBuilder();
    int count = 0;
    int start = i;
    while(i < n && count < 8 && isUnicodeDigit(str.charAt(i))) {
      numSb.append(str.charAt(i++));
      ++count;
    }
    sb.append(java.lang.Character.toString((int)Integer.valueOf(numSb.toString(),16)));
    return i-start;
  }


  // Returns length of the escaped oct sequence (supports \0-\177777 [0-65535 in decimal])
  private static int insertOctUnescapedChar(StringBuilder sb, String str, int i, int n) {
    StringBuilder numSb = new StringBuilder();
    int count = 0;
    int start = i;
    while(i < n && count < 6 && isOctEscape(str.charAt(i))) {
      numSb.append(str.charAt(i++));
      ++count;
    }
    int val = (int)Integer.valueOf(numSb.toString(),8);
    if(val > 65535) {
      numSb.deleteCharAt(numSb.length()-1);
      val = (int)Integer.valueOf(numSb.toString(),8);
      --i;
    }
    sb.append((char)val);
    return i-start;
  }


  public static String unescape(String str) {
    StringBuilder sb = new StringBuilder();
    int i = 0, n = str.length();
    while(i < n) {
      if(str.charAt(i) == '\\' && i+1 < n) {
        if(isEscapeableChar(str.charAt(i+1))) {
          sb.append(specialCharVariant(str.charAt(i+1)));
          i += 2;
        } else if(i+2 < n && isUnicodeEscape(str.charAt(i+1),str.charAt(i+2))) {
          i += 2;
          i += insertUnicodeUnescapedChar(sb,str,i,n);
        } else if(i+2 < n && isUnicodeSurrogatePairEscape(str.charAt(i+1),str.charAt(i+2))) {
          i += 2;
          i += insertUnicodeSurrogatePairUnescapedChar(sb,str,i,n);
        } else if(isOctEscape(str.charAt(i+1))) {
          ++i;
          i += insertOctUnescapedChar(sb,str,i,n);
        } else {
          sb.append(str.charAt(i++));
        }
      } else {
        sb.append(str.charAt(i++));
      }
    }
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing String Escaping: "\n" => "\\n"
  private static String get16bitHexString(char c) {
    String hexString = Integer.toHexString((int)c);
    StringBuilder sb = new StringBuilder("\\u");
    for(int i = 0, n = 4-hexString.length(); i < n; ++i) {
      sb.append('0');
    }
    sb.append(hexString);
    return sb.toString();
  }


  private static String get32bitHexString(int c) {
    String hexString = Integer.toHexString(c);
    StringBuilder sb = new StringBuilder("\\U");
    for(int i = 0, n = 8-hexString.length(); i < n; ++i) {
      sb.append('0');
    }
    sb.append(hexString);
    return sb.toString();
  }


  private static String escapedChar(char c) {
    switch(c) {
      case '"':  return "\\\"";
      case '\\': return "\\\\";
      case '\b': return "\\b";
      case '\f': return "\\f";
      case '\n': return "\\n";
      case '\r': return "\\r";
      case '\t': return "\\t";
      default: return get16bitHexString(c); // Escape non-printable chars that are NOT one of the above as unicode
    }
  }


  private static String escapedCodePoint(int i) {
    switch(i) {
      case '"':  return "\\\"";
      case '\\': return "\\\\";
      case '\b': return "\\b";
      case '\f': return "\\f";
      case '\n': return "\\n";
      case '\r': return "\\r";
      case '\t': return "\\t";
      default: { // Escape non-printable chars that are NOT one of the above as unicode
        if((i & 0x00000000ffffffffL) <= 0xffffL) {
          return get16bitHexString((char)i);
        } else {
          return get32bitHexString(i);
        }
      }
    }
  }


  private static boolean noCharEscapeNeeded(int c) {
    return c != '"' && c != '\\' && c >= 32 && c <= 126;
  }


  // Prints 32bit surrogate char pairs as 1 "U" instance (an invalid Java String)
  public static String escapeWithCustomUnicodeEscape(String str) {
    StringBuilder escaped = new StringBuilder();
    int offset = 0, strLength = str.length();
    while(offset < strLength) {
      int codepoint = str.codePointAt(offset);
      if(noCharEscapeNeeded(codepoint)) {
        escaped.append(java.lang.Character.toString(codepoint));
      } else {
        escaped.append(escapedCodePoint(codepoint));
      }
      offset += java.lang.Character.charCount(codepoint);
    }
    return escaped.toString();
  }


  // Prints 32bit surrogate char pairs as 2 "u" instances (a valid Java String)
  public static String escape(String str) {
    StringBuilder escaped = new StringBuilder();
    for(int i = 0, n = str.length(); i < n; ++i) {
      char c = str.charAt(i);
      if(noCharEscapeNeeded(c)) {
        escaped.append(c);
      } else {
        escaped.append(escapedChar(c));
      }
    }
    return escaped.toString();
  }


  // Returned int represents the new idx position of <indexToUpdate> in escaped version of <str>
  // Prints 32bit surrogate char pairs as 2 "u" instances (a valid Java String)
  public static Pair<Integer,String> escape(int indexToUpdate, String str) {
    int originalIndexToUpdate = indexToUpdate;
    StringBuilder escaped = new StringBuilder();
    for(int i = 0, n = str.length(); i < n; ++i) {
      char c = str.charAt(i);
      if(noCharEscapeNeeded(c)) {
        escaped.append(c);
      } else {
        String escapedChar = escapedChar(c);
        if(i < originalIndexToUpdate) indexToUpdate += escapedChar.length()-1;
        escaped.append(escapedChar);
      }
    }
    return new Pair<Integer,String>(indexToUpdate,escaped.toString());
  }
}