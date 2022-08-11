// Author: Jordan Randleman - escm.util.json.util.StringParser
// Purpose:
//    String parsing utility to provide string escaping and unescaping via
//    "escm.util.json.util.StringParser.escape" & 
//    "escm.util.json.util.StringParser.unescape" when reading/printing 
//    strings in human and machine readable formats.

package escm.util.json.util;
import java.util.ArrayList;

public class StringParser {
  ////////////////////////////////////////////////////////////////////////////
  // Implementing String Unescaping: "\\n" => "\n"
  private static boolean isEscapeableChar(char c) {
    return c=='\''|| c=='"' || c=='?' || c=='\\' || 
           c=='b' || c=='f' || c=='n' || c=='r' || c=='t';
  }


  private static boolean isHexDigit(char c) {
    return (c>='0' && c<='9')||(c>='A' && c<='F')||(c>='a' && c<='f');
  }


  private static boolean isHexEscape(char c1, char c2) {
    return c1=='x' && isHexDigit(c2);
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


  // Returns length of the escaped hex sequence
  private static int insertHexUnescapedChar(StringBuilder sb, String str, int i, int n) {
    StringBuilder numSb = new StringBuilder();
    int start = i;
    while(i < n && isHexDigit(str.charAt(i)))
      numSb.append(str.charAt(i++));
    sb.append((char)(int)Integer.valueOf(numSb.toString(),16));
    return i-start;
  }


  // Returns length of the escaped oct sequence
  private static int insertOctUnescapedChar(StringBuilder sb, String str, int i, int n) {
    StringBuilder numSb = new StringBuilder();
    int start = i;
    while(i < n && isOctEscape(str.charAt(i)))
      numSb.append(str.charAt(i++));
    sb.append((char)(int)Integer.valueOf(numSb.toString(),8));
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
        } else if(i+2 < n && isHexEscape(str.charAt(i+1),str.charAt(i+2))) {
          i += 2;
          i += insertHexUnescapedChar(sb,str,i,n);
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
  private static String escapedChar(char c) {
    c %= 256;
    if(c < 0) c += 256;
    switch(c) {
      case '"':  return "\\\"";
      case '\\': return "\\\\";
      case '\b': return "\\b";
      case '\f': return "\\f";
      case '\n': return "\\n";
      case '\r': return "\\r";
      case '\t': return "\\t";
      default: // Escape non-printable chars that are NOT one of the above in hex
        char left_digit = (char)(c/16), right_digit = (char)(c%16);
        StringBuilder sb = new StringBuilder("\\x");
        sb.append((char)(left_digit>9?'a'+left_digit-10:'0'+left_digit));
        sb.append((char)(right_digit>9?'a'+right_digit-10:'0'+right_digit));
        return sb.toString();
    }
  }


  public static String escape(String str) {
    StringBuilder escaped = new StringBuilder();
    for(int i = 0, n = str.length(); i < n; ++i) {
      if(str.charAt(i) != '"' && str.charAt(i) != '\\' && !Character.isISOControl(str.charAt(i)))
        escaped.append(str.charAt(i));
      else
        escaped.append(escapedChar(str.charAt(i)));
    }
    return escaped.toString();
  }


  public static Pair<Integer,String> escape(int indexToUpdate, String str) {
    int originalIndexToUpdate = indexToUpdate;
    StringBuilder escaped = new StringBuilder();
    for(int i = 0, n = str.length(); i < n; ++i) {
      if(str.charAt(i) != '"' && str.charAt(i) != '\\' && !Character.isISOControl(str.charAt(i))) {
        escaped.append(str.charAt(i));
      } else {
        String escapedChar = escapedChar(str.charAt(i));
        if(i < originalIndexToUpdate) indexToUpdate += escapedChar.length()-1;
        escaped.append(escapedChar);
      }
    }
    return new Pair<Integer,String>(indexToUpdate,escaped.toString());
  }
}