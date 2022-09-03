// Author: Jordan Randleman - escm.util.json.JsonDatum
// Purpose:
//    Abstract base class of all JSON data.
// Interface:
//    1. Parse a string via <JsonDatum.parse(String s)>.
//    2. Convert <JsonDatum> to a string via <toString(int tabWidth)>.

package escm.util.json;
import java.util.ArrayList;
import java.util.HashMap;
import escm.util.Pair;
import escm.util.StringParser;
import escm.util.json.util.JsonException;

public abstract class JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public abstract String toString();

  
  public abstract String toString(int tabWidth);


  protected abstract String toString(int accWidth, int tabWidth);

  
  static String getSpaceString(int spaces) {
    StringBuilder sb = new StringBuilder();
    while(spaces > 0) {
      sb.append(' ');
      --spaces;
    }
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public abstract boolean equals(Object o);


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public abstract int hashCode();


  ////////////////////////////////////////////////////////////////////////////
  // Parse Comments
  private static boolean isComment(String s, int i, int n) throws JsonException {
    if(s.charAt(i) != '/') return false;
    if(i+1 == n) throw new JsonException(i,s,"Incomplete JSON comment.");
    char nextChar = s.charAt(i+1);
    return nextChar == '/' || nextChar == '*';
  }


  private static int parseComment(String s, int i, int n) throws JsonException {
    // Single-line
    if(s.charAt(i+1) == '/') {
      while(i < n && s.charAt(i) != '\n') ++i;
      return i;
    // Multi-line
    } else {
      while(i+1 < n && s.charAt(i) != '*' && s.charAt(i+1) != '/') ++i;
      if(i+1 == n) {
        throw new JsonException(i,s,"Incomplete JSON multi-line comment.");
      }
      return i+1;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Strings
  private static boolean isString(String s, int i, int n) {
    return s.charAt(i) == '"';
  }


  private static Pair<JsonDatum,Integer> parseString(String s, int i, int n) throws JsonException {
    int start = i; 
    StringBuilder sb = new StringBuilder();
    ++i; // move past '"'
    while(i < n) {
      if(s.charAt(i) == '"') {
        // verify a non-escaped quote
        int j = i-1, escapeCount = 0;
        while(j > start && s.charAt(j) == '\\') {
          ++escapeCount;
          --j;
        }
        if(escapeCount % 2 == 0) { // non-escaped <">
          return new Pair<JsonDatum,Integer>(new JsonString(StringParser.unescape(sb.toString())),i+1);
        } else { // escaped <">
          sb.append(s.charAt(i));
        }
      } else {
        sb.append(s.charAt(i));
      }
      ++i;
    }
    throw new JsonException(start,s,"Incomplete JSON string.");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Numbers
  private static boolean isNumberChar(char c) {
    return Character.isDigit(c) || c=='-' || c=='+' || c=='.' || c=='e' || c=='E';
  }


  private static boolean isNumber(String s, int i, int n) {
    return isNumberChar(s.charAt(i));
  }


  private static Pair<JsonDatum,Integer> parseNumber(String s, int i, int n) throws JsonException {
    int start = i;
    StringBuilder sb = new StringBuilder();
    for(; i < n && isNumberChar(s.charAt(i)); ++i) {
      sb.append(s.charAt(i));
    }
    try {
      return new Pair<JsonDatum,Integer>(new JsonNumber(Double.parseDouble(sb.toString())),i);
    } catch(Exception e) {
      throw new JsonException(start,s,"Invalid JSON number.");
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Objects
  private static boolean isObject(String s, int i, int n) {
    return s.charAt(i)=='{';
  }


  private static Pair<JsonDatum,Integer> parseObject(String s, int i, int n) throws JsonException {
    int start = i;
    ++i; // move past '{'
    HashMap<String,JsonDatum> o = new HashMap<String,JsonDatum>();
    while(i < n && s.charAt(i) != '}') {
      // parse key
      Pair<JsonDatum,Integer> key = parse(s,i,n);
      if(!(key.first instanceof JsonString))
        throw new JsonException(i,s,"Invalid JSON object (key %s isn't a string!).", key.first.toString());
      i = key.second;
      // parse value
      while(i < n && Character.isWhitespace(s.charAt(i))) ++i;
      if(i == n || s.charAt(i) != ':')
        throw new JsonException(i,s,"Invalid JSON object (missing val!).");
      ++i; // move past ':'
      Pair<JsonDatum,Integer> val = parse(s,i,n);
      i = val.second;
      while(i < n && Character.isWhitespace(s.charAt(i))) ++i;
      if(i == n || (s.charAt(i) != ',' && s.charAt(i) != '}'))
        throw new JsonException(i,s,"Invalid JSON object (invalid key/val set spacing!): Given char '%c'", s.charAt(i));
      if(s.charAt(i) == ',') ++i;
      o.put(((JsonString)key.first).value(),val.first);
    }
    if(i == n) throw new JsonException(start,s,"Invalid JSON object.");
    return new Pair<JsonDatum,Integer>(new JsonObject(o),i+1);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Arrays
  private static boolean isArray(String s, int i, int n) {
    return s.charAt(i)=='[';
  }


  private static Pair<JsonDatum,Integer> parseArray(String s, int i, int n) throws JsonException {
    int start = i;
    ++i; // move past '{'
    ArrayList<JsonDatum> a = new ArrayList<JsonDatum>();
    while(i < n && s.charAt(i) != ']') {
      Pair<JsonDatum,Integer> val = parse(s,i,n);
      i = val.second;
      while(i < n && Character.isWhitespace(s.charAt(i))) ++i;
      if(i == n || (s.charAt(i) != ',' && s.charAt(i) != ']'))
        throw new JsonException(i,s,"Invalid JSON array (invalid item set spacing!).");
      if(s.charAt(i) == ',') ++i;
      a.add(val.first);
    }
    if(i == n) throw new JsonException(start,s,"Invalid JSON array.");
    return new Pair<JsonDatum,Integer>(new JsonArray(a),i+1);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Booleans
  private static boolean isBoolean(String s, int i, int n) {
    if(s.charAt(i)=='t' && i+3 < n) {
      return s.charAt(i+1)=='r' && s.charAt(i+2)=='u' && s.charAt(i+3)=='e';
    } else if(s.charAt(i)=='f' && i+4 < n) {
      return s.charAt(i+1)=='a' && s.charAt(i+2)=='l' && s.charAt(i+3)=='s' && s.charAt(i+4)=='e';
    } else {
      return false;
    }
  }


  private static Pair<JsonDatum,Integer> parseBoolean(String s, int i, int n) {
    if(s.charAt(i)=='t') return new Pair<JsonDatum,Integer>(JsonBoolean.TRUE,i+4);
     return new Pair<JsonDatum,Integer>(JsonBoolean.FALSE,i+5);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Parse Null
  private static boolean isNull(String s, int i, int n) {
    return i+3<n && s.charAt(i)=='n' && s.charAt(i+1)=='u' && s.charAt(i+2)=='l' && s.charAt(i+3)=='l';
  }


  private static Pair<JsonDatum,Integer> parseNull(String s, int i, int n) {
    return new Pair<JsonDatum,Integer>(JsonNull.VALUE,i+4);
  }


  ////////////////////////////////////////////////////////////////////////////
  // General Parser
  private static Pair<JsonDatum,Integer> parse(String s, int i, int n) throws JsonException {
    int start = i;
    for(; i < n; ++i) {
      if(Character.isWhitespace(s.charAt(i))) {
        continue;
      } else if(isComment(s,i,n)) {
        i = parseComment(s,i,n);
      } else if(isString(s,i,n)) {
        return parseString(s,i,n);
      } else if(isNumber(s,i,n)) {
        return parseNumber(s,i,n);
      } else if(isObject(s,i,n)) {
        return parseObject(s,i,n);
      } else if(isArray(s,i,n)) {
        return parseArray(s,i,n);
      } else if(isBoolean(s,i,n)) {
        return parseBoolean(s,i,n);
      } else if(isNull(s,i,n)) {
        return parseNull(s,i,n);
      } else {
        throw new JsonException(i,s,"Invalid JSON string!");
      }
    }
    throw new JsonException(start,s,"Invalid JSON string!");
  }


  public static JsonDatum parse(String s) throws JsonException {
    return parse(s,0,s.length()).first;
  }
}