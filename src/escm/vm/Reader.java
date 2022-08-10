// Author: Jordan Randleman - escm.vm.Reader
// Purpose:
//    EScheme reader -- given a string containing EScheme source code, parses such into a
//    EScheme data structure that may be either manipulated as data or evaluated as code 
//    => Hence code IS data and data IS code!

package escm.vm;
import java.util.ArrayList;
import java.util.Stack;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Keyword;
import escm.type.Nil;
import escm.type.number.Number;
import escm.util.Pair;
import escm.util.StringParser;
import escm.util.Exceptionf;
import escm.vm.util.SourceInformation;

public class Reader {
  ////////////////////////////////////////////////////////////////////////////
  // Exception Type Specialization
  public static class IncompleteException extends Exception {
    public IncompleteException() {}
    public IncompleteException(String msg) {super(msg);}
  }


  ////////////////////////////////////////////////////////////////////////////
  // General Helpers
  private static String writeString(String str) {
    return (new escm.type.String(str)).write();
  }


  public static boolean isDelimiter(char c) {
    return Character.isWhitespace(c) || c=='(' || c==')' || c=='[' || c==']' || c=='{' || c=='}' || c=='"' || c==';';
  }


  ////////////////////////////////////////////////////////////////////////////
  // Reader Shorthand Literal Parsing Helpers
  public static boolean isReaderShorthand(char c) {
    if(c == '\'') return true;
    if(c == '`') return true;
    if(c == ',') return true;
    return false;
  }


  // @return: pair of parsed reader shorthand literal expansion & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseReaderShorthandLiteralLogic(String sourceCode, String longhandName, int shorthandEndIdx, Stack<Character> containerStack, SourceInformation source, SourceInformation shorthandSource) throws Exception {
    Pair<Datum,Integer> parsedItem = readLoop(sourceCode,shorthandEndIdx,containerStack,source);
    if(parsedItem.first == null)
      throw new IncompleteException("READ ERROR: Incomplete "+longhandName+" reader shorthand literal!");
    return new Pair<Datum,Integer>(escm.type.Pair.List(new Symbol(longhandName,shorthandSource),parsedItem.first), parsedItem.second);
  }


  // @return: pair of parsed reader shorthand literal expansion & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseReaderShorthandLiteral(String sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source) throws Exception {
    SourceInformation shorthandSource = source.clone();
    if(sourceCode.charAt(i) == '\'') {
      source.updatePosition('\'');
      return parseReaderShorthandLiteralLogic(sourceCode,"quote",i+1,containerStack,source,shorthandSource);
    }
    if(sourceCode.charAt(i) == '`') {
      source.updatePosition('`');
      return parseReaderShorthandLiteralLogic(sourceCode,"quasiquote",i+1,containerStack,source,shorthandSource);
    }
    if(sourceCode.charAt(i) == ',') {
      source.updatePosition(',');
      if(i+1 < n && sourceCode.charAt(i+1) == '@') {
        source.updatePosition('@');
        return parseReaderShorthandLiteralLogic(sourceCode,"unquote-splicing",i+2,containerStack,source,shorthandSource);
      }
      return parseReaderShorthandLiteralLogic(sourceCode,"unquote",i+1,containerStack,source,shorthandSource);
    }
    // Should be unreachable but just in case ...
    throw new Exception("READ ERROR: Unknown Reader Shorthand Literal at index "+i+" in "+writeString(sourceCode)+"!");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Reader Lambda Literal Parsing Helpers
  private static final Symbol LAMBDA_SYMBOL = new escm.type.Symbol("lambda");


  // @return: 0: nothing, -1: variadic, else: param #
  private static int parseParam(Symbol s) {
    if(s.value().length() == 0 || s.value().charAt(0) != '%') return 0;
    if(s.value().equals("%%")) return -1;
    int result = 0;
    try { result = Integer.parseInt(s.value().substring(1)); } catch (Exception e) {}
    if(result <= 0) return 0;
    return result;
  }


  private static void accumulateParamParsingResults(Pair<Integer,String> acc, Pair<Integer,String> newVal) {
    if(newVal == null) return;
    acc.first = Math.max(acc.first,newVal.first);
    if(newVal.second != null) acc.second = newVal.second;
  }


  // @return: Pair<>(numberOfParamsFound, variadicParam)
  private static Pair<Integer,String> parseParams(Datum body) {
    if(body instanceof Symbol) {
      int result = parseParam((Symbol)body);
      switch(result) {
        case -1: return new Pair<Integer,String>(0,"%%");
        case 0:  return null;
        default: return new Pair<Integer,String>(result,null);
      }
    } else if(!(body instanceof escm.type.Pair)) {
      return null;
    } else {
      Pair<Integer,String> accedResults = new Pair<Integer,String>(0,null);
      while(body instanceof escm.type.Pair) {
        escm.type.Pair bodyPair = (escm.type.Pair)body;
        accumulateParamParsingResults(accedResults,parseParams(bodyPair.car()));
        body = bodyPair.cdr();
      }
      if(accedResults.first == 0 && accedResults.second == null) return null;
      return accedResults;
    }
  }


  private static Datum generateExpandedLambda(Datum body) {
    Pair<Integer,String> parsedParams = parseParams(body);
    Datum params = Nil.VALUE;
    if(parsedParams != null) {
      if(parsedParams.second != null) params = new Symbol(parsedParams.second);
      for(int i = parsedParams.first; i >= 1; --i)
        params = new escm.type.Pair(new Symbol("%"+i),params);
    }
    return escm.type.Pair.List(LAMBDA_SYMBOL, params, body);
  }


  // @return: pair of parsed lambda literal expansion & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseReaderLambdaLiteralLogic(String sourceCode, int i, Stack<Character> containerStack, SourceInformation source) throws Exception {
    Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source);
    if(parsedItem.first == null)
      throw new IncompleteException("READ ERROR: Incomplete lambda reader shorthand literal!");
    return new Pair<Datum,Integer>(generateExpandedLambda(parsedItem.first),parsedItem.second);
  }


  ////////////////////////////////////////////////////////////////////////////
  // List Literal Parsing Helpers
  private static boolean isPeriodSymbol(Datum d) throws Exception {
    return d instanceof Symbol && ((Symbol)d).value().equals(".");
  }


  // Returns whether the list is dotted
  private static boolean validatePeriodSymbolPosition(ArrayList<Datum> arr) throws Exception {
    boolean isDotted = false;
    for(int i = 0, n = arr.size(); i < n; ++i) {
      if(isPeriodSymbol(arr.get(i))) {
        if(i+2 != n) throw new Exception("READ ERROR: Invalid list literal: \".\" MUST be the penultimate symbol!");
        isDotted = true; // "(. <obj>)" is equivalent to "<obj>" for the reader
      }
    }
    return isDotted;
  }


  // Returns <arr> as a <Datum> scheme pair data structure
  private static Datum convertArrayListToSchemeList(ArrayList<Datum> arr) throws Exception {
    boolean isDotted = validatePeriodSymbolPosition(arr);
    if(isDotted) {
      arr.remove(arr.size()-2); // remove the dot
      if(arr.size() == 1) return arr.get(0); // "(. <obj>)" is equivalent to "<obj>" for the reader
    } else {
      arr.add(Nil.VALUE); // add NIL at the end of the sequence
    }
    Datum list = arr.get(arr.size()-1);
    for(int i = arr.size()-2; i >= 0; --i)
      list = new escm.type.Pair(arr.get(i),list);
    return list;
  }


  // @param: <i> is where to start parsing
  // @return: pair of parsed list & position in <sourceCode> after the closing <)>
  private static Pair<Datum,Integer> parseListLiteral(String sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source) throws Exception {
    if(i == n)
      throw new IncompleteException("READ ERROR: Incomplete list literal!");
    // parse NIL
    if(sourceCode.charAt(i) == ')') {
      source.updatePosition(')');
      return new Pair<Datum,Integer>(Nil.VALUE,i+1);
    }
    // parse PAIR
    ArrayList<Datum> listItems = new ArrayList<Datum>();
    while(i < n && sourceCode.charAt(i) != ')') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source);
      if(parsedItem.first != null) // if actually parsed something more than just whitespace & comments
        listItems.add(parsedItem.first);
      i = parsedItem.second;
    }
    if(i >= n)
      throw new IncompleteException(String.format("READ ERROR: Invalid input %s, incomplete list literal!", writeString(sourceCode)));
    source.updatePosition(')');
    return new Pair<Datum,Integer>(convertArrayListToSchemeList(listItems),i+1);
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Vector Literal Parsing Helper(s)
  // @param: <i> is where to start parsing
  // @return: pair of parsed vector & position in <sourceCode> after the closing <]>
  private static Pair<Datum,Integer> parseVectorLiteral(String sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source) throws Exception {
    if(i == n)
      throw new IncompleteException("READ ERROR: Incomplete vector literal!");
    escm.type.Vector vect = new escm.type.Vector();
    while(i < n && sourceCode.charAt(i) != ']') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source);
      if(parsedItem.first != null) // if actually parsed something more than just whitespace & comments
        vect.push(parsedItem.first);
      i = parsedItem.second;
    }
    if(i >= n)
      throw new IncompleteException(String.format("READ ERROR: Invalid input %s, incomplete vector literal!", writeString(sourceCode)));
    source.updatePosition(']');
    return new Pair<Datum,Integer>(vect,i+1);
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Hashmap Literal Parsing Helper(s)
  // @param: <i> is where to start parsing
  // @return: pair of parsed hashmap & position in <sourceCode> after the closing <]>
  private static Pair<Datum,Integer> parseHashmapLiteral(String sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source) throws Exception {
    if(i == n)
      throw new IncompleteException("READ ERROR: Incomplete hashmap literal!");
    escm.type.Hashmap hmap = new escm.type.Hashmap();
    Datum key = null;
    while(i < n && sourceCode.charAt(i) != '}') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source);
      if(parsedItem.first != null) { // if actually parsed something more than just whitespace & comments
        if(key == null) {
          key = parsedItem.first;
        } else {
          hmap.set(key,parsedItem.first);
          key = null;
        }
      }
      i = parsedItem.second;
    }
    if(i >= n)
      throw new IncompleteException(String.format("READ ERROR: Invalid input %s, incomplete hashmap literal!", writeString(sourceCode)));
    if(key != null)
      throw new Exceptionf("READ ERROR: Invalid hashmap literal %s key %s doesn't have a value!", hmap.write(), key.profile());
    source.updatePosition('}');
    return new Pair<Datum,Integer>(hmap,i+1);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Boolean Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.size()>
  private static boolean isBooleanLiteral(String sourceCode, int i, int n) {
    return i+1 < n && (sourceCode.charAt(i+1) == 't' || sourceCode.charAt(i+1) == 'f') && 
                      (i+2 == n || isDelimiter(sourceCode.charAt(i+2)));
  }


  // @return: pair of parsed boolean & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseBooleanLiteral(String sourceCode, int i, SourceInformation source) {
    if(sourceCode.charAt(i+1) == 't') {
      source.updatePosition("#t");
      return new Pair<Datum,Integer>(escm.type.Boolean.TRUE,i+2);
    }
    source.updatePosition("#f");
    return new Pair<Datum,Integer>(escm.type.Boolean.FALSE,i+2);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Nil Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.size()>
  private static boolean isNilLiteral(String sourceCode, int i, int n) {
    return i+3 < n && sourceCode.charAt(i+1) == 'n' && 
                      sourceCode.charAt(i+2) == 'i' && 
                      sourceCode.charAt(i+3) == 'l' &&
                      (i+4 == n || isDelimiter(sourceCode.charAt(i+4)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseNilLiteral(String sourceCode, int i, SourceInformation source) {
    source.updatePosition("#nil");
    return new Pair<Datum,Integer>(Nil.VALUE,i+4);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Void Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.size()>
  private static boolean isVoidLiteral(String sourceCode, int i, int n) {
    return i+4 < n && sourceCode.charAt(i+1) == 'v' && 
                      sourceCode.charAt(i+2) == 'o' && 
                      sourceCode.charAt(i+3) == 'i' && 
                      sourceCode.charAt(i+4) == 'd' &&
                      (i+5 == n || isDelimiter(sourceCode.charAt(i+5)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseVoidLiteral(String sourceCode, int i, SourceInformation source) {
    source.updatePosition("#void");
    return new Pair<Datum,Integer>(escm.type.Void.VALUE,i+5);
  }


  ////////////////////////////////////////////////////////////////////////////
  // EOF Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.size()>
  private static boolean isEofLiteral(String sourceCode, int i, int n) {
    return i+3 < n && sourceCode.charAt(i+1) == 'e' && 
                      sourceCode.charAt(i+2) == 'o' && 
                      sourceCode.charAt(i+3) == 'f' && 
                      (i+4 == n || isDelimiter(sourceCode.charAt(i+4)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseEofLiteral(String sourceCode, int i, SourceInformation source) {
    source.updatePosition("#eof");
    return new Pair<Datum,Integer>(escm.type.port.Eof.VALUE,i+4);
  }


  ////////////////////////////////////////////////////////////////////////////
  // String Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed string & position in <sourceCode> after the closing <">
  private static Pair<Datum,Integer> parseStringLiteral(String sourceCode, int i, int n, SourceInformation source) throws IncompleteException {
    int start = i; 
    StringBuilder sb = new StringBuilder();
    while(i < n) {
      source.updatePosition(sourceCode.charAt(i));
      if(sourceCode.charAt(i) == '"') {
        // verify a non-escaped quote
        int j = i-1, escapeCount = 0;
        while(j >= start && sourceCode.charAt(j) == '\\') {
          ++escapeCount;
          --j;
        }
        if(escapeCount % 2 == 0) { // non-escaped <">
          return new Pair<Datum,Integer>(new escm.type.String(StringParser.unescape(sb.toString())),i+1);
        } else { // escaped <">
          sb.append(sourceCode.charAt(i));
        }
      } else {
        sb.append(sourceCode.charAt(i));
      }
      ++i;
    }
    throw new IncompleteException("READ ERROR: Unterminating string literal detected!");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Keyword Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed keyword & position in <sourceCode> after the parsed keyword
  //          => NOTE: returns <null> if at an "empty keyword" (IE if the reader was only given whitespace & comments)
  private static Pair<Datum,Integer> parseKeywordLiteral(String sourceCode, int i, int n, SourceInformation source) {
    StringBuilder sb = new StringBuilder();
    while(i < n && !isDelimiter(sourceCode.charAt(i))) {
      char c = sourceCode.charAt(i);
      source.updatePosition(c);
      sb.append(c);
      ++i;
    }
    if(sb.length() == 0) return new Pair<Datum,Integer>(null,i);
    return new Pair<Datum,Integer>(new Keyword(sb.toString()),i);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Number Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed number & position in <sourceCode> after the parsed double
  private static Pair<Number,Integer> parseNumberLiteral(String sourceCode, int i, int n, SourceInformation source) {
    int start = i;
    StringBuilder sb = new StringBuilder();
    while(i < n && !isDelimiter(sourceCode.charAt(i))) {
      sb.append(sourceCode.charAt(i));
      ++i;
    }
    try {
      Number num = Number.valueOf(sb.toString());
      while(start != i) {
        source.updatePosition(sourceCode.charAt(start));
        ++start;
      }
      return new Pair<Number,Integer>(num,i);
    } catch(Exception e) {
      return null;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Symbol Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed symbol & position in <sourceCode> after the parsed symbol
  //          => NOTE: returns <null> if at an "empty symbol" (IE if the reader was only given whitespace & comments)
  private static Pair<Datum,Integer> parseSymbolLiteral(String sourceCode, int i, int n, SourceInformation source) {
    StringBuilder sb = new StringBuilder();
    SourceInformation symbolSource = source.clone();
    while(i < n && !isDelimiter(sourceCode.charAt(i))) {
      char c = sourceCode.charAt(i);
      source.updatePosition(c);
      sb.append(c);
      ++i;
    }
    if(sb.length() == 0) return new Pair<Datum,Integer>(null,i);
    return new Pair<Datum,Integer>(new Symbol(sb.toString(),symbolSource),i);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main Reader Loop
  // => <.first> is <null> if only read whitespace/comments
  private static Pair<Datum,Integer> readLoop(String sourceCode, int startIndex, Stack<Character> containerStack, SourceInformation source) throws Exception {

    for(int i = startIndex, n = sourceCode.length(); i < n; ++i) {

      // Account for paren scoping
      if(sourceCode.charAt(i) == '(') {
        containerStack.push('(');
      } else if(sourceCode.charAt(i) == ')') {
        if(containerStack.empty())
          throw new Exception("READ ERROR: Invalid parenthesis: found a ')' prior an associated '('!");
        char opener = containerStack.pop();
        if(opener != '(')
          throw new Exceptionf("READ ERROR: Invalid parenthesis: found a closing ')' prior to closing '%c'!", opener);
      }

      // Account for bracket scoping
      if(sourceCode.charAt(i) == '[') {
        containerStack.push('[');
      } else if(sourceCode.charAt(i) == ']') {
        if(containerStack.empty())
          throw new Exception("READ ERROR: Invalid bracket: found a ']' prior an associated '['!");
        char opener = containerStack.pop();
        if(opener != '[')
          throw new Exceptionf("READ ERROR: Invalid bracket: found a closing ']' prior to closing '%c'!", opener);
      }

      // Account for curly-brace scoping
      if(sourceCode.charAt(i) == '{') {
        containerStack.push('{');
      } else if(sourceCode.charAt(i) == '}') {
        if(containerStack.empty())
          throw new Exception("READ ERROR: Invalid curly-brace: found a '}' prior an associated '{'!");
        char opener = containerStack.pop();
        if(opener != '{')
          throw new Exceptionf("READ ERROR: Invalid curly-brace: found a closing '}' prior to closing '%c'!", opener);
      }

      // Ignore whitespace
      if(Character.isWhitespace(sourceCode.charAt(i))) {
        source.updatePosition(sourceCode.charAt(i));
        continue;
      }

      // Skip comments
      if(sourceCode.charAt(i) == ';') {
        while(i < n && sourceCode.charAt(i) != '\n') {
          source.updatePosition(sourceCode.charAt(i));
          ++i;
        }
        if(i == n) return new Pair<Datum,Integer>(null,i);
        source.updatePosition('\n');
        continue;
      }

      // Expand reader lambda literals
      if(sourceCode.charAt(i) == '\\') {
        source.updatePosition('\\');
        return parseReaderLambdaLiteralLogic(sourceCode,i+1,containerStack,source);
      }

      // Expand reader shorthand literals
      if(isReaderShorthand(sourceCode.charAt(i))) 
        return parseReaderShorthandLiteral(sourceCode,i,n,containerStack,source);

      // Parse List/Pair
      if(sourceCode.charAt(i) == '(') {
        source.updatePosition('(');
        return parseListLiteral(sourceCode,i+1,n,containerStack,source);
      }

      // Parse Vector
      if(sourceCode.charAt(i) == '[') {
        source.updatePosition('[');
        return parseVectorLiteral(sourceCode,i+1,n,containerStack,source);
      }

      // Parse Hashmap
      if(sourceCode.charAt(i) == '{') {
        source.updatePosition('{');
        return parseHashmapLiteral(sourceCode,i+1,n,containerStack,source);
      }

      // Check for atomic-value literals
      if(sourceCode.charAt(i) == '#') {
        // Parse Boolean Literals
        if(isBooleanLiteral(sourceCode,i,n)) 
          return parseBooleanLiteral(sourceCode,i,source);

        // Parse Nil Literals
        if(isNilLiteral(sourceCode,i,n)) 
          return parseNilLiteral(sourceCode,i,source);

        // Parse Void Literals
        if(isVoidLiteral(sourceCode,i,n)) 
          return parseVoidLiteral(sourceCode,i,source);

        // Parse EOF Literals
        if(isEofLiteral(sourceCode,i,n)) 
          return parseEofLiteral(sourceCode,i,source);
      }

      // Parse String Literals
      if(sourceCode.charAt(i) == '"') {
        source.updatePosition('"');
        return parseStringLiteral(sourceCode,i+1,n,source);
      }

      // Parse Keyword Literals
      if(sourceCode.charAt(i) == ':') {
        source.updatePosition(':');
        return parseKeywordLiteral(sourceCode,i+1,n,source);
      }

      // Parse Number Literals
      Pair<Number,Integer> numberParseObject = parseNumberLiteral(sourceCode,i,n,source);
      if(numberParseObject != null)
        return new Pair<Datum,Integer>(numberParseObject.first,numberParseObject.second);

      // Parse Symbol Literals
      return parseSymbolLiteral(sourceCode,i,n,source);
    }
    throw new IncompleteException(String.format("READ ERROR: Invalid input %s terminated prior to being able to parse a datum!", writeString(sourceCode)));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing "read": returns a pair: 
  //                      1. the read datum (null/#void if only read whitespace/comments)
  //                      2. the length of characters read from <sourceCode> to produce the read datum

  // @return: <.first> is <null> if only read in whitespace/comments!
  public static Pair<Datum,Integer> nullableRead(String sourceCode, SourceInformation source) throws Exception {
    return readLoop(sourceCode,0,new Stack<Character>(),source);
  }


  // @return: <.first> is <#void> if only read in whitespace/comments!
  public static Pair<Datum,Integer> read(String sourceCode, SourceInformation source) throws Exception {
    Pair<Datum,Integer> result = nullableRead(sourceCode,source);
    if(result.first != null) return result;
    return new Pair<Datum,Integer>(escm.type.Void.VALUE,result.second);
  }
}