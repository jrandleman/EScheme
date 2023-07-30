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
import escm.util.json.util.SubstringIndexError;
import escm.primitive.FilePrimitives;

public class Reader {
  ////////////////////////////////////////////////////////////////////////////
  // Flags to denote whether ignoring <IncompleteException> (for efficiency)
  public static final boolean GIVE_DETAILED_INCOMPLETE_ERRORS = false;

  public static final boolean GIVE_EMPTY_INCOMPLETE_ERRORS = true;


  ////////////////////////////////////////////////////////////////////////////
  // Character prefix denoting a reader lambda literal
  // => ___Must be followed by a '(' to register as a lambda literal!___
  public static final char LAMBDA_LITERAL_PREFIX = '#';


  ////////////////////////////////////////////////////////////////////////////
  // Length of the Reader's Error Substring
  private static final int READER_ERROR_SUBSTRING_LENGTH = 80;


  ////////////////////////////////////////////////////////////////////////////
  // Exception Type(s) Specialization
  public static class ReaderException extends Exception {
    public static String generateMessage(boolean ignoringIncomplete, int errorIndex, CharSequence sourceCode, SourceInformation source, String fmt, Object ... args) {
      if(ignoringIncomplete) return "";
      StringBuilder sb = new StringBuilder();
      sb.append(String.format(fmt,args));
      if(sourceCode.length() > 0) {
        String[] errorSubstring = SubstringIndexError.run(errorIndex,sourceCode.toString(),READER_ERROR_SUBSTRING_LENGTH);
        sb.append("\n>> Found Here: " + errorSubstring[0] + 
                  "\n               " + errorSubstring[1]);
      }
      sb.append("\n>> Location: " + source.toString());
      return sb.toString();
    }

    private ReaderException(String msg) {
      super(msg);
    }

    public ReaderException(int errorIndex, CharSequence sourceCode, SourceInformation source, String fmt, Object ... args) {
      super(generateMessage(false,errorIndex,sourceCode,source,fmt,args));
    }
  }


  public static class IncompleteException extends ReaderException {
    public IncompleteException(boolean ignoringIncomplete, int errorIndex, CharSequence sourceCode, SourceInformation source, String fmt, Object ... args) {
      super(generateMessage(ignoringIncomplete,errorIndex,sourceCode,source,fmt,args));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // General Helpers
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
  private static Pair<Datum,Integer> parseReaderShorthandLiteralLogic(CharSequence sourceCode, String longhandName, int shorthandStartIdx, int shorthandEndIdx, Stack<Character> containerStack, SourceInformation source, SourceInformation shorthandSource, boolean ignoringIncomplete) throws ReaderException {
    Pair<Datum,Integer> parsedItem = readLoop(sourceCode,shorthandEndIdx,containerStack,source,ignoringIncomplete);
    if(parsedItem.first == null)
      throw new IncompleteException(ignoringIncomplete,shorthandStartIdx, sourceCode, shorthandSource, "READ ERROR: Incomplete \"%s\" reader shorthand literal!", longhandName);
    return new Pair<Datum,Integer>(escm.type.Pair.List(new Symbol(longhandName,shorthandSource),parsedItem.first), parsedItem.second);
  }


  // @return: pair of parsed reader shorthand literal expansion & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseReaderShorthandLiteral(CharSequence sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    SourceInformation shorthandSource = source.clone();
    if(sourceCode.charAt(i) == '\'') {
      source.updatePosition('\'');
      return parseReaderShorthandLiteralLogic(sourceCode,"quote",i,i+1,containerStack,source,shorthandSource,ignoringIncomplete);
    }
    if(sourceCode.charAt(i) == '`') {
      source.updatePosition('`');
      return parseReaderShorthandLiteralLogic(sourceCode,"quasiquote",i,i+1,containerStack,source,shorthandSource,ignoringIncomplete);
    }
    if(sourceCode.charAt(i) == ',') {
      source.updatePosition(',');
      if(i+1 < n && sourceCode.charAt(i+1) == '@') {
        source.updatePosition('@');
        return parseReaderShorthandLiteralLogic(sourceCode,"unquote-splicing",i,i+2,containerStack,source,shorthandSource,ignoringIncomplete);
      }
      return parseReaderShorthandLiteralLogic(sourceCode,"unquote",i,i+1,containerStack,source,shorthandSource,ignoringIncomplete);
    }
    // Should be unreachable but just in case ...
    throw new ReaderException(i, sourceCode, shorthandSource, "READ ERROR: Unknown reader shorthand literal!");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Reader Lambda Literal Parsing Helpers
  private static final Symbol LAMBDA_SYMBOL = new escm.type.Symbol("lambda");


  // Reader.LAMBDA_LITERAL_PREFIX + '('
  private static boolean isReaderLambdaLiteral(CharSequence sourceCode, int i) {
    if(sourceCode.charAt(i) != Reader.LAMBDA_LITERAL_PREFIX) return false;
    return i+1 < sourceCode.length() && sourceCode.charAt(i+1) == '(';
  }


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
  private static Pair<Datum,Integer> parseReaderLambdaLiteralLogic(CharSequence sourceCode, int i, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    SourceInformation lambdaSource = source.clone();
    source.updatePosition(LAMBDA_LITERAL_PREFIX);
    Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source,ignoringIncomplete);
    if(parsedItem.first == null)
      throw new IncompleteException(ignoringIncomplete,i-1, sourceCode, lambdaSource, "READ ERROR: Incomplete lambda reader shorthand literal!");
    return new Pair<Datum,Integer>(generateExpandedLambda(parsedItem.first),parsedItem.second);
  }


  ////////////////////////////////////////////////////////////////////////////
  // List Literal Parsing Helpers
  private static boolean isPeriodSymbol(Datum d) throws ReaderException {
    return d instanceof Symbol && ((Symbol)d).value().equals(".");
  }


  // Returns whether the list is dotted
  private static boolean validatePeriodSymbolPosition(int listIndex, CharSequence sourceCode, SourceInformation listSource, ArrayList<Datum> arr) throws ReaderException {
    boolean isDotted = false;
    for(int i = 0, n = arr.size(); i < n; ++i) {
      if(isPeriodSymbol(arr.get(i))) {
        if(i+2 != n) throw new ReaderException(listIndex, sourceCode, listSource, "READ ERROR: Invalid list literal: \".\" MUST be the penultimate symbol!");
        isDotted = true; // "(. <obj>)" is equivalent to "<obj>" for the reader
      }
    }
    return isDotted;
  }


  // Returns <arr> as a <Datum> scheme pair data structure
  private static Datum convertArrayListToSchemeList(int listIndex, CharSequence sourceCode, SourceInformation listSource, ArrayList<Datum> arr) throws ReaderException {
    boolean isDotted = validatePeriodSymbolPosition(listIndex,sourceCode,listSource,arr);
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
  private static Pair<Datum,Integer> parseListLiteral(CharSequence sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    int listIndex = i-1;
    SourceInformation listSource = source.clone();
    source.updatePosition('(');
    if(i == n)
      throw new IncompleteException(ignoringIncomplete,listIndex,sourceCode,listSource,"READ ERROR: Incomplete list literal!");
    // parse NIL
    if(sourceCode.charAt(i) == ')') {
      source.updatePosition(')');
      return new Pair<Datum,Integer>(Nil.VALUE,i+1);
    }
    // parse PAIR
    ArrayList<Datum> listItems = new ArrayList<Datum>();
    while(i < n && sourceCode.charAt(i) != ')') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source,ignoringIncomplete);
      if(parsedItem.first != null) // if actually parsed something more than just whitespace & comments
        listItems.add(parsedItem.first);
      i = parsedItem.second;
    }
    if(i >= n)
      throw new IncompleteException(ignoringIncomplete,listIndex,sourceCode,listSource,"READ ERROR: Incomplete list literal!");
    source.updatePosition(')');
    return new Pair<Datum,Integer>(convertArrayListToSchemeList(listIndex,sourceCode,listSource,listItems),i+1);
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Vector Literal Parsing Helper(s)
  // @param: <i> is where to start parsing
  // @return: pair of parsed vector & position in <sourceCode> after the closing <]>
  private static Pair<Datum,Integer> parseVectorLiteral(CharSequence sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    int vectorIndex = i-1;
    SourceInformation vectorSource = source.clone();
    source.updatePosition('[');
    if(i == n)
      throw new IncompleteException(ignoringIncomplete,vectorIndex,sourceCode,vectorSource,"READ ERROR: Incomplete vector literal!");
    escm.type.Vector vect = new escm.type.Vector();
    while(i < n && sourceCode.charAt(i) != ']') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source,ignoringIncomplete);
      if(parsedItem.first != null) // if actually parsed something more than just whitespace & comments
        vect.push(parsedItem.first);
      i = parsedItem.second;
    }
    if(i >= n)
      throw new IncompleteException(ignoringIncomplete,vectorIndex,sourceCode,vectorSource,"READ ERROR: Incomplete vector literal!");
    source.updatePosition(']');
    containerStack.pop(); // opening ']'
    return new Pair<Datum,Integer>(vect,i+1);
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Hashmap Literal Parsing Helper(s)
  // @param: <i> is where to start parsing
  // @return: pair of parsed hashmap & position in <sourceCode> after the closing <}>
  private static Pair<Datum,Integer> parseHashmapLiteral(CharSequence sourceCode, int i, int n, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    int hashmapIndex = i-1;
    SourceInformation hashmapSource = source.clone();
    source.updatePosition('{');
    if(i == n)
      throw new IncompleteException(ignoringIncomplete,hashmapIndex,sourceCode,hashmapSource,"READ ERROR: Incomplete hashmap literal!");
    escm.type.Hashmap hmap = new escm.type.Hashmap();
    Datum key = null;
    while(i < n && sourceCode.charAt(i) != '}') {
      Pair<Datum,Integer> parsedItem = readLoop(sourceCode,i,containerStack,source,ignoringIncomplete);
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
      throw new IncompleteException(ignoringIncomplete,hashmapIndex,sourceCode,hashmapSource,"READ ERROR: Incomplete hashmap literal!");
    if(key != null)
      throw new ReaderException(hashmapIndex,sourceCode,hashmapSource,"READ ERROR: Hashmap literal %s key %s doesn't have a value!", hmap.write(), key.profile());
    source.updatePosition('}');
    containerStack.pop(); // opening '}'
    return new Pair<Datum,Integer>(hmap,i+1);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Boolean Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.length()>
  private static boolean isBooleanLiteral(CharSequence sourceCode, int i, int n) {
    return i+1 < n && (sourceCode.charAt(i+1) == 't' || sourceCode.charAt(i+1) == 'f') && 
                      (i+2 == n || isDelimiter(sourceCode.charAt(i+2)));
  }


  // @return: pair of parsed boolean & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseBooleanLiteral(CharSequence sourceCode, int i, SourceInformation source) {
    if(sourceCode.charAt(i+1) == 't') {
      source.updatePosition("#t");
      return new Pair<Datum,Integer>(escm.type.bool.Boolean.TRUE,i+2);
    }
    source.updatePosition("#f");
    return new Pair<Datum,Integer>(escm.type.bool.Boolean.FALSE,i+2);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Nil Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.length()>
  private static boolean isNilLiteral(CharSequence sourceCode, int i, int n) {
    return i+3 < n && sourceCode.charAt(i+1) == 'n' && 
                      sourceCode.charAt(i+2) == 'i' && 
                      sourceCode.charAt(i+3) == 'l' &&
                      (i+4 == n || isDelimiter(sourceCode.charAt(i+4)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseNilLiteral(CharSequence sourceCode, int i, SourceInformation source) {
    source.updatePosition("#nil");
    return new Pair<Datum,Integer>(Nil.VALUE,i+4);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Void Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.length()>
  private static boolean isVoidLiteral(CharSequence sourceCode, int i, int n) {
    return i+4 < n && sourceCode.charAt(i+1) == 'v' && 
                      sourceCode.charAt(i+2) == 'o' && 
                      sourceCode.charAt(i+3) == 'i' && 
                      sourceCode.charAt(i+4) == 'd' &&
                      (i+5 == n || isDelimiter(sourceCode.charAt(i+5)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseVoidLiteral(CharSequence sourceCode, int i, SourceInformation source) {
    source.updatePosition("#void");
    return new Pair<Datum,Integer>(escm.type.Void.VALUE,i+5);
  }


  ////////////////////////////////////////////////////////////////////////////
  // EOF Literal Identification & Parsing Helpers
  // @precondition: sourceCode.charAt(i) == '#'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.length()>
  private static boolean isEofLiteral(CharSequence sourceCode, int i, int n) {
    return i+3 < n && sourceCode.charAt(i+1) == 'e' && 
                      sourceCode.charAt(i+2) == 'o' && 
                      sourceCode.charAt(i+3) == 'f' && 
                      (i+4 == n || isDelimiter(sourceCode.charAt(i+4)));
  }


  // @return: pair of parsed literal & position in <sourceCode> after the literal
  private static Pair<Datum,Integer> parseEofLiteral(CharSequence sourceCode, int i, SourceInformation source) {
    source.updatePosition("#eof");
    return new Pair<Datum,Integer>(escm.type.port.Eof.VALUE,i+4);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Character Literal Identification & Parsing Helpers
  private static escm.type.Character NUL_CHARACTER = new escm.type.Character('\0');


  // @precondition: sourceCode.charAt(i) == '#' && sourceCode.charAt(i+1) == '\\'
  // @param: <i> is where to start parsing
  // @param: <n> is <sourceCode.length()>
  private static boolean isCharacterLiteral(CharSequence sourceCode, int i, int n) {
    return i+1 < n && sourceCode.charAt(i+1) == '\\';
  }


  // @return: pair of parsed character & position in <sourceCode> after the parsed literal
  private static Pair<Datum,Integer> parseCharacterLiteral(CharSequence sourceCode, int i, int n, SourceInformation source) throws ReaderException {
    if(i+2 == n) return new Pair<Datum,Integer>(NUL_CHARACTER,i+2);
    SourceInformation charSource = source.clone();
    int charIndex = i;
    char firstChar = sourceCode.charAt(i+2);
    StringBuilder sb = new StringBuilder();
    sb.append(firstChar);
    source.updatePosition("#\\");
    source.updatePosition(firstChar);
    i += 3;
    while(i < n && isDelimiter(sourceCode.charAt(i)) == false) {
      char c = sourceCode.charAt(i);
      source.updatePosition(c);
      sb.append(c);
      ++i;
    }
    escm.type.Character ch = escm.type.Character.parse(sb.toString());
    if(ch == null)
      throw new ReaderException(charIndex,sourceCode,charSource,"READ ERROR: Invalid character literal \"#\\%s\"", sb.toString());
    return new Pair<Datum,Integer>(ch,i);
  }


  ////////////////////////////////////////////////////////////////////////////
  // String Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed string & position in <sourceCode> after the closing <">
  private static Pair<Datum,Integer> parseStringLiteral(CharSequence sourceCode, int i, int n, SourceInformation source, boolean ignoringIncomplete) throws IncompleteException {
    int start = i-1; 
    SourceInformation stringSource = source.clone();
    StringBuilder sb = new StringBuilder();
    source.updatePosition('"');
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
    throw new IncompleteException(ignoringIncomplete,start,sourceCode,stringSource,"READ ERROR: Unterminating string literal detected!");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Keyword Literal Parsing Helper
  // @param: <i> is where to start parsing
  // @return: pair of parsed keyword & position in <sourceCode> after the parsed keyword
  //          => NOTE: returns <null> if at an "empty keyword" (IE if the reader was only given whitespace & comments)
  private static Pair<Datum,Integer> parseKeywordLiteral(CharSequence sourceCode, int i, int n, SourceInformation source) {
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
  private static Pair<Number,Integer> parseNumberLiteral(CharSequence sourceCode, int i, int n, SourceInformation source) {
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
  private static final String CURRENT_PARENT_PATH_READER_LITERAL = "#path";

  private static String getCurrentPathParent(int symbolStart, CharSequence sourceCode, SourceInformation symbolSource) throws ReaderException {
    try {
      String fileName = symbolSource.fileName();
      String parent = FilePrimitives.PathParent.logic(fileName);
      if(parent == null) return FilePrimitives.CurrentDirectory.logic();
      return parent;
    } catch(Exception e) {
      throw new ReaderException(symbolStart,sourceCode,symbolSource,"READ ERROR: Can't generate \"#path\": %s", e.getMessage());
    }
  }

  private static Pair<Datum,Integer> parseSymbolLiteral(CharSequence sourceCode, int i, int n, SourceInformation source) throws ReaderException {
    StringBuilder sb = new StringBuilder();
    SourceInformation symbolSource = source.clone();
    int symbolStart = i;
    while(i < n && !isDelimiter(sourceCode.charAt(i))) {
      char c = sourceCode.charAt(i);
      source.updatePosition(c);
      sb.append(c);
      ++i;
    }
    if(sb.length() == 0) return new Pair<Datum,Integer>(null,i);
    String symbolString = sb.toString();
    if(symbolString.equals(CURRENT_PARENT_PATH_READER_LITERAL)) {
      return new Pair<Datum,Integer>(new escm.type.String(getCurrentPathParent(symbolStart,sourceCode,symbolSource)),i);
    } else {
      return new Pair<Datum,Integer>(new Symbol(symbolString,symbolSource),i);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Main Reader Loop
  // => <.first> is <null> if only read whitespace/comments
  private static Pair<Datum,Integer> readLoop(CharSequence sourceCode, int startIndex, Stack<Character> containerStack, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {

    for(int i = startIndex, n = sourceCode.length(); i < n; ++i) {

      // Account for paren scoping
      if(sourceCode.charAt(i) == '(') {
        containerStack.push('(');
      } else if(sourceCode.charAt(i) == ')') {
        if(containerStack.empty())
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid parenthesis: found a ')' prior an associated '('!");
        char opener = containerStack.pop();
        if(opener != '(')
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid parenthesis: found a closing ')' prior to closing '%c'!", opener);
      }

      // Account for bracket scoping
      if(sourceCode.charAt(i) == '[') {
        containerStack.push('[');
      } else if(sourceCode.charAt(i) == ']') {
        if(containerStack.empty())
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid bracket: found a ']' prior an associated '['!");
        char opener = containerStack.pop();
        if(opener != '[')
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid bracket: found a closing ']' prior to closing '%c'!", opener);
      }

      // Account for curly-brace scoping
      if(sourceCode.charAt(i) == '{') {
        containerStack.push('{');
      } else if(sourceCode.charAt(i) == '}') {
        if(containerStack.empty())
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid curly-brace: found a '}' prior an associated '{'!");
        char opener = containerStack.pop();
        if(opener != '{')
          throw new ReaderException(i,sourceCode,source,"READ ERROR: Invalid curly-brace: found a closing '}' prior to closing '%c'!", opener);
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
      if(isReaderLambdaLiteral(sourceCode,i)) {
        return parseReaderLambdaLiteralLogic(sourceCode,i+1,containerStack,source,ignoringIncomplete);
      }

      // Expand reader shorthand literals
      if(isReaderShorthand(sourceCode.charAt(i))) 
        return parseReaderShorthandLiteral(sourceCode,i,n,containerStack,source,ignoringIncomplete);

      // Parse List/Pair
      if(sourceCode.charAt(i) == '(') {
        return parseListLiteral(sourceCode,i+1,n,containerStack,source,ignoringIncomplete);
      }

      // Parse Vector
      if(sourceCode.charAt(i) == '[') {
        return parseVectorLiteral(sourceCode,i+1,n,containerStack,source,ignoringIncomplete);
      }

      // Parse Hashmap
      if(sourceCode.charAt(i) == '{') {
        return parseHashmapLiteral(sourceCode,i+1,n,containerStack,source,ignoringIncomplete);
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

        // Parse Character Literals
        if(isCharacterLiteral(sourceCode,i,n)) 
          return parseCharacterLiteral(sourceCode,i,n,source);
      }

      // Parse String Literals
      if(sourceCode.charAt(i) == '"') {
        return parseStringLiteral(sourceCode,i+1,n,source,ignoringIncomplete);
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
    throw new IncompleteException(ignoringIncomplete,sourceCode.length()-1,sourceCode,source,"READ ERROR: Terminated prior to being able to parse a datum!");
  }


  ////////////////////////////////////////////////////////////////////////////
  // Implementing "read": returns a pair: 
  //                      1. the read datum (null/#void if only read whitespace/comments)
  //                      2. the length of characters read from <sourceCode> to produce the read datum

  // @return: <.first> is <null> if only read in whitespace/comments!
  public static Pair<Datum,Integer> nullableRead(CharSequence sourceCode, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    return readLoop(sourceCode,0,new Stack<Character>(),source,ignoringIncomplete);
  }


  // @return: <.first> is <#void> if only read in whitespace/comments!
  public static Pair<Datum,Integer> read(CharSequence sourceCode, SourceInformation source, boolean ignoringIncomplete) throws ReaderException {
    Pair<Datum,Integer> result = nullableRead(sourceCode,source,ignoringIncomplete);
    if(result.first != null) return result;
    return new Pair<Datum,Integer>(escm.type.Void.VALUE,result.second);
  }
}