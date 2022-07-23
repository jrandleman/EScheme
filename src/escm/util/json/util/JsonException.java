// Author: Jordan Randleman - escm.util.json.util.JsonException
// Purpose:
//    Class that generates exceptions from the JSON library

package escm.util.json.util;

public class JsonException extends Exception {
  private static String getArrowString(int arrowIndex, int strlength) {
    StringBuilder sb = new StringBuilder();
    for(int i = 0; i < strlength+1; ++i) { // "+1" accounts for '"'
      if(i == arrowIndex) {
        sb.append('^');
      } else {
        sb.append(' ');
      }
    }
    return sb.toString();
  }


  private static String getJsonErrorSubstring(int index, String jsonStr) {
    int substringSpan = 40;
    String indent = "\n    ";
    int n = jsonStr.length();
    int startIndex = index-substringSpan < 0 ? 0 : index-substringSpan;
    int endPosition = index+substringSpan+1 > n ? n : index+substringSpan+1;
    Pair<Integer,String> result = StringParser.escape(index-startIndex,jsonStr.substring(startIndex,endPosition));
    return "\n\n    Error Substring: " + '"' + result.second +  '"' +
             "\n                     " + getArrowString(result.first,result.second.length());
  }


  private static String getJsonErrorEntireString(int index, String jsonStr) {
    Pair<Integer,String> result = StringParser.escape(index,jsonStr);
    return String.format("\n\n    At index %d: \"%s\"", result.first, result.second);
  }


  public JsonException(int index, String jsonStr, String fmt, Object ... args) {
    super(String.format(fmt,args) + getJsonErrorSubstring(index,jsonStr) + getJsonErrorEntireString(index,jsonStr));
  }
}