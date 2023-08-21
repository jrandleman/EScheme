// Author: Jordan Randleman - escm.util.json.util.JsonException
// Purpose:
//    Class that generates exceptions from the JSON library

package escm.util.json.util;
import escm.util.Pair;
import escm.util.string.StringParser;

public class JsonException extends Exception {
  // Length of the substring to show for Json parsing errors
  private static final int ERROR_SUBSTRING_LENGTH = 80;


  private static String getJsonErrorSubstring(int index, String jsonStr) {
    String [] errorMessage = SubstringIndexError.run(index,jsonStr,ERROR_SUBSTRING_LENGTH);
    return "\n\n    Error Substring: " + errorMessage[0] +
             "\n                     " + errorMessage[1];
  }


  private static String getJsonErrorEntireString(int index, String jsonStr) {
    Pair<Integer,String> result = StringParser.escape(index,jsonStr);
    return String.format("\n\n    At index %d: \"%s\"", result.first, result.second);
  }


  public JsonException(int index, String jsonStr, String fmt, Object ... args) {
    super(String.format(fmt,args) + getJsonErrorSubstring(index,jsonStr) + getJsonErrorEntireString(index,jsonStr));
  }
}