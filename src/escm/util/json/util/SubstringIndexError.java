// Author: Jordan Randleman - escm.util.json.util.SubstringIndexError
// Purpose:
//    Class that generates the arrow-pointing substring message in JSON errors.

package escm.util.json.util;
import escm.util.Pair;
import escm.util.StringParser;

public class SubstringIndexError {
  private static String getArrowString(int arrowIndex, int strlength) {
    StringBuilder sb = new StringBuilder(" "); // " " accounts for passing the opening '"'
    for(int i = 0; i < strlength+1; ++i) { // "+1" accounts for possibly pointing to last '"'
      if(i == arrowIndex) {
        sb.append('^');
        return sb.toString();
      } else {
        sb.append(' ');
      }
    }
    return sb.toString();
  }


  // Returns: ["machine-readable-substring", "arrow-pointing-to-error-index"]
  public static String[] run(int index, String str, int substringLength) {
    int substringSpan = substringLength/2;
    int n = str.length();
    int startIndex = index-substringSpan <= 0 ? 0 : index-substringSpan;
    int endPosition = index+substringSpan+1 >= n ? n : index+substringSpan+1;
    Pair<Integer,String> result = StringParser.escape(index-startIndex,str.substring(startIndex,endPosition));
    return new String[]{'"' + result.second + '"', getArrowString(result.first,result.second.length())};
  }
}