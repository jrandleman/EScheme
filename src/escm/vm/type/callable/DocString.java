// Author: Jordan Randleman - escm.vm.type.callable.DocString
// Purpose:
//    Primitive interface for callables to enable introspection 
//    on their associated documentation.

package escm.vm.type.callable;
import java.io.Serializable;

public interface DocString extends Serializable {
  ////////////////////////////////////////////////////////////////////////////
  // <docstring> Formatter
  private static String stripLeadingNewlines(String docstring) {
    int i = 0, n = docstring.length();
    while(i < n && docstring.charAt(i) == '\n') ++i;
    if(i == 0) return docstring;
    if(i == n) return "";
    return docstring.substring(i);
  }

  private static int getMinimumLeftPad(String[] lines) {
    int min_left_pad = Integer.MAX_VALUE;
    for(int i = 0; i < lines.length; ++i) {
      lines[i] = lines[i].stripTrailing();
      if(!lines[i].isEmpty()) {
        int j = 0, n = lines[i].length();
        while(j < n && lines[i].charAt(j) == ' ') ++j;
        if(j < min_left_pad) min_left_pad = j;
      }
    }
    return min_left_pad;
  }

  private static String joinTrimmedLines(String[] lines, int min_left_pad) {
    StringBuilder sb = new StringBuilder();
    for(int i = 0; i < lines.length; ++i) {
      if(!lines[i].isEmpty()) sb.append(lines[i].substring(min_left_pad));
      if(i+1 < lines.length) sb.append('\n');
    }
    return sb.toString();
  }

  // Converts all tabs to be 2 spaces.
  // Trims strings to align along the left side with 0 padding
  public static String format(String docstring) {
    docstring = stripLeadingNewlines(docstring).stripTrailing().replaceAll("\t","  ");
    if(docstring.isEmpty()) return "";
    String[] lines = docstring.split("\n");
    int min_left_pad = getMinimumLeftPad(lines);
    if(min_left_pad == 0) return docstring;
    return joinTrimmedLines(lines,min_left_pad);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Required Method
  public String docstring();
}