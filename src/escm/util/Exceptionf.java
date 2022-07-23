// Author: Jordan Randleman - escm.util.Exceptionf
// Purpose:
//    Simple <Exception> wrapper for formatted strings upon construction, and
//      with a helper static method to profile an argument list (used extensively 
//      by Java primitives).

package escm.util;
import java.util.ArrayList;
import escm.type.Datum;

public class Exceptionf extends Exception {
  ////////////////////////////////////////////////////////////////////////////
  // Static method to profile a set of Java primitive arguments
  public static String profileArgs(ArrayList<Datum> args) {
    int n = args.size();
    if(n == 0) return "given 0 args";
    StringBuilder sb = new StringBuilder("given ");
    for(int i = 0; i < n; ++i) {
      sb.append(args.get(i).profile());
      if(i+1 < n) sb.append(", ");
    }
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Exceptionf(String fmt, Object ... args) {
    super(String.format(fmt,args));
  }
}