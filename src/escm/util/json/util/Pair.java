// Author: Jordan Randleman - escm.util.json.util.Pair
// Purpose:
//    Simple generic pair data structure. Used extensively by the parser.

package escm.util.json.util;

public class Pair<T,U> {
  public T first;
  public U second;
  public Pair(T t, U u) {
    first = t;
    second = u;
  }
}