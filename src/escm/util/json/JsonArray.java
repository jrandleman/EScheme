// Author: Jordan Randleman - escm.util.json.JsonArray
// Purpose:
//    JSON Array class.

package escm.util.json;
import java.util.ArrayList;
import java.util.Objects;

public class JsonArray extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  private ArrayList<JsonDatum> value = new ArrayList<JsonDatum>();

  public ArrayList<JsonDatum> value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public JsonArray(ArrayList<JsonDatum> value) {
    this.value = value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    StringBuilder sb = new StringBuilder("[");
    for(int i = 0, n = value.size(); i < n; ++i) {
      sb.append(value.get(i).toString());
      if(i+1 < n) sb.append(",");
    }
    sb.append("]");
    return sb.toString();
  }


  public String toString(int tabWidth) {
    return toString(0,tabWidth);
  }


  protected String toString(int accWidth, int tabWidth) {
    if(tabWidth == 0) return toString();
    String acc = JsonDatum.getSpaceString(accWidth);
    String tab = JsonDatum.getSpaceString(tabWidth);
    StringBuilder sb = new StringBuilder("[\n");
    for(int i = 0, n = value.size(); i < n; ++i) {
      sb.append(acc + tab + value.get(i).toString(accWidth+tabWidth,tabWidth));
      if(i+1 < n) sb.append(",\n");
    }
    sb.append('\n' + acc + ']');
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean equals(Object o) {
    return o instanceof JsonArray && ((JsonArray)o).value.equals(value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("array",value);
  }
}