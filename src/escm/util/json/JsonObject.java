// Author: Jordan Randleman - escm.util.json.JsonObject
// Purpose:
//    JSON Object class.

package escm.util.json;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import escm.util.json.util.StringParser;

public class JsonObject extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  private HashMap<String,JsonDatum> value;

  public HashMap<String,JsonDatum> value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public JsonObject(HashMap<String,JsonDatum> value) {
    this.value = value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    StringBuilder sb = new StringBuilder("{");
    int i = 0, n = value.size();
    for(Map.Entry<String,JsonDatum> e : value.entrySet()) {
      sb.append('"');
      sb.append(StringParser.escape(e.getKey()));
      sb.append("\":");
      sb.append(e.getValue().toString());
      if(i+1 < n) sb.append(',');
      ++i;
    }
    sb.append('}');
    return sb.toString();
  }


  public String toString(int tabWidth) {
    return toString(0,tabWidth);
  }


  protected String toString(int accWidth, int tabWidth) {
    if(tabWidth == 0) return toString();
    String acc = JsonDatum.getSpaceString(accWidth);
    String tab = JsonDatum.getSpaceString(tabWidth);
    StringBuilder sb = new StringBuilder("{\n");
    int i = 0, n = value.size();
    for(Map.Entry<String,JsonDatum> e : value.entrySet()) {
      sb.append(acc + tab + '"');
      sb.append(StringParser.escape(e.getKey()));
      sb.append("\": ");
      sb.append(e.getValue().toString(accWidth+tabWidth,tabWidth));
      if(i+1 < n) sb.append(",\n");
      ++i;
    }
    sb.append('\n' + acc + '}');
    return sb.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean equals(Object o) {
    return o instanceof JsonObject && ((JsonObject)o).value.equals(value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("object",value);
  }
}