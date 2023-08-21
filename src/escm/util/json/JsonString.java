// Author: Jordan Randleman - escm.util.json.JsonString
// Purpose:
//    JSON String class.

package escm.util.json;
import java.util.Objects;
import escm.util.string.StringParser;

public class JsonString extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  private String value = "";

  public String value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public JsonString(String value) {
    this.value = value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    return '"'+StringParser.escape(value)+'"';
  }


  public String toString(int tabWidth) {
    return toString();
  }


  protected String toString(int accWidth, int tabWidth) {
    return toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean equals(Object o) {
    return o instanceof JsonString && ((JsonString)o).value.equals(value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("string",toString());
  }
}