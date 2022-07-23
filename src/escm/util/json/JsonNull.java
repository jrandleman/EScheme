// Author: Jordan Randleman - escm.util.json.JsonNull
// Purpose:
//    JSON Null class.

package escm.util.json;
import java.util.Objects;

public class JsonNull extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  public static final JsonNull VALUE = new JsonNull();


  ////////////////////////////////////////////////////////////////////////////
  // Ban using the Constructor
  private JsonNull(){}


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    return "null";
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
    return o instanceof JsonNull;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("null","null");
  }
}