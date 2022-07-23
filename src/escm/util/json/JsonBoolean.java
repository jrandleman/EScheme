// Author: Jordan Randleman - escm.util.json.JsonBoolean
// Purpose:
//    JSON Boolean class.

package escm.util.json;
import java.util.Objects;

public class JsonBoolean extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  public static final JsonBoolean TRUE = new JsonBoolean();

  public static final JsonBoolean FALSE = new JsonBoolean();


  ////////////////////////////////////////////////////////////////////////////
  // Create From Value
  public static JsonBoolean valueOf(boolean b) {
    if(b) return TRUE;
    return FALSE;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Ban using the Constructor
  private JsonBoolean(){}


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    if(this == TRUE) return "true";
    return "false";
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
    return o instanceof JsonBoolean && ((JsonBoolean)o) == this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("boolean",toString());
  }
}