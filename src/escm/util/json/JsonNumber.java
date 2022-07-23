// Author: Jordan Randleman - escm.util.json.JsonNumber
// Purpose:
//    JSON Number class.

package escm.util.json;
import java.util.Objects;

public class JsonNumber extends JsonDatum {
  ////////////////////////////////////////////////////////////////////////////
  // Value Storage
  private double value = 0.0;

  public double value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public JsonNumber(double value) {
    this.value = value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public String toString() {
    return String.valueOf(value);
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
    return o instanceof JsonNumber && ((JsonNumber)o).value == value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hashing
  public int hashCode() {
    return Objects.hash("number",toString());
  }
}