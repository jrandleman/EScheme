// Author: Jordan Randleman - escm.type.String
// Purpose:
//    String primitive type.

package escm.type;
import java.util.Objects;
import escm.util.StringParser;
import escm.vm.util.ExecutionState;

public class String extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private java.lang.String value = "";


  ////////////////////////////////////////////////////////////////////////////
  // Value Getter
  public java.lang.String value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Character & Codepoint Extraction
  // From: https://stackoverflow.com/questions/1527856/how-can-i-iterate-through-the-unicode-codepoints-of-a-java-string
  public escm.type.Character[] toChars() {
    int offset = 0, charsIdx = 0, strLength = value.length();
    escm.type.Character[] chars = new escm.type.Character[value.codePointCount(0,strLength)];
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      chars[charsIdx++] = new escm.type.Character(codepoint);
      offset += java.lang.Character.charCount(codepoint);
    }
    return chars;
  }

  public int[] toCodepoints() {
    int offset = 0, cpIdx = 0, strLength = value.length();
    int[] codepoints = new int[value.codePointCount(0,strLength)];
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      codepoints[cpIdx++] = codepoint;
      offset += java.lang.Character.charCount(codepoint);
    }
    return codepoints;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public String(java.lang.String s) {
    value = s;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "string";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof String && ((String)o).value.equals(value);
  }

  public boolean equal(Object o) {
    return eq(o);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),value);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Serialization
  public java.lang.String display() {
    return value;
  }

  public java.lang.String write() {
    return '"' + StringParser.escape(value) + '"';
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public String loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public String loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying semantics
  public String copy() {
    return this;
  }
}