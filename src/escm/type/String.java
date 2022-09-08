// Author: Jordan Randleman - escm.type.String
// Purpose:
//    String primitive type.

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.number.Real;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.StringParser;
import escm.vm.util.ExecutionState;
import escm.vm.util.ExecutionState;
import escm.vm.type.Callable;

public class String extends Datum implements Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private java.lang.String value = "";


  ////////////////////////////////////////////////////////////////////////////
  // Value Getter
  public java.lang.String value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // CodePoint Count
  public int codePointLength() {
    return value.codePointCount(0,value.length());
  }


  ////////////////////////////////////////////////////////////////////////////
  // CodePoint Getter (effectively our version of <charAt>)
  // => Can't use <java.lang.String.codePointAt(int)> since it will create a
  //    codepoint at the char-relative index arg it's given.
  public int codePointAt(int index) throws Exception {
    int offset = 0, codePointIndex = 0, strLength = value.length();
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      if(codePointIndex == index) return codepoint;
      offset += java.lang.Character.charCount(codepoint);
      ++codePointIndex;
    }
    throw new Exceptionf("STRING [GET]: Invalid index %d (size %d) for string %s", index, codePointLength(), write());
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

  public int[] toCodePoints() {
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
  // Callable (unary index getter)
  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    if(arguments.size() != 1)
      throw new Exceptionf("STRING [CALLABLE-GET]: Expects exactly 1 integer index arg for string %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum idxDatum = arguments.get(0);
    if(!(idxDatum instanceof Real) || !((Real)idxDatum).isInteger())
      throw new Exceptionf("STRING [CALLABLE-GET]: Expects exactly 1 integer index arg for string %s: %s", write(), Exceptionf.profileArgs(arguments));
    int index = ((Real)idxDatum).intValue();
    if(index < 0 || index >= codePointLength())
      throw new Exceptionf("STRING [CALLABLE-GET]: Invalid index %d (size %d) for string %s", index, codePointLength(), write());
    return continuation.run(new escm.type.Character(codePointAt(index)));
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
    return '"' + StringParser.escapeWithCustomUnicodeEscape(value) + '"';
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