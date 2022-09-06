// Author: Jordan Randleman - escm.type.Character
// Purpose:
//    Character primitive type.
//    Unicode note (after #\): 
//      => uXXXX creates a true 16bit java character.
//      => UXXXXXXXX creates a 32bit code-point (becomes 2 chars in Strings).

package escm.type;
import java.util.Objects;
import escm.vm.util.ExecutionState;

public class Character extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Character parsing
  private static final Character SPACE     = new Character(' ');
  private static final Character TAB       = new Character('\t');
  private static final Character NEWLINE   = new Character('\n');
  private static final Character PAGE      = new Character('\f');
  private static final Character RETURN    = new Character('\r');
  private static final Character BACKSPACE = new Character('\b');
  private static final Character NUL       = new Character('\0');
  private static final Character ESCAPE    = new Character(27);
  private static final Character DELETE    = new Character(127);

  private static Integer parseHexString(java.lang.String hexString) {
    try {
      return (int)Long.parseLong(hexString,16);
    } catch(Exception e) {
      return null;
    }
  }

  private static Character parseUnicodeCharacter(int n, char firstChar, java.lang.String token) {
    if((firstChar == 'u' && n > 5) || (firstChar == 'U' && n > 9)) return null;
    Integer codePoint = parseHexString(token.substring(1));
    if(codePoint == null) return null;
    return new Character(codePoint);
  }

  // @PRECONDITION: Assumes given w/o the "#\" prefix 
  // @POSTCONDITION: Returns <null> if an invalid character
  public static Character parse(java.lang.String token) {
    int n = token.length();
    if(n == 0) return NUL;
    char firstChar = token.charAt(0);
    if(n == 1) return new Character(firstChar);
    switch(token) {
      case "space":     return SPACE;
      case "tab":       return TAB;
      case "newline":   return NEWLINE;
      case "page":      return PAGE;
      case "return":    return RETURN;
      case "backspace": return BACKSPACE;
      case "nul":       return NUL;
      case "esc":       return ESCAPE;
      case "delete":    return DELETE;
      default: {
        if(firstChar == 'u' || firstChar == 'U')
          return parseUnicodeCharacter(n,firstChar,token);
        return null;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Private Value Field
  private int value = 0;


  ////////////////////////////////////////////////////////////////////////////
  // Value Getter
  public int value() {
    return value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructors
  public Character(int codePoint) {
    value = codePoint;
  }

  public Character(char ch) {
    value = ch;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Size Predicates
  public boolean isSurrogateCharPair() {
    return (value & 0x00000000ffffffffL) > 0xffffL;
  }

  public boolean isJavaChar() {
    return (value & 0x00000000ffffffffL) <= 0xffffL;
  }

  public boolean isAsciiChar() {
    return (value & 0x00000000ffffffffL) <= 0xffL;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "character";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Character && ((Character)o).value == value;
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
    return java.lang.Character.toString(value);
  }

  public java.lang.String write() {
    switch(value) {
      case ' ':  return "#\\space";
      case '\t': return "#\\tab";
      case '\n': return "#\\newline";
      case '\f': return "#\\page";
      case '\r': return "#\\return";
      case '\b': return "#\\backspace";
      case '\0': return "#\\nul";
      case 27:   return "#\\esc";
      case 127:  return "#\\delete";
      default: {
        if(value >= 32 && value <= 126) return "#\\"+(char)value;
        Long unsignedValue = value & 0x00000000ffffffffL;
        if(unsignedValue > 0xffffL) {
          return "#\\U" + Long.toHexString(unsignedValue);
        }
        return "#\\u" + Long.toHexString(unsignedValue);
      }
    }
  }

  public java.lang.String pprint() {
    return write();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Character loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Character loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Character copy() {
    return this;
  }
}