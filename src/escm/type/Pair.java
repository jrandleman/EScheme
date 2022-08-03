// Author: Jordan Randleman - escm.type.Pair
// Purpose:
//    Pair primitive type.

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import escm.vm.type.ExecutionState;

public class Pair extends Datum {
  ////////////////////////////////////////////////////////////////////////////
  // Private Car/Cdr Fields
  private Datum car;
  private Datum cdr;


  ////////////////////////////////////////////////////////////////////////////
  // Public Car/Cdr Getters
  public Datum car() {
    return car;
  }
  
  public Datum cdr() {
    return cdr;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public Pair(Datum car, Datum cdr) {
    this.car = car;
    this.cdr = cdr;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Generator
  public static Datum List(Datum ... listContents) {
    Datum d = Nil.VALUE;
    for(int i = listContents.length-1; i >= 0; --i)
      d = new Pair(listContents[i],d);
    return d;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Predicate
  public static boolean isList(Datum d) {
    if(d instanceof Nil) return true;
    if(!(d instanceof Pair)) return false;
    Datum iterator = d;
    while(iterator instanceof Pair) iterator = ((Pair)iterator).cdr;
    return iterator instanceof Nil;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type
  public java.lang.String type() {
    return "pair";
  }


  ////////////////////////////////////////////////////////////////////////////
  // Truthiness
  public boolean isTruthy() {
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Equality
  public boolean eq(Object o) {
    return o instanceof Pair && ((Pair)o).car.eq(car) && ((Pair)o).cdr.eq(cdr);
  }

  public boolean eqv(Object o) {
    return eq(o);
  }

  public boolean equals(Object o) {
    return o instanceof Pair && ((Pair)o).car.equals(car) && ((Pair)o).cdr.equals(cdr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Hash code
  public int hashCode() {
    return Objects.hash(type(),car,cdr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Basic Serialization
  public java.lang.String display() {
    if(!(cdr instanceof Pair) && !(cdr instanceof Nil)) { // printing non-list pair
      return "(" + car.display() + " . " + cdr.display() + ")";
    } else { // printing a list
      Datum iterator = this;
      boolean addSpace = false;
      StringBuilder list = new StringBuilder("(");
      while(iterator instanceof Pair) {
        if(addSpace) {
          list.append(' ');
        } else {
          addSpace = true;
        }
        list.append(((Pair)iterator).car.display());
        iterator = ((Pair)iterator).cdr;
      }
      if(!(iterator instanceof Nil)) { // dotted list
        list.append(" . ");
        list.append(iterator.display());
      }
      list.append(')');
      return list.toString();
    }
  }

  public java.lang.String write() {
    if(!(cdr instanceof Pair) && !(cdr instanceof Nil)) { // printing non-list pair
      return "(" + car.write() + " . " + cdr.write() + ")";
    } else { // printing a list
      Datum iterator = this;
      boolean addSpace = false;
      StringBuilder list = new StringBuilder("(");
      while(iterator instanceof Pair) {
        if(addSpace) {
          list.append(' ');
        } else {
          addSpace = true;
        }
        list.append(((Pair)iterator).car.write());
        iterator = ((Pair)iterator).cdr;
      }
      if(!(iterator instanceof Nil)) { // dotted list
        list.append(" . ");
        list.append(iterator.write());
      }
      list.append(')');
      return list.toString();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Pretty-Printing
  private static final int PRETTY_PRINT_COLUMN_WIDTH = 80;
  

  // Proper list predicate
  private static boolean isProperList(Datum d) {
    while(d instanceof Pair) d = ((Pair)d).cdr();
    return d instanceof Nil;
  }


  // Pprint Datum Storing Stringified Data & Length of Stringified Data
  private static class PPrintDatum {
    // <output_len> Denotes length of <exp> or <datum_str> once ouput
    public int output_len = 0;
    public boolean is_exp = false;
    public boolean is_sym = false;
    // Either a datum_str non-proper-list-pair or and <exp> of <PPrintDatum>
    public java.lang.String datum_str = null;
    public ArrayList<PPrintDatum> exp = null;
    // Constructors
    public PPrintDatum(java.lang.String str) {
      output_len = str.length();
      is_sym = false;
      datum_str = str;
    }
    public PPrintDatum(java.lang.String str, boolean is_symbol) {
      output_len = str.length();
      is_sym = is_symbol;
      datum_str = str;
    }
    public PPrintDatum(ArrayList<PPrintDatum> e, int len) {
      output_len = len;
      is_exp = true;
      exp = e;
    }
  }


  // Gets the length of <list_as_strs> once output
  private static int get_pprint_data_ouput_length(ArrayList<PPrintDatum> list_as_strs, int output_length) {
    // initial value accounts for outputting both parens & spaces between elts
    output_length = 2 + list_as_strs.size()-1;
    for(PPrintDatum e : list_as_strs) output_length += e.output_len;
    return output_length;
  }


  // Converts Scheme lists of data to an AST list of those data as strings
  // NOTE: the <int> of the pair denotes the length of the <data> once output 
  private static int stringify_list_data(ArrayList<PPrintDatum> list_as_strs, int length, Pair p) {
    // Strify car
    Datum car = p.car();
    if(!(car instanceof Pair)) {
      list_as_strs.add(new PPrintDatum(car.pprint(),car instanceof Symbol));
    } else if(!isProperList(car)) {
      list_as_strs.add(new PPrintDatum(car.write()));
    } else {
      ArrayList<PPrintDatum> sub_exp = new ArrayList<PPrintDatum>();
      int sub_exp_len = stringify_list_data(sub_exp,0,(Pair)car);
      list_as_strs.add(new PPrintDatum(sub_exp,sub_exp_len));
    }
    Datum cdr = p.cdr();
    // Strify cdr
    if(cdr instanceof Nil) {
      return get_pprint_data_ouput_length(list_as_strs,length); // get length of this stringified list as output (also @ end of list)
    }
    if(!(cdr instanceof Pair)) {
      list_as_strs.add(new PPrintDatum(cdr.pprint(),cdr instanceof Symbol));
    } else if(!isProperList(cdr)) {
      list_as_strs.add(new PPrintDatum(cdr.write()));
    } else {
      return stringify_list_data(list_as_strs,length,(Pair)cdr);
    }
    return length;
  }


  // Stringifies <list_as_strs> w/o any tabs
  private static void print_pprint_data_as_is(ArrayList<PPrintDatum> list_as_strs, StringBuilder buffer) {
    buffer.append('(');
    for(int i = 0, n = list_as_strs.size(); i < n; ++i) {
      if(list_as_strs.get(i).is_exp) {
        print_pprint_data_as_is(list_as_strs.get(i).exp,buffer);
      } else {
        buffer.append(list_as_strs.get(i).datum_str);
      }
      if(i+1 != n) buffer.append(' ');
    }
    buffer.append(')');
  }


  // Prints a list beginning w/ a non-symbol atom
  private static int pretty_print_list_of_data(ArrayList<PPrintDatum> list_as_strs, int depth, StringBuilder buffer, char[] tabs) {
    tabs[2*depth+1] = 0; // shorten tabs to account for specialized stringification
    for(int col_length = 2*depth, i = 0, n = list_as_strs.size(); i < n; ++i) {
      if(i > 0 && list_as_strs.get(i).output_len + col_length > PRETTY_PRINT_COLUMN_WIDTH) {
        buffer.append('\n');
        buffer.append(tabs);
        col_length = 2*depth;
      }
      col_length += list_as_strs.get(i).output_len + 1; // +1 accounts for spaces
      if(list_as_strs.get(i).is_exp) {
        pretty_print_pprint_data(list_as_strs.get(i).exp,list_as_strs.get(i).output_len,depth+1,buffer);
      } else {
        buffer.append(list_as_strs.get(i).datum_str);
      }
      if(i+1 != n) buffer.append(' ');
    }
    return depth;
  }


  // Show info on the parsed stringified data
  private static void pretty_print_pprint_data(ArrayList<PPrintDatum> list_as_strs, int len, int depth, StringBuilder buffer) {
    // Print as is if possible
    if(len + 2*depth <= PRETTY_PRINT_COLUMN_WIDTH || len < 2) {
      print_pprint_data_as_is(list_as_strs,buffer);
      return;
    }
    // Get tab (2 spaces per tab) width as per the depth
    char[] tabs = new char [2*depth+3];
    for(int i = 0, tabs_len = 2*depth+2; i < tabs_len; ++i) tabs[i] = ' ';
    tabs[2*depth+2] = 0;
    // Open paren
    buffer.append('(');
    int n = list_as_strs.size();
    int i = 1;
    // If 1st elt is a non-symbol atom, specialize stringification
    if(!list_as_strs.get(0).is_sym && !list_as_strs.get(0).is_exp) {
      depth = pretty_print_list_of_data(list_as_strs,depth,buffer,tabs);
      buffer.append(')');
      return;
    }
    // If 1st elt is a list, hence special stringification case for such applications
    if(list_as_strs.get(0).is_exp) {
      pretty_print_pprint_data(list_as_strs.get(0).exp,list_as_strs.get(0).output_len,depth+1,buffer);
    } else {
      buffer.append(list_as_strs.get(0).datum_str);
      buffer.append(' ');
      // If 2nd elt printable on the current line (another special case)
      if(list_as_strs.get(1).output_len + list_as_strs.get(0).output_len + 2*depth < PRETTY_PRINT_COLUMN_WIDTH){
        i = 2;
        if(list_as_strs.get(1).is_exp) {
          print_pprint_data_as_is(list_as_strs.get(1).exp,buffer);
        } else {
          buffer.append(list_as_strs.get(1).datum_str);
        }
      }
    }
    if(i < n) buffer.append('\n');
    // Print body of the list
    for(; i < n; ++i) {
      buffer.append(tabs);
      if(list_as_strs.get(i).is_exp) {
        pretty_print_pprint_data(list_as_strs.get(i).exp,list_as_strs.get(i).output_len,depth+1,buffer);
      } else {
        buffer.append(list_as_strs.get(i).datum_str);
      }
      if(i+1 != n) buffer.append('\n');
    }
    // Free <tabs> & add the closing paren
    buffer.append(')');
  }


  // Pretty printer launch
  public java.lang.String pprint() {
    // If non-proper-list-pair, print as-is
    if(!isProperList(this)) return write();
    // Else check if pair as string is of valid length
    java.lang.String as_string = write();
    if(as_string.length() <= PRETTY_PRINT_COLUMN_WIDTH) return as_string;
    // Otherwise get list as string-ified objects
    ArrayList<PPrintDatum> list_as_strs = new ArrayList<PPrintDatum>();
    int output_length = stringify_list_data(list_as_strs,0,this);
    // Print the list w/ indents
    StringBuilder buffer = new StringBuilder();
    pretty_print_pprint_data(list_as_strs,output_length,0,buffer);
    return buffer.toString();
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Pair loadWithState(ExecutionState state) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Pair loadWithName(java.lang.String name) throws Exception {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Datum copy() {
    return this;
  }
}