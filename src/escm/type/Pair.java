// Author: Jordan Randleman - escm.type.Pair
// Purpose:
//    Pair primitive type.
//
//    => NOTE THAT AC/OC PRIMITIVES EXPECT ALL ARGS TO BE LISTS!

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.util.Environment;
import escm.vm.util.ExecutionState;
import escm.type.procedure.types.TypeChecker.Predicate;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.callable.Callable;

public class Pair extends Datum implements OrderedCollection {
  ////////////////////////////////////////////////////////////////////////////
  // Value returned by <length> for dotted lists
  public static final int DOTTED_LIST_LENGTH = -1;


  ////////////////////////////////////////////////////////////////////////////
  // Private Car/Cdr & Length Fields
  private Datum car;
  private Datum cdr;

  private int length; // thanks immutability!


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
    if(cdr instanceof Pair) {
      this.length = ((Pair)cdr).length;
      if(this.length != DOTTED_LIST_LENGTH) ++this.length;
    } else if(cdr instanceof Nil) {
      this.length = 1;
    } else {
      this.length = DOTTED_LIST_LENGTH;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Generator
  public static Datum List(Datum ... listContents) {
    Datum d = Nil.VALUE;
    for(int i = listContents.length-1; i >= 0; --i)
      d = new Pair(listContents[i],d);
    return d;
  }

  public static Datum DottedList(Datum value, Datum ... listContents) {
    if(listContents.length == 0) return value;
    Datum d = listContents[listContents.length-1];
    for(int i = listContents.length-2; i >= 0; --i)
      d = new Pair(listContents[i],d);
    d = new Pair(value,d);
    return d;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Static List Predicates
  public static boolean isList(Datum d) {
    return d instanceof Nil || (d instanceof Pair && ((Pair)d).length != DOTTED_LIST_LENGTH);
  }

  public static boolean isListPair(Datum d) {
    return d instanceof Pair && ((Pair)d).length != DOTTED_LIST_LENGTH;
  }

  public static boolean isDottedList(Datum d) {
    return d instanceof Pair && ((Pair)d).length == DOTTED_LIST_LENGTH;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Length Getter (returns <Pair.DOTTED_LIST_LENGTH> for dotted-lists)
  public int length() {
    return length;
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

  public boolean equal(Object o) {
    return o instanceof Pair && ((Pair)o).car.equal(car) && ((Pair)o).cdr.equal(cdr);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Documentation String
  public java.lang.String docstring() {
    if(length == DOTTED_LIST_LENGTH) return "Pair that points to a dotted list.";
    return "Pair that points to a list of length "+length+".";
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
  // Pretty-Printing (adapted from my Heist-Scheme C++ pretty-printer)
  private static final int PRETTY_PRINT_COLUMN_WIDTH = 80;


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


  // Add leading non-0 characters from <chars> to <buffer>
  private static void add_chars_to_buffer(StringBuilder buffer, char[] chars) {
    for(int i = 0; i < chars.length && chars[i] != 0; ++i) 
      buffer.append(chars[i]);
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
  // NOTE: We pass <escm.type.Character> as a thin wrapper around a Java <int>
  //       as the ultimate return value of this function.
  private static Trampoline.SafeBounce stringify_list_car_data(ArrayList<PPrintDatum> list_as_strs, int length, Datum car, Trampoline.SafeContinuation continuation) {
    if(!(car instanceof Pair)) {
      list_as_strs.add(new PPrintDatum(car.pprint(),car instanceof Symbol));
    } else if(!isList(car)) {
      list_as_strs.add(new PPrintDatum(car.write()));
    } else {
      ArrayList<PPrintDatum> sub_exp = new ArrayList<PPrintDatum>();
      return () -> stringify_list_data(sub_exp,0,(Pair)car,(subExpLenChar) -> () -> {
        int sub_exp_len = ((Character)subExpLenChar).value();
        list_as_strs.add(new PPrintDatum(sub_exp,sub_exp_len));
        return continuation.run(Void.VALUE);
      });
    }
    return continuation.run(Void.VALUE);
  }

  private static Trampoline.SafeBounce stringify_list_cdr_data(ArrayList<PPrintDatum> list_as_strs, int length, Datum cdr, Trampoline.SafeContinuation continuation) {
    if(cdr instanceof Nil) {
      // get length of this stringified list as output (also @ end of list)
      return continuation.run(new Character(get_pprint_data_ouput_length(list_as_strs,length)));
    }
    if(!(cdr instanceof Pair)) {
      list_as_strs.add(new PPrintDatum(cdr.pprint(),cdr instanceof Symbol));
    } else if(!isList(cdr)) {
      list_as_strs.add(new PPrintDatum(cdr.write()));
    } else {
      return () -> stringify_list_data(list_as_strs,length,(Pair)cdr,continuation);
    }
    return continuation.run(new Character(length));
  }

  private static Trampoline.SafeBounce stringify_list_data(ArrayList<PPrintDatum> list_as_strs, int length, Pair p, Trampoline.SafeContinuation continuation) {
    return stringify_list_car_data(list_as_strs,length,p.car(),(ignore) -> () -> {
      return stringify_list_cdr_data(list_as_strs,length,p.cdr(),continuation);
    });
  }


  // Stringifies <list_as_strs> w/o any tabs
  private static Trampoline.SafeBounce print_pprint_data_as_is_iter(ArrayList<PPrintDatum> list_as_strs, StringBuilder buffer, int i, int n, Trampoline.SafeContinuation continuation) {
    if(i >= n) return continuation.run(Void.VALUE);
    if(list_as_strs.get(i).is_exp) {
      return () -> print_pprint_data_as_is(list_as_strs.get(i).exp,buffer,(ignore) -> () -> {
        if(i+1 != n) buffer.append(' ');
        return () -> print_pprint_data_as_is_iter(list_as_strs,buffer,i+1,n,continuation);
      });
    } else {
      buffer.append(list_as_strs.get(i).datum_str);
      if(i+1 != n) buffer.append(' ');
      return () -> print_pprint_data_as_is_iter(list_as_strs,buffer,i+1,n,continuation);
    }
  }

  private static Trampoline.SafeBounce print_pprint_data_as_is(ArrayList<PPrintDatum> list_as_strs, StringBuilder buffer, Trampoline.SafeContinuation continuation) {
    buffer.append('(');
    return print_pprint_data_as_is_iter(list_as_strs,buffer,0,list_as_strs.size(),(ignore) -> () -> {
      buffer.append(')');
      return continuation.run(Void.VALUE);
    });
  }


  // Prints a list beginning w/ a non-symbol atom
  private static Trampoline.SafeBounce pretty_print_list_of_data_iter(ArrayList<PPrintDatum> list_as_strs, int depth, StringBuilder buffer, char[] tabs, int col_length, int i, int n, Trampoline.SafeContinuation continuation) {
    if(i >= n) return continuation.run(Void.VALUE);
    if(i > 0 && list_as_strs.get(i).output_len + col_length > PRETTY_PRINT_COLUMN_WIDTH) {
      buffer.append('\n');
      add_chars_to_buffer(buffer,tabs);
      col_length = 2*depth;
    }
    int new_col_length = col_length + list_as_strs.get(i).output_len + 1; // +1 accounts for spaces
    if(list_as_strs.get(i).is_exp) {
      return () -> pretty_print_pprint_data(list_as_strs.get(i).exp,list_as_strs.get(i).output_len,depth+1,buffer,(ignore) -> () -> {
        if(i+1 != n) buffer.append(' ');
        return () -> pretty_print_list_of_data_iter(list_as_strs,depth,buffer,tabs,new_col_length,i+1,n,continuation);
      });
    } else {
      buffer.append(list_as_strs.get(i).datum_str);
      if(i+1 != n) buffer.append(' ');
      return () -> pretty_print_list_of_data_iter(list_as_strs,depth,buffer,tabs,new_col_length,i+1,n,continuation);
    }
  }

  private static Trampoline.SafeBounce pretty_print_list_of_data(ArrayList<PPrintDatum> list_as_strs, int depth, StringBuilder buffer, char[] tabs, Trampoline.SafeContinuation continuation) {
    tabs[2*depth+1] = 0; // shorten tabs to account for specialized stringification
    return pretty_print_list_of_data_iter(list_as_strs,depth,buffer,tabs,2*depth,0,list_as_strs.size(),continuation);
  }


  // Show info on the parsed stringified data
  private static Trampoline.SafeBounce pretty_print_pprint_data_list_body_iter(ArrayList<PPrintDatum> list_as_strs, int depth, StringBuilder buffer, char[] tabs, int i, int n, Trampoline.SafeContinuation continuation) {
    if(i >= n) return continuation.run(Void.VALUE);
    add_chars_to_buffer(buffer,tabs);
    if(list_as_strs.get(i).is_exp) {
      return () -> pretty_print_pprint_data(list_as_strs.get(i).exp,list_as_strs.get(i).output_len,depth+1,buffer,(ignore) -> () -> {
        if(i+1 != n) buffer.append('\n');
        return () -> pretty_print_pprint_data_list_body_iter(list_as_strs,depth,buffer,tabs,i+1,n,continuation);
      });
    } else {
      buffer.append(list_as_strs.get(i).datum_str);
      if(i+1 != n) buffer.append('\n');
      return () -> pretty_print_pprint_data_list_body_iter(list_as_strs,depth,buffer,tabs,i+1,n,continuation);
    }
  }

  private static Trampoline.SafeBounce pretty_print_pprint_data_list_body(ArrayList<PPrintDatum> list_as_strs, int depth, StringBuilder buffer, char[] tabs, int i, int n, Trampoline.SafeContinuation continuation) {
    if(i < n) buffer.append('\n');
    return pretty_print_pprint_data_list_body_iter(list_as_strs,depth,buffer,tabs,i,n,(ignore) -> () -> {
      buffer.append(')'); // Free <tabs> & add the closing paren
      return continuation.run(Void.VALUE);
    });
  }

  private static Trampoline.SafeBounce pretty_print_pprint_data(ArrayList<PPrintDatum> list_as_strs, int len, int depth, StringBuilder buffer, Trampoline.SafeContinuation continuation) {
    // Print as is if possible
    if(len + 2*depth <= PRETTY_PRINT_COLUMN_WIDTH || len < 2) {
      return () -> print_pprint_data_as_is(list_as_strs,buffer,continuation);
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
      return () -> pretty_print_list_of_data(list_as_strs,depth,buffer,tabs,(ignore) -> () -> {
        buffer.append(')');
        return continuation.run(Void.VALUE);
      });
    }
    // If 1st elt is a list, hence special stringification case for such applications
    if(list_as_strs.get(0).is_exp) {
      int body_start = i;
      return () -> pretty_print_pprint_data(list_as_strs.get(0).exp,list_as_strs.get(0).output_len,depth+1,buffer,(ignore) -> () -> {
        return () -> pretty_print_pprint_data_list_body(list_as_strs,depth,buffer,tabs,body_start,n,continuation);
      });
    } else {
      buffer.append(list_as_strs.get(0).datum_str);
      buffer.append(' ');
      // If 2nd elt printable on the current line (another special case)
      if(list_as_strs.get(1).output_len + list_as_strs.get(0).output_len + 2*depth < PRETTY_PRINT_COLUMN_WIDTH){
        i = 2;
        if(list_as_strs.get(1).is_exp) {
          int body_start = i;
          return () -> print_pprint_data_as_is(list_as_strs.get(1).exp,buffer,(ignore) -> () -> {
            return () -> pretty_print_pprint_data_list_body(list_as_strs,depth,buffer,tabs,body_start,n,continuation);
          });
        } else {
          buffer.append(list_as_strs.get(1).datum_str);
        }
      }
    }
    // Print body of the list
    int body_start = i;
    return () -> pretty_print_pprint_data_list_body(list_as_strs,depth,buffer,tabs,body_start,n,continuation);
  }


  // Box class to catch the final value in <pprint>'s continuation closure
  private static class StringBox {
    public java.lang.String value = "";
  }


  // Pretty printer launch
  public java.lang.String pprint() {
    // If non-proper-list-pair, print as-is
    if(!isList(this)) return write();
    // Else check if pair as string is of valid length
    java.lang.String as_string = write();
    if(as_string.length() <= PRETTY_PRINT_COLUMN_WIDTH) return as_string;
    // Otherwise get list as string-ified objects
    // => Note that we use trampolining to get around recursion depth issues with printing deep lists
    ArrayList<PPrintDatum> list_as_strs = new ArrayList<PPrintDatum>();
    StringBox sb = new StringBox();
    Trampoline.safeResolve(stringify_list_data(list_as_strs,0,this,(outputLengthChar) -> () -> {
      int output_length = ((Character)outputLengthChar).value();
      // Print the list w/ indents
      StringBuilder buffer = new StringBuilder();
      return pretty_print_pprint_data(list_as_strs,output_length,0,buffer,(ignore) -> () -> {
        sb.value = buffer.toString();
        return Trampoline.SAFE_LAST_BOUNCE_SIGNAL;
      });
    }));
    return sb.value;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Quoting semantics for the VM's interpreter
  // => WARNING: THIS WILL INFINITELY RECURSE ON CYCLIC VECTORS/HASHMAPS!
  public Pair quote(ExecutionState state) {
    return new Pair(car.quote(state),cdr.quote(state));
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-memory semantics for the VM's interpreter
  public Pair loadWithState(ExecutionState state) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Loading-into-environment semantics for the VM's interpreter
  public Pair loadWithName(java.lang.String name) {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Copying
  public Pair shallowCopy() {
    return this;
  }


  //////////////////////////////////////////////////////////////////////
  // Type Checking
  public boolean containsTypes(Environment env, Predicate keyTypePredicate, Predicate valTypePredicate) throws Exception {
    return keyTypePredicate.check(env,car) && valTypePredicate.check(env,cdr);
  }

  public boolean containsKeyType(Environment env, Predicate keyTypePredicate) throws Exception {
    return keyTypePredicate.check(env,car);
  }

  public boolean containsValueType(Environment env, Predicate valTypePredicate) throws Exception {
    return valTypePredicate.check(env,cdr);
  }

  public boolean containsType(Environment env, Predicate typePredicate) throws Exception {
    if(length == DOTTED_LIST_LENGTH || typePredicate.check(env,car) == false) {
      return false;
    }
    Datum ptr = cdr;
    while(ptr instanceof Pair) {
      Pair p = (Pair)ptr;
      if(typePredicate.check(env,p.car) == false) return false;
      ptr = p.cdr;
    }
    return true;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    return car;
  }

  public AssociativeCollection tail() throws Exception {
    if(cdr instanceof AssociativeCollection) return (AssociativeCollection)cdr;
    throw new Exceptionf("PAIR [TAIL]: can't get <tail> of DOTTED-LIST %s", write());
  }

  //////////////////////////////////////
  // fold (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldIter(Callable c, Datum seed, Datum lis, Trampoline.Continuation continuation) throws Exception { // -> Datum
    if(!(lis instanceof Pair)) return continuation.run(seed);
    Pair plis = (Pair)lis;
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(seed);
    args.add(plis.car);
    return c.callWith(args,(acc) -> () -> foldIter(c,acc,plis.cdr,continuation));
  }

  public Trampoline.Bounce fold(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception { // -> Datum
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOLD]: can't fold over dotted-list %s", profile());
    }
    return foldIter(c,seed,this,continuation);
  }

  //////////////////////////////////////
  // fold (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldIter(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    AssociativeCollection[] tails = new AssociativeCollection[acs.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length+1);
    args.add(seed);
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() == 0) return continuation.run(seed);
      args.add(acs[i].head());
      tails[i] = (AssociativeCollection)acs[i].tail();
    }
    return c.callWith(args,(acc) -> () -> FoldIter(c,acc,tails,continuation));
  }

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOLD]: can't fold over dotted-list %s", profile());
    }
    return FoldIter(c,seed,acs,continuation);
  }

  ///////////////////////////////////////
  // map (unary)
  ///////////////////////////////////////

  private Trampoline.Bounce mapIter(Callable c, Datum lis, Trampoline.Continuation continuation) throws Exception {
    if(!(lis instanceof Pair)) return continuation.run(Nil.VALUE);
    Pair plis = (Pair)lis;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(plis.car);
    return c.callWith(args,(mappedVal) -> () -> mapIter(c,plis.cdr,(mappedRest) -> () -> continuation.run(new Pair(mappedVal,mappedRest))));
  }

  public Trampoline.Bounce map(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [MAP]: can't map over dotted-list %s", profile());
    }
    return mapIter(c,this,continuation);
  }

  ///////////////////////////////////////
  // map (binary+)
  ///////////////////////////////////////

  private Trampoline.Bounce MapIter(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    AssociativeCollection[] tails = new AssociativeCollection[acs.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() == 0) return continuation.run(Nil.VALUE);
      args.add(acs[i].head());
      tails[i] = (AssociativeCollection)acs[i].tail();
    }
    return c.callWith(args,(mappedVal) -> () -> MapIter(c,tails,(mappedRest) -> () -> continuation.run(new Pair(mappedVal,mappedRest))));
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [MAP]: can't map over dotted-list %s", profile());
    }
    return MapIter(c,acs,continuation);
  }

  //////////////////////////////////////
  // for-each (unary)
  //////////////////////////////////////

  private Trampoline.Bounce forEachIter(Callable c, Datum lis, Trampoline.Continuation continuation) throws Exception {
    if(!(lis instanceof Pair)) return continuation.run(Void.VALUE);
    Pair plis = (Pair)lis;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(plis.car);
    return c.callWith(args,(ignore) -> () -> forEachIter(c,plis.cdr,continuation));
  }

  public Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOR-EACH]: can't iterate over dotted-list %s", profile());
    }
    return forEachIter(c,this,continuation);
  }

  //////////////////////////////////////
  // for-each (binary+)
  //////////////////////////////////////

  private Trampoline.Bounce ForEachIter(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    AssociativeCollection[] tails = new AssociativeCollection[acs.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() == 0) return continuation.run(Void.VALUE);
      args.add(acs[i].head());
      tails[i] = (AssociativeCollection)acs[i].tail();
    }
    return c.callWith(args,(ignore) -> () -> ForEachIter(c,tails,continuation));
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOR-EACH]: can't iterate over dotted-list %s", profile());
    }
    return ForEachIter(c,acs,continuation);
  }

  //////////////////////////////////////
  // filter
  //////////////////////////////////////

  private static Trampoline.Bounce filterIter(Callable predicate, Datum l, Trampoline.Continuation continuation) throws Exception {
    if(!(l instanceof Pair)) return continuation.run(Nil.VALUE);
    Pair p = (Pair)l;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(p.car);
    return predicate.callWith(args,(shouldKeep) -> () -> {
      if(shouldKeep.isTruthy()) {
        return filterIter(predicate,p.cdr,(filteredRest) -> () -> continuation.run(new Pair(p.car,filteredRest)));
      }
      return filterIter(predicate,p.cdr,continuation);
    });
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FILTER]: can't filter a dotted-list %s", profile());
    }
    return filterIter(predicate,this,continuation);
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private static Trampoline.Bounce countIter(Callable predicate, int n, Datum l, Trampoline.Continuation continuation) throws Exception {
    if(!(l instanceof Pair)) return continuation.run(new Exact(n));
    Pair p = (Pair)l;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(p.car);
    return predicate.callWith(args,(shouldCount) -> () -> {
      if(shouldCount.isTruthy()) return countIter(predicate,n+1,p.cdr,continuation);
      return countIter(predicate,n,p.cdr,continuation);
    });
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [COUNT]: can't count in a dotted-list %s", profile());
    }
    return countIter(predicate,0,this,continuation);
  }

  //////////////////////////////////////
  // remove (inverse of filter)
  //////////////////////////////////////

  private static Trampoline.Bounce removeIter(Callable predicate, Datum l, Trampoline.Continuation continuation) throws Exception {
    if(!(l instanceof Pair)) return continuation.run(Nil.VALUE);
    Pair p = (Pair)l;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(p.car);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) {
        return removeIter(predicate,p.cdr,continuation);
      }
      return removeIter(predicate,p.cdr,(removedRest) -> () -> continuation.run(new Pair(p.car,removedRest)));
    });
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [REMOVE]: can't remove from a dotted-list %s", profile());
    }
    return removeIter(predicate,this,continuation);
  }

  //////////////////////////////////////
  // val
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    if(length == DOTTED_LIST_LENGTH)
      throw new Exceptionf("PAIR [VAL]: can't get value in a dotted-list %s", profile());
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("PAIR [VAL]: invalid index %s for list value", key.profile());
    int keyInt = ((Real)key).intValue();
    if(keyInt < 0 || keyInt >= length)
      throw new Exceptionf("PAIR [VAL]: index %s out of list range [0,%d)", key.write(), length);
    int idx = 0;
    Datum l = this;
    while(l instanceof Pair) {
      Pair p = (Pair)l;
      if(idx == keyInt) return p.car;
      ++idx;
      l = p.cdr;
    }
    throw new Exceptionf("PAIR [VAL]: invalid index %s for list value", key.profile());
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private static Trampoline.Bounce keyIter(Callable predicate, Datum orginalL, int idx, Datum l, Trampoline.Continuation continuation) throws Exception {
    if(!(l instanceof Pair)) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("PAIR [KEY]: no value in %s satisfies value predicate %s", orginalL.write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("PAIR [KEY]: no value in %s satisfies value predicate %s", orginalL.write(), predicate);
    }
    Pair p = (Pair)l;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(p.car);
    return predicate.callWith(args,(matchedValue) -> () -> {
      if(matchedValue.isTruthy()) return continuation.run(new Exact(idx));
      return keyIter(predicate,orginalL,idx+1,p.cdr,continuation);
    });
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum 
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [KEY]: can't get key in a dotted-list %s", profile());
    }
    return keyIter(predicate,this,0,this,continuation);
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

  public static Datum binaryAppend(Datum p1, Datum p2) {
    if(!(p1 instanceof Pair)) return p2;
    Pair p = (Pair)p1;
    return new Pair(p.car,binaryAppend(p.cdr,p2));
  }

  public AssociativeCollection append(AssociativeCollection ac) throws Exception {
    return (AssociativeCollection)binaryAppend(this,(Datum)ac);
  }

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    Datum appended = (Datum)acs[0];
    for(int i = 1; i < acs.length; ++i) {
      appended = binaryAppend(appended,(Datum)acs[i]);
    }
    return (AssociativeCollection)appended;
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  private static AssociativeCollection deleteIter(int key, int idx, Datum l) throws Exception {
    if(!(l instanceof Pair)) 
      throw new Exceptionf("PAIR [DELETE]: invalid index %d for list deletion", key);
    Pair p = (Pair)l;
    if(key == idx) return (AssociativeCollection)p.cdr;
    return new Pair(p.car,(Datum)deleteIter(key,idx+1,p.cdr));
  }

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    if(length == DOTTED_LIST_LENGTH)
      throw new Exceptionf("PAIR [DELETE]: can't delete in a dotted-list %s", profile());
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("PAIR [DELETE]: invalid index %s for list deletion", key.profile());
    int keyInt = ((Real)key).intValue();
    if(keyInt < 0 || keyInt >= length)
      throw new Exceptionf("PAIR [DELETE]: index %s out of list range [0,%d)", key.write(), length);
    return deleteIter(keyInt,0,this);
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  private static Datum conjAtIndex(Datum lis, Datum value, int idx) {
    if(!(lis instanceof Pair)) return new Pair(value,Nil.VALUE);
    Pair p = (Pair)lis;
    if(idx <= 0) return new Pair(value,p.cdr);
    return new Pair(p.car,conjAtIndex(p.cdr,value,idx-1));
  }

  public AssociativeCollection conj(Datum key, Datum value) throws Exception {
    if(length == DOTTED_LIST_LENGTH)
      throw new Exceptionf("PAIR [CONJ]: can't conj onto a dotted-list %s", profile());
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("PAIR [CONJ]: invalid index %s for list", key.profile());
    int idx = ((Real)key).intValue();
    if(idx < -1 || idx > length)
      throw new Exceptionf("PAIR [CONJ]: index %d violates string bounds [-1,%d]", idx, length);
    if(idx == -1) return new Pair(value,this);
    return (AssociativeCollection)conjAtIndex(this,value,idx);
  }

  //////////////////////////////////////
  // drop
  //////////////////////////////////////

  public AssociativeCollection drop(int amount) throws Exception {
    amount = Math.max(0,amount);
    if(amount >= length) return Nil.VALUE;
    Datum p = this;
    while(amount > 0 && p instanceof Pair) {
      --amount;
      p = ((Pair)p).cdr;
    }
    return (AssociativeCollection)p;
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  private static Datum takeRecur(int amount, Datum l) throws Exception {
    if(amount <= 0 || !(l instanceof Pair)) return Nil.VALUE;
    Pair p = (Pair)l;
    return new Pair(p.car,takeRecur(amount-1,p.cdr));
  }

  public AssociativeCollection take(int amount) throws Exception {
    amount = Math.max(0,amount);
    return (AssociativeCollection)takeRecur(amount,(Datum)this);
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  public Datum toACList() throws Exception {
    return this;
  }

  public String toACString() throws Exception {
    StringBuilder sb = new StringBuilder();
    Datum l = this;
    while(l instanceof Pair) {
      Pair p = (Pair)l;
      if(!(p.car instanceof Character))
        throw new Exceptionf("PAIR [AC->STRING]: can't convert list with non-char %s into a string", p.car.profile());
      sb.append(p.car.display());
      l = p.cdr;
    }
    return new String(sb.toString());
  }

  public Vector toACVector() throws Exception {
    ArrayList<Datum> vals = new ArrayList<Datum>();
    Datum l = this;
    while(l instanceof Pair) {
      Pair p = (Pair)l;
      vals.add(p.car);
      l = p.cdr;
    }
    return new Vector(0,vals);
  }

  public Hashmap toACHashmap() throws Exception {
    Hashmap h = new Hashmap();
    Datum l = this;
    int i = 0;
    while(l instanceof Pair) {
      Pair p = (Pair)l;
      h.set(new Exact(i),p.car);
      ++i;
      l = p.cdr;
    }
    return h;
  }

  //////////////////////////////////////
  // union set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValues(Callable eltPredicate, Datum elt, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(!(values instanceof Pair)) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(elt);
    args.add(((Pair)values).car());
    return eltPredicate.callWith(args,(match) -> () -> {
      if(match.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValues(eltPredicate,elt,((Pair)values).cdr(),continuation);
    });
  }

  private Trampoline.Bounce UnionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int i, Datum l, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(values);
    if(!(l instanceof Pair)) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? (Datum)acs[i] : (Datum)acs[i+1],values,continuation);
    return inValues(eltPredicate, ((Pair)l).car(), values, (isInValues) -> () -> {
      if(isInValues.isTruthy()) {
        return UnionArrayIter(eltPredicate,acs,i,((Pair)l).cdr(),values,continuation);
      }
      return UnionArrayIter(eltPredicate,acs,i,((Pair)l).cdr(),new Pair(((Pair)l).car(),values),continuation);
    });
  }

  public Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return UnionArrayIter(eltPredicate,acs,0,(Datum)acs[0],Nil.VALUE,continuation);
  }

  //////////////////////////////////////
  // intersection set operation
  //////////////////////////////////////

  private Trampoline.Bounce itemIntersectsAC(Callable eltPredicate, Datum ac, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(!(ac instanceof Pair)) return continuation.run(Boolean.FALSE);
    Pair p = (Pair)ac;
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(item);
    args.add(p.car);
    return eltPredicate.callWith(args,(isSameItem) -> () -> {
      if(isSameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return itemIntersectsAC(eltPredicate,p.cdr,item,continuation);
    });
  }

  private Trampoline.Bounce itemIntersects(Callable eltPredicate, AssociativeCollection[] acs, int i, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(Boolean.TRUE);
    return itemIntersectsAC(eltPredicate,(Datum)acs[i],item,(itemInAC) -> () -> {
      if(itemInAC.isTruthy()) return () -> itemIntersects(eltPredicate,acs,i+1,item,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  private Trampoline.Bounce IntersectionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int acIdx, Datum l, Datum intersectingValues, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= acs.length) return continuation.run(intersectingValues);
    if(!(l instanceof Pair)) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? (Datum)acs[acIdx] : (Datum)acs[acIdx+1],intersectingValues,continuation);
    Pair p = (Pair)l;
    return inValues(eltPredicate, p.car, intersectingValues, (isInValues) -> () -> {
      if(!isInValues.isTruthy()) {
        return itemIntersects(eltPredicate,acs,0,p.car,(intersects) -> () -> {
          if(intersects.isTruthy()) {
            return IntersectionArrayIter(eltPredicate,acs,acIdx,p.cdr,new Pair(p.car,intersectingValues),continuation);
          }
          return IntersectionArrayIter(eltPredicate,acs,acIdx,p.cdr,intersectingValues,continuation);
        });
      }
      return IntersectionArrayIter(eltPredicate,acs,acIdx,p.cdr,intersectingValues,continuation);
    });
  }

  public Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return IntersectionArrayIter(eltPredicate,acs,0,(Datum)acs[0],Nil.VALUE,continuation);
  }

  //////////////////////////////////////
  // difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce binaryDifferenceArrayIter(Datum acc, Callable eltPredicate, Datum valList, Datum acList, Trampoline.Continuation continuation) throws Exception {
    if(!(valList instanceof Pair)) return continuation.run(acc);
    Pair p = (Pair)valList;
    return inValues(eltPredicate,p.car(),acList,(inList) -> () -> {
      if(inList.isTruthy()) {
        return binaryDifferenceArrayIter(acc,eltPredicate,p.cdr(),acList,continuation);
      }
      return binaryDifferenceArrayIter(new Pair(p.car(),acc),eltPredicate,p.cdr(),acList,continuation);
    });
  }

  private Trampoline.Bounce DifferenceArrayIter(Callable eltPredicate, Datum lhs, AssociativeCollection[] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(lhs);
    return binaryDifferenceArrayIter(Nil.VALUE,eltPredicate,lhs,(Datum)acs[i],(differenceArray) -> () -> DifferenceArrayIter(eltPredicate,differenceArray,acs,i+1,continuation));
  }

  public Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArrayIter(eltPredicate,(Datum)acs[0],acs,1,continuation);
  }

  //////////////////////////////////////
  // symmetric difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce binarySymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection a, AssociativeCollection b, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArray(eltPredicate,new AssociativeCollection[]{a,b},(ab) -> () -> {
      return DifferenceArray(eltPredicate,new AssociativeCollection[]{b,a},(ba) -> () -> {
        return UnionArray(eltPredicate,new AssociativeCollection[]{(AssociativeCollection)ab,(AssociativeCollection)ba},continuation);
      });
    });
  }

  private Trampoline.Bounce SymmetricDifferenceArrayIter(Callable eltPredicate, AssociativeCollection lhs, AssociativeCollection[] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run((Datum)lhs);
    return binarySymmetricDifferenceArray(eltPredicate,lhs,acs[i],(symDiff) -> () -> {
      return SymmetricDifferenceArrayIter(eltPredicate,(AssociativeCollection)symDiff,acs,i+1,continuation);
    });
  }

  public Trampoline.Bounce SymmetricDifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return SymmetricDifferenceArrayIter(eltPredicate,acs[0],acs,1,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // <OrderedCollection> Instance Methods

  //////////////////////////////////////
  // constructor
  //////////////////////////////////////

  public OrderedCollection conj(Datum value) throws Exception {
    return new Pair(value,this);
  }

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////

  public OrderedCollection init() throws Exception {
    if(!(cdr instanceof Pair)) return Nil.VALUE;
    Pair rest = (Pair)cdr;
    if(!(rest.cdr instanceof Pair)) return new Pair(car,Nil.VALUE);
    return new Pair(car,(Datum)rest.init());
  }

  public Datum last() throws Exception {
    if(!(cdr instanceof Pair)) return car;
    return ((Pair)cdr).last();
  }

  //////////////////////////////////////
  // slicing
  //////////////////////////////////////

  public OrderedCollection slice(int startIdx) throws Exception {
    return slice(startIdx,length);
  }

  public OrderedCollection slice(int startIdx, int length) throws Exception {
    if(startIdx <= 0) {
      if(length <= 0) return Nil.VALUE;
      if(!(cdr instanceof Pair)) return new Pair(car,Nil.VALUE);
      return new Pair(car,(Datum)((Pair)cdr).slice(0,length-1));
    }
    if(!(cdr instanceof Pair)) return Nil.VALUE;
    return ((Pair)cdr).slice(startIdx-1,length);
  }

  public Trampoline.Bounce slice(int startIdx, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    if(startIdx <= 0) {
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(car);
      return continuePredicate.callWith(args,(shouldContinue) -> () -> {
        if(!shouldContinue.isTruthy()) return continuation.run(Nil.VALUE);
        if(!(cdr instanceof Pair)) return continuation.run(new Pair(car,Nil.VALUE));
        return ((Pair)cdr).slice(0,continuePredicate,(sliced) -> () -> continuation.run(new Pair(car,sliced)));
      });
    }
    if(!(cdr instanceof Pair)) return continuation.run(Nil.VALUE);
    return () -> ((Pair)cdr).slice(startIdx-1,continuePredicate,continuation);
  }

  //////////////////////////////////////
  // reversing
  //////////////////////////////////////

  private OrderedCollection reverse(OrderedCollection acc) {
    if(!(cdr instanceof Pair)) return new Pair(car,(Datum)acc);
    return ((Pair)cdr).reverse(new Pair(car,(Datum)acc));
  }

  public OrderedCollection reverse() {
    return reverse(Nil.VALUE);
  }

  //////////////////////////////////////
  // removing items
  //////////////////////////////////////

  private static Datum generateDatumFromCallable(Callable c) {
    if(c instanceof Datum) return (Datum)c;
    return new PrimitiveProcedure(Procedure.DEFAULT_NAME,c);
  }

  private static Datum alwaysFalseCallable() {
    return new PrimitiveProcedure(Procedure.DEFAULT_NAME,new Callable() {
      public java.lang.String docstring() {
        return "Always returns #f irrespective of arguments.";
      }
      public Datum signature() { 
        return Pair.List(new Symbol(Procedure.DEFAULT_NAME)); 
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception { 
        return cont.run(Boolean.FALSE); 
      }
    });
  }

  private static Trampoline.Bounce removeLastIter(Datum predicate, Datum lis, Trampoline.Continuation continuation) throws Exception {
    if(!(lis instanceof Pair)) return continuation.run(new Pair(predicate,Nil.VALUE));
    Pair plis = (Pair)lis;
    return () -> removeLastIter(predicate,plis.cdr,(result) -> () -> {
      Pair p = (Pair)result;
      Datum newPredicate = p.car;
      Datum recurResult = p.cdr;
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(plis.car);
      return ((Callable)newPredicate).callWith(args,(shouldRemove) -> () -> {
        if(shouldRemove.isTruthy()) {
          return continuation.run(new Pair(alwaysFalseCallable(),recurResult));
        } else {
          return continuation.run(new Pair(newPredicate,new Pair(plis.car,recurResult)));
        }
      });
    });
  }

  public Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(car);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) return continuation.run(cdr);
      if(!(cdr instanceof Pair)) return continuation.run(this);
      return () -> ((Pair)cdr).removeFirst(predicate,(removed) -> () -> continuation.run(new Pair(car,removed)));
    });
  }

  public Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return removeLastIter(generateDatumFromCallable(predicate),this,(result) -> () -> continuation.run(((Pair)result).cdr));
  }

  //////////////////////////////////////
  // skipping
  //////////////////////////////////////

  public Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(car);
    return predicate.callWith(args,(keepSkipping) -> () -> {
      if(keepSkipping.isTruthy()) {
        if(!(cdr instanceof Pair)) return continuation.run(Boolean.FALSE);
        return ((Pair)cdr).skip(predicate,continuation);
      }
      return continuation.run(car);
    });
  }

  public Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(car);
    return predicate.callWith(args,(keepSkipping) -> () -> {
      if(cdr instanceof Pair) {
        return ((Pair)cdr).skipRight(predicate,(elt) -> () -> {
          if(!(elt instanceof Boolean) || keepSkipping.isTruthy()) return continuation.run(elt);
          return continuation.run(car);
        });
      }
      if(keepSkipping.isTruthy()) return continuation.run(Boolean.FALSE);
      return continuation.run(car);
    });
  }

  //////////////////////////////////////
  // fold-right (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldRightIter(Callable c, Datum seed, Datum lis, Trampoline.Continuation continuation) throws Exception { // -> Datum
    if(!(lis instanceof Pair)) return continuation.run(seed);
    Pair plis = (Pair)lis;
    ArrayList<Datum> params = new ArrayList<Datum>(1);
    params.add(plis.car);
    return () -> foldRightIter(c,seed,plis.cdr,(acc) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }

  public Trampoline.Bounce foldRight(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception {
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOLD-RIGHT]: can't fold over dotted-list %s", profile());
    }
    return foldRightIter(c,seed,this,continuation);
  }

  //////////////////////////////////////
  // fold-right (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldRightIter(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    ArrayList<Datum> params = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acs[i].length() == 0) return continuation.run(seed);
      params.add(acs[i].head());
      acs[i] = (AssociativeCollection)acs[i].tail(); // can mutate <acs> since never captured in a continuation (unlike <fold>)!
    }
    return () -> FoldRightIter(c,seed,acs,(acc) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }

  public Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    if(length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [FOLD-RIGHT]: can't fold over dotted-list %s", profile());
    }
    return FoldRightIter(c,seed,acs,continuation);
  }

  //////////////////////////////////////
  // key-right
  //////////////////////////////////////

  private Trampoline.Bounce keyRight(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(car);
    return predicate.callWith(args,(shouldGetKey) -> () -> {
      if(cdr instanceof Pair) {
        return ((Pair)cdr).keyRight(predicate,idx+1,(key) -> () -> {
          if(key instanceof Real || !shouldGetKey.isTruthy()) return continuation.run(key);
          return continuation.run(new Exact(idx));
        });
      }
      if(!shouldGetKey.isTruthy()) return continuation.run(Boolean.FALSE);
      return continuation.run(new Exact(idx));
    });
  }

  public Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return keyRight(predicate,0,(key) -> () -> {
      if(key instanceof Real) return continuation.run(key);
      if(predicate instanceof Datum) {
        throw new Exceptionf("PAIR [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("PAIR [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), predicate);
    });
  }

  //////////////////////////////////////
  // dropping
  //////////////////////////////////////

  private static final Boolean KEEP_DROPPING_FLAG = Boolean.valueOf(false);

  public OrderedCollection dropRight(int length) throws Exception {
    if(this.length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [DROP-RIGHT]: can't drop %d from dotted-list %s", length, profile());
    }
    if(this.length <= length) return Nil.VALUE;
    if(!(cdr instanceof Pair)) return new Pair(car,Nil.VALUE);
    return new Pair(car,(Datum)((Pair)cdr).dropRight(length));
  }

  public Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(car);
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(!keepDropping.isTruthy()) return continuation.run(this);
      if(!(cdr instanceof Pair)) return continuation.run(Nil.VALUE);
      return ((Pair)cdr).dropWhile(predicate,continuation);
    });
  }

  private Trampoline.Bounce dropRightWhileIter(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(car);
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(cdr instanceof Pair) {
        return ((Pair)cdr).dropRightWhileIter(predicate,(dropped) -> () -> {
          if(!dropped.eq(KEEP_DROPPING_FLAG)) return continuation.run(new Pair(car,dropped));
          if(keepDropping.isTruthy()) return continuation.run(KEEP_DROPPING_FLAG);
          return continuation.run(new Pair(car,Nil.VALUE));
        });
      }
      if(keepDropping.isTruthy()) return continuation.run(KEEP_DROPPING_FLAG);
      return continuation.run(new Pair(car,Nil.VALUE));
    });
  }

  public Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropRightWhileIter(predicate,(result) -> () -> {
      if(result.eq(KEEP_DROPPING_FLAG)) return continuation.run(Nil.VALUE);
       return continuation.run(result);
    });
  }

  //////////////////////////////////////
  // taking
  //////////////////////////////////////

  private Trampoline.Bounce takeRightWhileIter(Callable predicate, Datum lis, Trampoline.Continuation escape, Trampoline.Continuation continuation) throws Exception {
    if(!(lis instanceof Pair)) return continuation.run(Nil.VALUE);
    Pair plis = (Pair)lis;
    return () -> takeRightWhileIter(predicate,plis.cdr,escape,(tail) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>();
      args.add(plis.car);
      return predicate.callWith(args,(keepTaking) -> () -> {
        if(!keepTaking.isTruthy()) return escape.run(tail);
        return continuation.run(new Pair(plis.car,tail));
      });
    });
  }

  public OrderedCollection takeRight(int length) throws Exception {
    if(this.length == DOTTED_LIST_LENGTH) {
      throw new Exceptionf("PAIR [TAKE-RIGHT]: can't take %d from dotted-list %s", length, profile());
    }
    if(this.length <= length) return this;
    if(!(cdr instanceof Pair)) return Nil.VALUE;
    return ((Pair)cdr).takeRight(length);
  }

  public Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(car);
    return predicate.callWith(args,(keepTaking) -> () -> {
      if(!keepTaking.isTruthy()) return continuation.run(Nil.VALUE);
      if(!(cdr instanceof Pair)) return continuation.run(new Pair(car,Nil.VALUE));
      return ((Pair)cdr).takeWhile(predicate,(taken) -> () -> continuation.run(new Pair(car,taken)));
    });
  }

  public Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return takeRightWhileIter(predicate,this,continuation,continuation);
  }

  //////////////////////////////////////
  // sorting
  //////////////////////////////////////

  public Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    if(this.length == DOTTED_LIST_LENGTH) {
      if(binaryPredicate instanceof Datum) {
        throw new Exceptionf("PAIR [SORT]: can't sort dotted-list %s with predicate %s", write(), ((Datum)binaryPredicate).profile());
      }
      throw new Exceptionf("PAIR [SORT]: can't sort dotted-list %s with predicate %s", write(), binaryPredicate);
    }
    Callable trueCondPrimitive = new Callable() {
      public java.lang.String docstring() {
        return "Quicksort: move values to the left of the pivot.";
      }
      public Datum signature() { 
        return Pair.List(new Symbol("escm-sort-in-lhs?"),new Symbol("<obj>"),new Symbol("<obj>")); 
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
        params.add(car);
        return binaryPredicate.callWith(params,cont);
      }
    };
    Callable falseCondPrimitive = new Callable() {
      public java.lang.String docstring() {
        return "Quicksort: move values to the right of the pivot.";
      }
      public Datum signature() { 
        return Pair.List(new Symbol("escm-sort-in-rhs?"),new Symbol("<obj>"),new Symbol("<obj>")); 
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
        params.add(car);
        return binaryPredicate.callWith(params,(value) -> () -> cont.run(Boolean.valueOf(!value.isTruthy())));
      }
    };
    PrimitiveProcedure trueCond = new PrimitiveProcedure("escm-sort-in-lhs?", trueCondPrimitive);
    PrimitiveProcedure falseCond = new PrimitiveProcedure("escm-sort-in-rhs?", falseCondPrimitive);
    return filterIter(trueCond,cdr,(lhs) -> () -> {
      return ((OrderedCollection)lhs).sort(binaryPredicate,(sortedLhs) -> () -> {
        return filterIter(falseCond,cdr,(rhs) -> () -> {
          return ((OrderedCollection)rhs).sort(binaryPredicate,(sortedRhs) -> () -> {
            return continuation.run(binaryAppend(sortedLhs,new Pair(car,sortedRhs)));
          });
        });
      });
    });
  }

  public Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    if(!(cdr instanceof Pair)) return continuation.run(Boolean.TRUE);
    Pair pcdr = (Pair)cdr;
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(car);
    args.add(pcdr.car);
    return binaryPredicate.callWith(args,(isEq) -> () -> {
      if(isEq.isTruthy()) return pcdr.sorted(binaryPredicate,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  //////////////////////////////////////
  // merging
  //////////////////////////////////////

  private static Trampoline.Bounce mergeIter(Callable binaryPredicate, AssociativeCollection ac1, AssociativeCollection ac2, Trampoline.Continuation continuation) throws Exception {
    if(ac1.length() == 0) return continuation.run((Datum)ac2);
    if(ac2.length() == 0) return continuation.run((Datum)ac1);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(ac1.head());
    args.add(ac2.head());
    return binaryPredicate.callWith(args,(lt) -> () -> {
      if(lt.isTruthy()) {
        return mergeIter(binaryPredicate,ac1.tail(),ac2,(merged) -> () -> continuation.run(new Pair(ac1.head(),merged)));
      }
      return mergeIter(binaryPredicate,ac1,ac2.tail(),(merged) -> () -> continuation.run(new Pair(ac2.head(),merged)));
    });
  }

  public Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception {
    return mergeIter(binaryPredicate,this,oc,continuation);
  }

  //////////////////////////////////////
  // duplicate neighbor deletion
  //////////////////////////////////////

  private static Trampoline.Bounce skipWhileHaveDuplicates(Callable binaryPredicate, Datum d, OrderedCollection tail, Trampoline.Continuation continuation) throws Exception {
    Callable matchCondPrimitive = new Callable() {
      public java.lang.String docstring() {
        return "Delete neighbor duplicates: check for value equality.";
      }
      public Datum signature() { 
        return Pair.List(new Symbol("escm-del-neigh-dups-eq?"),new Symbol("<obj>"),new Symbol("<obj>")); 
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
        params.add(d);
        return binaryPredicate.callWith(params,cont);
      }
    };
    PrimitiveProcedure matchCond = new PrimitiveProcedure("escm-del-neigh-dups-eq?",matchCondPrimitive);
    return tail.dropWhile(matchCond,continuation);
  }

  public Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    if(!(cdr instanceof Pair)) return continuation.run(this);
    return skipWhileHaveDuplicates(binaryPredicate,car,(Pair)cdr,(withoutLeadingDuplicates) -> () -> {
      return ((OrderedCollection)withoutLeadingDuplicates).deleteNeighborDuplicates(binaryPredicate,(deld) -> () -> continuation.run(new Pair(car,deld)));
    });
  }

  //////////////////////////////////////
  // array-list conversion
  //////////////////////////////////////

  public ArrayList<Datum> toArrayList() {
    ArrayList<Datum> vals = new ArrayList<Datum>();
    Datum iter = this;
    while(iter instanceof Pair) {
      Pair p = (Pair)iter;
      vals.add(p.car);
      iter = p.cdr;
    }
    return vals;
  }
}