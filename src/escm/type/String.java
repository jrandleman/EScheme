// Author: Jordan Randleman - escm.type.String
// Purpose:
//    String primitive type.
//
//    => NOTE THAT AC/OC PRIMITIVES EXPECT ALL ARGS TO BE STRINGS!

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.bool.Boolean;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.util.string.StringParser;
import escm.vm.util.ExecutionState;
import escm.vm.util.ExecutionState;
import escm.vm.type.collection.AssociativeCollection;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.callable.Callable;

public class String extends Datum implements OrderedCollection, Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private final java.lang.String value;
  private int codePointLength = -1; // computed by <codePointLength()>


  ////////////////////////////////////////////////////////////////////////////
  // Value Getters
  public java.lang.String value() {
    return value;
  }

  public int codePointLength() {
    if(codePointLength != -1) return codePointLength;
    codePointLength = value.codePointCount(0,value.length());
    return codePointLength;
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

  public escm.type.Character charAt(int index) throws Exception {
    return new escm.type.Character(codePointAt(index));
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Character & Codepoint Extraction
  // From: https://stackoverflow.com/questions/1527856/how-can-i-iterate-through-the-unicode-codepoints-of-a-java-string
  public escm.type.Character[] toChars() {
    int offset = 0, charsIdx = 0, strLength = value.length();
    escm.type.Character[] chars = new escm.type.Character[codePointLength()];
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      chars[charsIdx++] = new escm.type.Character(codepoint);
      offset += java.lang.Character.charCount(codepoint);
    }
    return chars;
  }

  public int[] toCodePoints() {
    int offset = 0, cpIdx = 0, strLength = value.length();
    int[] codepoints = new int[codePointLength()];
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      codepoints[cpIdx++] = codepoint;
      offset += java.lang.Character.charCount(codepoint);
    }
    return codepoints;
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Character Iteration
  private static interface CharacterIterationProcedure {
    public boolean exec(int idx, escm.type.Character ch); // return if continuing
  }

  public void forEachChar(CharacterIterationProcedure cip) {
    int offset = 0, charsIdx = 0, strLength = value.length();
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      if(!cip.exec(charsIdx++,new escm.type.Character(codepoint))) return;
      offset += java.lang.Character.charCount(codepoint);
    }
  }

  public void forEachCharReverse(CharacterIterationProcedure cip) {
    int lastIdx = value.length()-1;
    for(int offset = lastIdx, charsIdx = lastIdx; offset >= 0; --offset, --charsIdx) {
      char c = value.charAt(offset);
      if(java.lang.Character.isLowSurrogate(c)) {
        --offset;
        if(!cip.exec(charsIdx++,new escm.type.Character(value.codePointAt(offset)))) return;
      } else {
        if(!cip.exec(charsIdx++,new escm.type.Character(c))) return;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Codepoint Iteration
  private static interface CodepointIterationProcedure {
    public boolean exec(int idx, int cp); // return if continuing
  }

  public void forEachCodepoint(CodepointIterationProcedure cip) {
    int offset = 0, charsIdx = 0, strLength = value.length();
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      if(!cip.exec(charsIdx++,codepoint)) return;
      offset += java.lang.Character.charCount(codepoint);
    }
  }

  public void forEachCodepointReverse(CodepointIterationProcedure cip) {
    int lastIdx = value.length()-1;
    for(int offset = lastIdx, charsIdx = lastIdx; offset >= 0; --offset, --charsIdx) {
      char c = value.charAt(offset);
      if(java.lang.Character.isLowSurrogate(c)) {
        --offset;
        if(!cip.exec(charsIdx++,value.codePointAt(offset))) return;
      } else {
        if(!cip.exec(charsIdx++,(int)c)) return;
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public String(java.lang.String s) {
    value = s;
    codePointLength = s.length();
    if(codePointLength > 1) {
      codePointLength = -1; // recompute 1st time <codePointLength()> is called
    }
  }

  public String() {
    value = "";
    codePointLength = 0;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Callable (unary index getter)
  public java.lang.String docstring() {
    int n = codePointLength();
    if(n == 0) {
      return "String of length 0.";
    } else if(n == 1) {
      return "String of length 1.\nApply to index 0 to get the char.";
    } else {
      return "String of length "+n+".\nApply to any index from 0 to "+(n-1)+" to get a char.";
    }
  }

  public Datum signature() {
    return Pair.List(this,new Symbol("<index>"));
  }

  public Trampoline.Bounce callWith(ArrayList<Datum> arguments, Trampoline.Continuation continuation) throws Exception {
    if(arguments.size() != 1)
      throw new Exceptionf("STRING [CALLABLE-GET]: Expects exactly 1 integer index arg for string %s: %s", write(), Exceptionf.profileArgs(arguments));
    Datum idxDatum = arguments.get(0);
    if(!(idxDatum instanceof Real) || !((Real)idxDatum).isInteger())
      throw new Exceptionf("STRING [CALLABLE-GET]: Expects exactly 1 integer index arg for string %s: %s", write(), Exceptionf.profileArgs(arguments));
    int index = ((Real)idxDatum).intValue();
    if(index < 0 || index >= codePointLength())
      throw new Exceptionf("STRING [CALLABLE-GET]: Invalid index %d (size %d) for string %s", index, codePointLength(), write());
    return continuation.run(charAt(index));
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
  public String shallowCopy() {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods
  private static String EMPTY_STRING = new String();

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    if(codePointLength() == 0) throw new Exceptionf("STRING [HEAD]: Can't get head of an empty string!");
    return new escm.type.Character(value.codePointAt(0));
  }

  public AssociativeCollection tail() throws Exception {
    if(codePointLength() == 0) throw new Exceptionf("STRING [TAIL]: Can't get tail of an empty string!");
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { 
      if(idx > 0) sb.append(java.lang.Character.toString(cp)); 
      return true;
    });
    return new String(sb.toString());
  }

  public int length() {
    return codePointLength();
  }

  //////////////////////////////////////
  // fold (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldIter(Callable c, Datum seed, String s, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(seed);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(seed);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return c.callWith(args,(acc) -> () -> foldIter(c,acc,s,offset+increment,n,continuation));
  }

  public Trampoline.Bounce fold(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return foldIter(c,seed,this,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // fold (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldArrayIter(Callable c, Datum seed, AssociativeCollection[] acs, int[] offsets, Trampoline.Continuation continuation) throws Exception {
    int[] newOffsets = new int[offsets.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length+1);
    args.add(seed);
    for(int i = 0; i < acs.length; ++i) {
      String s = (String)acs[i];
      if(offsets[i] >= s.value.length()) return continuation.run(seed);
      int codepoint = s.value.codePointAt(offsets[i]);
      args.add(new escm.type.Character(codepoint));
      newOffsets[i] = offsets[i]+java.lang.Character.charCount(codepoint);
    }
    return c.callWith(args,(acc) -> () -> FoldArrayIter(c,acc,acs,newOffsets,continuation));
  }

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return FoldArrayIter(c,seed,acs,new int[acs.length],continuation);
  }

  ///////////////////////////////////////
  // map (unary)
  ///////////////////////////////////////

  private static Trampoline.Bounce mapIter(Callable c, String s, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(EMPTY_STRING);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return c.callWith(args,
      (mappedValue) -> () -> mapIter(c,s,offset+increment,n,
        (mappedRest) -> () -> continuation.run(new String(mappedValue.display()+mappedRest.display()))));
  }

  public Trampoline.Bounce map(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return mapIter(c,this,0,value.length(),continuation);
  }

  ///////////////////////////////////////
  // map (binary+)
  ///////////////////////////////////////

  private static Trampoline.Bounce MapIter(Callable c, AssociativeCollection[] acs, int[] offsets, Trampoline.Continuation continuation) throws Exception {
    int[] newOffsets = new int[offsets.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      String s = (String)acs[i];
      if(offsets[i] >= s.value.length()) return continuation.run(EMPTY_STRING);
      int codepoint = s.value.codePointAt(offsets[i]);
      args.add(new escm.type.Character(codepoint));
      newOffsets[i] = offsets[i]+java.lang.Character.charCount(codepoint);
    }
    return c.callWith(args,(mappedValue) -> () -> MapIter(c,acs,newOffsets,(mappedRest) -> () -> continuation.run(new String(mappedValue.display()+mappedRest.display()))));
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return MapIter(c,acs,new int[acs.length],continuation);
  }

  //////////////////////////////////////
  // for-each (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce forEachIter(Callable c, String s, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(Void.VALUE);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return c.callWith(args,(ignore) -> () -> forEachIter(c,s,offset+increment,n,continuation));
  }

  public Trampoline.Bounce forEach(Callable c, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return forEachIter(c,this,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // for-each (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce ForEachIter(Callable c, AssociativeCollection[] acs, int[] offsets, Trampoline.Continuation continuation) throws Exception {
    int[] newOffsets = new int[offsets.length];
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      String s = (String)acs[i];
      if(offsets[i] >= s.value.length()) return continuation.run(Void.VALUE);
      int codepoint = s.value.codePointAt(offsets[i]);
      args.add(new escm.type.Character(codepoint));
      newOffsets[i] = offsets[i]+java.lang.Character.charCount(codepoint);
    }
    return c.callWith(args,(ignore) -> () -> ForEachIter(c,acs,newOffsets,continuation));
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return ForEachIter(c,acs,new int[acs.length],continuation);
  }

  //////////////////////////////////////
  // filter
  //////////////////////////////////////

  private Trampoline.Bounce filterIter(Callable predicate, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(EMPTY_STRING);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    Datum hd = new escm.type.Character(codepoint);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldKeep) -> () -> {
      if(shouldKeep.isTruthy()) {
        return filterIter(predicate,offset+increment,n,(filteredRest) -> () -> continuation.run(new String(hd.display()+filteredRest.display())));
      }
      return filterIter(predicate,offset+increment,n,continuation);
    });
  }

  // Helper also used by <sort>
  private Trampoline.Bounce filterFrom(Callable predicate, int offset, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,offset,value.length(),continuation);
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private Trampoline.Bounce countIter(Callable predicate, int count, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(new Exact(count));
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(shouldCount) -> () -> {
      if(shouldCount.isTruthy()) {
        return countIter(predicate,count+1,offset+increment,n,continuation);
      }
      return countIter(predicate,count,offset+increment,n,continuation);
    });
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return countIter(predicate,0,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // remove (inverse of filter)
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(EMPTY_STRING);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    Datum hd = new escm.type.Character(codepoint);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) return removeIter(predicate,offset+increment,n,continuation); 
      return removeIter(predicate,offset+increment,n,(removedRest) -> () -> continuation.run(new String(hd.display()+removedRest.display())));
    });
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return removeIter(predicate,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // val
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("STRING [VAL]: invalid index %s for string value", key.profile());
    Real r = (Real)key;
    if(r.isNegative() || r.gte(new Exact(codePointLength())))
      throw new Exceptionf("STRING [VAL]: index %s out of string range [0,%d)", r.write(), codePointLength());
    return charAt(r.intValue());
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private Trampoline.Bounce keyIter(Callable predicate, int offset, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("STRING [KEY]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("STRING [KEY]: no value in %s satisfies value predicate %s", write(), predicate);
    }
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(matchedKey) -> () -> {
      if(matchedKey.isTruthy()) return continuation.run(new Exact(idx));
      return keyIter(predicate,offset+increment,idx+1,n,continuation);
    });
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return keyIter(predicate,0,0,value.length(),continuation);
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

  public AssociativeCollection append(AssociativeCollection ac) throws Exception {
    return new String(value+((String)ac).value);
  }

  public AssociativeCollection AppendArray(AssociativeCollection[] acs) throws Exception {
    StringBuilder sb = new StringBuilder();
    for(int i = 0; i < acs.length; ++i) {
      sb.append(((String)acs[i]).value);
    }
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // delete
  //////////////////////////////////////

  public AssociativeCollection delete(Datum key) throws Exception { // returns <this> if deletion fails
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("STRING [DELETE]: invalid index %s for string deletion", key.profile());
    int deleteIdx = ((Real)key).intValue();
    if(deleteIdx < 0 || deleteIdx >= codePointLength())
      throw new Exceptionf("STRING [DELETE]: index %d out of string range [0,%d)", deleteIdx, codePointLength());
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> {
      if(idx != deleteIdx) sb.append(java.lang.Character.toString(cp));
      return true;
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum newValue) throws Exception {
    if(!(newValue instanceof escm.type.Character))
      throw new Exceptionf("STRING [CONJ]: invalid non-char value %s for string", newValue.profile());
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("STRING [CONJ]: invalid index %s for string", key.profile());
    int idx = ((Real)key).intValue();
    int n = codePointLength();
    if(idx < -1 || idx > n)
      throw new Exceptionf("STRING [CONJ]: index %d violates string bounds [-1,%d]", idx, n);
    if(idx == -1) {
      return new String(newValue.display() + value);
    } else if(idx == n) {
      return new String(value + newValue.display());
    } else {
      StringBuilder sb = new StringBuilder();
      forEachCodepoint((i,cp) -> {
        if(i != idx) {
          sb.append(java.lang.Character.toString(cp));
        } else {
          sb.append(newValue.display());
        }
        return true;
      });
      return new String(sb.toString());
    }
  }

  //////////////////////////////////////
  // drop
  //////////////////////////////////////

  public AssociativeCollection drop(int amount) throws Exception {
    int start = Math.max(0,amount);
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((i,cp) -> {
      if(i >= start) sb.append(java.lang.Character.toString(cp));
      return true;
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    amount = Math.max(0,amount);
    if(amount == 0) return EMPTY_STRING;
    StringBuilder sb = new StringBuilder();
    int end = Math.min(codePointLength(),amount);
    forEachCodepoint((i,cp) -> {
      if(i < end) {
        sb.append(java.lang.Character.toString(cp));
        return true;
      } else {
        return false;
      }
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  private static class ListBox {
    public Datum value = Nil.VALUE;
  }

  public Datum toACList() throws Exception {
    ListBox lb = new ListBox();
    forEachCharReverse((idx,chr)-> {
      lb.value = new escm.type.Pair(chr,lb.value);
      return true;
    });
    return lb.value;
  }

  public String toACString() throws Exception {
    return this;
  }

  public Vector toACVector() throws Exception {
    ArrayList<Datum> vals = new ArrayList<Datum>();
    forEachChar((i,chr) -> {
      vals.add(chr);
      return true;
    });
    return new Vector(0,vals);
  }

  public Hashmap toACHashmap() throws Exception {
    Hashmap h = new Hashmap();
    forEachChar((i,chr) -> {
      h.set(new Exact(i),chr);
      return true;
    });
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

  private Trampoline.Bounce UnionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int i, String s, int offset, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(values);
    if(offset >= s.value.length()) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? (String)acs[i] : (String)acs[i+1],0,values,continuation);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    escm.type.Character chr = new escm.type.Character(codepoint);
    return inValues(eltPredicate, chr, values, (isInValues) -> () -> {
      if(isInValues.isTruthy()) {
        return UnionArrayIter(eltPredicate,acs,i,s,offset+increment,values,continuation);
      }
      return UnionArrayIter(eltPredicate,acs,i,s,offset+increment,new Pair(chr,values),continuation);
    });
  }

  public Trampoline.Bounce UnionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return UnionArrayIter(eltPredicate,acs,0,(String)acs[0],0,Nil.VALUE,(valuesList) -> () -> {
      Datum vl = valuesList; // lambda arg must be <final>
      StringBuilder sb = new StringBuilder();
      while(vl instanceof Pair) {
        Pair p = (Pair)vl;
        sb.append(p.car().display());
        vl = p.cdr();
      }
      return continuation.run(new String(sb.toString()));
    });
  }

  //////////////////////////////////////
  // intersection set operation
  //////////////////////////////////////

  private Trampoline.Bounce itemIntersectsAC(Callable eltPredicate, String ac, int offset, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(offset >= ac.value.length()) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(item);
    int codepoint = ac.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return eltPredicate.callWith(args,(isSameItem) -> () -> {
      if(isSameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return itemIntersectsAC(eltPredicate,ac,offset+increment,item,continuation);
    });
  }

  private Trampoline.Bounce itemIntersects(Callable eltPredicate, AssociativeCollection[] acs, int i, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(Boolean.TRUE);
    return itemIntersectsAC(eltPredicate,(String)acs[i],0,item,(itemInAC) -> () -> {
      if(itemInAC.isTruthy()) return () -> itemIntersects(eltPredicate,acs,i+1,item,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  private Trampoline.Bounce IntersectionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int acIdx, String s, int offset, Datum intersectingValues, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= acs.length) return continuation.run(intersectingValues);
    if(offset >= s.value.length()) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? (String)acs[acIdx] : (String)acs[acIdx+1],0,intersectingValues,continuation);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    escm.type.Character chr = new escm.type.Character(codepoint);
    return inValues(eltPredicate, chr, intersectingValues, (isInValues) -> () -> {
      if(!isInValues.isTruthy()) {
        return itemIntersects(eltPredicate,acs,0,chr,(intersects) -> () -> {
          if(intersects.isTruthy()) {
            return IntersectionArrayIter(eltPredicate,acs,acIdx,s,offset+increment,new Pair(chr,intersectingValues),continuation);
          }
          return IntersectionArrayIter(eltPredicate,acs,acIdx,s,offset+increment,intersectingValues,continuation);
        });
      }
      return IntersectionArrayIter(eltPredicate,acs,acIdx,s,offset+increment,intersectingValues,continuation);
    });
  }

  public Trampoline.Bounce IntersectionArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return IntersectionArrayIter(eltPredicate,acs,0,(String)acs[0],0,Nil.VALUE,(intersectingValues) -> () -> {
      Datum ivs = intersectingValues; // lambda arg must be <final>
      StringBuilder sb = new StringBuilder();
      while(ivs instanceof Pair) {
        Pair p = (Pair)ivs;
        sb.append(p.car().display());
        ivs = p.cdr();
      }
      return continuation.run(new String(sb.toString()));
    });
  }

  //////////////////////////////////////
  // difference set operation
  //////////////////////////////////////

  private Trampoline.Bounce inValuesString(Callable eltPredicate, Datum item, String valString, int offset, Trampoline.Continuation continuation) throws Exception {
    if(offset >= valString.value.length()) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(item);
    int codepoint = valString.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return eltPredicate.callWith(args,(sameItem) -> () -> {
      if(sameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValuesString(eltPredicate,item,valString,offset+increment,continuation);
    });
  }

  private Trampoline.Bounce binaryDifferenceArrayIter(Datum acc, Callable eltPredicate, Datum valList, String valString, Trampoline.Continuation continuation) throws Exception {
    if(!(valList instanceof Pair)) return continuation.run(acc);
    Pair p = (Pair)valList;
    return inValuesString(eltPredicate,p.car(),valString,0,(inString) -> () -> {
      if(inString.isTruthy()) {
        return binaryDifferenceArrayIter(acc,eltPredicate,p.cdr(),valString,continuation);
      }
      return binaryDifferenceArrayIter(new Pair(p.car(),acc),eltPredicate,p.cdr(),valString,continuation);
    });
  }

  private Trampoline.Bounce DifferenceArrayIter(Callable eltPredicate, Datum lhs, AssociativeCollection[] acs, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(lhs);
    return binaryDifferenceArrayIter(Nil.VALUE,eltPredicate,lhs,(String)acs[i],(differenceArray) -> () -> DifferenceArrayIter(eltPredicate,differenceArray,acs,i+1,continuation));
  }

  public Trampoline.Bounce DifferenceArray(Callable eltPredicate, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return DifferenceArrayIter(eltPredicate,acs[0].toACList(),acs,1,(differenceList) -> () -> {
      Datum dl = differenceList; // lambda arg must be <final>
      StringBuilder sb = new StringBuilder();
      while(dl instanceof Pair) {
        Pair p = (Pair)dl;
        sb.append(p.car().display());
        dl = p.cdr();
      }
      return continuation.run(new String(sb.toString()));
    });
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
    if(!(value instanceof escm.type.Character))
      throw new Exceptionf("STRING [CONJ]: invalid non-char value %s for string", value.profile());
    return new String(this.value + value.display());
  }

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////

  public OrderedCollection init() throws Exception {
    int n = codePointLength();
    if(n == 0) throw new Exceptionf("STRING [INIT]: Can't get init of an empty string!");
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { 
      if(idx < n-1) {
        sb.append(java.lang.Character.toString(cp)); 
        return true;
      } else {
        return false;
      }
    });
    return new String(sb.toString());
  }

  public Datum last() throws Exception {
    int n = codePointLength();
    if(n == 0) throw new Exceptionf("STRING [LAST]: Can't get last of an empty string!");
    return charAt(n-1);
  }

  //////////////////////////////////////
  // slicing
  //////////////////////////////////////

  private int getOffsetForIndex(int index) throws Exception {
    int offset = 0, codePointIndex = 0, strLength = value.length();
    while(offset < strLength) {
      if(codePointIndex == index) return offset;
      offset += java.lang.Character.charCount(value.codePointAt(offset));
      ++codePointIndex;
    }
    return strLength;
  }

  private static class CounterWrapper {
    public int count = 0;
  }

  private Trampoline.Bounce sliceGetLastIdx(int i, int n, int offset, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    if(i >= n) return continuation.run(new Exact(i));
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return continuePredicate.callWith(args,(shouldContinue) -> () -> {
      if(!shouldContinue.isTruthy()) return continuation.run(new Exact(i));
      return sliceGetLastIdx(i+1,n,offset+increment,continuePredicate,continuation);
    });
  }

  public OrderedCollection slice(int startIdx) throws Exception {
    return slice(startIdx,codePointLength());
  }

  public OrderedCollection slice(int startIdx, int length) throws Exception {
    int n = codePointLength();
    if(startIdx < 0 || length <= 0 || startIdx >= n) return EMPTY_STRING;
    int sliceLength = Math.min(length,n-startIdx);
    CounterWrapper cw = new CounterWrapper();
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { 
      if(cw.count >= sliceLength) return false;
      if(idx >= startIdx) {
        sb.append(java.lang.Character.toString(cp));
        ++cw.count;
      }
      return true;
    });
    return new String(sb.toString());
  }

  public Trampoline.Bounce slice(int startIdx, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    int n = codePointLength();
    int idx = startIdx <= 0 ? 0 : startIdx;
    if(idx >= n) return continuation.run(EMPTY_STRING);
    return sliceGetLastIdx(idx,n,getOffsetForIndex(idx),continuePredicate,(endIdx) -> () -> {
      return continuation.run((Datum)slice(idx,((Real)endIdx).intValue()-idx));
    });
  }

  //////////////////////////////////////
  // reversing
  //////////////////////////////////////

  public OrderedCollection reverse() throws Exception {
    StringBuilder sb = new StringBuilder();
    forEachCodepointReverse((idx,cp) -> {
      sb.append(java.lang.Character.toString(cp));
      return true;
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // removing items
  //////////////////////////////////////

  private Trampoline.Bounce removeFirstIter(Callable predicate, int i, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(i >= n) return continuation.run(this);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) {
        StringBuilder sb = new StringBuilder();
        forEachCodepoint((idx,cp) -> { 
          if(idx != i) sb.append(java.lang.Character.toString(cp)); 
          return true;
        });
        return continuation.run(new String(sb.toString()));
      }
      return removeFirstIter(predicate,i+1,n,offset+increment,continuation);
    });
  }

  public Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return removeFirstIter(predicate,0,codePointLength(),0,continuation);
  }

  private Trampoline.Bounce removeLastIter(Callable predicate, int i, int offset, Trampoline.Continuation continuation) throws Exception {
    if(i < 0) return continuation.run(this);
    char c = value.charAt(offset);
    boolean at32bitUnicode = java.lang.Character.isLowSurrogate(c);
    int codepoint = at32bitUnicode ? value.codePointAt(offset-1) : c;
    int decrement = at32bitUnicode ? 2 : 1;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) {
        StringBuilder sb = new StringBuilder();
        forEachCodepoint((idx,cp) -> { 
          if(idx != i) sb.append(java.lang.Character.toString(cp)); 
          return true;
        });
        return continuation.run(new String(sb.toString()));
      }
      return removeLastIter(predicate,i-1,offset-decrement,continuation);
    });
  }

  public Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return removeLastIter(predicate,codePointLength()-1,value.length()-1,continuation);
  }

  //////////////////////////////////////
  // skipping
  //////////////////////////////////////

  private Trampoline.Bounce skipIter(Callable predicate, int i, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(i >= n) return continuation.run(Boolean.FALSE);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    escm.type.Character ithChar = new escm.type.Character(codepoint);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(ithChar);
    return predicate.callWith(args,(keepSkipping) -> () -> {
      if(keepSkipping.isTruthy()) return skipIter(predicate,i+1,n,offset+increment,continuation);
      return continuation.run(ithChar);
    });
  }

  public Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return skipIter(predicate,0,codePointLength(),0,continuation);
  }

  private Trampoline.Bounce skipRightIter(Callable predicate, int i, int offset, Trampoline.Continuation continuation) throws Exception {
    if(i < 0) return continuation.run(Boolean.FALSE);
    char c = value.charAt(offset);
    boolean at32bitUnicode = java.lang.Character.isLowSurrogate(c);
    int codepoint = at32bitUnicode ? value.codePointAt(offset-1) : c;
    int decrement = at32bitUnicode ? 2 : 1;
    escm.type.Character ithChar = new escm.type.Character(codepoint);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(ithChar);
    return predicate.callWith(args,(keepSkipping) -> () -> {
      if(keepSkipping.isTruthy()) return skipRightIter(predicate,i-1,offset-decrement,continuation);
      return continuation.run(ithChar);
    });
  }

  public Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return skipRightIter(predicate,codePointLength()-1,value.length()-1,continuation);
  }

  //////////////////////////////////////
  // fold-right (unary)
  //////////////////////////////////////

  private static Trampoline.Bounce foldRightIter(Callable c, Datum seed, int offset, int n, String s, Trampoline.Continuation continuation) throws Exception { // -> Datum
    if(offset >= n) return continuation.run(seed);
    ArrayList<Datum> params = new ArrayList<Datum>(1);
    int codepoint = s.value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    params.add(new escm.type.Character(codepoint));
    return () -> foldRightIter(c,seed,offset+increment,n,s,(acc) -> () -> {
      ArrayList<Datum> args = (ArrayList<Datum>)params.clone();
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }

  public Trampoline.Bounce foldRight(Callable c, Datum seed, Trampoline.Continuation continuation) throws Exception {
    return foldRightIter(c,seed,0,value.length(),this,continuation);
  }

  //////////////////////////////////////
  // fold-right (binary+)
  //////////////////////////////////////

  private static Trampoline.Bounce FoldRightArray(Callable c, Datum seed, int[] offsets, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> params = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      String s = (String)acs[i];
      if(offsets[i] >= s.value.length()) return continuation.run(seed);
      int codepoint = s.value.codePointAt(offsets[i]);
      params.add(new escm.type.Character(codepoint));
      offsets[i] += java.lang.Character.charCount(codepoint); // can mutate <offsets> since never captured in a continuation (unlike <fold>)!
    }
    return () -> FoldRightArray(c,seed,offsets,acs,(acc) -> () -> {
      ArrayList<Datum> args = (ArrayList<Datum>)params.clone();
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }


  public Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return FoldRightArray(c,seed,new int[acs.length],acs,continuation);
  }

  //////////////////////////////////////
  // key-right
  //////////////////////////////////////

  private Trampoline.Bounce keyRight(Callable predicate, int idx, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0 || idx >= n) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("STRING [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("STRING [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), predicate);
    }
    char c = value.charAt(offset);
    boolean at32bitUnicode = java.lang.Character.isLowSurrogate(c);
    int codepoint = at32bitUnicode ? value.codePointAt(offset-1) : c;
    int decrement = at32bitUnicode ? 2 : 1;
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(shouldGetKey) -> () -> {
      if(!shouldGetKey.isTruthy()) return keyRight(predicate,idx-1,n,offset-decrement,continuation);
      return continuation.run(new Exact(idx));
    });
  }

  public Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    int n = codePointLength();
    return keyRight(predicate,n-1,n,value.length()-1,(key) -> () -> {
      if(key instanceof Real) return continuation.run(key);
      if(predicate instanceof Datum) {
        throw new Exceptionf("STRING [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("STRING [KEY-RIGHT]: no value in %s satisfies value predicate %s", write(), predicate);
    }); 
  }

  //////////////////////////////////////
  // dropping
  //////////////////////////////////////

  private static final Boolean KEEP_DROPPING_FLAG = Boolean.valueOf(false);

  public OrderedCollection dropRight(int length) throws Exception {
    return slice(0,codePointLength()-length);
  }

  private Trampoline.Bounce dropWhile(Callable predicate, int idx, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(EMPTY_STRING);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(!keepDropping.isTruthy()) return continuation.run((Datum)slice(idx));
      return dropWhile(predicate,idx+1,offset+increment,n,continuation);
    });
  }

  public Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropWhile(predicate,0,0,value.length(),continuation);
  }

  private Trampoline.Bounce dropRightWhile(Callable predicate, int idx, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0) return continuation.run(EMPTY_STRING);
    char c = value.charAt(offset);
    boolean at32bitUnicode = java.lang.Character.isLowSurrogate(c);
    int codepoint = at32bitUnicode ? value.codePointAt(offset-1) : c;
    int decrement = at32bitUnicode ? 2 : 1;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(!keepDropping.isTruthy()) return continuation.run((Datum)slice(0,idx+1));
      return dropRightWhile(predicate,idx-1,offset-decrement,continuation);
    });
  }

  public Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropRightWhile(predicate,codePointLength()-1,value.length()-1,continuation);
  }

  //////////////////////////////////////
  // taking
  //////////////////////////////////////

  public OrderedCollection takeRight(int length) throws Exception {
    int n = codePointLength();
    if(length >= n) return this;
    return slice(n-length,n);
  }

  private Trampoline.Bounce takeWhile(Callable predicate, int idx, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(this);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(keepTaking) -> () -> {
      if(!keepTaking.isTruthy()) return continuation.run((Datum)slice(0,idx));
      return takeWhile(predicate,idx+1,offset+increment,n,continuation);
    });
  }

  public Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return takeWhile(predicate,0,0,value.length(),continuation);
  }

  private Trampoline.Bounce takeRightWhile(Callable predicate, int idx, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0) return continuation.run(this);
    char c = value.charAt(offset);
    boolean at32bitUnicode = java.lang.Character.isLowSurrogate(c);
    int codepoint = at32bitUnicode ? value.codePointAt(offset-1) : c;
    int decrement = at32bitUnicode ? 2 : 1;
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(new escm.type.Character(codepoint));
    return predicate.callWith(args,(keepTaking) -> () -> {
      if(!keepTaking.isTruthy()) return continuation.run((Datum)slice(idx+1,n));
      return takeRightWhile(predicate,idx-1,n,offset-decrement,continuation);
    });
  }

  public Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    int n = codePointLength();
    return takeRightWhile(predicate,n-1,n,value.length()-1,continuation);
  }

  //////////////////////////////////////
  // sorting
  //////////////////////////////////////

  private Datum mergeSortedHalves(String lhs, escm.type.Character hd, String rhs) throws Exception {
    return new String(lhs.value + hd.display() + rhs.value);
  }

  private Trampoline.Bounce sort(Callable binaryPredicate, int offset, int n, Trampoline.Continuation continuation) throws Exception {
    if(offset >= n) return continuation.run(EMPTY_STRING);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    escm.type.Character hd = new escm.type.Character(codepoint);
    Callable trueCondPrimitive = new Callable() {
      public java.lang.String docstring() {
        return "Quicksort: move values to the left of the pivot.";
      }
      public Datum signature() { 
        return Pair.List(new Symbol("escm-sort-in-lhs?"),new Symbol("<obj>"),new Symbol("<obj>")); 
      }
      public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
        params.add(hd);
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
        params.add(hd);
        return binaryPredicate.callWith(params,(value) -> () -> cont.run(Boolean.valueOf(!value.isTruthy())));
      }
    };
    PrimitiveProcedure trueCond = new PrimitiveProcedure("escm-sort-in-lhs?", trueCondPrimitive);
    PrimitiveProcedure falseCond = new PrimitiveProcedure("escm-sort-in-rhs?", falseCondPrimitive);
    return filterFrom(trueCond,offset+increment,(lhs) -> () -> {
      return ((OrderedCollection)lhs).sort(binaryPredicate,(sortedLhs) -> () -> {
        return filterFrom(falseCond,offset+increment,(rhs) -> () -> {
          return ((OrderedCollection)rhs).sort(binaryPredicate,(sortedRhs) -> () -> {
            return continuation.run(mergeSortedHalves((String)sortedLhs,hd,(String)sortedRhs));
          });
        });
      });
    });
  }

  public Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sort(binaryPredicate,0,value.length(),continuation);
  }

  private Trampoline.Bounce sorted(Callable binaryPredicate, int idx, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx+1 >= n) return continuation.run(Boolean.TRUE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    args.add(new escm.type.Character(value.codePointAt(offset+increment)));
    return binaryPredicate.callWith(args,(isEq) -> () -> {
      if(isEq.isTruthy()) return sorted(binaryPredicate,idx+1,n,offset+increment,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  public Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sorted(binaryPredicate,0,codePointLength(),0,continuation);
  }

  //////////////////////////////////////
  // merging
  //////////////////////////////////////

  private static Trampoline.Bounce mergeIter(Callable binaryPredicate, String s1, String s2, int i1, int i2, int n1, int n2, int offset1, int offset2, Trampoline.Continuation continuation) throws Exception {
    if(i1 >= n1) return continuation.run((Datum)s2.slice(i2));
    if(i2 >= n2) return continuation.run((Datum)s1.slice(i1));
    int codepoint1 = s1.value.codePointAt(offset1);
    int increment1 = java.lang.Character.charCount(codepoint1);
    int codepoint2 = s2.value.codePointAt(offset2);
    int increment2 = java.lang.Character.charCount(codepoint2);
    Datum s1Elt = new escm.type.Character(codepoint1);
    Datum s2Elt = new escm.type.Character(codepoint2);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(s1Elt);
    args.add(s2Elt);
    return binaryPredicate.callWith(args,(lt) -> () -> {
      if(lt.isTruthy()) {
        return mergeIter(binaryPredicate,s1,s2,i1+1,i2,n1,n2,offset1+increment1,offset2,(merged) -> () -> continuation.run(new String(s1Elt.display()+merged.display())));
      }
      return mergeIter(binaryPredicate,s1,s2,i1,i2+1,n1,n2,offset1,offset2+increment2,(merged) -> () -> continuation.run(new String(s2Elt.display()+merged.display())));
    });
  }

  public Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception {
    return mergeIter(binaryPredicate,this,(String)oc,0,0,codePointLength(),((String)oc).codePointLength(),0,0,continuation);
  }

  //////////////////////////////////////
  // duplicate neighbor deletion
  //////////////////////////////////////

  private Trampoline.Bounce skipWhileHaveDuplicates(Callable binaryPredicate, Datum d, int idx, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx >= n) return continuation.run(new Pair(new Exact(idx),new Exact(offset)));
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(d);
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    args.add(new escm.type.Character(codepoint));
    return binaryPredicate.callWith(args,(isEq) -> () -> {
      if(isEq.isTruthy()) {
        return skipWhileHaveDuplicates(binaryPredicate,d,idx+1,n,offset+increment,continuation);
      }
      return continuation.run(new Pair(new Exact(idx),new Exact(offset)));
    });
  }

  private Trampoline.Bounce deleteListNeighborDuplicates(Callable binaryPredicate, int idx, int n, int offset, Trampoline.Continuation continuation) throws Exception {
    if(idx+1 >= n) return continuation.run((Datum)slice(idx));
    int codepoint = value.codePointAt(offset);
    int increment = java.lang.Character.charCount(codepoint);
    Datum elt = new escm.type.Character(codepoint);
    return skipWhileHaveDuplicates(binaryPredicate,elt,idx+1,n,offset+increment,(idxAndOffsetAfterDups) -> () -> {
      Pair p = (Pair)idxAndOffsetAfterDups;
      return deleteListNeighborDuplicates(binaryPredicate,((Real)p.car()).intValue(),n,((Real)p.cdr()).intValue(),
        (deld) -> () -> continuation.run(new String(elt.display()+deld.display())));
    });
  }

  public Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return deleteListNeighborDuplicates(binaryPredicate,0,codePointLength(),0,continuation);
  }
}