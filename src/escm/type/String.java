// Author: Jordan Randleman - escm.type.String
// Purpose:
//    String primitive type.

package escm.type;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.bool.Boolean;
import escm.type.number.Number;
import escm.type.number.Exact;
import escm.type.number.Real;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.StringParser;
import escm.vm.util.ExecutionState;
import escm.vm.util.ExecutionState;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.OrderedCollection;
import escm.vm.type.Callable;

public class String extends Datum implements OrderedCollection, Callable {
  ////////////////////////////////////////////////////////////////////////////
  // Private Value Fields
  private final java.lang.String value;
  private final int codePointLength;


  ////////////////////////////////////////////////////////////////////////////
  // Value Getter
  public java.lang.String value() {
    return value;
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
    throw new Exceptionf("STRING [GET]: Invalid index %d (size %d) for string %s", index, codePointLength, write());
  }

  public escm.type.Character charAt(int index) throws Exception {
    return new escm.type.Character(codePointAt(index));
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
  // EScheme Character Iteration
  private static interface CharacterIterationProcedure {
    public void exec(int idx, escm.type.Character ch);
  }

  public void forEachChar(CharacterIterationProcedure cip) {
    int offset = 0, charsIdx = 0, strLength = value.length();
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      cip.exec(charsIdx++,new escm.type.Character(codepoint));
      offset += java.lang.Character.charCount(codepoint);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // EScheme Codepoint Iteration
  private static interface CodepointIterationProcedure {
    public void exec(int idx, int cp);
  }

  public void forEachCodepoint(CodepointIterationProcedure cip) {
    int offset = 0, charsIdx = 0, strLength = value.length();
    while(offset < strLength) {
      int codepoint = value.codePointAt(offset);
      cip.exec(charsIdx++,codepoint);
      offset += java.lang.Character.charCount(codepoint);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public String(java.lang.String s) {
    value = s;
    codePointLength = s.codePointCount(0,s.length());
  }

  public String() {
    value = "";
    codePointLength = 0;
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
    if(index < 0 || index >= codePointLength)
      throw new Exceptionf("STRING [CALLABLE-GET]: Invalid index %d (size %d) for string %s", index, codePointLength, write());
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
  public String copy() {
    return this;
  }


  ////////////////////////////////////////////////////////////////////////////
  // <AssociativeCollection> Instance Methods

  //////////////////////////////////////
  // basic accessors
  //////////////////////////////////////
  public Datum head() throws Exception {
    if(codePointLength == 0) throw new Exceptionf("STRING [HEAD]: Can't get head of an empty string!");
    return charAt(0);
  }

  public AssociativeCollection tail() throws Exception {
    if(codePointLength == 0) throw new Exceptionf("STRING [TAIL]: Can't get tail of an empty string!");
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { if(idx > 0) sb.append(java.lang.Character.toString(cp)); });
    return new String(sb.toString());
  }

  public int length() {
    return codePointLength;
  }

  //////////////////////////////////////
  // fold
  //////////////////////////////////////

  private static Trampoline.Bounce FoldArrayIter(Callable c, Datum seed, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length+1);
    args.add(seed);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length()) return continuation.run(seed);
      args.add(((String)acs[i]).charAt(acPos));
    }
    return c.callWith(args,(acc) -> () -> FoldArrayIter(c,acc,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce FoldArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return FoldArrayIter(c,seed,acs,0,continuation);
  }

  ///////////////////////////////////////
  // map
  ///////////////////////////////////////

  private static Trampoline.Bounce MapIter(Callable c, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length()) return continuation.run(Nil.VALUE);
      args.add(((String)acs[i]).charAt(acPos));
    }
    return c.callWith(args,(mappedValue) -> () -> MapIter(c,acs,acPos+1,(mappedRest) -> () -> continuation.run(new Pair(mappedValue,mappedRest))));
  }

  public Trampoline.Bounce MapArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return MapIter(c,acs,0,(mappedList) -> () -> continuation.run(((AssociativeCollection)mappedList).toACString()));
  }

  //////////////////////////////////////
  // for-each
  //////////////////////////////////////

  private static Trampoline.Bounce ForEachIter(Callable c, AssociativeCollection[] acs, int acPos, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> args = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      if(acPos >= acs[i].length()) return continuation.run(Void.VALUE);
      args.add(((String)acs[i]).charAt(acPos));
    }
    return c.callWith(args,(ignore) -> () -> ForEachIter(c,acs,acPos+1,continuation));
  }

  public Trampoline.Bounce ForEachArray(Callable c, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return ForEachIter(c,acs,0,continuation);
  }

  //////////////////////////////////////
  // filter
  //////////////////////////////////////

  private Trampoline.Bounce filterIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength) return continuation.run(Nil.VALUE);
    Datum hd = charAt(acPos);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldKeep) -> () -> {
      if(shouldKeep.isTruthy()) {
        return filterIter(predicate,acPos+1,(filteredRest) -> () -> continuation.run(new Pair(hd,filteredRest)));
      }
      return filterIter(predicate,acPos+1,continuation);
    });
  }

  // Helper also used by <sort>
  private Trampoline.Bounce filterFrom(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,acPos,(filteredList) -> () -> continuation.run(((AssociativeCollection)filteredList).toACString()));
  }

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterFrom(predicate,0,continuation);
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private static final Exact ZERO = new Exact();
  private static final Exact ONE = new Exact(1);

  private Trampoline.Bounce countIter(Callable predicate, Number n, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength) return continuation.run(n);
    Datum hd = charAt(acPos);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldCount) -> () -> {
      if(shouldCount.isTruthy()) {
        return countIter(predicate,n.add(ONE),acPos+1,continuation);
      }
      return countIter(predicate,n,acPos+1,continuation);
    });
  }

  public Trampoline.Bounce count(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Exact
    return countIter(predicate,ZERO,0,continuation);
  }

  //////////////////////////////////////
  // remove (inverse of filter)
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength) return continuation.run(Nil.VALUE);
    Datum hd = charAt(acPos);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(hd);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) return removeIter(predicate,acPos+1,continuation); 
      return removeIter(predicate,acPos+1,(removedRest) -> () -> continuation.run(new Pair(hd,removedRest)));
    });
  }

  public Trampoline.Bounce remove(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return removeIter(predicate,0,(removedList) -> () -> continuation.run(((AssociativeCollection)removedList).toACString()));
  }

  //////////////////////////////////////
  // val
  //////////////////////////////////////

  public Datum val(Datum key) throws Exception {
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("STRING [VAL]: invalid index %s for string value", key.profile());
    Real r = (Real)key;
    if(r.lt(ZERO) || r.gte(new Exact(codePointLength)))
      throw new Exceptionf("STRING [VAL]: index %s out of string range [0,%d)", r.write(), codePointLength);
    return charAt(r.intValue());
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private Trampoline.Bounce keyIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("STRING [KEY]: no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("STRING [KEY]: no value in %s satisfies value predicate %s", write(), predicate);
    }
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(acPos));
    return predicate.callWith(args,(matchedKey) -> () -> {
      if(matchedKey.isTruthy()) return continuation.run(new Exact(acPos));
      return keyIter(predicate,acPos+1,continuation);
    });
  }

  public Trampoline.Bounce key(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> Datum
    return keyIter(predicate,0,continuation);
  }

  //////////////////////////////////////
  // append
  //////////////////////////////////////

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
    if(deleteIdx < 0 || deleteIdx >= codePointLength)
      throw new Exceptionf("STRING [DELETE]: index %d out of string range [0,%d)", deleteIdx, codePointLength);
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> {
      if(idx != deleteIdx) sb.append(java.lang.Character.toString(cp));
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
    if(idx < -1 || idx > codePointLength)
      throw new Exceptionf("STRING [CONJ]: index %d extends string bounds [-1,%d)", idx, codePointLength);
    if(idx == -1) {
      return new String(newValue.display() + value);
    } else if(idx == codePointLength) {
      return new String(value + newValue.display());
    } else {
      StringBuilder sb = new StringBuilder();
      forEachCodepoint((i,cp) -> {
        if(i != idx) {
          sb.append(java.lang.Character.toString(cp));
        } else {
          sb.append(newValue.display());
        }
      });
      return new String(sb.toString());
    }
  }

  //////////////////////////////////////
  // drop
  //////////////////////////////////////

  public AssociativeCollection drop(int amount) throws Exception {
    StringBuilder sb = new StringBuilder();
    int start = Math.max(0,amount);
    forEachCodepoint((i,cp) -> {
      if(i >= start) sb.append(java.lang.Character.toString(cp));
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    StringBuilder sb = new StringBuilder();
    int end = Math.min(codePointLength,amount);
    forEachCodepoint((i,cp) -> {
      if(i < end) sb.append(java.lang.Character.toString(cp));
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // coercions
  //////////////////////////////////////

  public Datum toACList() throws Exception {
    escm.type.Character[] chars = toChars();
    Datum lis = escm.type.Nil.VALUE;
    for(int i = chars.length-1; i >= 0; --i)
      lis = new escm.type.Pair(chars[i],lis);
    return lis; 
  }

  public String toACString() throws Exception {
    return this;
  }

  public Vector toACVector() throws Exception {
    Vector v = new Vector();
    forEachChar((i,chr) -> {
      v.push(chr);
    });
    return v;
  }

  public Hashmap toACHashmap() throws Exception {
    Hashmap h = new Hashmap();
    forEachChar((i,chr) -> {
      h.set(new Exact(i),chr);
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

  private Trampoline.Bounce UnionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int i, String s, int j, Datum values, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(values);
    if(j >= s.codePointLength) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? (String)acs[i] : (String)acs[i+1],0,values,continuation);
    escm.type.Character chr = s.charAt(j);
    return inValues(eltPredicate, chr, values, (isInValues) -> () -> {
      if(isInValues.isTruthy()) {
        return UnionArrayIter(eltPredicate,acs,i,s,j+1,values,continuation);
      }
      return UnionArrayIter(eltPredicate,acs,i,s,j+1,new Pair(chr,values),continuation);
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

  private Trampoline.Bounce itemIntersectsAC(Callable eltPredicate, String ac, int acIdx, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= ac.codePointLength) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(item);
    args.add(ac.charAt(acIdx));
    return eltPredicate.callWith(args,(isSameItem) -> () -> {
      if(isSameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return itemIntersectsAC(eltPredicate,ac,acIdx+1,item,continuation);
    });
  }

  private Trampoline.Bounce itemIntersects(Callable eltPredicate, AssociativeCollection[] acs, int i, Datum item, Trampoline.Continuation continuation) throws Exception {
    if(i >= acs.length) return continuation.run(Boolean.TRUE);
    return itemIntersectsAC(eltPredicate,(String)acs[i],0,item,(itemInAC) -> () -> {
      if(itemInAC.isTruthy()) return () -> itemIntersects(eltPredicate,acs,i+1,item,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  private Trampoline.Bounce IntersectionArrayIter(Callable eltPredicate, AssociativeCollection[] acs, int acIdx, String s, int sIdx, Datum intersectingValues, Trampoline.Continuation continuation) throws Exception {
    if(acIdx >= acs.length) return continuation.run(intersectingValues);
    if(sIdx >= s.codePointLength) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? (String)acs[acIdx] : (String)acs[acIdx+1],0,intersectingValues,continuation);
    escm.type.Character chr = s.charAt(sIdx);
    return inValues(eltPredicate, chr, intersectingValues, (isInValues) -> () -> {
      if(!isInValues.isTruthy()) {
        return itemIntersects(eltPredicate,acs,0,chr,(intersects) -> () -> {
          if(intersects.isTruthy()) {
            return IntersectionArrayIter(eltPredicate,acs,acIdx,s,sIdx+1,new Pair(chr,intersectingValues),continuation);
          }
          return IntersectionArrayIter(eltPredicate,acs,acIdx,s,sIdx+1,intersectingValues,continuation);
        });
      }
      return IntersectionArrayIter(eltPredicate,acs,acIdx,s,sIdx+1,intersectingValues,continuation);
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

  private Trampoline.Bounce inValuesString(Callable eltPredicate, Datum item, String valString, int i, Trampoline.Continuation continuation) throws Exception {
    if(i >= valString.codePointLength) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(item);
    args.add(valString.charAt(i));
    return eltPredicate.callWith(args,(sameItem) -> () -> {
      if(sameItem.isTruthy()) return continuation.run(Boolean.TRUE);
      return inValuesString(eltPredicate,item,valString,i+1,continuation);
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
    if(codePointLength == 0) throw new Exceptionf("STRING [INIT]: Can't get init of an empty string!");
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { if(idx < codePointLength-1) sb.append(java.lang.Character.toString(cp)); });
    return new String(sb.toString());
  }

  public Datum last() throws Exception {
    if(codePointLength == 0) throw new Exceptionf("STRING [LAST]: Can't get last of an empty string!");
    return charAt(codePointLength-1);
  }

  //////////////////////////////////////
  // slicing
  //////////////////////////////////////

  private static class CounterWrapper {
    public int count = 0;
  }

  private Trampoline.Bounce sliceGetLastIdx(int i, int n, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    if(i >= n) return continuation.run(new Exact(i));
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(i));
    return continuePredicate.callWith(args,(shouldContinue) -> () -> {
      if(!shouldContinue.isTruthy()) return continuation.run(new Exact(i));
      return sliceGetLastIdx(i+1,n,continuePredicate,continuation);
    });
  }

  public OrderedCollection slice(int startIdx) throws Exception {
    return slice(startIdx,codePointLength);
  }

  public OrderedCollection slice(int startIdx, int length) throws Exception {
    if(startIdx < 0 || length <= 0 || startIdx >= codePointLength) return new String();
    int sliceLength = Math.min(length,codePointLength-startIdx);
    CounterWrapper cw = new CounterWrapper();
    StringBuilder sb = new StringBuilder();
    forEachCodepoint((idx,cp) -> { 
      if(idx >= startIdx && cw.count < sliceLength) {
        sb.append(java.lang.Character.toString(cp));
        ++cw.count;
      }
    });
    return new String(sb.toString());
  }

  public Trampoline.Bounce slice(int startIdx, Callable continuePredicate, Trampoline.Continuation continuation) throws Exception {
    if(startIdx < 0 || startIdx >= codePointLength) return continuation.run(new String());
    return sliceGetLastIdx(startIdx,codePointLength,continuePredicate,(endIdx) -> () -> {
      return continuation.run((Datum)slice(startIdx,((Real)endIdx).intValue()-startIdx));
    });
  }

  //////////////////////////////////////
  // reversing
  //////////////////////////////////////

  public OrderedCollection reverse() throws Exception {
    escm.type.Character[] chars = toChars();
    StringBuilder sb = new StringBuilder();
    for(int i = chars.length-1; i >= 0; --i) {
      sb.append(chars[i].display());
    }
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // removing items
  //////////////////////////////////////

  private Trampoline.Bounce removeIter(Callable predicate, escm.type.Character[] chars, int i, boolean increasing, int mod, Trampoline.Continuation continuation) throws Exception {
    if((increasing && i >= chars.length) || (!increasing && i < 0)) return continuation.run(this);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(chars[i]);
    return predicate.callWith(args,(shouldRemove) -> () -> {
      if(shouldRemove.isTruthy()) {
        StringBuilder sb = new StringBuilder();
        for(int idx = 0; idx < chars.length; ++idx) {
          if(idx != i) sb.append(chars[idx].display());
        }
        return continuation.run(new String(sb.toString()));
      }
      return removeIter(predicate,chars,i+mod,increasing,mod,continuation);
    });
  }

  public Trampoline.Bounce removeFirst(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return removeIter(predicate,toChars(),0,true,1,continuation);
  }

  public Trampoline.Bounce removeLast(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    escm.type.Character[] chars = toChars();
    return removeIter(predicate,chars,chars.length-1,false,-1,continuation);
  }

  //////////////////////////////////////
  // skipping
  //////////////////////////////////////

  private Trampoline.Bounce skipIter(Callable predicate, escm.type.Character[] chars, int i, boolean increasing, int mod, Trampoline.Continuation continuation) throws Exception {
    if((increasing && i >= chars.length) || (!increasing && i < 0)) return continuation.run(Boolean.FALSE);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(chars[i]);
    return predicate.callWith(args,(keepSkipping) -> () -> {
      if(keepSkipping.isTruthy()) return skipIter(predicate,chars,i+mod,increasing,mod,continuation);
      return continuation.run(chars[i]);
    });
  }

  public Trampoline.Bounce skip(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return skipIter(predicate,toChars(),0,true,1,continuation);
  }

  public Trampoline.Bounce skipRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    escm.type.Character[] chars = toChars();
    return skipIter(predicate,chars,chars.length-1,false,-1,continuation);
  }

  //////////////////////////////////////
  // fold-right
  //////////////////////////////////////

  private static Trampoline.Bounce FoldRightArray(Callable c, Datum seed, int eltIdx, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Datum> params = new ArrayList<Datum>(acs.length);
    for(int i = 0; i < acs.length; ++i) {
      String s = (String)acs[i];
      if(eltIdx >= s.codePointLength) return continuation.run(seed);  
      params.add(s.charAt(eltIdx));
    }
    return () -> FoldRightArray(c,seed,eltIdx+1,acs,(acc) -> () -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(acc);
      return c.callWith(args,continuation);
    });
  }


  public Trampoline.Bounce FoldRightArray(Callable c, Datum seed, AssociativeCollection[] acs, Trampoline.Continuation continuation) throws Exception {
    return FoldRightArray(c,seed,0,acs,continuation);
  }

  //////////////////////////////////////
  // key-right
  //////////////////////////////////////

  private Trampoline.Bounce keyRight(Callable predicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0 || idx >= n) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("STRING [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("STRING [KEY-RIGHT]: can't find a right key from %s with predicate %s!", write(), predicate);
    }
    ArrayList<Datum> args = new ArrayList<Datum>();
    args.add(charAt(idx));
    return predicate.callWith(args,(shouldGetKey) -> () -> {
      if(!shouldGetKey.isTruthy()) return keyRight(predicate,idx-1,n,continuation);
      return continuation.run(new Exact(idx));
    });
  }

  public Trampoline.Bounce keyRight(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return keyRight(predicate,codePointLength-1,codePointLength,(key) -> () -> {
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
    return slice(0,codePointLength-length);
  }

  private Trampoline.Bounce dropWhile(Callable predicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx >= n) return continuation.run(new String());
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(idx));
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(!keepDropping.isTruthy()) return continuation.run((Datum)slice(idx,n));
      return dropWhile(predicate,idx+1,n,continuation);
    });
  }

  public Trampoline.Bounce dropWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropWhile(predicate,0,codePointLength,continuation);
  }

  private Trampoline.Bounce dropRightWhile(Callable predicate, int idx, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0) return continuation.run(new String());
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(idx));
    return predicate.callWith(args,(keepDropping) -> () -> {
      if(!keepDropping.isTruthy()) return continuation.run((Datum)slice(0,idx+1));
      return dropRightWhile(predicate,idx-1,continuation);
    });
  }

  public Trampoline.Bounce dropRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return dropRightWhile(predicate,codePointLength-1,continuation);
  }

  //////////////////////////////////////
  // taking
  //////////////////////////////////////

  public OrderedCollection takeRight(int length) throws Exception {
    if(length >= codePointLength) return this;
    return slice(codePointLength-length,codePointLength);
  }

  private Trampoline.Bounce takeWhile(Callable predicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx >= n) return continuation.run(this);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(idx));
    return predicate.callWith(args,(keepTaking) -> () -> {
      if(!keepTaking.isTruthy()) return continuation.run((Datum)slice(0,idx));
      return takeWhile(predicate,idx+1,n,continuation);
    });
  }

  public Trampoline.Bounce takeWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return takeWhile(predicate,0,codePointLength,continuation);
  }

  private Trampoline.Bounce takeRightWhile(Callable predicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx < 0) return continuation.run(this);
    ArrayList<Datum> args = new ArrayList<Datum>(1);
    args.add(charAt(idx));
    return predicate.callWith(args,(keepTaking) -> () -> {
      if(!keepTaking.isTruthy()) return continuation.run((Datum)slice(idx+1,n));
      return takeRightWhile(predicate,idx-1,n,continuation);
    });
  }

  public Trampoline.Bounce takeRightWhile(Callable predicate, Trampoline.Continuation continuation) throws Exception {
    return takeRightWhile(predicate,codePointLength-1,codePointLength,continuation);
  }

  //////////////////////////////////////
  // sorting
  //////////////////////////////////////

  private Datum mergeSortedHalves(String lhs, escm.type.Character hd, String rhs) throws Exception {
    return new String(lhs.value + hd.display() + rhs.value);
  }

  private Trampoline.Bounce sort(Callable binaryPredicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx >= n) return continuation.run(new String());
    escm.type.Character hd = charAt(idx);
    Callable trueCondPrimitive = (params, cont) -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(hd);
      return binaryPredicate.callWith(args,cont);
    };
    Callable falseCondPrimitive = (params, cont) -> {
      ArrayList<Datum> args = new ArrayList<Datum>(params);
      args.add(hd);
      return binaryPredicate.callWith(args,(value) -> () -> cont.run(Boolean.valueOf(!value.isTruthy())));
    };
    PrimitiveProcedure trueCond = new PrimitiveProcedure("escm-sort-in-lhs?", trueCondPrimitive);
    PrimitiveProcedure falseCond = new PrimitiveProcedure("escm-sort-in-rhs?", falseCondPrimitive);
    return filterFrom(trueCond,idx+1,(lhs) -> () -> {
      return ((OrderedCollection)lhs).sort(binaryPredicate,(sortedLhs) -> () -> {
        return filterFrom(falseCond,idx+1,(rhs) -> () -> {
          return ((OrderedCollection)rhs).sort(binaryPredicate,(sortedRhs) -> () -> {
            return continuation.run(mergeSortedHalves((String)sortedLhs,hd,(String)sortedRhs));
          });
        });
      });
    });
  }

  public Trampoline.Bounce sort(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sort(binaryPredicate,0,codePointLength,continuation);
  }

  private Trampoline.Bounce sorted(Callable binaryPredicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx+1 >= n) return continuation.run(Boolean.TRUE);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(charAt(idx));
    args.add(charAt(idx+1));
    return binaryPredicate.callWith(args,(isEq) -> () -> {
      if(isEq.isTruthy()) return sorted(binaryPredicate,idx+1,n,continuation);
      return continuation.run(Boolean.FALSE);
    });
  }

  public Trampoline.Bounce sorted(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return sorted(binaryPredicate,0,codePointLength,continuation);
  }

  //////////////////////////////////////
  // merging
  //////////////////////////////////////

  private Datum toListFromIndex(int idx) throws Exception {
    escm.type.Character[] chars = toChars();
    Datum lis = escm.type.Nil.VALUE;
    for(int i = chars.length-1; i >= idx; --i)
      lis = new escm.type.Pair(chars[i],lis);
    return lis;
  }

  private static Trampoline.Bounce mergeIter(Callable binaryPredicate, String s1, String s2, int i1, int i2, int n1, int n2, Trampoline.Continuation continuation) throws Exception {
    if(i1 >= n1) return continuation.run(s2.toListFromIndex(i2));
    if(i2 >= n2) return continuation.run(s1.toListFromIndex(i1));
    Datum s1Elt = s1.charAt(i1);
    Datum s2Elt = s2.charAt(i2);
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(s1Elt);
    args.add(s2Elt);
    return binaryPredicate.callWith(args,(lt) -> () -> {
      if(lt.isTruthy()) {
        return mergeIter(binaryPredicate,s1,s2,i1+1,i2,n1,n2,(merged) -> () -> continuation.run(new Pair(s1Elt,merged)));
      }
      return mergeIter(binaryPredicate,s1,s2,i1,i2+1,n1,n2,(merged) -> () -> continuation.run(new Pair(s2Elt,merged)));
    });
  }

  public Trampoline.Bounce merge(Callable binaryPredicate, OrderedCollection oc, Trampoline.Continuation continuation) throws Exception {
    return mergeIter(binaryPredicate,this,(String)oc,0,0,codePointLength,((String)oc).codePointLength,(mergedList) -> () -> continuation.run(((AssociativeCollection)mergedList).toACString()));
  }

  //////////////////////////////////////
  // duplicate neighbor deletion
  //////////////////////////////////////

  private Trampoline.Bounce skipWhileHaveDuplicates(Callable binaryPredicate, Datum d, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx >= n) return continuation.run(new Exact(idx));
    ArrayList<Datum> args = new ArrayList<Datum>(2);
    args.add(d);
    args.add(charAt(idx));
    return binaryPredicate.callWith(args,(isEq) -> () -> {
      if(isEq.isTruthy()) {
        return skipWhileHaveDuplicates(binaryPredicate,d,idx+1,n,continuation);
      }
      return continuation.run(new Exact(idx));
    });
  }

  private Trampoline.Bounce deleteListNeighborDuplicates(Callable binaryPredicate, int idx, int n, Trampoline.Continuation continuation) throws Exception {
    if(idx+1 >= n) return continuation.run(toListFromIndex(idx));
    Datum elt = charAt(idx);
    return skipWhileHaveDuplicates(binaryPredicate,elt,idx+1,n,(idxAfterDups) -> () -> {
      return deleteListNeighborDuplicates(binaryPredicate,((Real)idxAfterDups).intValue(),n,(deldList) -> () -> continuation.run(new Pair(elt,deldList)));
    });
  }

  public Trampoline.Bounce deleteNeighborDuplicates(Callable binaryPredicate, Trampoline.Continuation continuation) throws Exception {
    return deleteListNeighborDuplicates(binaryPredicate,0,codePointLength,(deldList) -> () -> continuation.run(((AssociativeCollection)deldList).toACString()));
  }
}