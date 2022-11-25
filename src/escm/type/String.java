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
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.StringParser;
import escm.vm.util.ExecutionState;
import escm.vm.util.ExecutionState;
import escm.vm.type.AssociativeCollection;
import escm.vm.type.Callable;

public class String extends Datum implements AssociativeCollection, Callable {
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
    if(codePointLength() == 0) throw new Exceptionf("STRING [HEAD]: Can't get head of an empty string!");
    return charAt(0);
  }

  public Datum tail() throws Exception {
    if(codePointLength() == 0) throw new Exceptionf("STRING [TAIL]: Can't get tail of an empty string!");
    StringBuilder sb = new StringBuilder();
    forEachChar((idx,chr) -> { if(idx > 0) sb.append(chr.display()); });
    return new String(sb.toString());
  }

  public int length() {
    return codePointLength();
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
    if(acPos >= codePointLength()) return continuation.run(Nil.VALUE);
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

  public Trampoline.Bounce filter(Callable predicate, Trampoline.Continuation continuation) throws Exception { // -> AssociativeCollection
    return filterIter(predicate,0,(filteredList) -> () -> continuation.run(((AssociativeCollection)filteredList).toACString()));
  }

  //////////////////////////////////////
  // count
  //////////////////////////////////////

  private static final Exact ZERO = new Exact();
  private static final Exact ONE = new Exact(1);

  private Trampoline.Bounce countIter(Callable predicate, Number n, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength()) return continuation.run(n);
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
    if(acPos >= codePointLength()) return continuation.run(Nil.VALUE);
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
      throw new Exceptionf("'val invalid index %s for string value", key.profile());
    Real r = (Real)key;
    if(r.lt(ZERO) || r.gte(new Exact(codePointLength())))
      throw new Exceptionf("'val index %s out of string range [0,%d)", r.write(), codePointLength());
    return charAt(r.intValue());
  }

  //////////////////////////////////////
  // key
  //////////////////////////////////////

  private Trampoline.Bounce keyIter(Callable predicate, int acPos, Trampoline.Continuation continuation) throws Exception {
    if(acPos >= codePointLength()) {
      if(predicate instanceof Datum) {
        throw new Exceptionf("'key no value in %s satisfies value predicate %s", write(), ((Datum)predicate).profile());
      }
      throw new Exceptionf("'key no value in %s satisfies value predicate %s", write(), predicate);
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
      throw new Exceptionf("'delete invalid index %s for string deletion", key.profile());
    int deleteIdx = ((Real)key).intValue();
    if(deleteIdx < 0 || deleteIdx >= codePointLength())
      throw new Exceptionf("'delete index %d out of string range [0,%d)", deleteIdx, codePointLength());
    StringBuilder sb = new StringBuilder();
    forEachChar((idx,chr) -> {
      if(idx != deleteIdx) sb.append(chr.display());
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // conj
  //////////////////////////////////////

  public AssociativeCollection conj(Datum key, Datum newValue) throws Exception {
    if(!(newValue instanceof escm.type.Character))
      throw new Exceptionf("'conj invalid non-char value %s for string", newValue.profile());
    if(!(key instanceof Real) || !((Real)key).isInteger())
      throw new Exceptionf("'conj invalid index %s for string", key.profile());
    int n = codePointLength();
    int idx = ((Real)key).intValue();
    if(idx < -1 || idx > n)
      throw new Exceptionf("'conj index %d extends string bounds [-1,%d)", idx, n);
    if(idx == -1) {
      return new String(newValue.display() + value);
    } else if(idx == n) {
      return new String(value + newValue.display());
    } else {
      StringBuilder sb = new StringBuilder();
      forEachChar((i,chr) -> {
        if(i != idx) {
          sb.append(chr.display());
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
    forEachChar((i,chr) -> {
      if(i >= start) sb.append(chr.display());
    });
    return new String(sb.toString());
  }

  //////////////////////////////////////
  // take
  //////////////////////////////////////

  public AssociativeCollection take(int amount) throws Exception {
    StringBuilder sb = new StringBuilder();
    int end = Math.min(codePointLength(),amount);
    forEachChar((i,chr) -> {
      if(i < end) sb.append(chr.display());
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
    if(j >= s.codePointLength()) return () -> UnionArrayIter(eltPredicate,acs,i+1,(i+1 >= acs.length) ? (String)acs[i] : (String)acs[i+1],0,values,continuation);
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
    if(acIdx >= ac.codePointLength()) return continuation.run(Boolean.FALSE);
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
    if(sIdx >= s.codePointLength()) return () -> IntersectionArrayIter(eltPredicate,acs,acIdx+1,acIdx+1 >= acs.length ? (String)acs[acIdx] : (String)acs[acIdx+1],0,intersectingValues,continuation);
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
    if(i >= valString.codePointLength()) return continuation.run(Boolean.FALSE);
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
    return inValuesString(eltPredicate,p.car(),valString,0,(inVector) -> () -> {
      if(inVector.isTruthy()) {
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
}