// Author: Jordan Randleman - escm.primitive.ListPrimitives
// Purpose:
//    Java primitives for list procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Void;
import escm.type.Symbol;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.type.bool.Boolean;
import escm.type.procedure.PrimitiveProcedure;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.runtime.GlobalState;

public class ListPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Size Integer Validation Helper
  public static boolean isValidSize(Datum d) throws Exception {
    return (d instanceof Real) && ((Real)d).isInteger() && !((Real)d).isNegative();
  }


  ////////////////////////////////////////////////////////////////////////////
  // list
  public static class List implements Primitive {
    public java.lang.String escmName() {
      return "list";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum d = Nil.VALUE;
      for(int i = parameters.size()-1; i >= 0; --i)
        d = new Pair(parameters.get(i),d);
      return d;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list*
  public static class ListStar implements Primitive {
    public java.lang.String escmName() {
      return "list*";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(list* <obj> <obj> ...) received less than the 2 required args: %s", Exceptionf.profileArgs(parameters));
      Datum d = parameters.get(parameters.size()-1);
      for(int i = parameters.size()-2; i >= 0; --i)
        d = new Pair(parameters.get(i),d);
      return d;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // append
  public static class Append implements Primitive {
    public java.lang.String escmName() {
      return "append";
    }

    public static Datum binaryAppend(Datum lis1, Datum lis2) throws Exception {
      if(lis1 instanceof Pair) {
        Pair lis1Pair = (Pair)lis1;
        return new Pair(lis1Pair.car(),binaryAppend(lis1Pair.cdr(),lis2));
      }
      return lis2; // immutable pairs means we don't need a shallow-copy of <lis2>!
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalParameters = parameters.size();
      if(totalParameters == 0) return Nil.VALUE;
      if(totalParameters == 1) return parameters.get(0);
      Datum lhs = parameters.get(0);
      for(int i = 1; i < totalParameters; ++i) {
        if(!Pair.isList(lhs))
          throw new Exceptionf("'(append <list> ...) can't append data to non-list %s", lhs.profile());
        lhs = binaryAppend(lhs,parameters.get(i));
      }
      return lhs;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // length
  public static class Length implements Primitive {
    public java.lang.String escmName() {
      return "length";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !Pair.isList(parameters.get(0))) 
        throw new Exceptionf("'(length <list>) didn't receive exactly 1 list: %s", Exceptionf.profileArgs(parameters));
      Datum lis = parameters.get(0);
      if(lis instanceof Nil) return new Exact(); // 0
      return new Exact(((Pair)lis).length());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // reverse
  public static class Reverse implements Primitive {
    public java.lang.String escmName() {
      return "reverse";
    }

    public static Datum logic(Datum lis) {
      Datum reversed = Nil.VALUE;
      while(lis instanceof Pair) {
        Pair pair = (Pair)lis;
        reversed = new Pair(pair.car(),reversed);
        lis = pair.cdr();
      }
      return reversed;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !Pair.isList(parameters.get(0)))
        throw new Exceptionf("'(reverse <list>) didn't receive exactly 1 list: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // last
  public static class Last implements Primitive {
    public java.lang.String escmName() {
      return "last";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !Pair.isList(parameters.get(0)) || !(parameters.get(0) instanceof Pair))
        throw new Exceptionf("'(last <list>) didn't receive exactly 1 non-empty list: %s", Exceptionf.profileArgs(parameters));
      Datum iterator = parameters.get(0);
      while(true) {
        Pair iteratorPair = (Pair)iterator;
        if(iteratorPair.cdr() instanceof Nil) return iteratorPair.car();
        iterator = iteratorPair.cdr();
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // init
  public static class Init implements Primitive {
    public java.lang.String escmName() {
      return "init";
    }

    private Datum initRecur(Datum lis) throws Exception {
      Pair lisPair = (Pair)lis;
      if(lisPair.cdr() instanceof Nil) return Nil.VALUE;
      return new Pair(lisPair.car(),initRecur(lisPair.cdr()));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !Pair.isList(parameters.get(0)) || !(parameters.get(0) instanceof Pair))
        throw new Exceptionf("'(init <list>) didn't receive exactly 1 non-empty list: %s", Exceptionf.profileArgs(parameters));
      return initRecur(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // ref
  public static class Ref implements Primitive {
    public java.lang.String escmName() {
      return "ref";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(ref <list> <index>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum lis = parameters.get(0);
      Datum index = parameters.get(1);
      if(!(lis instanceof Pair)) 
        throw new Exceptionf("'(ref <list> <index>) 1st arg %s isn't a non-empty list!", lis.profile());
      if(!isValidSize(index)) 
        throw new Exceptionf("'(ref <list> <index>) 2nd arg %s isn't a non-negative integer!", index.profile());
      int indexValue = ((Real)index).intValue();
      for(int count = 0; lis instanceof Pair; ++count, lis = ((Pair)lis).cdr())
        if(count == indexValue)
          return ((Pair)lis).car();
      throw new Exceptionf("'(ref <list> <index>) index %d is out of bounds for list %s", indexValue, parameters.get(0).write());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sublist
  public static class Sublist implements Primitive {
    public java.lang.String escmName() {
      return "sublist";
    }

    private static double getSublistLength(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 2) return Double.POSITIVE_INFINITY; // defaults till the end of the list
      Datum endIndex = parameters.get(2);
      if(!isValidSize(endIndex)) 
        throw new Exceptionf("'(sublist <list> <start-index> <optional-length>) 3rd arg %s isn't a non-negative integer!", endIndex.profile());
      return ((Real)endIndex).doubleValue();
    }

    private static Datum sublistRecur(Datum lis, double count, double startIndex, double length) throws Exception {
      if(lis instanceof Nil || count >= length) return Nil.VALUE;
      Pair lisPair = (Pair)lis;
      if(count < startIndex) return sublistRecur(lisPair.cdr(),count+1,startIndex,length);
      return new Pair(lisPair.car(),sublistRecur(lisPair.cdr(),count+1,startIndex,length));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 && parameters.size() != 3)
        throw new Exceptionf("'(sublist <list> <start-index> <optional-length>) didn't receive 2 or 3 args: %s", Exceptionf.profileArgs(parameters));
      double length = getSublistLength(parameters);
      Datum lis = parameters.get(0);
      Datum startIndex = parameters.get(1);
      if(!Pair.isList(lis)) 
        throw new Exceptionf("'(sublist <list> <start-index> <optional-length>) 1st arg %s isn't a list!", lis.profile());
      if(!isValidSize(startIndex)) 
        throw new Exceptionf("'(sublist <list> <start-index> <optional-length>) 2nd arg %s isn't a non-negative integer!", startIndex.profile());
      double startIndexValue = ((Real)startIndex).doubleValue();
      return sublistRecur(lis,0,startIndexValue,length+startIndexValue);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // memq
  public static class Memq implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "memq";
    }

    private static Symbol EQP_SYMBOL = new Symbol("eq?");

    public static Callable getGlobalEqpProcedure() throws Exception {
      Datum eqp = GlobalState.globalEnvironment.get(EQP_SYMBOL);
      if(eqp instanceof Callable) return (Callable)eqp;
      throw new Exceptionf("<eq?> global variable %s isn't a callable!", eqp.profile());
    }

    public static Trampoline.Bounce logic(Callable eqp, Datum obj, Datum iterator, Trampoline.Continuation continuation) throws Exception {
      if(!(iterator instanceof Pair)) return continuation.run(Boolean.FALSE);
      Pair iteratorPair = (Pair)iterator;
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(iteratorPair.car());
      args.add(obj);
      return eqp.callWith(args,(isEqp) -> () -> {
        if(isEqp.isTruthy()) return continuation.run(iteratorPair);
        return logic(eqp,obj,iteratorPair.cdr(),continuation);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(memq <obj> <list>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(memq <obj> <list>) 2nd arg %s isn't a list!", parameters.get(1).profile());
      return logic(getGlobalEqpProcedure(),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // member
  public static class Member implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "member";
    }

    private static Symbol EQUALP_SYMBOL = new Symbol("equal?");

    public static Callable getGlobalEqualpProcedure() throws Exception {
      Datum equalp = GlobalState.globalEnvironment.get(EQUALP_SYMBOL);
      if(equalp instanceof Callable) return (Callable)equalp;
      throw new Exceptionf("<equal?> global variable %s isn't a callable!", equalp.profile());
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(member <obj> <list>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(member <obj> <list>) 2nd arg %s isn't a list!", parameters.get(1).profile());
      return Memq.logic(getGlobalEqualpProcedure(),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // assq
  public static class Assq implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "assq";
    }

    public static Trampoline.Bounce logic(Callable eqp, String fcnName, Datum originalList, Datum key, Datum iterator, Trampoline.Continuation continuation) throws Exception {
      if(!(iterator instanceof Pair)) return continuation.run(Boolean.FALSE);
      Pair iteratorPair = (Pair)iterator;
      if(!(iteratorPair.car() instanceof Pair))
        throw new Exceptionf("'(%s <key> <alist>) 2nd arg %s isn't an alist (list of key-value pair lists)!", fcnName, originalList.profile());
      Pair innerList = (Pair)iteratorPair.car();
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(innerList.car());
      args.add(key);
      return eqp.callWith(args,(isEqp) -> () -> {
        if(isEqp.isTruthy()) return continuation.run(innerList);
        return logic(eqp,fcnName,originalList,key,iteratorPair.cdr(),continuation);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(assq <key> <alist>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(assq <key> <alist>) 2nd arg %s isn't an alist (list of key-value pair lists)!", parameters.get(1).profile());
      return logic(Memq.getGlobalEqpProcedure(),"assq",parameters.get(1),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // assoc
  public static class Assoc implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "assoc";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(assoc <key> <alist>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(assoc <key> <alist>) 2nd arg %s isn't an alist (list of key-value pair lists)!", parameters.get(1).profile());
      return Assq.logic(Member.getGlobalEqualpProcedure(),"assoc",parameters.get(1),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list?
  public static class IsList implements Primitive {
    public java.lang.String escmName() {
      return "list?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(list? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Pair.isList(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list*?
  public static class IsListStar implements Primitive {
    public java.lang.String escmName() {
      return "list*?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(list*? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Pair.isDottedList(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // alist?
  public static class IsAlist implements Primitive {
    public java.lang.String escmName() {
      return "alist?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(alist? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum iterator = parameters.get(0);
      if(!Pair.isList(iterator)) return Boolean.FALSE;
      while(iterator instanceof Pair) {
        Pair iteratorPair = (Pair)iterator;
        if(!(iteratorPair.car() instanceof Pair))
          return Boolean.FALSE;
        iterator = iteratorPair.cdr();
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // null?
  public static class IsNull implements Primitive {
    public java.lang.String escmName() {
      return "null?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(null? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Nil);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // filter
  public static class Filter implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "filter";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum l, Trampoline.Continuation continuation) throws Exception {
      if(!(l instanceof Pair)) return continuation.run(l);
      Pair pair = (Pair)l;
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(pair.car());
      return p.callWith(args,(predicateResult) -> () -> {
        if(predicateResult.isTruthy()) {
          return logic(p,pair.cdr(),(filteredList) -> () -> continuation.run(new Pair(pair.car(),filteredList)));
        } else {
          return logic(p,pair.cdr(),continuation);
        }
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(filter <predicate> <list>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(filter <predicate> <list>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(!(target instanceof Pair) && !(target instanceof Nil))
        throw new Exceptionf("'(filter <predicate> <list>) 2nd arg %s isn't a list: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)procedure,target,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // map
  public static class Map implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "map";
    }
    
    public static ArrayList<Datum> getCars(ArrayList<Datum> ls, String callerName) throws Exception {
      ArrayList<Datum> cars = new ArrayList<Datum>(ls.size());
      for(Datum l : ls) {
        if(!(l instanceof Pair))
          throw new Exceptionf("'%s can't get the <car> of non-pair %s", callerName, l.profile());
        cars.add(((Pair)l).car());
      }
      return cars;
    }

    public static ArrayList<Datum> getCdrs(ArrayList<Datum> ls, String callerName) throws Exception {
      ArrayList<Datum> cdrs = new ArrayList<Datum>(ls.size());
      for(Datum l : ls) {
        if(!(l instanceof Pair))
          throw new Exceptionf("'%s can't get the <cdr> of non-pair %s", callerName, l.profile());
        cdrs.add(((Pair)l).cdr());
      }
      return cdrs;
    }

    public static Trampoline.Bounce logic(Callable p, ArrayList<Datum> ls, Trampoline.Continuation continuation) throws Exception {
      if(!(ls.get(0) instanceof Pair)) return continuation.run(Nil.VALUE);
      return p.callWith(getCars(ls,"map"),
                        (mappedValue) -> () -> 
                          logic(p,
                                getCdrs(ls,"map"),
                                (mappedList) -> () -> 
                                  continuation.run(new Pair(mappedValue,mappedList))));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2)
        throw new Exceptionf("'(map <callable> <list> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(map <callable> <list> ...) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      ArrayList<Datum> lists = new ArrayList<Datum>(parameters);
      lists.remove(0);
      return logic((Callable)procedure,lists,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // for-each
  public static class ForEach implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "for-each";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum l, Trampoline.Continuation continuation) throws Exception {
      if(!(l instanceof Pair)) return continuation.run(Void.VALUE);
      Pair pair = (Pair)l;
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(pair.car());
      return p.callWith(args,(ignored) -> () -> logic(p,pair.cdr(),continuation));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(for-each <callable> <list>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(for-each <callable> <list>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(!(target instanceof Pair) && !(target instanceof Nil))
        throw new Exceptionf("'(for-each <callable> <list>) 2nd arg %s isn't a list: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)procedure,target,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // fold
  public static class Fold implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "fold";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum seed, ArrayList<Datum> ls, Trampoline.Continuation continuation) throws Exception {
      if(!(ls.get(0) instanceof Pair)) return continuation.run(seed);
      ArrayList<Datum> args = new ArrayList<Datum>(ls.size()+1);
      args.add(seed);
      args.addAll(Map.getCars(ls,"fold"));
      return p.callWith(args,
                        (foldedValue) -> () -> 
                          logic(p,foldedValue,Map.getCdrs(ls,"fold"),continuation));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 3)
        throw new Exceptionf("'(fold <callable> <seed> <list> ...) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(fold <callable> <seed> <list> ...) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum seed = parameters.get(1);
      ArrayList<Datum> lists = new ArrayList<Datum>(parameters);
      lists.remove(0);
      lists.remove(0);
      return logic((Callable)procedure,seed,lists,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // fold-right
  public static class FoldRight implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "fold-right";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum seed, ArrayList<Datum> ls, Trampoline.Continuation continuation) throws Exception {
      if(!(ls.get(0) instanceof Pair)) return continuation.run(seed);
      ArrayList<Datum> args = new ArrayList<Datum>(Map.getCars(ls,"fold-right"));
      return () -> logic(p,seed,Map.getCdrs(ls,"fold-right"),(accumulatedValue) -> () -> {
        args.add(accumulatedValue);
        return p.callWith(args,continuation);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 3)
        throw new Exceptionf("'(fold-right <callable> <seed> <list> ...) expects at least 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(fold-right <callable> <seed> <list> ...) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum seed = parameters.get(1);
      ArrayList<Datum> lists = new ArrayList<Datum>(parameters);
      lists.remove(0);
      lists.remove(0);
      return logic((Callable)procedure,seed,lists,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sort
  public static class Sort implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "sort";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum l, Trampoline.Continuation continuation) throws Exception {
      if(!(l instanceof Pair)) return continuation.run(Nil.VALUE);
      Pair lPair = (Pair)l;
      Callable trueCondPrimitive = (params, cont) -> {
        params.add(lPair.car());
        return p.callWith(params,cont);
      };
      Callable falseCondPrimitive = (params, cont) -> {
        params.add(lPair.car());
        return p.callWith(params,(value) -> () -> cont.run(Boolean.valueOf(!value.isTruthy())));
      };
      PrimitiveProcedure trueCond = new PrimitiveProcedure("escm-sort-in-lhs?", trueCondPrimitive);
      PrimitiveProcedure falseCond = new PrimitiveProcedure("escm-sort-in-rhs?", falseCondPrimitive);
      return Filter.logic(trueCond, lPair.cdr(), (filteredLhs) -> () -> {
        return logic(p, filteredLhs, (sortedLhs) -> () -> {
          return Filter.logic(falseCond, lPair.cdr(), (filteredRhs) -> () -> {
            return logic(p, filteredRhs, (sortedRhs) -> () -> {
              return continuation.run(Append.binaryAppend(sortedLhs, new Pair(lPair.car(),sortedRhs)));
            });
          });
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(sort <predicate?> <list>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(sort <predicate?> <list>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(!(target instanceof Pair) && !(target instanceof Nil))
        throw new Exceptionf("'(sort <predicate?> <list>) 2nd arg %s isn't a list: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)procedure,target,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // sorted?
  public static class IsSorted implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "sorted?";
    }
    
    public static Trampoline.Bounce logic(Callable p, Datum l, Trampoline.Continuation continuation) throws Exception {
      if(!(l instanceof Pair)) return continuation.run(Boolean.TRUE);
      Pair lPair = (Pair)l;
      if(!(lPair.cdr() instanceof Pair)) return continuation.run(Boolean.TRUE);
      ArrayList<Datum> args = new ArrayList<Datum>(2);
      args.add(lPair.car());
      args.add(((Pair)lPair.cdr()).car());
      return p.callWith(args, (predicateResult) -> () -> {
        if(predicateResult.isTruthy()) {
          return logic(p,lPair.cdr(),continuation);
        } else {
          return continuation.run(Boolean.FALSE);
        }
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(sorted? <predicate?> <list>) expects exactly 2 args (callable & list): %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      if(!(procedure instanceof Callable))
        throw new Exceptionf("'(sorted? <predicate?> <list>) 1st arg %s isn't a callable: %s", procedure.profile(), Exceptionf.profileArgs(parameters));
      Datum target = parameters.get(1);
      if(target instanceof Nil) return continuation.run(Boolean.TRUE);
      if(!(target instanceof Pair))
        throw new Exceptionf("'(sorted? <predicate?> <list>) 2nd arg %s isn't a list: %s", target.profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)procedure,target,continuation);
    }
  }
}