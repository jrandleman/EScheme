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
  // unfold
  public static class Unfold implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "unfold";
    }

    public static Trampoline.Bounce logic(Datum acc, Callable breakCond, Callable mapper, Callable successor, Datum seed, Trampoline.Continuation continuation) throws Exception {
      ArrayList<Datum> breakArgs = new ArrayList<Datum>(1);
      breakArgs.add(seed);
      return breakCond.callWith(breakArgs,(shouldBreak) -> () -> {
        if(shouldBreak.isTruthy()) return continuation.run(acc);
        ArrayList<Datum> mapArgs = new ArrayList<Datum>(1);
        mapArgs.add(seed);
        return mapper.callWith(mapArgs,(mapValue) -> () -> {
          ArrayList<Datum> sucArgs = new ArrayList<Datum>(1);
          sucArgs.add(seed);
          return successor.callWith(sucArgs,(sucValue) -> () -> {
            return logic(acc,breakCond,mapper,successor,sucValue,(unfolded) -> () -> {
              return continuation.run(new Pair(mapValue,unfolded));
            });
          });
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(unfold <break-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(unfold <break-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(unfold <break-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(unfold <break-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // unfold-right
  public static class UnfoldRight implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "unfold-right";
    }

    public static Trampoline.Bounce logic(Datum acc, Callable breakCond, Callable mapper, Callable successor, Datum seed, Trampoline.Continuation continuation) throws Exception {
      ArrayList<Datum> breakArgs = new ArrayList<Datum>(1);
      breakArgs.add(seed);
      return breakCond.callWith(breakArgs,(shouldBreak) -> () -> {
        if(shouldBreak.isTruthy()) return continuation.run(acc);
        ArrayList<Datum> mapArgs = new ArrayList<Datum>(1);
        mapArgs.add(seed);
        return mapper.callWith(mapArgs,(mapValue) -> () -> {
          ArrayList<Datum> sucArgs = new ArrayList<Datum>(1);
          sucArgs.add(seed);
          return successor.callWith(sucArgs,(sucValue) -> () -> {
            return logic(new Pair(mapValue,acc),breakCond,mapper,successor,sucValue,continuation);
          });
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(unfold-right <break-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(unfold-right <break-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return logic(Nil.VALUE,(Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),parameters.get(3),continuation);
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
}