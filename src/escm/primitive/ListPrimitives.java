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
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.callable.Signature;
import escm.vm.util.Environment;
import escm.vm.runtime.GlobalState;

public class ListPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Size Integer Validation Helper
  public static boolean isValidSize(Datum d) throws Exception {
    return (d instanceof Real) && ((Real)d).isInteger() && !((Real)d).isNegative();
  }


  ////////////////////////////////////////////////////////////////////////////
  // list
  public static class List extends Primitive {
    public java.lang.String escmName() {
      return "list";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Return a list containing \"<obj> ...\". Given no args, returns '().\nLists are right-nested pairs ending in nil: '()\nCreate list literals via the (<literal> ...) syntax.";
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
  public static class ListStar extends Primitive {
    public java.lang.String escmName() {
      return "list*";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list*"),new Symbol("<obj>"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Return a dotted.list containing \"<obj> <obj> ...\".";
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
  public static class Unfold extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "unfold";
    }

    public Datum signature() {
      return Pair.List(new Symbol("unfold"),new Symbol("<break?-condition>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public String docstring() {
      return "Unfolds a list from left to right, starting with <seed>. <break?-condition>\ndetermines when unfolding stops, <mapper-callable> maps the <seed> to a value\nin the unfolded list, and <update-callable> increments <seed> for the\nnext round of unfolding.";
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
  public static class UnfoldRight extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "unfold-right";
    }

    public Datum signature() {
      return Pair.List(new Symbol("unfold-right"),new Symbol("<break?-condition>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public String docstring() {
      return "Unfolds a list from right to left, starting with <seed>. <break?-condition>\ndetermines when unfolding stops, <mapper-callable> maps the <seed> to a value\nin the unfolded list, and <update-callable> increments <seed> for the\nnext round of unfolding.";
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
  public static class Memq extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "memq";
    }

    public Datum signature() {
      return Pair.List(new Symbol("memq"),new Symbol("<obj>"),new Symbol("<list>"));
    }

    public String docstring() {
      return "Returns the sublist in <list> starting with <obj> based on <eq?> item\nequality. Returns #f if <obj> isn't in <list>.";
    }

    private static Symbol EQP_SYMBOL = new Symbol("eq?");

    public static Callable getEqpProcedure(Environment definitionEnvironment) throws Exception {
      Datum eqp = definitionEnvironment.get(EQP_SYMBOL);
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
      return logic(getEqpProcedure(this.definitionEnvironment),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // member
  public static class Member extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "member";
    }

    public Datum signature() {
      return Pair.List(new Symbol("member"),new Symbol("<obj>"),new Symbol("<list>"));
    }

    public String docstring() {
      return "Returns the sublist in <list> starting with <obj> based on <equal?> item\nequality. Returns #f if <obj> isn't in <list>.";
    }

    private static Symbol EQUALP_SYMBOL = new Symbol("equal?");

    public static Callable getEqualpProcedure(Environment definitionEnvironment) throws Exception {
      Datum equalp = definitionEnvironment.get(EQUALP_SYMBOL);
      if(equalp instanceof Callable) return (Callable)equalp;
      throw new Exceptionf("<equal?> global variable %s isn't a callable!", equalp.profile());
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(member <obj> <list>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(member <obj> <list>) 2nd arg %s isn't a list!", parameters.get(1).profile());
      return Memq.logic(getEqualpProcedure(this.definitionEnvironment),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // assq
  public static class Assq extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "assq";
    }

    public Datum signature() {
      return Pair.List(new Symbol("assq"),new Symbol("<key>"),new Symbol("<associative-list>"));
    }

    public String docstring() {
      return "Returns the pair in associative-lsit <alist> starting with <key> based\non <eq?> item equality. Returns #f if <obj> isn't a key in in <alist>.";
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
      return logic(Memq.getEqpProcedure(this.definitionEnvironment),"assq",parameters.get(1),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // assoc
  public static class Assoc extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "assoc";
    }

    public Datum signature() {
      return Pair.List(new Symbol("assoc"),new Symbol("<key>"),new Symbol("<associative-list>"));
    }

    public String docstring() {
      return "Returns the pair in associative-lsit <alist> starting with <key> based\non <equal?> item equality. Returns #f if <obj> isn't a key in in <alist>.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(assoc <key> <alist>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      if(!Pair.isList(parameters.get(1))) 
        throw new Exceptionf("'(assoc <key> <alist>) 2nd arg %s isn't an alist (list of key-value pair lists)!", parameters.get(1).profile());
      return Assq.logic(Member.getEqualpProcedure(this.definitionEnvironment),"assoc",parameters.get(1),parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list?
  public static class IsList extends Primitive {
    public java.lang.String escmName() {
      return "list?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a proper list.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(list? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Pair.isList(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // list*?
  public static class IsListStar extends Primitive {
    public java.lang.String escmName() {
      return "list*?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("list*?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a dotted-list.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(list*? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(Pair.isDottedList(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // alist?
  public static class IsAlist extends Primitive {
    public java.lang.String escmName() {
      return "alist?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("alist?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is an associative list (list of pairs).";
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
  public static class IsNull extends Primitive {
    public java.lang.String escmName() {
      return "null?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("null?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is nill (a.k.a. '() and #nil).";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(null? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Nil);
    }
  }
}