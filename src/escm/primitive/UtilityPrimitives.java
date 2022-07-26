// Author: Jordan Randleman - escm.primitive.UtilityPrimitives
// Purpose:
//    Java primitives for utility procedures. 
//
//    => NOTE: The serialization procedures are implemented 
//             seperately in <SerializationPrimitives.java>

package escm.primitive;
import java.util.ArrayList;
import java.util.Calendar;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Void;
import escm.type.bool.Boolean;
import escm.type.Symbol;
import escm.type.number.Exact;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.primitive.UtilityPrimitives_util.ContinuationProcedure;
import escm.primitive.FunctionalPrimitives;
import escm.primitive.MetaPrimitives;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.runtime.EscmThread;

public class UtilityPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // typeof
  public static class Typeof implements Primitive {
    public java.lang.String escmName() {
      return "typeof";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(typeof <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return new Symbol(parameters.get(0).type());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // error
  public static class Error implements Primitive {
    public java.lang.String escmName() {
      return "error";
    }
    
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) 
        throw new Exceptionf("'(error <reason> <arg> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      StringBuffer sb = new StringBuffer();
      sb.append(parameters.get(0).display());
      for(int i = 1, n = parameters.size(); i < n; ++i)
        sb.append(" " + parameters.get(i).write());
      throw new Exception("'error: " + sb.toString());
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // errorf
  public static class Errorf implements Primitive {
    public java.lang.String escmName() {
      return "errorf";
    }
    
    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) 
        throw new Exceptionf("'(errorf <format-string> <args> ...) expects at least 1 string: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof escm.type.String))
        throw new Exceptionf("'(errorf <format-string> <args> ...) 1st arg isn't a string: %s", Exceptionf.profileArgs(parameters));
      String formatString = ((escm.type.String)parameters.get(0)).value();
      ArrayList<Datum> args = IOPrimitives.PrettyPrintf.getStringfArgs(parameters,1);
      String formatted = FormatPrimitives.Stringf.logic(formatString,args,"(errorf <format-string> <args> ...)");
      throw new Exception("'error: " + formatted);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // copy
  public static class Copy implements Primitive {
    public java.lang.String escmName() {
      return "copy";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(copy <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return parameters.get(0).copy();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // force
  public static class Force implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "force";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Callable)) 
        throw new Exceptionf("'(force <delayed>) didn't receive exactly 1 callable arg: %s", Exceptionf.profileArgs(parameters));
      return ((Callable)parameters.get(0)).callWith(new ArrayList<Datum>(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-current-continuation
  public static class CallWithCurrentContinuation implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-current-continuation";
    }
    
    public static Datum convertCallableToProcedure(Callable c, String name) {
      return new escm.type.procedure.PrimitiveProcedure(name,c);
    }

    public static Trampoline.Bounce r4rs(Callable procedure, Trampoline.Continuation continuation) throws Exception {
      ArrayList<Datum> args = new ArrayList<Datum>(1);
      args.add(new ContinuationProcedure(continuation));
      return procedure.callWith(args,continuation);
    }

    // Accounts for <dynamic-wind> and <call-with-values>
    public static Trampoline.Bounce r5rs(Callable procedure, Trampoline.Continuation continuation) throws Exception {
      EscmThread ct = (EscmThread)Thread.currentThread();
      Datum winds = ct.dynamicWinds;
      Callable lambda1 = (params1, cont1) -> {
        Datum cont = params1.get(0);
        Callable lambda2 = (params2, cont2) -> {
          return DynamicWind.doWinds(ct.dynamicWinds,winds,(ignored) -> () -> {
            ArrayList<Datum> args = new ArrayList<Datum>(1);
            args.add(Values.logic(params2));
            return ((Callable)cont).callWith(args,cont2);
          });
        };
        ArrayList<Datum> args = new ArrayList<Datum>(1);
        args.add(convertCallableToProcedure(lambda2,"callcc-withDynamicWind-lambda2"));
        return procedure.callWith(args,cont1);
      };
      return r4rs(lambda1,continuation);
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(call-with-current-continuation <unary-callable>) didn't receive exactly 1 callable arg: %s", Exceptionf.profileArgs(parameters));
      return r5rs((Callable)parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // dynamic-wind
  //   See the below for an implementation of <dynamic-wind> w/ r4rs call/cc:
  //     https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1992/msg00194.html
  public static class DynamicWind implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "dynamic-wind";
    }
    
    // used by CallWithCurrentContinuation.r5rs
    public static Trampoline.Bounce doWinds(Datum from, Datum to, Trampoline.Continuation continuation) throws Exception {
      EscmThread ct = (EscmThread)Thread.currentThread();
      ct.dynamicWinds = from;
      Trampoline.Continuation setWindsContinuation = (ignored) -> () -> {
        ct.dynamicWinds = to;
        return continuation.run(Void.VALUE);
      };
      if(from.eq(to)) return setWindsContinuation.run(Boolean.TRUE);
      if(from instanceof Nil) {
        return () -> doWinds(from,((Pair)to).cdr(),(ignored) -> () -> {
          return ((Callable)(((Pair)((Pair)to).car()).car())).callWith(new ArrayList<Datum>(),setWindsContinuation);
        });
      } else if(to instanceof Nil) {
        return ((Callable)(((Pair)((Pair)from).car()).cdr())).callWith(new ArrayList<Datum>(),(ignored) -> () -> {;
          return doWinds(((Pair)from).cdr(),to,setWindsContinuation);
        });
      } else {
        return ((Callable)(((Pair)((Pair)from).car()).cdr())).callWith(new ArrayList<Datum>(),(ignored1) -> () -> {
          return doWinds(((Pair)from).cdr(),((Pair)to).cdr(),(ignored2) -> () -> {
            return ((Callable)(((Pair)((Pair)to).car()).car())).callWith(new ArrayList<Datum>(),setWindsContinuation);
          });
        });
      }
    }

    public static Trampoline.Bounce logic(Callable thunk1, Callable thunk2, Callable thunk3, Trampoline.Continuation continuation) throws Exception {
      return thunk1.callWith(new ArrayList<Datum>(),(ignoreThis1) -> () -> {
        EscmThread ct = (EscmThread)Thread.currentThread();
        ct.dynamicWinds = new Pair(new Pair(CallWithCurrentContinuation.convertCallableToProcedure(thunk1,"dynamic-wind-thunk1"),
                                            CallWithCurrentContinuation.convertCallableToProcedure(thunk3,"dynamic-wind-thunk3")),
                                   ct.dynamicWinds);
        return thunk2.callWith(new ArrayList<Datum>(),(answer) -> () -> {
          ct.dynamicWinds = ((Pair)ct.dynamicWinds).cdr();
          return thunk3.callWith(new ArrayList<Datum>(),(ignoreThis2) -> () -> continuation.run(answer));
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 3)
        throw new Exceptionf("'(dynamic-wind <thunk1> <thunk2> <thunk3>) didn't receive exactly 3 callable thunks: %s", 
          Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(dynamic-wind <thunk1> <thunk2> <thunk3>) 1st arg %s isn't a callable thunk: %s", 
          parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(dynamic-wind <thunk1> <thunk2> <thunk3>) 2nd arg %s isn't a callable thunk: %s", 
          parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(dynamic-wind <thunk1> <thunk2> <thunk3>) 3rd arg %s isn't a callable thunk: %s", 
          parameters.get(2).profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)parameters.get(0),(Callable)parameters.get(1),(Callable)parameters.get(2),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // values
  //   See the below for an implementation of <values> & <call-with-values>:
  //     https://www.scheme.com/tspl4/control.html#./control:h8
  //       => Scroll right to the end of the section for the implementation!
  public static class Values implements Primitive {
    public java.lang.String escmName() {
      return "values";
    }
    
    public static final Datum MAGIC = new Pair(new Symbol("multiple"),new Symbol("values"));

    public static boolean isMagic(Datum x) {
      return x instanceof Pair && ((Pair)x).car().eq(MAGIC);
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 1) return parameters.get(0);
      return new Pair(MAGIC,FunctionalPrimitives.Compose.convertArrayListToList(parameters));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // call-with-values
  //   See the below for an implementation of <values> & <call-with-values>:
  //     https://www.scheme.com/tspl4/control.html#./control:h8
  //       => Scroll right to the end of the section for the implementation!
  public static class CallWithValues implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "call-with-values";
    }
    
    public static Trampoline.Bounce logic(Callable producer, Callable consumer, Trampoline.Continuation continuation) throws Exception {
      return producer.callWith(new ArrayList<Datum>(),(x) -> () -> {
        if(Values.isMagic(x))
          return consumer.callWith(MetaPrimitives.Apply.convertListToArrayList(((Pair)x).cdr()),continuation);
        ArrayList<Datum> args = new ArrayList<Datum>(1);
        args.add(x);
        return consumer.callWith(args,continuation); 
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(call-with-values <thunk-producer> <callable-consumer>) didn't receive exactly 2 callables: %s", 
          Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(call-with-values <thunk-producer> <callable-consumer>) 1st arg %s isn't a callable thunk: %s", 
          parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(call-with-values <thunk-producer> <callable-consumer>) 2nd arg %s isn't a callable thunk: %s", 
          parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
      return logic((Callable)parameters.get(0),(Callable)parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // with-exception-handler
  //   See the below for an implementation of <with-exception-handler>, <raise>, & <guard>:
  //     https://srfi.schemers.org/srfi-34/srfi-34.html
  public static class WithExceptionHandler implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "with-exception-handler";
    }
    
    public static Trampoline.Bounce WithExceptionHandlers(Datum newHandlers, Callable thunk2, Trampoline.Continuation continuation) throws Exception {
      EscmThread ct = (EscmThread)Thread.currentThread();
      Datum previousHandles = ct.currentExceptionHandlers;
      Callable thunk1 = (params, cont) -> {
        ct.currentExceptionHandlers = newHandlers;
        return cont.run(Void.VALUE);
      };
      Callable thunk3 = (params, cont) -> {
        ct.currentExceptionHandlers = previousHandles;
        return cont.run(Void.VALUE);
      };
      return DynamicWind.logic(thunk1,thunk2,thunk3,continuation);
    }

    public static Trampoline.Bounce logic(Datum handler, Callable thunk2, Trampoline.Continuation continuation) throws Exception {
      return WithExceptionHandlers(new Pair(handler,((EscmThread)Thread.currentThread()).currentExceptionHandlers),thunk2,continuation);
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(with-exception-handler <unary-callable-handler> <callable-thunk>) didn't receive exactly 2 callables: %s", 
          Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(with-exception-handler <unary-callable-handler> <callable-thunk>) 1st arg handler %s isn't a callable: %s", 
          parameters.get(0).profile(), Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(with-exception-handler <unary-callable-handler> <callable-thunk>) 2nd arg thunk %s isn't a callable thunk: %s", 
          parameters.get(1).profile(), Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),(Callable)parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // raise
  //   See the below for an implementation of <with-exception-handler>, <raise>, & <guard>:
  //     https://srfi.schemers.org/srfi-34/srfi-34.html
  public static class Raise implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "raise";
    }
    
    public static Trampoline.Bounce logic(Datum obj, Trampoline.Continuation continuation) throws Exception {
      EscmThread ct = (EscmThread)Thread.currentThread();
      Pair handlers = (Pair)ct.currentExceptionHandlers;
      Callable lambda = (params, cont) -> {
        ArrayList<Datum> args1 = new ArrayList<Datum>(1);
        args1.add(obj);
        return ((Callable)handlers.car()).callWith(args1,(ignore) -> () -> {
          ArrayList<Datum> args2 = new ArrayList<Datum>(3);
          args2.add(new escm.type.String("handler returned:"));
          args2.add(handlers.car());
          args2.add(obj);
          return cont.run(Error.logic(args2));
        });
      };
      return WithExceptionHandler.WithExceptionHandlers(handlers.cdr(),new escm.type.procedure.PrimitiveProcedure("raise-lambda",lambda),continuation);
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(raise <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // time
  public static class Time implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "time";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(time <callable> <arg> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum callable = parameters.get(0);
      if(!(callable instanceof Callable))
        throw new Exceptionf("'(time <callable> <arg> ...) invalid non-callable 1st arg: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Datum> args = new ArrayList<Datum>(parameters);
      args.remove(0);
      long start = Calendar.getInstance().getTimeInMillis();
      return ((Callable)callable).callWith(args,(result) -> () -> {
        long end = Calendar.getInstance().getTimeInMillis();
        return continuation.run(new Pair(new Exact(end-start),result));
      });
    }
  }
}