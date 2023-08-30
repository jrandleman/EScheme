// Author: Jordan Randleman - escm.primitive.syntax.StreamPrimitives
// Purpose:
//    Java primitives for core stream syntax macros/procedures. All of this
//    logic used to be implemented in a <stdlib.scm> file, however, we now 
//    implement such natively in Java to maximize EScheme's performance :)
//      => Note that the original EScheme code is still included in comments
//         throughout this file.

package escm.primitive.syntax;
import java.util.ArrayList;
import escm.util.UniqueSymbol;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.type.number.Exact;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.primitive.PrimitiveSyntax;
import escm.vm.type.callable.Signature;
import escm.vm.util.Environment;
import escm.vm.runtime.GlobalState;
import escm.primitive.UtilityPrimitives;
import escm.primitive.MetaPrimitives;

public class StreamPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Symbols
  public static Symbol DELAY = new Symbol("delay");
  public static Symbol SCONS = new Symbol("scons");
  public static Symbol SCAR = new Symbol("scar");
  public static Symbol SCDR = new Symbol("scdr");
  public static Symbol MAP = new Symbol("map");
  public static Symbol STREAM_MAP = new Symbol("stream-map");
  public static Symbol STREAM_FILTER = new Symbol("stream-filter");
  public static Symbol STREAM_ITERATE = new Symbol("stream-iterate");
  public static Symbol ESCM_STREAM_CONSTANT = new Symbol("escm-stream-constant");
  public static Symbol STREAM_APPEND = new Symbol("stream-append");
  public static Symbol STREAM_INTERLEAVE = new Symbol("stream-interleave");
  public static Symbol IS_NULLP = new Symbol("null?");
  public static Symbol STREAM_TAKE = new Symbol("stream-take");
  public static Symbol STREAM_TAKE_WHILE = new Symbol("stream-take-while");
  public static Symbol STREAM_UNFOLD = new Symbol("stream-unfold");
  public static Symbol STREAM_UNFOLDS = new Symbol("stream-unfolds");


  ////////////////////////////////////////////////////////////////////////////
  // (scons <obj> <obj>)
  //
  // (define-syntax scons 
  //   (lambda (a b)
  //     (list (quote cons) 
  //           (list (quote delay) a) 
  //           (list (quote delay) b))))
  public static class Scons extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "scons";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scons"),new Symbol("<scar-obj>"),new Symbol("<scdr-obj>"));
    }

    public String docstring() {
      return "Create a stream-pair of \"<obj> <obj>\". Streams don't evaluate their\nitems until the items are accessed via <scar>, <scdr>, <scaar>, etc.\nCreate a stream by nesting stream pairs that end with (quote ()) [think lists].";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(scons <obj> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(CorePrimitives.CONS,Pair.List(DELAY,parameters.get(0)),Pair.List(DELAY,parameters.get(1)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-pair? <obj>)
  //
  // (define (stream-pair? o)
  //   (and (pair? o) (procedure? (car o)) (procedure? (cdr o))))
  public static class IsStreamPairP extends Primitive {
    public java.lang.String escmName() {
      return "stream-pair?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-pair?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a stream-pair created by <scons>.";
    }

    public static boolean logic(Datum d) {
      if(!(d instanceof Pair)) return false;
      Pair p = (Pair)d;
      return p.car() instanceof Callable && p.cdr() instanceof Callable;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(stream-pair? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream? <obj>)
  //
  // (define (stream? o)
  //   (or (null? o) (stream-pair? o)))
  public static class IsStreamP extends Primitive {
    public java.lang.String escmName() {
      return "stream?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a stream. Equivalent to:\n  (or (stream-pair? <obj>) (null? <obj>))";
    }

    public static boolean logic(Datum d) {
      return d instanceof Nil || IsStreamPairP.logic(d);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(stream? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scar <stream-pair>)
  //
  // (define (scar spair) (force (car spair)))
  public static class Scar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Access the first item in a stream-pair.";
    }

    public static Trampoline.Bounce logic(Datum streamPair, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamPairP.logic(streamPair)) 
        throw new Exceptionf("'(scar <stream-pair>) expects a <stream-pair>, given %s", streamPair.profile());
      Pair p = (Pair)streamPair;
      return UtilityPrimitives.Force.logic(p.car(),continuation);
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdr <stream-pair>)
  //
  // (define (scdr spair) (force (cdr spair)))
  public static class Scdr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Access the second item in a stream-pair.";
    }

    public static Trampoline.Bounce logic(Datum streamPair, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamPairP.logic(streamPair)) 
        throw new Exceptionf("'(scdr <stream-pair>) expects a <stream-pair>, given %s", streamPair.profile());
      Pair p = (Pair)streamPair;
      return UtilityPrimitives.Force.logic(p.cdr(),continuation);
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaar <obj>)
  //
  // (define (scaar spair) (scar (scar spair)))
  public static class Scaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar <stream-pair>))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,continuation));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scadr <obj>)
  //
  // (define (scadr spair) (scar (scdr spair)))
  public static class Scadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr <stream-pair>))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,continuation));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdar <obj>)
  //
  // (define (scdar spair) (scdr (scar spair)))
  public static class Scdar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar <stream-pair>))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,continuation));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scddr <obj>)
  //
  // (define (scddr spair) (scdr (scdr spair)))
  public static class Scddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr <stream-pair>))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,continuation));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaaar <obj>)
  //
  // (define (scaaar spair) (scar (scar (scar spair))))
  public static class Scaaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scar <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaadr <obj>)
  //
  // (define (scaadr spair) (scar (scar (scdr spair))))
  public static class Scaadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scdr <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scadar <obj>)
  //
  // (define (scadar spair) (scar (scdr (scar spair))))
  public static class Scadar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scadar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scar <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scadar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaddr <obj>)
  //
  // (define (scaddr spair) (scar (scdr (scdr spair))))
  public static class Scaddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scdr <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdaar <obj>)
  //
  // (define (scdaar spair) (scdr (scar (scar spair))))
  public static class Scdaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scar <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdadr <obj>)
  //
  // (define (scdadr spair) (scdr (scar (scdr spair))))
  public static class Scdadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scdr <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scddar <obj>)
  //
  // (define (scddar spair) (scdr (scdr (scar spair))))
  public static class Scddar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scddar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scar <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scddar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdddr <obj>)
  //
  // (define (scdddr spair) (scdr (scdr (scdr spair))))
  public static class Scdddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scdr <stream-pair>)))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,continuation)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaaaar <obj>)
  //
  // (define (scaaaar spair) (scar (scar (scar (scar spair)))))
  public static class Scaaaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaaaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaaaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scar (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaaaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaaadr <obj>)
  //
  // (define (scaaadr spair) (scar (scar (scar (scdr spair)))))
  public static class Scaaadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaaadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaaadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scar (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaaadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaadar <obj>)
  //
  // (define (scaadar spair) (scar (scar (scdr (scar spair)))))
  public static class Scaadar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaadar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scdr (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaadar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaaddr <obj>)
  //
  // (define (scaaddr spair) (scar (scar (scdr (scdr spair)))))
  public static class Scaaddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaaddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaaddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scar (scdr (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaaddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scadaar <obj>)
  //
  // (define (scadaar spair) (scar (scdr (scar (scar spair)))))
  public static class Scadaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scadaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scadaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scar (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scadaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scadadr <obj>)
  //
  // (define (scadadr spair) (scar (scdr (scar (scdr spair)))))
  public static class Scadadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scadadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scadadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scar (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scadadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scaddar <obj>)
  //
  // (define (scaddar spair) (scar (scdr (scdr (scar spair)))))
  public static class Scaddar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scaddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scaddar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scdr (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scaddar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scadddr <obj>)
  //
  // (define (scadddr spair) (scar (scdr (scdr (scdr spair)))))
  public static class Scadddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scadddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scadddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scar (scdr (scdr (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scadddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scar.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdaaar <obj>)
  //
  // (define (scdaaar spair) (scdr (scar (scar (scar spair)))))
  public static class Scdaaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdaaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdaaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scar (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdaaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdaadr <obj>)
  //
  // (define (scdaadr spair) (scdr (scar (scar (scdr spair)))))
  public static class Scdaadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdaadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdaadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scar (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdaadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdadar <obj>)
  //
  // (define (scdadar spair) (scdr (scar (scdr (scar spair)))))
  public static class Scdadar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdadar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scdr (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdadar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdaddr <obj>)
  //
  // (define (scdaddr spair) (scdr (scar (scdr (scdr spair)))))
  public static class Scdaddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdaddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdaddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scar (scdr (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdaddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scar.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scddaar <obj>)
  //
  // (define (scddaar spair) (scdr (scdr (scar (scar spair)))))
  public static class Scddaar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scddaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scddaar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scar (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scddaar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scddadr <obj>)
  //
  // (define (scddadr spair) (scdr (scdr (scar (scdr spair)))))
  public static class Scddadr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scddadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scddadr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scar (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scddadr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scar.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scdddar <obj>)
  //
  // (define (scdddar spair) (scdr (scdr (scdr (scar spair)))))
  public static class Scdddar extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scdddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scdddar"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scdr (scar <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scdddar <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scar.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (scddddr <obj>)
  //
  // (define (scddddr spair) (scdr (scdr (scdr (scdr spair)))))
  public static class Scddddr extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "scddddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("scddddr"),new Symbol("<stream-pair>"));
    }

    public String docstring() {
      return "Equivalent to: (scdr (scdr (scdr (scdr <stream-pair>))))";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(scddddr <stream-pair>) expects exactly 1 <stream-pair>: %s", Exceptionf.profileArgs(parameters));
      return Scdr.logic(parameters.get(0),(val1) -> () -> Scdr.logic(val1,(val2) -> () -> Scdr.logic(val2,(val3) -> () -> Scdr.logic(val3,continuation))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream->list <stream> <list-length>)
  //
  // (define (stream->list s list-length) ; facilitates printing stream contents
  //   (if (> list-length 0)
  //       (cons (scar s) (stream->list (scdr s) (- list-length 1)))
  //       (quote ())))
  public static class ConvertStreamToList extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream->list";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream->list"),new Symbol("<stream>"),new Symbol("<list-length>"));
    }

    public String docstring() {
      return "Return the first <list-length> items in <stream> as a list.";
    }

    public static boolean isValidStreamIndex(Datum i) {
      if(!(i instanceof Real)) return false;
      Real idx = (Real)i;
      return idx.isInteger() && !idx.isNegative();
    }

    private static Trampoline.Bounce logic(Datum s, int length, Trampoline.Continuation continuation) throws Exception {
      if(length <= 0 || !IsStreamPairP.logic(s)) return continuation.run(Nil.VALUE);
      return Scar.logic(s,(scarValue) -> () -> Scdr.logic(s,(scdrValue) -> () -> {
        return logic(scdrValue,length-1,(convertedList) -> () -> continuation.run(new Pair(scarValue,convertedList)));
      }));
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream->list <stream> <list-length>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream->list <stream> <list-length>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum l = parameters.get(1);
      if(!isValidStreamIndex(l))
        throw new Exceptionf("'(stream->list <stream> <list-length>) invalid <list-length>: %s", Exceptionf.profileArgs(parameters));
      return logic(s,((Real)l).intValue(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-val <stream> <index>)
  //
  // (define (stream-val s index)
  //   (if (= 0 index)
  //       (scar s)
  //       (stream-val (scdr s) (- index 1))))
  public static class StreamVal extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-val";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-val"),new Symbol("<stream>"),new Symbol("<index>"));
    }

    public String docstring() {
      return "Return the <index>th item in <stream>.";
    }

    private static Trampoline.Bounce logic(Datum s, int idx, Trampoline.Continuation continuation) throws Exception {
      if(idx <= 0) return Scar.logic(s,continuation);
      return Scdr.logic(s,(scdrValue) -> () -> logic(scdrValue,idx-1,continuation));
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-val <stream> <index>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-val <stream> <index>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum i = parameters.get(1);
      if(!ConvertStreamToList.isValidStreamIndex(i))
        throw new Exceptionf("'(stream-val <stream> <index>) invalid <list-length>: %s", Exceptionf.profileArgs(parameters));
      return logic(s,((Real)i).intValue(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-map <callable> <stream> ...)
  //
  // (define (stream-map callable . streams)
  //   (define (stream-map streams)
  //     (if (null? (car streams))
  //         '()
  //         (scons
  //           (apply callable (map scar streams))
  //           (stream-map (map scdr streams)))))
  //   (stream-map streams))
  public static class StreamMap extends Primitive {
    public java.lang.String escmName() {
      return "stream-map";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-map"),new Symbol("<callable>"),new Symbol("<stream>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Generate a new stream by applying <callable> to each of the items in\n\"<stream> ...\". Executes lazily.";
    }

    public static Datum compiledAtom(Datum atom) {
      return Pair.List(CorePrimitives.BYTECODE,Pair.List(CorePrimitives.LOAD,atom));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(stream-map <callable> <stream> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum c = parameters.get(0);
      if(!(c instanceof Callable))
        throw new Exceptionf("'(stream-map <callable> <stream> ...) invalid <callable>: %s", Exceptionf.profileArgs(parameters));
      Pair streams = (Pair)CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      if(streams.car() instanceof Nil) return Nil.VALUE;
      Datum compiledCallable = compiledAtom(c);
      Datum compiledStreams = compiledAtom(streams);
      Datum carCode = Pair.List(CorePrimitives.APPLY,compiledCallable,Pair.List(MAP,SCAR,compiledStreams));
      Datum cdrCode = Pair.List(CorePrimitives.APPLY,STREAM_MAP,Pair.List(CorePrimitives.CONS,compiledCallable,Pair.List(MAP,SCDR,compiledStreams)));
      Datum delayedScar = CorePrimitives.Delay.valueOf(carCode,this.definitionEnvironment);
      Datum delayedScdr = CorePrimitives.Delay.valueOf(cdrCode,this.definitionEnvironment);
      return new Pair(delayedScar,delayedScdr);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-filter <callable> <stream>)
  //
  // (define (stream-filter ? s)
  //   (cond ((null? s) (quote ()))
  //         ((? (scar s)) (scons (scar s) (stream-filter ? (scdr s))))
  //         (else (stream-filter ? (scdr s)))))
  public static class StreamFilter extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-filter";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-filter"),new Symbol("<keep?-callable>"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Generate a new stream by filtering out items in <stream> that don't\nsatisfy <keep?-callable>. Executes lazily.";
    }

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum c, Datum compiledCallable, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream-filter <callable> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(Nil.VALUE);
      return Scar.logic(stream,(scar) -> () -> {
        Datum compiledScar = StreamMap.compiledAtom(scar);
        return Scdr.logic(stream,(scdr) -> () -> {
          Datum compiledScdr = StreamMap.compiledAtom(scdr);
          Datum recursiveCode = Pair.List(STREAM_FILTER,compiledCallable,compiledScdr);
          ArrayList<Datum> args = new ArrayList<Datum>();
          args.add(scar);
          return ((Callable)c).callWith(args,(passedTest) -> () -> {
            if(passedTest.isTruthy()) {
              Datum delayedScar = CorePrimitives.Delay.valueOf(compiledScar,this.definitionEnvironment);
              Datum delayedFilteredScdr = CorePrimitives.Delay.valueOf(recursiveCode,this.definitionEnvironment);
              return continuation.run(new Pair(delayedScar,delayedFilteredScdr));
            } else {
              return logic(parameters,c,compiledCallable,scdr,continuation);
            }
          });
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-filter <callable> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum c = parameters.get(0);
      if(!(c instanceof Callable))
        throw new Exceptionf("'(stream-filter <callable> <stream>) invalid <callable>: %s", Exceptionf.profileArgs(parameters));
      Datum compiledCallable = StreamMap.compiledAtom(c);
      return logic(parameters,c,compiledCallable,parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-iterate <update-callable> <obj>)
  //
  // (define (stream-iterate update-callable seed)
  //     (scons seed (stream-iterate update-callable (update-callable seed))))
  public static class StreamIterate extends Primitive {
    public java.lang.String escmName() {
      return "stream-iterate";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-iterate"),new Symbol("<update-callable>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Generate an infinite stream by starting with <seed> and updating it\nwith <update-callable>. Executes lazily.";
    }

    private Datum logic(ArrayList<Datum> parameters, Datum c, Datum seed) throws Exception {
      Datum compiledCallable = StreamMap.compiledAtom(c);
      Datum compiledSeed = StreamMap.compiledAtom(seed);
      Datum scdrCode = Pair.List(STREAM_ITERATE,compiledCallable,Pair.List(compiledCallable,compiledSeed));
      Datum delayedScar = CorePrimitives.Delay.valueOf(compiledSeed,this.definitionEnvironment);
      Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
      return new Pair(delayedScar,delayedScdr);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-iterate <update-callable> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum c = parameters.get(0);
      if(!(c instanceof Callable))
        throw new Exceptionf("'(stream-iterate <update-callable> <obj>) invalid <callable>: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters,c,parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-constant <obj> ...)
  //
  // (define (stream-constant . objs)
  //   (define (stream-constant obj-list)
  //     (if (null? obj-list)
  //         (stream-constant objs)
  //         (scons (car obj-list) (stream-constant (cdr obj-list)))))
  //   (if (null? objs)
  //       '()
  //       (stream-constant objs)))
  public static class EscmStreamConstant extends Primitive {
    public java.lang.String escmName() {
      return "escm-stream-constant";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-stream-constant"),new Symbol("<obj-list>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Return an infinite stream of items in <obj-list> repeating in a cycle. Executes lazily.";
    }

    public static Datum logic(Environment env, Datum originalObjs, Datum objs) throws Exception {
      if(!(objs instanceof Pair)) return logic(env,originalObjs,originalObjs);
      Pair objsP = (Pair)objs;
      Datum carCode = StreamMap.compiledAtom(objsP.car());
      Datum cdrCode = Pair.List(ESCM_STREAM_CONSTANT,StreamMap.compiledAtom(originalObjs),StreamMap.compiledAtom(objsP.cdr()));
      Datum delayedScar = CorePrimitives.Delay.valueOf(carCode,env);
      Datum delayedScdr = CorePrimitives.Delay.valueOf(cdrCode,env);
      return new Pair(delayedScar,delayedScdr);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(this.definitionEnvironment,parameters.get(0),parameters.get(1));
    }
  }


  public static class StreamConstant extends Primitive {
    public java.lang.String escmName() {
      return "stream-constant";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-constant"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Return an infinite stream of \"<obj> ...\" repeating in a cycle. Executes lazily.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) return Nil.VALUE;
      Datum objs = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      return EscmStreamConstant.logic(this.definitionEnvironment,objs,objs);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-append <stream> ...)
  //
  // (define (stream-append s . streams)
  //   (define (stream-append s streams)
  //     (if (null? s)
  //         (if (null? streams)
  //             '()
  //             (stream-append (car streams) (cdr streams)))
  //         (scons (scar s) (stream-append (scdr s) streams))))
  //   (if (null? streams) s (stream-append s streams)))
  public static class StreamAppend extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-append";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-append"),new Symbol("<stream>"),new Symbol("<stream>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "Returns a new stream of \"<stream> ...\" appended to one another.";
    }

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum s, Datum streams, Trampoline.Continuation continuation) throws Exception {
      if(!(s instanceof Pair)) {
        if(!(streams instanceof Pair)) return continuation.run(Nil.VALUE);
        Pair p = (Pair)streams;
        return () -> logic(parameters,p.car(),p.cdr(),continuation);
      } else {
        if(!IsStreamP.logic(s))
          throw new Exceptionf("'(stream-append <stream> ...) invalid <stream> %s: %s", s.profile(), Exceptionf.profileArgs(parameters));
        Datum carPromise = ((Pair)s).car();
        return Scdr.logic(s,(scdr) -> () -> {
          Datum scdrCode = Pair.List(CorePrimitives.APPLY,STREAM_APPEND,Pair.List(CorePrimitives.CONS,StreamMap.compiledAtom(scdr),StreamMap.compiledAtom(streams)));
          Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
          return continuation.run(new Pair(carPromise,delayedScdr));
        });
      }
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(stream-append <stream> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      Datum streams = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      if(!(streams instanceof Pair)) return continuation.run(s);
      return logic(parameters,s,streams,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-interleave <stream> <stream>)
  //
  // (define (stream-interleave stream1 stream2)
  //   (if (null? stream1)
  //       stream2
  //       (scons (scar stream1) (stream-interleave stream2 (scdr stream1)))))
  public static class StreamInterleave extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-interleave";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-interleave"),new Symbol("<stream1>"),new Symbol("<stream2>"));
    }

    public String docstring() {
      return "Return a stream with the interleaved values of <stream1> and <stream2>.\nExecutes lazily.";
    }

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum stream1, Datum stream2, Trampoline.Continuation continuation) throws Exception {
      if(!(stream1 instanceof Pair)) return continuation.run(stream2);
      if(!IsStreamP.logic(stream1))
        throw new Exceptionf("'(stream-interleave <stream> <stream>) invalid <stream> %s: %s", stream1.profile(), Exceptionf.profileArgs(parameters));
      Datum stream2Code = StreamMap.compiledAtom(stream2);
      Datum carPromise = ((Pair)stream1).car();
      return Scdr.logic(stream1,(scdr) -> () -> {
        Datum scdrCode = Pair.List(STREAM_INTERLEAVE,stream2Code,StreamMap.compiledAtom(scdr));
        Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
        return continuation.run(new Pair(carPromise,delayedScdr));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-interleave <stream> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters,parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream->generator <stream>)
  //
  // (define (stream->generator stream-obj)
  //   (define s (scons #f stream-obj))
  //   (lambda ()
  //     (if (null? s)
  //         *generator-complete*
  //         (begin 
  //           (set! s (scdr s))
  //           (scar s)))))
  public static class ConvertStreamToGenerator extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream->generator";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream->generator"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Return a thunk (nullary procedure) that, upon invocation, returns the\nnext item in the stream.\nReturns <*generator-complete*> once at the end of the stream.";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(stream->generator <stream>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum stream = parameters.get(0);
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream->generator <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Symbol s = UniqueSymbol.generate("stream->generator-stream");
      Datum sValue = Pair.List(SCONS,Boolean.FALSE,StreamMap.compiledAtom(stream));
      Datum generatorProcedure = Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,
        Pair.List(CorePrimitives.IF,Pair.List(IS_NULLP,s),
          GlobalState.GLOBAL_GENERATOR_COMPLETE,
          Pair.List(CorePrimitives.BEGIN,
            Pair.List(CorePrimitives.SET_BANG,s,Pair.List(SCDR,s)),
            Pair.List(SCAR,s))));
      Datum letExpr = Pair.List(CorePrimitives.LET,Pair.List(Pair.List(s,sValue)),generatorProcedure);
      return MetaPrimitives.Eval.logic(letExpr,this.definitionEnvironment,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-member <obj> <stream>)
  //
  // (define (stream-member obj stream-obj)
  //   (if (null? stream-obj)
  //       #f
  //       (if (equal? obj (scar stream-obj))
  //           stream-obj
  //           (stream-member obj (scdr stream-obj)))))
  public static class StreamMember extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-member";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-member"),new Symbol("<obj>"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Returns the substream in <stream> starting with <obj> based on <equal?> item\nequality. Returns #f if <obj> isn't in <stream>. Note that this will run\nforever if <obj> isn't in <stream> and <stream> is infinite.";
    }

    private static Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum obj, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream-member <obj> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(Boolean.FALSE);
      return Scar.logic(stream,(scar) -> () -> {
        if(scar.equal(obj)) return continuation.run(stream);
        return Scdr.logic(stream,(scdr) -> () -> logic(parameters,obj,scdr,continuation));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-member <obj> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters,parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-memq <obj> <stream>)
  //
  // (define (stream-memq obj stream-obj)
  //   (if (null? stream-obj)
  //       #f
  //       (if (eq? obj (scar stream-obj))
  //           stream-obj
  //           (stream-memq obj (scdr stream-obj)))))
  public static class StreamMemq extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-memq";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-memq"),new Symbol("<obj>"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Returns the substream in <stream> starting with <obj> based on <eq?> item\nequality. Returns #f if <obj> isn't in <stream>. Note that this will run\nforever if <obj> isn't in <stream> and <stream> is infinite.";
    }

    private static Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum obj, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream-memq <obj> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(Boolean.FALSE);
      return Scar.logic(stream,(scar) -> () -> {
        if(scar.eq(obj)) return continuation.run(stream);
        return Scdr.logic(stream,(scdr) -> () -> logic(parameters,obj,scdr,continuation));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-memq <obj> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters,parameters.get(0),parameters.get(1),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-take <stream> <length>)
  //
  // (define (stream-take stream-obj n)
  //   (if (or (null? stream-obj) (<= n 0))
  //       '()
  //       (scons (scar stream-obj) (stream-take (scdr stream-obj) (- n 1)))))
  public static class StreamTake extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-take";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-take"),new Symbol("<stream>"),new Symbol("<length>"));
    }

    public String docstring() {
      return "Returns the first <length> items in <stream> as another stream.";
    }

    public static Trampoline.Bounce logic(String sig, ArrayList<Datum> parameters, Environment env, Datum stream, int length, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'%s invalid <stream>: %s", sig, Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil || length <= 0) return continuation.run(Nil.VALUE);
      Datum carPromise = ((Pair)stream).car();
      return Scdr.logic(stream,(scdr) -> () -> {
        Datum scdrCode = Pair.List(STREAM_TAKE,StreamMap.compiledAtom(scdr),new Exact(length-1));
        Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,env);
        return continuation.run(new Pair(carPromise,delayedScdr));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-take <stream> <length>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-take <stream> <length>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum l = parameters.get(1);
      if(!ConvertStreamToList.isValidStreamIndex(l))
        throw new Exceptionf("'(stream-take <stream> <length>) invalid <length>: %s", Exceptionf.profileArgs(parameters));
      return logic("(stream-take <stream> <length>)",parameters,this.definitionEnvironment,s,((Real)l).intValue(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-take-while <predicate?> <stream>)
  //
  // (define (stream-take-while continue? stream-obj)
  //   (if (or (null? stream-obj) (not (continue? (scar stream-obj))))
  //       '()
  //       (scons (scar stream-obj) (stream-take-while continue? (scdr stream-obj)))))
  public static class StreamTakeWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-take-while";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-take-while"),new Symbol("<continue?-callable>"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Returns a stream with the first items in <stream> while they satisfy\n<continue?-callable>.";
    }

    public static Trampoline.Bounce logic(String sig, ArrayList<Datum> parameters, Environment env, Datum contPred, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'%s invalid <stream>: %s", sig, Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(Nil.VALUE);
      Datum compiledCallable = StreamMap.compiledAtom(contPred);
      return Scar.logic(stream,(scar) -> () -> {
        Datum scarCode = StreamMap.compiledAtom(scar);
        ArrayList<Datum> args = new ArrayList<Datum>();
        args.add(scar);
        return ((Callable)contPred).callWith(args,(shouldContinue) -> () -> {
          if(!shouldContinue.isTruthy()) return continuation.run(Nil.VALUE);
          return Scdr.logic(stream,(scdr) -> () -> {
            Datum scdrCode = Pair.List(STREAM_TAKE_WHILE,compiledCallable,StreamMap.compiledAtom(scdr));
            Datum delayedScar = CorePrimitives.Delay.valueOf(scarCode,env);
            Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,env);
            return continuation.run(new Pair(delayedScar,delayedScdr));
          });
        });
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-take-while <predicate?> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum contPred = parameters.get(0);
      if(!(contPred instanceof Callable))
        throw new Exceptionf("'(stream-take-while <predicate?> <stream>) invalid <predicate?>: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(1);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-take-while <predicate?> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      return logic("(stream-take-while <predicate?> <stream>)",parameters,this.definitionEnvironment,contPred,s,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-drop <stream> <length>)
  //
  // (define (stream-drop stream-obj n)
  //   (if (or (null? stream-obj) (<= n 0))
  //       stream-obj
  //       (stream-drop (scdr stream-obj) (- n 1))))
  public static class StreamDrop extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-drop";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-drop"),new Symbol("<stream>"),new Symbol("<length>"));
    }

    public String docstring() {
      return "Returns <stream> without its first <length> items as another stream.";
    }

    public static Trampoline.Bounce logic(String sig, ArrayList<Datum> parameters, Datum stream, int length, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'%s invalid <stream>: %s", sig, Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil || length <= 0) return continuation.run(stream);
      return Scdr.logic(stream,(scdr) -> () -> logic(sig,parameters,scdr,length-1,continuation));
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-drop <stream> <length>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-drop <stream> <length>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum l = parameters.get(1);
      if(!ConvertStreamToList.isValidStreamIndex(l))
        throw new Exceptionf("'(stream-drop <stream> <length>) invalid <length>: %s", Exceptionf.profileArgs(parameters));
      return logic("(stream-drop <stream> <length>)",parameters,s,((Real)l).intValue(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (stream-drop-while <predicate?> <stream>)
  //
  // (define (stream-drop-while continue? stream-obj)
  //   (if (or (null? stream-obj) (not (continue? (scar stream-obj))))
  //       stream-obj
  //       (stream-drop-while continue? (scdr stream-obj))))
  public static class StreamDropWhile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-drop-while";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-drop-while"),new Symbol("<continue?-callable>"),new Symbol("<stream>"));
    }

    public String docstring() {
      return "Returns <stream> without all of the leading items that satisfy <continue?-callable>\nas another stream. Note that this will run forever if <stream> is infinite\nand every item in it satisfies <predicate?>.";
    }

    public static Trampoline.Bounce logic(String sig, ArrayList<Datum> parameters, Datum contPred, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'%s invalid <stream>: %s", sig, Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(stream);
      return Scar.logic(stream,(scar) -> () -> {
        ArrayList<Datum> args = new ArrayList<Datum>();
        args.add(scar);
        return ((Callable)contPred).callWith(args,(shouldContinue) -> () -> {
          if(!shouldContinue.isTruthy()) return continuation.run(stream);
          return Scdr.logic(stream,(scdr) -> () -> logic(sig,parameters,contPred,scdr,continuation));
        });
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-drop-while <predicate?> <stream>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum contPred = parameters.get(0);
      if(!(contPred instanceof Callable))
        throw new Exceptionf("'(stream-drop-while <predicate?> <stream>) invalid <predicate?>: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(1);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-drop-while <predicate?> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      return logic("(stream-drop-while <predicate?> <stream>)",parameters,contPred,s,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // stream-slice
  //
  // (defn stream-slice
  //   ((stream-obj idx) (stream-drop stream-obj idx))
  //   ((stream-obj idx n-or-continue?)
  //     (if (number? n-or-continue?)
  //         (stream-take (stream-drop stream-obj idx) n-or-continue?)
  //         (stream-take-while n-or-continue? (stream-drop stream-obj idx)))))
  public static class StreamSlice extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-slice";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("stream-slice"),new Symbol("<index>")),
        Pair.List(new Symbol("stream-slice"),new Symbol("<index>"),new Symbol("<length>")),
        Pair.List(new Symbol("stream-slice"),new Symbol("<index>"),new Symbol("<continue?-callable>")));
    }

    public String docstring() {
      return "Slices a subset of the items in <stream> starting from <start-index>.\nIf no other args are given, returns the rest of the items from <start-index>.\nIf <length> is given, returns at most <length> items.\nGiven <continue?-callable>, slices while values satisfy <continue?-callable>.\n\nNote that this may run infinitely if given an infinite stream where every\nvalue satisfies <continue?-callable>";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 2 || parameters.size() > 3) 
        throw new Exceptionf("'(stream-slice <stream> <start-index> <optional-continue-predicate-or-length>) invalid args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-slice <stream> <start-index> <optional-continue-predicate-or-length>) 1st arg isn't a <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum i = parameters.get(1);
      if(!ConvertStreamToList.isValidStreamIndex(i))
        throw new Exceptionf("'(stream-slice <stream> <start-index> <optional-continue-predicate-or-length>) 2nd arg isn't an index: %s", Exceptionf.profileArgs(parameters));
      if(parameters.size() == 2) {
        return StreamDrop.logic("(stream-slice <stream> <start-index>)",parameters,s,((Real)i).intValue(),continuation);
      }
      if(ConvertStreamToList.isValidStreamIndex(parameters.get(2))) {
        Datum l = parameters.get(2);
        return StreamDrop.logic("(stream-slice <stream> <start-index> <length>)",parameters,s,((Real)i).intValue(),(dropped) -> () -> {
          return StreamTake.logic("(stream-slice <stream> <start-index> <length>)",parameters,this.definitionEnvironment,dropped,((Real)l).intValue(),continuation);
        });
      }
      if(parameters.get(2) instanceof Callable) {
        Datum contPred = parameters.get(2);
        return StreamDrop.logic("(stream-slice <stream> <start-index> <predicate?>)",parameters,s,((Real)i).intValue(),(dropped) -> () -> {
          return StreamTakeWhile.logic("(stream-slice <stream> <start-index> <predicate?>)",parameters,this.definitionEnvironment,contPred,dropped,continuation);
        });
      }
      throw new Exceptionf("'(stream-slice <stream> <start-index> <optional-continue-predicate-or-length>) invalid 3rd arg: %s", Exceptionf.profileArgs(parameters));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // stream-unfold
  //
  // (define (stream-unfold break? mapper succer seed)
  //   (if (break? seed)
  //       '()
  //       (scons (mapper seed) (stream-unfold break? mapper succer (succer seed)))))
  public static class StreamUnfold extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-unfold";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-unfold"),new Symbol("<break?-callable>"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public String docstring() {
      return "Unfolds a stream from left to right, starting with <seed>. <break?-callable>\ndetermines when unfolding stops, <mapper-callable> maps the <seed> to a value\nin the unfolded stream, and <update-callable> increments <seed> for the\nnext round of unfolding.\n\nSee <stream-unfolds> for an alternative without <break?-callable>.";
    }

    private Trampoline.Bounce logic(Datum breakCond, Datum mapper, Datum successor, Datum seed, Trampoline.Continuation continuation) throws Exception {
      Datum compiledBreakCond = StreamMap.compiledAtom(breakCond);
      Datum compiledMapper = StreamMap.compiledAtom(mapper);
      Datum compiledSuccessor = StreamMap.compiledAtom(successor);
      ArrayList<Datum> breakArgs = new ArrayList<Datum>(1);
      breakArgs.add(seed);
      return ((Callable)breakCond).callWith(breakArgs,(shouldBreak) -> () -> {
        if(shouldBreak.isTruthy()) return continuation.run(Nil.VALUE);
        ArrayList<Datum> mapArgs = new ArrayList<Datum>(1);
        mapArgs.add(seed);
        return ((Callable)mapper).callWith(mapArgs,(mapValue) -> () -> {
          Datum scarCode = StreamMap.compiledAtom(mapValue);
          ArrayList<Datum> sucArgs = new ArrayList<Datum>(1);
          sucArgs.add(seed);
          return ((Callable)successor).callWith(sucArgs,(sucValue) -> () -> {
            Datum scdrCode = Pair.List(STREAM_UNFOLD,compiledBreakCond,compiledMapper,compiledSuccessor,StreamMap.compiledAtom(sucValue));
            Datum delayedScar = CorePrimitives.Delay.valueOf(scarCode,this.definitionEnvironment);
            Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
            return continuation.run(new Pair(delayedScar,delayedScdr));
          });
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 4) 
        throw new Exceptionf("'(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(2) instanceof Callable))
        throw new Exceptionf("'(stream-unfold <break-condition> <map-callable> <successor-callable> <seed>) 3rd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),parameters.get(1),parameters.get(2),parameters.get(3),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // stream-unfolds
  //
  // (define (stream-unfolds mapper succer seed)
  //   (scons (mapper seed) (stream-unfolds mapper succer (succer seed))))
  public static class StreamUnfolds extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-unfolds";
    }

    public Datum signature() {
      return Pair.List(new Symbol("stream-unfolds"),new Symbol("<mapper-callable>"),new Symbol("<update-callable>"),new Symbol("<seed>"));
    }

    public String docstring() {
      return "Unfolds a stream from left to right, starting with <seed>. <mapper-callable>\nmaps the <seed> to a value in the unfolded stream, and <update-callable>\nincrements <seed> for the next round of unfolding.\n\nSee <stream-unfold> for an alternative with a <break?-callable>.";
    }

    private Trampoline.Bounce logic(Datum mapper, Datum successor, Datum seed, Trampoline.Continuation continuation) throws Exception {
      Datum compiledMapper = StreamMap.compiledAtom(mapper);
      Datum compiledSuccessor = StreamMap.compiledAtom(successor);
      ArrayList<Datum> mapArgs = new ArrayList<Datum>(1);
      mapArgs.add(seed);
      return ((Callable)mapper).callWith(mapArgs,(mapValue) -> () -> {
        Datum scarCode = StreamMap.compiledAtom(mapValue);
        ArrayList<Datum> sucArgs = new ArrayList<Datum>(1);
        sucArgs.add(seed);
        return ((Callable)successor).callWith(sucArgs,(sucValue) -> () -> {
          Datum scdrCode = Pair.List(STREAM_UNFOLDS,compiledMapper,compiledSuccessor,StreamMap.compiledAtom(sucValue));
          Datum delayedScar = CorePrimitives.Delay.valueOf(scarCode,this.definitionEnvironment);
          Datum delayedScdr = CorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
          return continuation.run(new Pair(delayedScar,delayedScdr));
        });
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 3) 
        throw new Exceptionf("'(stream-unfolds <map-callable> <successor-callable> <seed>) invalid args: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(0) instanceof Callable))
        throw new Exceptionf("'(stream-unfolds <map-callable> <successor-callable> <seed>) 1st arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      if(!(parameters.get(1) instanceof Callable))
        throw new Exceptionf("'(stream-unfolds <map-callable> <successor-callable> <seed>) 2nd arg isn't a callable: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),parameters.get(1),parameters.get(2),continuation);
    }
  }
}