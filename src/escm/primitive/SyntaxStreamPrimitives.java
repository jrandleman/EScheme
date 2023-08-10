// Author: Jordan Randleman - escm.primitive.SyntaxStreamPrimitives
// Purpose:
//    Java primitives for core stream syntax macros/procedures. All of this
//    logic used to be implemented in a <stdlib.scm> file, however, we now 
//    implement such natively in Java to maximize EScheme's performance :)
//      => Note that the original EScheme code is still included in comments
//         throughout this file.

package escm.primitive;
import java.util.ArrayList;
import escm.util.UniqueSymbol;
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.type.PrimitiveSyntax;
import escm.vm.util.Environment;
import escm.vm.runtime.GlobalState;

public class SyntaxStreamPrimitives {
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
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(scons <obj> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(SyntaxCorePrimitives.CONS,Pair.List(DELAY,parameters.get(0)),Pair.List(DELAY,parameters.get(1)));
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
  // (stream-ref <stream> <index>)
  //
  // (define (stream-ref s index)
  //   (if (= 0 index)
  //       (scar s)
  //       (stream-ref (scdr s) (- index 1))))
  public static class StreamRef extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "stream-ref";
    }

    private static Trampoline.Bounce logic(Datum s, int idx, Trampoline.Continuation continuation) throws Exception {
      if(idx <= 0) return Scar.logic(s,continuation);
      return Scdr.logic(s,(scdrValue) -> () -> logic(scdrValue,idx-1,continuation));
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(stream-ref <stream> <index>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      if(!IsStreamP.logic(s))
        throw new Exceptionf("'(stream-ref <stream> <index>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Datum i = parameters.get(1);
      if(!ConvertStreamToList.isValidStreamIndex(i))
        throw new Exceptionf("'(stream-ref <stream> <index>) invalid <list-length>: %s", Exceptionf.profileArgs(parameters));
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

    public static Datum compiledAtom(Datum atom) {
      return Pair.List(SyntaxCorePrimitives.BYTECODE,Pair.List(SyntaxCorePrimitives.LOAD,atom));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2) 
        throw new Exceptionf("'(stream-map <callable> <stream> ...) expects at least 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum c = parameters.get(0);
      if(!(c instanceof Callable))
        throw new Exceptionf("'(stream-map <callable> <stream> ...) invalid <callable>: %s", Exceptionf.profileArgs(parameters));
      Pair streams = (Pair)SyntaxCorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      if(streams.car() instanceof Nil) return Nil.VALUE;
      Datum compiledCallable = compiledAtom(c);
      Datum compiledStreams = compiledAtom(streams);
      Datum carCode = Pair.List(SyntaxCorePrimitives.APPLY,compiledCallable,Pair.List(MAP,SCAR,compiledStreams));
      Datum cdrCode = Pair.List(SyntaxCorePrimitives.APPLY,STREAM_MAP,Pair.List(SyntaxCorePrimitives.CONS,compiledCallable,Pair.List(MAP,SCDR,compiledStreams)));
      Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(carCode,this.definitionEnvironment);
      Datum delayedScdr = SyntaxCorePrimitives.Delay.valueOf(cdrCode,this.definitionEnvironment);
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

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum c, Datum stream, Trampoline.Continuation continuation) throws Exception {
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream-filter <callable> <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      if(stream instanceof Nil) return continuation.run(Nil.VALUE);
      Datum compiledCallable = StreamMap.compiledAtom(c);
      return Scar.logic(stream,(scar) -> () -> {
        Datum compiledScar = StreamMap.compiledAtom(scar);
        return Scdr.logic(stream,(scdr) -> () -> {
          Datum compiledScdr = StreamMap.compiledAtom(scdr);
          Datum recursiveCode = Pair.List(STREAM_FILTER,compiledCallable,compiledScdr);
          ArrayList<Datum> args = new ArrayList<Datum>();
          args.add(scar);
          return ((Callable)c).callWith(args,(passedTest) -> () -> {
            if(passedTest.isTruthy()) {
              Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(compiledScar,this.definitionEnvironment);
              Datum delayedFilteredScdr = SyntaxCorePrimitives.Delay.valueOf(recursiveCode,this.definitionEnvironment);
              return continuation.run(new Pair(delayedScar,delayedFilteredScdr));
            } else {
              return logic(parameters,c,scdr,continuation);
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
      return logic(parameters,c,parameters.get(1),continuation);
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

    private Datum logic(ArrayList<Datum> parameters, Datum c, Datum seed) throws Exception {
      Datum compiledCallable = StreamMap.compiledAtom(c);
      Datum compiledSeed = StreamMap.compiledAtom(seed);
      Datum scdrCode = Pair.List(STREAM_ITERATE,compiledCallable,Pair.List(compiledCallable,compiledSeed));
      Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(compiledSeed,this.definitionEnvironment);
      Datum delayedScdr = SyntaxCorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
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

    public static Datum logic(Environment env, Datum originalObjs, Datum objs) throws Exception {
      if(!(objs instanceof Pair)) return logic(env,originalObjs,originalObjs);
      Pair objsP = (Pair)objs;
      Datum carCode = StreamMap.compiledAtom(objsP.car());
      Datum cdrCode = Pair.List(ESCM_STREAM_CONSTANT,StreamMap.compiledAtom(originalObjs),StreamMap.compiledAtom(objsP.cdr()));
      Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(carCode,env);
      Datum delayedScdr = SyntaxCorePrimitives.Delay.valueOf(cdrCode,env);
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
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() == 0) return Nil.VALUE;
      Datum objs = SyntaxCorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
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

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum s, Datum streams, Trampoline.Continuation continuation) throws Exception {
      if(!(s instanceof Pair)) {
        if(!(streams instanceof Pair)) return continuation.run(Nil.VALUE);
        Pair p = (Pair)streams;
        return () -> logic(parameters,p.car(),p.cdr(),continuation);
      } else {
        if(!IsStreamP.logic(s))
          throw new Exceptionf("'(stream-append <stream> ...) invalid <stream> %s: %s", s.profile(), Exceptionf.profileArgs(parameters));
        return Scar.logic(s,(scar) -> () -> {
          Datum scarCode = StreamMap.compiledAtom(scar);
          return Scdr.logic(s,(scdr) -> () -> {
            Datum scdrCode = Pair.List(SyntaxCorePrimitives.APPLY,STREAM_APPEND,Pair.List(SyntaxCorePrimitives.CONS,StreamMap.compiledAtom(scdr),StreamMap.compiledAtom(streams)));
            Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(scarCode,this.definitionEnvironment);
            Datum delayedScdr = SyntaxCorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
            return continuation.run(new Pair(delayedScar,delayedScdr));
          });
        });
      }
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 1) 
        throw new Exceptionf("'(stream-append <stream> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum s = parameters.get(0);
      Datum streams = SyntaxCorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
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

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum stream1, Datum stream2, Trampoline.Continuation continuation) throws Exception {
      if(!(stream1 instanceof Pair)) return continuation.run(stream2);
      if(!IsStreamP.logic(stream1))
        throw new Exceptionf("'(stream-interleave <stream> <stream>) invalid <stream> %s: %s", stream1.profile(), Exceptionf.profileArgs(parameters));
      Datum stream2Code = StreamMap.compiledAtom(stream2);
      return Scar.logic(stream1,(scar) -> () -> {
        Datum scarCode = StreamMap.compiledAtom(scar);
        return Scdr.logic(stream1,(scdr) -> () -> {
          Datum scdrCode = Pair.List(STREAM_INTERLEAVE,stream2Code,StreamMap.compiledAtom(scdr));
          Datum delayedScar = SyntaxCorePrimitives.Delay.valueOf(scarCode,this.definitionEnvironment);
          Datum delayedScdr = SyntaxCorePrimitives.Delay.valueOf(scdrCode,this.definitionEnvironment);
          return continuation.run(new Pair(delayedScar,delayedScdr));
        });
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
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(stream->generator <stream>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum stream = parameters.get(0);
      if(!IsStreamP.logic(stream))
        throw new Exceptionf("'(stream->generator <stream>) invalid <stream>: %s", Exceptionf.profileArgs(parameters));
      Symbol s = UniqueSymbol.generate("stream->generator-stream");
      Datum sValue = Pair.List(SCONS,Boolean.FALSE,StreamMap.compiledAtom(stream));
      Datum generatorProcedure = Pair.List(SyntaxCorePrimitives.LAMBDA,Nil.VALUE,
        Pair.List(SyntaxCorePrimitives.IF,Pair.List(IS_NULLP,s),
          GlobalState.GLOBAL_GENERATOR_COMPLETE,
          Pair.List(SyntaxCorePrimitives.BEGIN,
            Pair.List(SyntaxCorePrimitives.SET_BANG,s,Pair.List(SCDR,s)),
            Pair.List(SCAR,s))));
      Datum letExpr = Pair.List(SyntaxCorePrimitives.LET,Pair.List(Pair.List(s,sValue)),generatorProcedure);
      return MetaPrimitives.Eval.logic(letExpr,this.definitionEnvironment,continuation);
    }
  }
}