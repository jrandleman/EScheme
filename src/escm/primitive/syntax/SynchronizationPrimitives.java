// Author: Jordan Randleman - escm.primitive.syntax.SynchronizationPrimitives
// Purpose:
//    Java primitives for core synchronization syntax macros. All of this logic 
//    used to be implemented in a <stdlib.scm> file, however, we now implement
//    such natively in Java to maximize EScheme's performance :)
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
import escm.type.concurrent.Mutex;
import escm.vm.type.PrimitiveSyntax;

public class SynchronizationPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Symbols
  public static Symbol DYNAMIC_WIND = new Symbol("dynamic-wind");
  public static Symbol MUTEX_LOCK = new Symbol("mutex-lock!");
  public static Symbol MUTEX_UNLOCK = new Symbol("mutex-unlock!");
  public static Symbol DOSYNC_LOCK = new Symbol("*dosync-lock*");
  public static Symbol DOSYNC_MODULE_LOCK = new Symbol("*dosync-module-lock*");
  public static Symbol THREAD_DEFINE_PRIME = new Symbol("thread-define'");
  public static Symbol THREAD_SET_BANG_PRIME = new Symbol("thread-set!'");
  public static Symbol THREAD_GET_PRIME = new Symbol("thread-get'");
  public static Symbol THREAD_DEFINEDP_PRIME = new Symbol("thread-defined?'");


  ////////////////////////////////////////////////////////////////////////////
  // (dosync <expr> ...)
  //
  // (define-syntax dosync
  //   (lambda (. exprs)
  //     (list 'dynamic-wind
  //       '(lambda () (mutex-lock! *dosync-lock*))
  //       (cons 'lambda (cons '() exprs))
  //       '(lambda () (mutex-unlock! *dosync-lock*)))))
  public static class Dosync extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "dosync";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum exprs = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      return Pair.List(DYNAMIC_WIND,
        Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_LOCK,DOSYNC_LOCK)),
        new Pair(CorePrimitives.LAMBDA,new Pair(Nil.VALUE,exprs)),
        Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_UNLOCK,DOSYNC_LOCK)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (dosync-module <expr> ...)
  //
  // (define-syntax dosync-module
  //   (lambda (. exprs)
  //     (list 'dynamic-wind
  //       '(lambda () (mutex-lock! *dosync-module-lock*))
  //       (cons 'lambda (cons '() exprs))
  //       '(lambda () (mutex-unlock! *dosync-module-lock*)))))
  public static class DosyncModule extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "dosync-module";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum exprs = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      return Pair.List(DYNAMIC_WIND,
        Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_LOCK,DOSYNC_MODULE_LOCK)),
        new Pair(CorePrimitives.LAMBDA,new Pair(Nil.VALUE,exprs)),
        Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_UNLOCK,DOSYNC_MODULE_LOCK)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (dosync-with <mutex> <expr> ...)
  //
  // (define-syntax dosync-with 
  //   (lambda (lock-expr . exprs)
  //     (define lock (gensym 'dosync-with-lock))
  //     (list 'begin
  //       (list 'define lock lock-expr) ; cache the lock!
  //       (list 'dynamic-wind
  //         (list 'lambda '() (list 'mutex-lock! lock))
  //         (cons 'lambda (cons '() exprs))
  //         (list 'lambda '() (list 'mutex-unlock! lock))))))
  public static class DosyncWith extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "dosync-with";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(dosync-with <mutex> <expr> ...) didn't receive <mutex>: %s", Exceptionf.profileArgs(parameters));
      Symbol cachedLock = UniqueSymbol.generate("dosync-with-lock");
      Datum lock = parameters.get(0);
      Datum exprs = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      return Pair.List(CorePrimitives.BEGIN,
        Pair.List(CorePrimitives.DEFINE,cachedLock,lock),
        Pair.List(DYNAMIC_WIND,
          Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_LOCK,cachedLock)),
          new Pair(CorePrimitives.LAMBDA,new Pair(Nil.VALUE,exprs)),
          Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,Pair.List(MUTEX_UNLOCK,cachedLock))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (thread-define <optional-thread> <symbol> <value>)
  //
  // (define-syntax thread-define
  //   (fn ((var val) (list 'thread-define' (list 'quote var) val))
  //       ((thread var val) (list 'thread-define' thread (list 'quote var) val))))
  public static class ThreadDefine extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "thread-define";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n != 2 && n != 3)
        throw new Exceptionf("'(thread-define <optional-thread> <symbol> <value>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 2) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-define <optional-thread> <symbol> <value>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_DEFINE_PRIME,Pair.List(CorePrimitives.QUOTE,parameters.get(0)),parameters.get(1));
      } else {
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-define <optional-thread> <symbol> <value>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_DEFINE_PRIME,parameters.get(0),Pair.List(CorePrimitives.QUOTE,parameters.get(1)),parameters.get(2));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (thread-set! <optional-thread> <symbol> <value>)
  //
  // (define-syntax thread-set!
  //   (fn ((var val) (list 'thread-set!' (list 'quote var) val))
  //       ((thread var val) (list 'thread-set!' thread (list 'quote var) val))))
  public static class ThreadSetBang extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "thread-set!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n != 2 && n != 3)
        throw new Exceptionf("'(thread-set! <optional-thread> <symbol> <value>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 2) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-set! <optional-thread> <symbol> <value>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_SET_BANG_PRIME,Pair.List(CorePrimitives.QUOTE,parameters.get(0)),parameters.get(1));
      } else {
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-set! <optional-thread> <symbol> <value>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_SET_BANG_PRIME,parameters.get(0),Pair.List(CorePrimitives.QUOTE,parameters.get(1)),parameters.get(2));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (thread-get <optional-thread> <symbol>)
  //
  // (define-syntax thread-get
  //   (fn ((var) (list 'thread-get' (list 'quote var)))
  //       ((thread var) (list 'thread-get' thread (list 'quote var)))))
  public static class ThreadGet extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "thread-get";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n != 1 && n != 2)
        throw new Exceptionf("'(thread-get <optional-thread> <symbol>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-get <optional-thread> <symbol>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_GET_PRIME,Pair.List(CorePrimitives.QUOTE,parameters.get(0)));
      } else {
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-get <optional-thread> <symbol>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_GET_PRIME,parameters.get(0),Pair.List(CorePrimitives.QUOTE,parameters.get(1)));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (thread-defined? <optional-thread> <symbol>)
  //
  // (define-syntax thread-defined?
  //   (fn ((var) (list 'thread-defined?' (list 'quote var)))
  //       ((thread var) (list 'thread-defined?' thread (list 'quote var)))))
  public static class ThreadIsDefinedP extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "thread-defined?";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n != 1 && n != 2)
        throw new Exceptionf("'(thread-defined? <optional-thread> <symbol>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        if(!(parameters.get(0) instanceof Symbol))
          throw new Exceptionf("'(thread-defined? <optional-thread> <symbol>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_DEFINEDP_PRIME,Pair.List(CorePrimitives.QUOTE,parameters.get(0)));
      } else {
        if(!(parameters.get(1) instanceof Symbol))
          throw new Exceptionf("'(thread-defined? <optional-thread> <symbol>) invalid <symbol>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(THREAD_DEFINEDP_PRIME,parameters.get(0),Pair.List(CorePrimitives.QUOTE,parameters.get(1)));
      }
    }
  }
}