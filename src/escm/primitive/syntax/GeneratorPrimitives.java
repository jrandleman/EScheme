// Author: Jordan Randleman - escm.primitive.syntax.GeneratorPrimitives
// Purpose:
//    Java primitives for generator syntax macros/procedures. All of this
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
import escm.type.Void;
import escm.type.Symbol;
import escm.type.bool.Boolean;
import escm.type.number.Real;
import escm.util.Trampoline;
import escm.vm.type.callable.Callable;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.type.primitive.PrimitiveSyntax;
import escm.vm.type.callable.Signature;
import escm.vm.runtime.GlobalState;

public class GeneratorPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Symbols
  public static Symbol DELAY = new Symbol("delay");
  public static Symbol ESCM_GENERATOR = new Symbol("escm-generator");
  public static Symbol ESCM_GENERATOR_ESCAPE = new Symbol("escm-generator-escape");
  public static Symbol ESCM_IS_GENERATORP = new Symbol("escm-generator?");
  public static Symbol CDR = new Symbol("cdr");
  public static Symbol CDAR = new Symbol("cdar");


  ////////////////////////////////////////////////////////////////////////////
  // (escm-generator? <obj>)
  //
  // (define (escm-generator? obj)
  //   (and (pair? obj) 
  //        (pair? (car obj))
  //        (eq? 'escm-generator (caar obj))
  //        (procedure? (cdr obj))))
  public static class EscmIsGeneratorP extends Primitive {
    public java.lang.String escmName() {
      return "escm-generator?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-generator?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Syntax:Generators\nReturns whether <obj> is a result of <define-generator>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(escm-generator? <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum obj = parameters.get(0);
      if(!(obj instanceof Pair)) return Boolean.FALSE;
      Pair p1 = (Pair)obj;
      if(!(p1.car() instanceof Pair)) return Boolean.FALSE;
      return Boolean.valueOf(((Pair)p1.car()).car().eq(ESCM_GENERATOR) && p1.cdr() instanceof Callable);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (yield <optional-obj>)
  //
  // (define-syntax yield 
  //   (fn (()
  //         (define k (gensym 'yield-k))
  //         (list 'set! 'escm-generator-escape 
  //           (list 'call/cc 
  //             (list 'lambda (list k)
  //               (list 'escm-generator-escape 
  //                 (list 'cons 
  //                   (list 'cons ''escm-generator #void)
  //                   (list 'lambda '() 
  //                     (list 'call/cc (list 'lambda '(escm-generator-escape) (list k 'escm-generator-escape))))))))))
  //       ((yielded)
  //         (define k (gensym 'yield-k))
  //         (list 'set! 'escm-generator-escape 
  //           (list 'call/cc 
  //             (list 'lambda (list k)
  //               (list 'escm-generator-escape 
  //                 (list 'cons 
  //                   (list 'cons ''escm-generator yielded)
  //                   (list 'lambda '() 
  //                     (list 'call/cc (list 'lambda '(escm-generator-escape) (list k 'escm-generator-escape))))))))))))
  public static class Yield extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "yield";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("yield")),
        Pair.List(new Symbol("yield"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Syntax:Generators\nPause the current execution of the generator and return <obj> (#void by\ndefault). Only valid within the \"<body> ...\" of <define-generator>.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n > 1) 
        throw new Exceptionf("'(yield <optional-obj>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum yielded = n == 1 ? parameters.get(0) : (Datum)Void.VALUE;
      Symbol k = UniqueSymbol.generate("yield-k");
      return Pair.List(CorePrimitives.SET_BANG,ESCM_GENERATOR_ESCAPE,
        Pair.List(CorePrimitives.CALL_CC,
          Pair.List(CorePrimitives.LAMBDA,Pair.List(k),
            Pair.List(ESCM_GENERATOR_ESCAPE,
              Pair.List(CorePrimitives.CONS,
                Pair.List(CorePrimitives.CONS,Pair.List(CorePrimitives.QUOTE,ESCM_GENERATOR),yielded),
                Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,
                  Pair.List(CorePrimitives.CALL_CC,
                    Pair.List(CorePrimitives.LAMBDA,Pair.List(ESCM_GENERATOR_ESCAPE),
                      Pair.List(k,ESCM_GENERATOR_ESCAPE)))))))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-generator (<generator-name> <param> ...) <optional-docstring> <body> ...)
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>.
  // =====
  //
  // (define-syntax define-generator 
  //   (lambda (bindings . body)
  //     (define generator-object (gensym 'define-generator-object))
  //     (list 'define bindings
  //       (list 'define generator-object
  //         (list 'cons
  //           (list 'cons ''escm-generator #f)
  //           (list 'lambda '()
  //             (list 'call/cc (cons 'lambda (cons '(escm-generator-escape) body))))))
  //       (list 'lambda '()
  //         (list 'if (list 'escm-generator? generator-object)
  //             (list 'begin 
  //               (list 'set! generator-object (list (list 'cdr generator-object)))
  //               (list 'if (list 'escm-generator? generator-object)
  //                   (list 'cdar generator-object)
  //                   generator-object))
  //             *generator-complete*)))))
  public static class DefineGenerator extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-generator";
    }

    public Datum signature() {
      return Pair.List(new Symbol("define-generator"),
        Pair.List(new Symbol("<generator-name>"), new Symbol("<parameter>"),Signature.VARIADIC),
        new Symbol("<optional-docstring>"),new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Generators\nDefine a generator constructor! Calling <generator-name>\nreturns a generator thunk, which can suspend its operation\n(until it gets invoked again) via the <yield> macro.\n\nNote that due to being a \"generator-constructor\", <generator-name>\nshould not be called recursively to emulate looping.\nInstead, define an inner procedure or use the \"named let\" construct.\n\nOptionally include <docstring> to yield further details on the generator\nconstructor's intended purpose in the <help> menu.\n\nAliased by <defgen>.\nFor example:\n  ;; Printing Numbers and Strings:\n  (define-generator (print-numbers start total)\n    (let loop ((i start))\n      (if (< i total)\n            (begin \n              (write i)\n              (display \" \")\n              (yield) ; pause execution until re-invoked\n              (loop (+ i 1))))))\n  \n  (define-generator (print-strings start total)\n    (let loop ((i start))\n      (if (< i total)\n            (begin \n              (write (number->string i))\n              (display \" \")\n              (yield) ; pause execution until re-invoked\n              (loop (+ i 1))))))\n  \n  (complete-all-generators! (print-numbers 0 10) (print-strings 0 10))\n  (newline)\n  \n  \n  ;; Creating a stream of integers from a starting number (0 by default):\n  (define-generator (ints-from (start 0))\n    (let loop ((n start))\n      (yield n)\n      (loop (+ n 1))))\n  \n  (define ints (ints-from 42))\n  (display (ints)) \n  (newline)\n  (display (ints)) \n  (newline)\n  (display (ints)) \n  (newline)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Symbol generatorObject = UniqueSymbol.generate("define-generator-object");
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(define-generator (<generator-name> <param> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      escm.type.String docstring = CorePrimitives.Fn.parseOptionalDocstring(parameters,n,1);
      int priorBodyIdx = docstring == null ? 0 : 1;
      Datum body = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,priorBodyIdx);
      Datum generatorObjectDefinition = Pair.List(
        CorePrimitives.DEFINE,generatorObject,
          Pair.List(CorePrimitives.CONS,
            Pair.List(CorePrimitives.CONS,Pair.List(CorePrimitives.QUOTE,ESCM_GENERATOR),Boolean.FALSE),
            Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,
              Pair.List(CorePrimitives.CALL_CC,new Pair(CorePrimitives.LAMBDA,new Pair(Pair.List(ESCM_GENERATOR_ESCAPE),body))))));
      Datum generatorThunkBody = Pair.List(
        CorePrimitives.IF,Pair.List(ESCM_IS_GENERATORP,generatorObject),
          Pair.List(CorePrimitives.BEGIN,
            Pair.List(CorePrimitives.SET_BANG,generatorObject,Pair.List(Pair.List(CDR,generatorObject))),
            Pair.List(CorePrimitives.IF,Pair.List(ESCM_IS_GENERATORP,generatorObject),
              Pair.List(CDAR,generatorObject),
              generatorObject)),
          GlobalState.GLOBAL_GENERATOR_COMPLETE);
      if(docstring != null) {
        return Pair.List(CorePrimitives.DEFINE,bindings,docstring,
          generatorObjectDefinition,
          Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,docstring,generatorThunkBody));
      } else {
        return Pair.List(CorePrimitives.DEFINE,bindings,
          generatorObjectDefinition,
          Pair.List(CorePrimitives.LAMBDA,Nil.VALUE,generatorThunkBody));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (complete-all-generators! <generator-object> ...)
  //
  // (define (complete-all-generators! . generator-objects)
  //   (let loop ()
  //     (if (not (fold (lambda (acc result) (and acc (eq? result *generator-complete*)))
  //                    #t
  //                    (map (lambda (g) (g)) generator-objects)))
  //         (loop))))
  public static class CompleteAllGeneratorsBang extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "complete-all-generators!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("complete-all-generators!"),new Symbol("<generator-object>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Generators\nContinues cycling through the generators until they all complete.";
    }

    private Trampoline.Bounce iterate(ArrayList<Datum> parameters, int finishedCount, int n, Pair generatorObjects, Datum remainingObjects, Trampoline.Continuation continuation) throws Exception {
      if(finishedCount == n) return continuation.run(Void.VALUE);
      if(!(remainingObjects instanceof Pair)) return () -> iterate(parameters,0,n,generatorObjects,generatorObjects,continuation);
      Pair p = (Pair)remainingObjects;
      Datum g = p.car();
      Datum remainingObjectsTail = p.cdr();
      if(!(g instanceof Callable))
        throw new Exceptionf("'(complete-all-generators! <generator-object> ...) invalid <generator-object> %s: %s", g.profile(), Exceptionf.profileArgs(parameters));
      return ((Callable)g).callWith(new ArrayList<Datum>(),(gResult) -> () -> {
        int increment = gResult.eq(GlobalState.GLOBAL_GENERATOR_COMPLETE) ? 1 : 0;
        return iterate(parameters,finishedCount+increment,n,generatorObjects,remainingObjectsTail,continuation);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      Datum generatorObjects = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      if(!(generatorObjects instanceof Pair)) return continuation.run(Void.VALUE);
      Pair p = (Pair)generatorObjects;
      return iterate(parameters,0,p.length(),p,p,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (complete-n-generators! <count> <generator-object> ...)
  //
  // (define (complete-n-generators! n . generator-objects)
  //   (define total-generator-objects (length generator-objects))
  //   (let loop ()
  //     (define count 0)
  //     (if (not (fold (lambda (acc result) 
  //                       (if (eq? result *generator-complete*)
  //                           (set! count (+ count 1)))
  //                       (or acc (>= count n) (= count total-generator-objects)))
  //                    #f
  //                    (map (lambda (g) (g)) generator-objects)))
  //         (loop))))
  public static class CompleteNGeneratorsBang extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "complete-n-generators!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("complete-n-generators!"),new Symbol("<count>"),new Symbol("<generator-object>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Generators\nContinues cycling through the generators until at least <n> of them have completed.";
    }

    private Trampoline.Bounce iterate(ArrayList<Datum> parameters, int finishedCount, int n, Pair generatorObjects, Datum remainingObjects, Trampoline.Continuation continuation) throws Exception {
      if(finishedCount >= n || finishedCount == generatorObjects.length()) return continuation.run(Void.VALUE);
      if(!(remainingObjects instanceof Pair)) return () -> iterate(parameters,0,n,generatorObjects,generatorObjects,continuation);
      Pair p = (Pair)remainingObjects;
      Datum g = p.car();
      Datum remainingObjectsTail = p.cdr();
      if(!(g instanceof Callable))
        throw new Exceptionf("'(complete-n-generators! <count> <generator-object> ...) invalid <generator-object> %s: %s", g.profile(), Exceptionf.profileArgs(parameters));
      return ((Callable)g).callWith(new ArrayList<Datum>(),(gResult) -> () -> {
        int increment = gResult.eq(GlobalState.GLOBAL_GENERATOR_COMPLETE) ? 1 : 0;
        return iterate(parameters,finishedCount+increment,n,generatorObjects,remainingObjectsTail,continuation);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(complete-n-generators! <count> <generator-object> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum countDatum = parameters.get(0);
      if(!StreamPrimitives.ConvertStreamToList.isValidStreamIndex(countDatum))
        throw new Exceptionf("'(complete-n-generators! <count> <generator-object> ...) invalid <count>: %s", Exceptionf.profileArgs(parameters));
      Datum generatorObjects = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      if(!(generatorObjects instanceof Pair)) return continuation.run(Void.VALUE);
      Pair p = (Pair)generatorObjects;
      return iterate(parameters,0,((Real)countDatum).intValue(),p,p,continuation);
    }
  }
}