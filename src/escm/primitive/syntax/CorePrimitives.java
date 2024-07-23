// Author: Jordan Randleman - escm.primitive.syntax.CorePrimitives
// Purpose:
//    Java primitives for core standard syntax macros. All of this logic 
//    used to be implemented in a <stdlib.scm> file, however, we now 
//    implement such natively in Java to maximize EScheme's performance :)
//      => Note that the original EScheme code is still included in comments
//         throughout this file.

package escm.primitive.syntax;
import java.util.ArrayList;
import escm.util.UniqueSymbol;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Vector;
import escm.type.Hashmap;
import escm.type.Void;
import escm.type.Symbol;
import escm.type.number.Exact;
import escm.type.bool.Boolean;
import escm.type.procedure.Procedure;
import escm.type.procedure.PrimitiveProcedure;
import escm.vm.Compiler;
import escm.vm.util.Environment;
import escm.vm.type.collection.OrderedCollection;
import escm.vm.type.primitive.PrimitiveSyntax;
import escm.vm.type.primitive.PrimitiveSyntaxCallable;
import escm.vm.type.callable.Callable;
import escm.vm.type.callable.Signature;
import escm.primitive.MetaPrimitives;

public class CorePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Bytecode Symbols
  public static Symbol BYTECODE = new Symbol("bytecode");
  public static Symbol QUOTE = new Symbol("quote");
  public static Symbol LOAD = new Symbol("load");
  public static Symbol LOAD_CLOSURE = new Symbol("load-closure");
  public static Symbol IFN = new Symbol("ifn");
  public static Symbol JUMP = new Symbol("jump");


  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Macro Symbols
  public static Symbol DEFINE = new Symbol("define");
  public static Symbol SET_BANG = new Symbol("set!");
  public static Symbol DEFINEDP = new Symbol("defined?");
  public static Symbol IF = new Symbol("if");
  public static Symbol COND = new Symbol("cond");
  public static Symbol BEGIN = new Symbol("begin");
  public static Symbol FN = new Symbol("fn");
  public static Symbol LAMBDA = new Symbol("lambda");
  public static Symbol LET = new Symbol("let");
  public static Symbol LETREC = new Symbol("letrec");
  public static Symbol ARROW = new Symbol("=>");
  public static Symbol ELSE = new Symbol("else");
  public static Symbol QUASIQUOTE = new Symbol("quasiquote");
  public static Symbol UNQUOTE = new Symbol("unquote");
  public static Symbol UNQUOTE_SPLICING = new Symbol("unquote-splicing");
  public static Symbol ARROW_WAND = new Symbol("-<>");
  public static Symbol ARROW_WAND_ARG = new Symbol("<>");
  public static Symbol CURRY = new Symbol("curry");
  public static Symbol LET_VALUES = new Symbol("let-values");
  public static Symbol WHILE = new Symbol("while");


  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Procedure Symbols
  public static Symbol ESCM_DEFINE_SYNTAX = new Symbol("escm-define-syntax");
  public static Symbol LIST_TO_VECTOR = new Symbol("list->vector");
  public static Symbol LIST_TO_HASHMAP = new Symbol("list->hashmap");
  public static Symbol LIST = new Symbol("list");
  public static Symbol CONS = new Symbol("cons");
  public static Symbol FOLD = new Symbol("fold");
  public static Symbol CAR = new Symbol("car");
  public static Symbol CDR = new Symbol("cdr");
  public static Symbol NOT = new Symbol("not");
  public static Symbol APPEND = new Symbol("append");
  public static Symbol MEMBER = new Symbol("member");
  public static Symbol CALL_WITH_VALUES = new Symbol("call-with-values");
  public static Symbol CALL_CC = new Symbol("call-with-current-continuation");
  public static Symbol WITH_EXCEPTION_HANDLER = new Symbol("with-exception-handler");
  public static Symbol ESCM_GUARD_AUX = new Symbol("escm-guard-aux");
  public static Symbol RAISE = new Symbol("raise");
  public static Symbol APPLY = new Symbol("apply");
  public static Symbol VALUES = new Symbol("values");
  public static Symbol ESCM_DEFINE_PARAMETER = new Symbol("escm-define-parameter");
  public static Symbol ESCM_SET_PARAMETER_BANG = new Symbol("escm-set-parameter!");
  public static Symbol ESCM_GET_PARAMETER = new Symbol("escm-get-parameter");
  public static Symbol ESCM_IS_PARAMETER_P = new Symbol("escm-parameter?");

  
  ////////////////////////////////////////////////////////////////////////////
  // (bytecode <obj>)
  //
  // (define-syntax bytecode
  //   (lambda instructions instructions))
  //
  // NOTE: The EScheme compiler itself actually explicitly parses for bytecode 
  //       clauses, rather than using this Java primitive as a regular macro
  //       object. This allows us to get fast compile-time bytecode expansions,
  //       since we then don't want to perform a function call for each <bytecode> 
  //       instance. 
  //
  //       We still keep this Bytecode macro here though for for <help> purposes:
  //       that way users can use `(help bytecode)` to learn how to inline 
  //       instructions in EScheme source code, without checking EScheme's `doc` 
  //       directory.
  public static class Bytecode extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "bytecode";
    }

    public Datum signature() {
      return Pair.List(BYTECODE,new Symbol("<instruction>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\n# An Introduction to EScheme's Bytecode\n\n## Brief Insight\nEScheme's bytecode structure can be thought of as a chain of instruction sets \nserving as function bodies.\n\nThe instructions (as outlined below) are designed __exclusively__ to serve as a \nmeans by which to perform control flow and bind items to the current environment. \n\n___All logical operations are abstracted away via user-defined & primitive functions!___ \n\nSuch is the crux of our incredibly reduced instruction set, and the fact that \nwe can get away with having all of our instructions be either unary or nullary.\nFor example, unlike most flavors of assembly, there is no `add` instruction. \nRather, to us, `+` is called like any other function.\n\n\n## Data Manipulation\nAll data is handled via 2 mediums in our bytecode: the \"current value register\" \n(CVR), and the function stack.\n\nThe CVR psuedo-register is never explicitely referenced in EScheme bytecode, \nrather it is implicitly used by certain instructions. For example, compiling \n`(define life 42)` yields the bytecode `(load 42) (define life)`. The `load` \ninstruction implicitly sets CVR to its argument, and `define` implicitly binds \nits argument to CVR in the environment.\n\nThe function stack is referred to via the `push`, `pop`, and `call` instructions.\n`call` affects the stack because, due to our lack of multiple registers, we push \nour arguments (and the function we're calling) onto the stack rather than putting \nthem in registers. More details on the `call` instruction may be found below.\n\n\n## Why Write Bytecode?\nThe vast majority of the bytecode that the interpreter works with is the result of\ncompiling EScheme expressions. However, it may be desirable to write out bytecode \nby hand under very specific circumstances. \n\nFor example, while EScheme certainly supports iteration via recursion, there is no \n\"true\" iteration by default a la `for`/`while` loop in Java or C++. However, by \nleveraging inlined bytecode, we can write a `while` macro that __does__ have true\niteration (such is outlined as an example below)!\n\n\n## Writing Bytecode\nIn order to write out bytecode, simply follow the instruction syntax outlined below.\nNote that any arguments specified as `<symbol>` or `<integer>` MUST be symbolic or \ninteger literals respectively. For example, `jump` and `call` both accept integers, \nbut whereas `jump` requires that it's argument is an integer literal, `call` also \naccepts variables that evaluate to integers.\n\n### NIL\nA note on writing NIL literals: use `()`. For example, `(define n (quote ()))`\ncompiles to `(load ()) (define n)`. You may alternatively use the `#nil` reader\nliteral.\n\n### VOID\nA note on writing VOID literals: use `#void`. For example, `(define n #void)`\ncompiles to `(load #void) (define n)`.\n\n### Closures\nA note on writing closures: you may nest the `load-closure` syntax as needed in order\nto denote closures. The `load-closure` syntax is as follows:\n\n`(load-closure <optional-docstring> ((<argument> ...) <instruction> ...) ...)`\n\nThis syntax will load the equivalent of: `(fn <optional-docstring> ((<argument> ...) <instruction> ...) ...)`\ninto CVR. Further note that closure expressions by default return the value left in \nCVR upon terminating execution. `<docstring>` may be optionally provided to yield\nfurther information on the closure's intended purpose in the `help` menu.\n\nFor example, you may write:\n(define (my-function y) \n  (map (lambda (x) (* x y)) (list 1 2 3)))\n\nAs:\n(load-closure \n  ((y)\n    (push map)\n    (load-closure \n      ((x)\n        (push *)\n        (push x)\n        (push y)\n        (call 3)))\n    (push)\n    (push list)\n    (push 1)\n    (push 2)\n    (push 3)\n    (call 4)\n    (push)\n    (call 3)))\n(define my-function)\n\n### Object Access Chains\nThe escm VM has built-in support for interpretting object access chains, hence\n`(define obj.prop1.prop2)` is perfectly valid bytecode syntax.\n\nAs such, all instructions that set or evaluate a symbolic datum support this \nsyntax. These include: `define`, `set!`, `load`, `call`, `push`, & `return`.\n\n\n\n\n------------------------------------------------------------------------------\n# The EScheme Bytecode Instruction Set\n\n(define <symbol>) ; bind <symbol> to CVR [sets CVR to <void>]\n\n(set! <symbol>) ; set! <symbol> to CVR [sets CVR to <void>]\n\n(defined? <symbol>) ; determine if <symbol> is defined as a variable [sets CVR to the boolean result]\n\n(ifn <integer>) ; if CVR is NOT truthy, jump <integer> instructions [sets CVR to <void>]\n\n(jump <integer>) ; jump <integer> instructions\n\n(quote <datum>) ; quote <datum> and load it into CVR. may recurse infinitely for cyclic vectors/hashmaps.\n\n(load <datum>) ; evaluate <datum> and load it into CVR\n\n(call <datum>) ; <datum> must evaluate to an integer. get the fcn & arguments being applied from the stack. \n               ; positive <integer> denotes pushes from left to right & negative denotes pushes from right to left (when compiling the application expressions). \n               ; pops (abs <datum>) items off of the stack after the call, and places the returned value of the fcn application in CVR.\n\n(push)         ; push CVR to the stack\n(push <datum>) ; push <datum> to the stack\n\n(pop) ; pop a value off of the stack into CVR\n\n(return)         ; returns the value in CVR (effectively jumps to the end of the function)\n(return <datum>) ; returns the value in <datum> (effectively jumps to the end of the function)\n\n------------------------------------------------------------------------------\n## Helper `bytecode` Syntax\n### Psuedo-instruction(s) converted into \"real\" instruction(s) by the assembler\n\n(load-closure <optional-docstring> ((<argument> ...) <instruction> ...) ...) ; syntax to load a closure\n\n\n\n\n------------------------------------------------------------------------------\n# Examples\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; Demoing the compilation of closures\n\n;; Source Code\n(define (my-function y) \n  (map (lambda (x) (* x y)) (list 1 2 3)))\n\n;; Compiled Bytecode\n(load-closure \n  ((y)\n    (push map)\n    (load-closure \n      ((x)\n        (push *)\n        (push x)\n        (push y)\n        (call 3)))\n    (push)\n    (push list)\n    (push 1)\n    (push 2)\n    (push 3)\n    (call 4)\n    (push)\n    (call 3)))\n(define my-function)\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; Defining a \"(while (<condition> <return-expr> ...) <body> ...)\" macro using\n;; inlined bytecode & compilation\n\n(define-syntax while\n  (lambda (condition-returns . body)\n    (define compiled-condition (compile (car condition-returns)))\n    (define compiled-body (apply append (map compile body)))\n    (define compiled-returns (apply append (map compile (cdr condition-returns))))\n    `(bytecode \n      ,@compiled-condition\n      (ifn ,(+ (length compiled-body) 2))\n      ,@compiled-body\n      (jump ,(- 0 (length compiled-condition) (length compiled-body) 1))\n      (load #void)\n      ,@compiled-returns)))\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; Implementing \"apply\" in bytecode (impossible in native Scheme!)\n\n(load-closure \n  ((f args-list)\n    (load 1)\n    (define count)\n    (push f)\n    (push null?)\n    (push args-list)\n    (call 2)\n    (ifn 2)\n    (jump 15)\n    (push car)\n    (push args-list)\n    (call 2)\n    (push) ; save an extracted value\n    (push cdr)\n    (push args-list)\n    (call 2)\n    (set! args-list)\n    (push +)\n    (push 1)\n    (push count)\n    (call 3)\n    (set! count)\n    (jump -18)\n    (call count)))\n(define apply)\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;; Implementing factorial (because of course we do so)\n\n;; Source Code\n(define (factorial n)\n  (if (< n 2)\n      1\n      (* n (factorial (- n 1)))))\n\n;; Compiled Bytecode\n(load-closure \n  ((n)\n    (push <)\n    (push n)\n    (push 2)\n    (call 3)\n    (ifn 2)\n    (return 1)\n    (push *)\n    (push n)\n    (push factorial)\n    (push -)\n    (push n)\n    (push 1)\n    (call 3)\n    (push)\n    (call 2)\n    (push)\n    (call 3)))\n(define factorial)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum instructions = Nil.VALUE;
      for(int i = parameters.size()-1; i >= 0; --i) {
        instructions = new Pair(parameters.get(i),instructions);
      }
      return instructions;
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // (quote <obj>)
  //
  // (define-syntax quote
  //   (lambda (x)
  //     (list 
  //       (quote bytecode)
  //       (list (quote quote) x))))
  public static class Quote extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "quote";
    }

    public Datum signature() {
      return Pair.List(QUOTE,new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nReturns <obj> as a data structure. Note that the reader will expand \"'<obj>\"\nto \"(quote <obj>)\".\n\nNOTE: Never quote cyclic vectors/hashmaps during tricky (likely incorrect)\n      macro use! <quote> has to deep copy vectors and hashmaps at runtime, to\n      avoid sharing state in a single compiled function body between threads.\n\nFor example:\n  (quote a)  ; a\n  (quote 1)  ; 1\n  (quote \"\") ; \"\"\n  (quote #t) ; #t\n  (quote ()) ; #nil\n  (quote (1 2 . 3)) ; (cons 1 (cons 2 3))\n  (quote (1 2 3))   ; (list 1 2 3)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(quote <obj>) expects exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(BYTECODE,Pair.List(QUOTE,parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-syntax <name> <procedure>)
  //
  // (define-syntax define-syntax
  //   (lambda (name procedure)
  //     (list (quote escm-define-syntax) (list (quote quote) name) procedure)))
  public static class DefineSyntax extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-syntax";
    }

    public Datum signature() {
      return Pair.List(new Symbol("define-syntax"),new Symbol("<name>"),new Symbol("<callable>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nDefines <macro-name> to be a macro that uses to <callable>\nto perform its code expansion.\n\nFor example:\n  (define-syntax and\n    (lambda (. conditions)\n      (fold (lambda (acc item) (list (quote if) acc item #f))\n            #t\n            conditions)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(define-syntax <symbol> <callable>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum name = parameters.get(0);
      if(!(name instanceof Symbol))
        throw new Exceptionf("'(define-syntax <symbol> <callable>) 1st arg %s isn't a symbol: %s", name.profile(), Exceptionf.profileArgs(parameters));
      return Pair.List(ESCM_DEFINE_SYNTAX,Pair.List(QUOTE,(Symbol)name),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (fn <optional-docstring> ((<param> ...) <body> ...) ...)
  //   => NOTE: Supports optional parameters via "(<param> <dflt-value>)" syntax
  // 
  // NOTE: The EScheme code below doesn't account for allowing mixing optional
  // ===== parameters with a variadic parameter too, nor parsing <optional-docstring>.
  //
  // (define (escm-fn-has-optional-parameters? params)
  //   (if (pair? params) 
  //       (if (list? params) 
  //           (pair? (last params))
  //           #f) 
  //       #f))
  //
  // (define (escm-fn-generate-expanded-clauses clause)
  //   (define params (car clause))
  //   (define body (cdr clause))
  //   (define req-params (filter atom? params))
  //   (define opt-params (filter pair? params))
  //   (define param-defs (map (lambda (p) (cons (quote define) p)) opt-params))
  //   (define n (length opt-params)) 
  //   (define expanded-clauses (quote ()))
  //   (define opt-param-names (map car opt-params))
  //   (while ((>= n 0) expanded-clauses)
  //     (set! expanded-clauses
  //       (cons 
  //         (cons 
  //           (append req-params (slice opt-param-names 0 n)) 
  //           (append (slice param-defs n) body))
  //         expanded-clauses))
  //     (set! n (- n 1))))
  //
  // (define (escm-fn-expand-clause clause)
  //   (if (escm-fn-has-optional-parameters? (car clause))
  //       (escm-fn-generate-expanded-clauses clause)
  //       (list clause)))
  //
  // (define-syntax fn
  //   (lambda clauses
  //     (set! clauses (apply append (map escm-fn-expand-clause clauses)))
  //     (list 
  //       (quote bytecode)
  //       (cons 
  //         (quote load-closure)
  //         (map
  //           (lambda (c)
  //             (cons 
  //               (car c)
  //               (apply append (map compile (cdr c)))))
  //           clauses)))))
  public static class Fn extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "fn";
    }

    public Datum signature() {
      Datum fnSignatures = Pair.List(
        Pair.List(Pair.List(),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("<variadic-parameter>"),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<parameter>"),Signature.VARIADIC,new Symbol("."),new Symbol("<variadic-parameter>")),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<param>"),Signature.VARIADIC,
                            Pair.List(new Symbol("<optional-param>"),new Symbol("<default-value>")),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<param>"),Signature.VARIADIC,
                            Pair.List(new Symbol("<optional-param>"),new Symbol("<default-value>")),Signature.VARIADIC,
                            new Symbol("."),new Symbol("<variadic-parameter>")),
          new Symbol("<body>"),Signature.VARIADIC),
        Signature.VARIADIC);
      return Pair.List(new Pair(FN,fnSignatures),new Pair(FN,new Pair(new Symbol("<docstring>"),fnSignatures)));
    }

    public String docstring() {
      return "@help:Syntax:Core\nCreate a multi-arity procedure, with support for optional parameters via the\n\"(<param-name> <default-value>)\" syntax. Denote variadic parameters via the\n\"(<param> ... . <variadic-param>)\" or \"<variadic-param>\" syntax.\n\nOptionally include <docstring> to yield further details on the procedure's\nintended purpose in the <help> menu.\n\nFor example:\n  ; Using Mult-arity\n  (define factorial \n    (fn ((n) (factorial n 1))\n        ((n p)\n          (if (< n 2)\n              p\n              (factorial (- n 1) (* n p))))))\n\n\n  ; Using optional parameters: p is <1> by default\n  (define factorial\n    (fn ((n (p 1))\n          (if (< n 2)\n              p\n              (factorial (- n 1) (* n p))))))\n\n\n  ; Combine <fn> and <define> via <defn>:\n  (defn factorial \n    ((n) (factorial n 1))\n    ((n p)\n      (if (< n 2)\n          p\n          (factorial (- n 1) (* n p)))))";
    }

    // Extract <docstring> if exists, else <null>
    public static escm.type.String parseOptionalDocstring(ArrayList<Datum> parameters, int n, int docstringIdx) {
      if(n > docstringIdx+1) {
        Datum d = parameters.get(docstringIdx);
        if(d instanceof escm.type.String) return (escm.type.String)d;
      }
      return null;
    }

    // Parse out required params (returned) & optional params (in <PairBox>)
    private static class PairBox {
      private Pair value = null;
    }

    private static Datum parseClauseParameters(Datum params, PairBox optionalParams, ArrayList<Datum> parameters) throws Exception {
      if(!(params instanceof Pair)) {
        // should be unreachable since we know we have some optional args in <params>
        throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid parameters %s: %s", params.profile(), Exceptionf.profileArgs(parameters));
      }
      Pair p = (Pair)params;
      Datum param = p.car();
      if(param instanceof Pair) {
        optionalParams.value = p;
        return Nil.VALUE;
      }
      return new Pair(param,parseClauseParameters(p.cdr(),optionalParams,parameters));
    }

    // Extract names of optional params (.first) & create <define> expressions from optional params (.second)
    private static escm.util.Pair<Datum,Datum> parseOptionalNamesAndDefs(Datum optionalParams) throws Exception {
      if(!(optionalParams instanceof Pair)) {
        if(optionalParams instanceof Symbol) {
          return new escm.util.Pair<Datum,Datum>(optionalParams,Pair.List(Pair.List(DEFINE,optionalParams,Nil.VALUE)));
        }
        return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      }
      Pair p = (Pair)optionalParams;
      escm.util.Pair<Datum,Datum> tails = parseOptionalNamesAndDefs(p.cdr());
      Pair optionalParam = (Pair)p.car();
      return new escm.util.Pair<Datum,Datum>(
        new Pair(optionalParam.car(),tails.first),
        new Pair(new Pair(DEFINE,optionalParam),tails.second));
    }

    // Return the function clause body
    private static Pair getClauseBody(Datum body) {
      if(body instanceof Pair) return (Pair)body;
      return new Pair(Void.VALUE,Nil.VALUE);
    }

    // Compile a clause body
    public static Trampoline.Bounce applyAppendMapCompile(Environment definitionEnvironment, Datum body, Datum compiledBody, Trampoline.Continuation continuation) throws Exception {
      if(!(body instanceof Pair)) return continuation.run(compiledBody);
      Pair p = (Pair)body;
      return Compiler.run(p.car(),definitionEnvironment,(compiledComponent) -> () -> {
        return applyAppendMapCompile(definitionEnvironment,p.cdr(),Pair.binaryAppend(compiledBody,compiledComponent),continuation);
      });
    }

    private static Trampoline.Bounce compileClauseBody(Environment defEnv, Datum body, Datum compiledBody, Trampoline.Continuation continuation) throws Exception {
      return applyAppendMapCompile(defEnv,body,compiledBody,continuation);
    }

    // Generate series of compiled <define> expansions to convert 1 optional-param clause into 2+ required-param clauses
    private static Trampoline.Bounce generateClauseDefineSequences(Environment defEnv, Datum requiredParams, Pair optionalNames, Pair optionalDefs, Datum compiledBody, int totalOptionalParams, int i, Datum expandedClauses, Trampoline.Continuation continuation) throws Exception {
      if(i < 0) return continuation.run(expandedClauses);
      if(i == totalOptionalParams) {
        Datum params = Pair.binaryAppend(requiredParams,optionalNames);
        return generateClauseDefineSequences(
          defEnv,
          requiredParams,
          optionalNames,
          optionalDefs,
          compiledBody,
          totalOptionalParams,
          i-(optionalNames.length()==Pair.DOTTED_LIST_LENGTH ? 2 : 1),
          new Pair(new Pair(params,compiledBody),expandedClauses),
          continuation);
      } else {
        Datum params = Pair.binaryAppend(requiredParams,(Datum)optionalNames.slice(0,i));
        return compileClauseBody(defEnv,(Datum)optionalDefs.slice(i),Nil.VALUE,(compiledDefines) -> () -> {
          return generateClauseDefineSequences(
            defEnv,
            requiredParams,
            optionalNames,
            optionalDefs,
            compiledBody,
            totalOptionalParams,
            i-1,
            new Pair(new Pair(params,Pair.binaryAppend(compiledDefines,compiledBody)),expandedClauses),
            continuation);
        });
      }
    }

    // Convert 1 optional params clause into 2+ required params clauses
    private static Trampoline.Bounce getExpandedClause(Environment defEnv, Pair clause, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      PairBox optionalParams = new PairBox();
      Datum requiredParams = parseClauseParameters((Pair)clause.car(),optionalParams,parameters);
      escm.util.Pair<Datum,Datum> namesAndDefs = parseOptionalNamesAndDefs(optionalParams.value);
      Pair optionalNames = (Pair)namesAndDefs.first;
      Pair optionalDefs = (Pair)namesAndDefs.second;
      int totalOptionalParams = optionalDefs.length();
      return compileClauseBody(defEnv,getClauseBody(clause.cdr()),Nil.VALUE,(compiledBody) -> () -> {
        return generateClauseDefineSequences(defEnv,requiredParams,optionalNames,optionalDefs,compiledBody,totalOptionalParams,totalOptionalParams,Nil.VALUE,continuation);
      });
    }

    // Determine if clause follows basic required syntax
    private static boolean isValidClause(Datum clause) {
      if(!(clause instanceof Pair)) return false;
      Datum params = ((Pair)clause).car();
      return params instanceof Pair || params instanceof Nil || params instanceof Symbol;
    }

    // Type-check clause & return if has any optional params
    private static boolean clauseHasOptionalParam(Datum clause, ArrayList<Datum> parameters) throws Exception {
      if(!isValidClause(clause))
        throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid clause %s: %s", clause.profile(), Exceptionf.profileArgs(parameters));
      Datum params = ((Pair)clause).car();
      Datum iter = params;
      if(!(params instanceof Pair)) return false;
      boolean hasOptional = false;
      while(iter instanceof Pair) {
        Pair p = (Pair)iter;
        Datum param = p.car();
        boolean paramIsPair = param instanceof Pair;
        // verify that all optional args are clumped (no more required args)
        if(hasOptional && !paramIsPair)
          throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid parameters %s: %s", params.profile(), Exceptionf.profileArgs(parameters));
        if(paramIsPair) {
          hasOptional = true;
          // verify first item in optional arg syntax is an arg name symbol
          if(!(((Pair)param).car() instanceof Symbol))
            throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid parameters %s: %s", params.profile(), Exceptionf.profileArgs(parameters));
        } else if(!(param instanceof Symbol)) {
          // verify required arg syntax is an arg name symbol
          throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid parameters %s: %s", params.profile(), Exceptionf.profileArgs(parameters));
        }
        iter = p.cdr();
      }
      if(!(iter instanceof Nil) && !(iter instanceof Symbol))
        throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) invalid parameters %s: %s", params.profile(), Exceptionf.profileArgs(parameters));
      return hasOptional;
    }

    // Compile a clause w/o optional parameters
    private static Trampoline.Bounce compileSingleClause(Environment defEnv, Pair clause, Trampoline.Continuation continuation) throws Exception {
      return compileClauseBody(defEnv,clause.cdr(),Nil.VALUE,(compiledBody) -> () -> {
        return continuation.run(new Pair(clause.car(),compiledBody));
      });
    }

    // Convert all of the function's clauses w/ optional params to clauses w/ required params.
    // Also compile each clause's body.
    private static Trampoline.Bounce getExpandedClauses(Environment defEnv, ArrayList<Datum> parameters, int i, int n, Datum expandedClauses, Trampoline.Continuation continuation) throws Exception {
      if(i >= n) return continuation.run(expandedClauses);
      Datum clause = parameters.get(i);
      if(clauseHasOptionalParam(clause,parameters)) {
        return getExpandedClause(defEnv,(Pair)clause,parameters,(expandedClause) -> () -> {
          return getExpandedClauses(defEnv,parameters,i+1,n,new Pair(expandedClause,expandedClauses),continuation);
        });
      } else {
        return compileSingleClause(defEnv,(Pair)clause,(compiledClause) -> () -> {
          return getExpandedClauses(defEnv,parameters,i+1,n,new Pair(Pair.List(compiledClause),expandedClauses),continuation);
        });
      }
    }

    // Convert list of lists to a single list
    public static Datum applyAppend(Datum listOfLists) throws Exception {
      Datum appended = Nil.VALUE;
      while(listOfLists instanceof Pair) {
        Pair p = (Pair)listOfLists;
        appended = Pair.binaryAppend(p.car(),appended);
        listOfLists = p.cdr();
      }
      return appended;
    }

    public static Trampoline.Bounce logic(Environment defEnv, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      escm.type.String docstring = parseOptionalDocstring(parameters,n,0);
      int minFnSize = docstring == null ? 1 : 2;
      if(n < minFnSize)
        throw new Exceptionf("'(fn <optional-docstring> ((<param> ...) <body> ...) ...) needs at least 1 body clause: %s", Exceptionf.profileArgs(parameters));
      if(docstring == null) {
        return getExpandedClauses(defEnv,parameters,0,n,Nil.VALUE,(expandedCompiledClauses) -> () -> {
          return continuation.run(Pair.List(BYTECODE,new Pair(LOAD_CLOSURE,applyAppend(expandedCompiledClauses))));
        }); 
      } else {
        return getExpandedClauses(defEnv,parameters,1,n,Nil.VALUE,(expandedCompiledClauses) -> () -> {
          return continuation.run(Pair.List(BYTECODE,new Pair(LOAD_CLOSURE,new Pair(docstring,applyAppend(expandedCompiledClauses)))));
        });
      }
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return logic(this.definitionEnvironment,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (lambda (<param> ...) <optional-docstring> <body> ...)
  //   => NOTE: Supports optional parameters via "(<param> <dflt-value>)" syntax
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>.
  // =====
  //
  // (define-syntax lambda
  //   (fn ((params . body)
  //         (list (quote fn) (cons params body)))))
  public static class Lambda extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "lambda";
    }

    public Datum signature() {
      Symbol BODY = new Symbol("<body>");
      Symbol PARAMETER = new Symbol("<parameter>");
      Symbol PARAM = new Symbol("<param>");
      Symbol OPTIONAL_PARAM = new Symbol("<optional-param>");
      Symbol DEFAULT_VALUE = new Symbol("<default-value>");
      Symbol DOT = new Symbol(".");
      Symbol VARIADIC_PARAM = new Symbol("<variadic-parameter>");
      Symbol DOCSTRING = new Symbol("<docstring>");
      return Pair.List(
        Pair.List(LAMBDA,Pair.List(),BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,Pair.List(),DOCSTRING,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,VARIADIC_PARAM,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,VARIADIC_PARAM,DOCSTRING,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,Pair.List(PARAMETER,Signature.VARIADIC),BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,Pair.List(PARAMETER,Signature.VARIADIC),DOCSTRING,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,Pair.List(PARAMETER,Signature.VARIADIC,DOT,VARIADIC_PARAM),BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,Pair.List(PARAMETER,Signature.VARIADIC,DOT,VARIADIC_PARAM),DOCSTRING,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,
          Pair.List(PARAM,Signature.VARIADIC,Pair.List(OPTIONAL_PARAM,DEFAULT_VALUE),Signature.VARIADIC),
          BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,
          Pair.List(PARAM,Signature.VARIADIC,Pair.List(OPTIONAL_PARAM,DEFAULT_VALUE),Signature.VARIADIC),
          DOCSTRING,BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,
          Pair.List(PARAM,Signature.VARIADIC,Pair.List(OPTIONAL_PARAM,DEFAULT_VALUE),Signature.VARIADIC,DOT,VARIADIC_PARAM),
          BODY,Signature.VARIADIC),
        Pair.List(LAMBDA,
          Pair.List(PARAM,Signature.VARIADIC,Pair.List(OPTIONAL_PARAM,DEFAULT_VALUE),Signature.VARIADIC,DOT,VARIADIC_PARAM),
          DOCSTRING,BODY,Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\nShorthand for single-arity procedures. Expands to <fn> under the hood.\nSupports optional parameters via the \"(<param-name> <default-value>)\"\nsyntax. Denote variadic parameters via \"(<param> ... . <variadic-param>)\"\nor \"<variadic-param>\" syntax. Reader-shorthand support via the <#(> syntax.\n\nOptionally include <docstring> to yield further details on the procedure's\nintended purpose in the <help> menu.\n\nFor example:\n  ; Using optional parameters: p is <1> by default\n  (define factorial\n    (lambda (n (p 1))\n      (if (< n 2)\n          p\n          (factorial (- n 1) (* n p)))))\n\n\n  ; Combine <lambda> and <define> via:\n  (define (factorial n (p 1))\n    (if (< n 2)\n        p\n        (factorial (- n 1) (* n p))))\n\n\n  ;; Reader-shorthand via <#(>: #<expr> => (lambda () <expr>)\n  ;; Denote the Nth parameter via %N, with N starting at 1. \n  ;; Use %% to denote the variadic parameter.\n\n  ; Add 2:\n  (define add2 #(+ %1 2))\n\n  ; Get the 2nd parameter:\n  (define get-2nd #(begin %2))\n\n  ; The 'id' procedure:\n  (define id #(begin %1))\n\n  ; The 'list' procedure:\n  (define list #(begin %%))";
    }

    public static Datum getAllExpressionsAfter(ArrayList<Datum> parameters, int startingIdx) {
      Datum body = Nil.VALUE;
      for(int i = parameters.size()-1; i > startingIdx; --i) {
        body = new Pair(parameters.get(i),body);
      }
      return body;
    }

    public static Trampoline.Bounce logic(Environment defEnv, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(lambda (<param> ...) <optional-docstring> <body> ...) didn't receive (<param> ...): %s", Exceptionf.profileArgs(parameters));
      Datum params = parameters.get(0);
      escm.type.String docstring = Fn.parseOptionalDocstring(parameters,n,1);
      int priorBodyIdx = docstring == null ? 0 : 1;
      Datum body = getAllExpressionsAfter(parameters,priorBodyIdx);
      if(body instanceof Nil) body = new Pair(Void.VALUE,Nil.VALUE);
      parameters.clear();
      if(docstring != null) parameters.add(docstring);
      parameters.add(new Pair(params,body));
      return Fn.logic(defEnv,parameters,continuation);
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return logic(this.definitionEnvironment,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (if <condition> <consequence> <alternative>)
  // (if <condition> <consequence>)
  //
  // (define-syntax if
  //   (fn ((condition consequence)
  //         (bytecode
  //           (push #void)
  //           (push compile)
  //           (call -2)
  //           (define compiled-alternative)
  //           (push consequence)
  //           (push compile)
  //           (call -2)
  //           (define compiled-consequence))
  //         (append
  //           (cons 
  //             (quote bytecode) 
  //             (compile condition))
  //           (cons 
  //             (list (quote ifn) (+ 2 (length compiled-consequence)))
  //             compiled-consequence)
  //           (cons
  //             (list (quote jump) (+ 1 (length compiled-alternative)))
  //             compiled-alternative)))
  //       ((condition consequence alternative)
  //         (bytecode
  //           (push alternative)
  //           (push compile)
  //           (call -2)
  //           (define compiled-alternative)
  //           (push consequence)
  //           (push compile)
  //           (call -2)
  //           (define compiled-consequence))
  //         (append
  //           (cons 
  //             (quote bytecode) 
  //             (compile condition))
  //           (cons 
  //             (list (quote ifn) (+ 2 (length compiled-consequence)))
  //             compiled-consequence)
  //           (cons
  //             (list (quote jump) (+ 1 (length compiled-alternative)))
  //             compiled-alternative)))))
  public static class If extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "if";
    }

    public Datum signature() {
      return Pair.List(
              Pair.List(IF,new Symbol("<condition>"),new Symbol("<consequence>")),
              Pair.List(IF,new Symbol("<condition>"),new Symbol("<consequence>"),new Symbol("<alternative>")));
    }

    public String docstring() {
      return "@help:Syntax:Core\nConditional branching:\n  if (not (eq? #f <condition>)): execute <consequence>\n  else: execute <alternative> (defaults to #void).";
    }

    private Trampoline.Bounce logic(ArrayList<Datum> parameters, Datum condition, Datum consequence, Datum alternative, Trampoline.Continuation continuation) throws Exception {
      return Compiler.run(condition,this.definitionEnvironment,(compiledCondition) -> () -> {
        return Compiler.run(consequence,this.definitionEnvironment,(compiledConsequence) -> () -> {
          return Compiler.run(alternative,this.definitionEnvironment,(compiledAlternative) -> () -> {
            return continuation.run(
              Pair.binaryAppend(
                new Pair(BYTECODE,compiledCondition),
                Pair.binaryAppend(
                  new Pair(Pair.List(IFN,new Exact(2+((OrderedCollection)compiledConsequence).length())),compiledConsequence),
                  new Pair(Pair.List(JUMP,new Exact(1+((OrderedCollection)compiledAlternative).length())),compiledAlternative))));
          });
        });
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 2 || n > 3)
        throw new Exceptionf("'(if <condition> <consequence> <optional-alternative>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 2) {
        return logic(parameters,parameters.get(0),parameters.get(1),Void.VALUE,continuation);
      } else { // if(n == 3)
        return logic(parameters,parameters.get(0),parameters.get(1),parameters.get(2),continuation);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (begin <expr> ...)
  //
  // (define-syntax begin
  //   (lambda (. exprs)
  //     (cons 
  //       (quote bytecode)
  //       (apply append (map compile exprs)))))
  public static class Begin extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "begin";
    }

    public Datum signature() {
      return Pair.List(BEGIN,new Symbol("<expression>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nCombines a series of EScheme expressions into a single expression.\nSimilar to how {} is used in C++/Java, BUT <begin> does NOT have\nits own scope (use <let> for such instead).";
    }

    private Trampoline.Bounce iterate(ArrayList<Datum> parameters, int i, Datum compiledExpressions, Trampoline.Continuation continuation) throws Exception {
      if(i < 0) return continuation.run(new Pair(BYTECODE,compiledExpressions));
      return Compiler.run(parameters.get(i),this.definitionEnvironment,(compiledExpression) -> () -> {
        return iterate(parameters,i-1,Pair.binaryAppend(compiledExpression,compiledExpressions),continuation);
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      return iterate(parameters,parameters.size()-1,Nil.VALUE,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (set! <var> <value>)
  // (set! <var1> <var2> ... <varN> <N-length-list-expression>) 
  //
  // NOTE: The EScheme code below doesn't account for multiple variable bindings.
  //
  // (define-syntax set!
  //   (lambda (var val)
  //     (append 
  //       (cons (quote bytecode)
  //             (compile val))
  //       (list (list (quote set!) var)))))
  public static class SetBang extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "set!";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(SET_BANG,new Symbol("<symbol>"),new Symbol("<obj>")),
        Pair.List(SET_BANG,new Symbol("<symbol1>"),new Symbol("<symbol2>"),Signature.VARIADIC,new Symbol("<symbolN>"),new Symbol("<N-length-list-expression>")));
    }

    public String docstring() {
      return "@help:Syntax:Core\nAssigns <symbol> to <obj>. <symbol> must have been previously\ndefined. Note that <symbol> may be an object property chain too,\nhence (set! obj.prop 42) is valid syntax!\n\nMay assign several variables to values in a list. For example:\n(set! a b (list 1 2)) sets variables <a> to 1 and <b> to 2.\n  * Alias <list> with <ls> to quickly create multiple values!";
    }

    public static Trampoline.Bounce expandCompoundBindings(String opName, Symbol opSymbol, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      Symbol hidden = UniqueSymbol.generate("compound-"+opName);
      Datum listCreationExpression = parameters.get(n-1);
      Datum beginExpression = Nil.VALUE;
      for(int i = n-2; i >= 0; --i) {
        Datum variable = parameters.get(i);
        if(!(variable instanceof Symbol))
          throw new Exceptionf("'("+opName+" <symbol1> <symbol2> ... <symbolN> <N-length-list-expression>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        beginExpression = new Pair(Pair.List(opSymbol,variable,Pair.List(CAR,hidden)),beginExpression);
        if(i == 0) {
          beginExpression = new Pair(Pair.List(DEFINE,hidden,listCreationExpression),beginExpression);
        } else {
          beginExpression = new Pair(Pair.List(SET_BANG,hidden,Pair.List(CDR,hidden)),beginExpression);
        }
      }
      return continuation.run(new Pair(BEGIN,beginExpression));
    }

    private Trampoline.Bounce compileSimpleSetBang(Datum variable, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      Datum value = parameters.size() == 2 ? parameters.get(1) : Void.VALUE;
      return Compiler.run(value,this.definitionEnvironment,(compiledValue) -> () -> {
        return continuation.run(Pair.binaryAppend(new Pair(BYTECODE,compiledValue),Pair.List(Pair.List(SET_BANG,variable))));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(set! <symbol> <obj>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum variable = parameters.get(0);
      if(!(variable instanceof Symbol))
        throw new Exceptionf("'(set! <symbol> <obj>) <symbol> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      if(n <= 2) return compileSimpleSetBang(variable,parameters,continuation);
      return expandCompoundBindings("set!",SET_BANG,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define <var> <value>) 
  // (define <var1> <var2> ... <varN> <N-length-list-expression>) 
  // (define (<function-name> <param> ...) <optional-docstring> <body> ...)
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>,
  //       nor multiple variable bindings.
  //
  //       For a multiple variable bindings example,
  //
  //         (def a b c <list-creation-expr>)
  //        
  //         becomes
  //        
  //         (begin
  //           (def hidden <list-creation-expr>)
  //           (def a (car hidden))
  //           (set! hidden (cdr hidden))
  //           (def b (car hidden))
  //           (set! hidden (cdr hidden))
  //           (def c (car hidden)))
  // =====
  //
  // (define-syntax define
  //   (lambda (bindings . vals)
  //     (cons 
  //       (quote bytecode)
  //       (if (symbol? bindings)
  //           (append
  //             (compile (car vals))
  //             (list (list (quote define) bindings)))
  //           (append 
  //             (compile
  //               (cons 
  //                 (quote lambda)
  //                 (cons 
  //                   (cdr bindings)
  //                   vals)))
  //             (list (list (quote define) (car bindings))))))))
  public static class Define extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "define";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(DEFINE,new Symbol("<symbol>"),new Symbol("<obj>")),
        Pair.List(DEFINE,new Symbol("<symbol1>"),new Symbol("<symbol2>"),Signature.VARIADIC,new Symbol("<symbolN>"),new Symbol("<N-length-list-expression>")),
        Pair.List(DEFINE,Pair.List(new Symbol("<function-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(DEFINE,Pair.List(new Symbol("<function-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<docstring>"),new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\nBinds <symbol> to <obj> in the current environment. Note that <symbol> \nmay be an object property chain too, hence (define obj.prop 42) is valid \nsyntax!\n\nMay bind several variables to values in a list. For example:\n(define a b (list 1 2)) binds variables <a> to 1 and <b> to 2.\n  * Alias <list> with <ls> to quickly create multiple values!\n\nThe 3rd signature is equivalent to:\n  (define <function-name> (lambda (<parameter> ...) <obj>))\n\nOptionally include <docstring> to yield further details on\nthe procedure's intended purpose in the <help> menu.\n\nAliased by <def>.";
    }

    private Trampoline.Bounce compileSimpleDefine(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      Datum bindings = parameters.get(0);
      Datum value = parameters.size() == 2 ? parameters.get(1) : Void.VALUE;
      return Compiler.run(value,this.definitionEnvironment,(compiledValue) -> () -> {
        return continuation.run(Pair.binaryAppend(new Pair(BYTECODE,compiledValue),Pair.List(Pair.List(DEFINE,bindings))));
      });
    }

    private Trampoline.Bounce compileProcedureDefine(Datum bindings, int n, ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(!(bindings instanceof Pair))
        throw new Exceptionf("'(define (<function-name> <param> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Pair bindingsPair = (Pair)bindings;
      Datum functionName = bindingsPair.car();
      if(!(functionName instanceof Symbol))
        throw new Exceptionf("'(define (<function-name> <param> ...) <body> ...) <function-name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      parameters.set(0,bindingsPair.cdr());
      return Lambda.logic(this.definitionEnvironment,parameters,(expandedLambda) -> () -> {
        return continuation.run(Pair.binaryAppend(expandedLambda,Pair.List(Pair.List(DEFINE,functionName))));
      });
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(define <symbol> <value>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      if(bindings instanceof Symbol) {
        if(n <= 2) return compileSimpleDefine(parameters,continuation);
        return SetBang.expandCompoundBindings("define",DEFINE,parameters,continuation);
      }
      return compileProcedureDefine(bindings,n,parameters,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (defined? <var>)
  //
  // (define-syntax defined?
  //   (lambda (var)
  //     (list (quote bytecode) (list (quote defined?) var))))
  public static class IsDefinedP extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "defined?";
    }

    public Datum signature() {
      return Pair.List(DEFINEDP,new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nChecks if <symbol> is defined in the current environment.\nAliased by <def?>. Note that <symbol> may be an object\nproperty chain too, hence (defined? obj.prop) is valid syntax!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(defined? <symbol>) expects exactly 1 symbol arg: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(BYTECODE,Pair.List(DEFINEDP,parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (defn <function-name> <optional-docstring> ((<param> ...) <body> ...) ...)
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>.
  // =====
  //
  // (define-syntax defn
  //   (lambda (name . clauses)
  //     (list (quote define) name (cons (quote fn) clauses))))
  public static class Defn extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "defn";
    }

    public Datum signature() {
      Symbol DEFN = new Symbol("defn");
      Symbol FCN_NAME = new Symbol("<function-name>");
      Datum defnSignatures = Pair.List(
        Pair.List(Pair.List(),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("<variadic-parameter>"),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<parameter>"),Signature.VARIADIC,new Symbol("."),new Symbol("<variadic-parameter>")),new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<param>"),Signature.VARIADIC,
                            Pair.List(new Symbol("<optional-param>"),new Symbol("<default-value>")),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(Pair.List(new Symbol("<param>"),Signature.VARIADIC,
                            Pair.List(new Symbol("<optional-param>"),new Symbol("<default-value>")),Signature.VARIADIC,
                            new Symbol("."),new Symbol("<variadic-parameter>")),
          new Symbol("<body>"),Signature.VARIADIC),
        Signature.VARIADIC);
      return Pair.List(
        new Pair(DEFN,new Pair(FCN_NAME,defnSignatures)),
        new Pair(DEFN,new Pair(FCN_NAME,new Pair(new Symbol("<docstring>"),defnSignatures))));
    }

    public String docstring() {
      return "@help:Syntax:Core\nCombines <define> and <fn> by binding <function-name> to\n(fn (<parameters> <body> ...) ...)\n\nOptionally include <docstring> to yield further details\non the procedure's intended purpose in the <help> menu.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 2)
        throw new Exceptionf("'(defn <function-name> <optional-docstring> ((<param> ...) <body> ...) ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum name = parameters.get(0);
      if(!(name instanceof Symbol))
        throw new Exceptionf("'(defn <function-name> <optional-docstring> ((<param> ...) <body> ...) ...) <function-name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      Datum clauses = Nil.VALUE;
      for(int i = n-1; i > 0; --i) {
        clauses = new Pair(parameters.get(i),clauses);
      }
      return Pair.List(DEFINE,name,new Pair(FN,clauses));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (and <obj> ...)
  //
  // (define-syntax and
  //   (lambda (. conditions)
  //     (fold (lambda (acc item) (list (quote if) acc item #f))
  //           #t
  //           conditions)))
  public static class And extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "and";
    }

    public Datum signature() {
      return Pair.List(new Symbol("and"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nReturns whether none of \"<obj> ...\" are #f.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 1) return parameters.get(0);
      Datum ifs = Boolean.TRUE;
      for(int i = 0; i < n; ++i) {
        ifs = Pair.List(IF,ifs,parameters.get(i),Boolean.FALSE);
      }
      return ifs;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (or <obj> ...)
  //
  // (define-syntax or
  //   (lambda (. conditions)
  //     (define or-value (gensym 'or-value))
  //     (fold-right (lambda (item acc)
  //                   (list (list (quote lambda) (list or-value)
  //                               (list (quote if) or-value or-value acc))
  //                         item))
  //                 #f
  //                 conditions)))
  public static class Or extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "or";
    }

    public Datum signature() {
      return Pair.List(new Symbol("or"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nReturns whether any of \"<obj> ...\" are #t.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n == 1) return parameters.get(0);
      Datum ifs = Boolean.FALSE;
      for(int i = n-1; i >= 0; --i) {
        Symbol cachedName = UniqueSymbol.generate("or-value");
        ifs = Pair.List(Pair.List(LAMBDA,Pair.List(cachedName),Pair.List(IF,cachedName,cachedName,ifs)),parameters.get(i));
      }
      return ifs;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (delay <obj>)
  //
  // (define-syntax delay
  //   (lambda (x)
  //     (define forced? (gensym 'delay-forced?))
  //     (define result (gensym 'delay-result))
  //     (list (quote let) (list (list forced? #f) (list result #f))
  //       (list (quote lambda) (quote ())
  //         (list (quote if) forced?
  //             result
  //             (list (quote begin)
  //               (list (quote set!) forced? #t)
  //               (list (quote set!) result x)
  //               result))))))
  public static class Delay extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "delay";
    }

    public Datum signature() {
      return Pair.List(new Symbol("delay"),new Symbol("<expression>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nDelay <expression>'s evaluation until it is forced via <force>.\nFor example: (force (delay (+ 1 2))) ; => 3";
    }

    private static class DatumBox {
      private Datum value = Boolean.FALSE;
      public synchronized Datum value() {return value;}
      public synchronized void setValue(Datum v) {value = v;}
    }

    private static class BooleanBox {
      private boolean value = false;
      public synchronized boolean value() {return value;}
      public synchronized void setValue(boolean v) {value = v;}
    }

    public static Datum valueOf(Datum delayedCode, Environment env) throws Exception {
      DatumBox result = new DatumBox();
      BooleanBox isForced = new BooleanBox();
      return new PrimitiveProcedure(Procedure.DEFAULT_NAME,new Callable() {
        public String docstring() {
          return "Generated <delay> expression.";
        }
        public Datum signature() {
          return Pair.List(new Symbol(Procedure.DEFAULT_NAME));
        }
        public Trampoline.Bounce callWith(ArrayList<Datum> params, Trampoline.Continuation cont) throws Exception {
          if(isForced.value()) return cont.run(result.value());
          return MetaPrimitives.Eval.logic(delayedCode,env,(codeResult) -> () -> {
            result.setValue(codeResult);
            isForced.setValue(true);
            return cont.run(codeResult);
          });
        }
      });
    }

    private static Datum logic(Datum delayedCode) throws Exception {
      Symbol forced = UniqueSymbol.generate("delay-forced?");
      Symbol result = UniqueSymbol.generate("delay-result");
      return Pair.List(LET,
        Pair.List(Pair.List(forced,Boolean.FALSE),Pair.List(result,Boolean.FALSE)),
        Pair.List(LAMBDA,Nil.VALUE,
          Pair.List(IF,forced,
            result,
            Pair.List(BEGIN,Pair.List(SET_BANG,forced,Boolean.TRUE),Pair.List(SET_BANG,result,delayedCode),result))));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(delay <obj>) expects exact 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (cond (<condition> <expr> ...) ...) 
  // (cond (<condition> <expr> ...) ... (<condition> => <callable>) ...) 
  // (cond (<condition> <expr> ...) ... (else <expr> ...))
  //
  // (define-syntax cond
  //   (lambda (. clauses)
  //     (define (make-condition c) (if (eq? c (quote else)) #t c))
  //     (define (make-consequence c) (cons (quote begin) c))
  //     (define (arrow-syntax? c) (and (= (length c) 3) (eq? (quote =>) (cadr c))))
  //     (define (arrow->let c a) 
  //       (define condition-result (gensym 'cond-result))
  //       (list (quote let) (list (list condition-result (car c)))
  //         (list (quote if) condition-result
  //             (list (caddr c) condition-result)
  //             a)))
  //     (fold-right (lambda (clause acc)
  //                   (if (arrow-syntax? clause)
  //                       (arrow->let clause acc)
  //                       (list (quote if) (make-condition (car clause))
  //                             (make-consequence (cdr clause))
  //                             acc)))
  //                 (quote (if #f #f)) ; inner expression yields #void
  //                 clauses)))
  public static class Cond extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "cond";
    }

    public Datum signature() {
      return Pair.List(COND,
              Pair.List(new Symbol("<condition>"),new Symbol("<expr>"),Signature.VARIADIC),
              Pair.List(new Symbol("<condition>"),ARROW,new Symbol("<callable>")),
              Pair.List(ELSE,new Symbol("<expr>"),Signature.VARIADIC),
              Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nConditional if-else chains. If <condition> is true, execute <body>.\nWith '=>' syntax, <condition> is passed to <callable> iff <condition> is #t.\n<else> is equivalent to #t.";
    }

    private static Datum makeCondition(Datum condition) {
      if(condition.eq(ELSE)) return Boolean.TRUE;
      return condition;
    }

    private static Datum makeConsequence(Datum consequence) {
      return new Pair(BEGIN,consequence);
    }

    public static boolean isArrowClause(Pair clause) {
      return clause.length() == 3 && ((Pair)clause.cdr()).car().eq(ARROW);
    }

    private static Datum convertArrowToLet(Pair clause, Datum ifs) {
      Symbol condResult = UniqueSymbol.generate("cond-result");
      return Pair.List(LET,
        Pair.List(Pair.List(condResult,clause.car())),
        Pair.List(IF,condResult,
          Pair.List(((Pair)((Pair)clause.cdr()).cdr()).car(),condResult),
          ifs));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum ifs = Void.VALUE;
      for(int i = parameters.size()-1; i >= 0; --i) {
        Datum clause = parameters.get(i);
        if(!(clause instanceof Pair))
          throw new Exceptionf("'(cond (<condition> <expr> ...) ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        Pair p = (Pair)clause;
        if(isArrowClause(p)) {
          ifs = convertArrowToLet(p,ifs);
        } else {
          ifs = Pair.List(IF,makeCondition(p.car()),makeConsequence(p.cdr()),ifs);
        }
      }
      return ifs;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (let ((<var> <value>) ...) <body> ...) 
  // (let <function-name> ((<param> <initial-value>) ...) <optional-docstring> <body> ...)
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>.
  // =====
  //
  // (define-syntax let
  //   (lambda (bindings . body)
  //     (define (get-params let-bindings) (map car let-bindings))
  //     (define (get-args let-bindings) (map cadr let-bindings))
  //     (define (get-body let-body) (cons (quote begin) let-body))
  //     (define (generate-anon-let)
  //       (cons (list (quote lambda) (get-params bindings)
  //                   (get-body body))
  //             (get-args bindings)))
  //     (define (generate-named-let)
  //       (list (list (quote lambda) (quote ())
  //               (list (quote begin)
  //                 (list (quote define) bindings
  //                       (list (quote lambda) (get-params (car body))
  //                             (get-body (cdr body))))
  //                 (cons bindings (get-args (car body)))))))
  //     (if (symbol? bindings)
  //         (generate-named-let)
  //         (generate-anon-let))))
  public static class Let extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "let";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(LET,
          Pair.List(Pair.List(new Symbol("<symbol>"),new Symbol("<obj>")),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(LET,new Symbol("<function-name>"),
          Pair.List(Pair.List(new Symbol("<parameter>"),new Symbol("<initial-value>")),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(LET,new Symbol("<function-name>"),
          Pair.List(Pair.List(new Symbol("<parameter>"),new Symbol("<initial-value>")),Signature.VARIADIC),
          new Symbol("<docstring>"),new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\nSignature 1 (nameless let):\n  Bind \"(<symbol> <obj>) ...\" in a local scope and execute <body>.\nSignature 2 (named let):\n  Bind <function-name> to (lambda (<parameter> ...) <body> ...) in a temporary\n  scope, and call it with \"<initial-value> ...\". Optionally include <docstring>\n  to yield further details on the procedure's intended purpose via <help>.";
    }

    private static escm.util.Pair<Datum,Datum> getParamsAndArgs(Datum bindings, ArrayList<Datum> parameters) throws Exception {
      if(!(bindings instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)bindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(let ((<symbol> <obj>) ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Datum bindingName = ((Pair)binding).car();
      if(!(bindingName instanceof Symbol))
        throw new Exceptionf("'(let ((<symbol> <obj>) ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Datum bindingValue = ((Pair)binding).cdr();
      if(bindingValue instanceof Pair) bindingValue = ((Pair)bindingValue).car();
      escm.util.Pair<Datum,Datum> paramsAndArgs = getParamsAndArgs(p.cdr(),parameters);
      return new escm.util.Pair<Datum,Datum>(
        new Pair(bindingName,paramsAndArgs.first),
        new Pair(bindingValue,paramsAndArgs.second));
    }

    private static Datum generateNamedLet(Datum functionName, Datum bindings, Datum body, ArrayList<Datum> parameters) throws Exception {
      escm.util.Pair<Datum,Datum> paramsAndArgs = getParamsAndArgs(bindings,parameters);
      return Pair.List(Pair.List(LAMBDA,Nil.VALUE,
              Pair.List(DEFINE,functionName,new Pair(LAMBDA,new Pair(paramsAndArgs.first,body))),
              new Pair(functionName,paramsAndArgs.second)));
    }

    private static Datum generateAnonLet(Datum bindings, Datum body, ArrayList<Datum> parameters) throws Exception {
      escm.util.Pair<Datum,Datum> paramsAndArgs = getParamsAndArgs(bindings,parameters);
      return new Pair(new Pair(LAMBDA,new Pair(paramsAndArgs.first,body)),paramsAndArgs.second);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(let ((<symbol> <obj>) ...) <body> ...) needs at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      if(bindings instanceof Symbol) {
        if(!(body instanceof Pair))
          throw new Exceptionf("'(let <function-name> ((<param> <initial-value>) ...) <body> ...) missing parameters: %s", Exceptionf.profileArgs(parameters));
        Pair p = (Pair)body;
        return generateNamedLet(bindings,p.car(),p.cdr(),parameters);
      } else {
        return generateAnonLet(bindings,body,parameters);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (quasiquote <obj>)
  // (unquote <obj>)
  // (unquote-splicing <obj>)
  //
  // (define (escm-quasiquote-tagged-list? obj tag)
  //   (and (eq? (car obj) tag) (not (null? (cdr obj)))))
  //
  // (define (escm-quasiquote->quote lst level)
  //   (define (iter lst)
  //     (define hd (if (not (atom? lst)) (car lst)))
  //           ; finished parsing expression (proper list)
  //     (cond ((null? lst) (quote ()))
  //           ; quasiquote vector
  //           ((vector? lst)
  //             (list (list (quote list->vector) (escm-quasiquote->quote (vector->list lst) level))))
  //           ; quasiquote hashmap
  //           ((hashmap? lst)
  //             (list (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list lst) level))))
  //           ; finished parsing expression (dotted list)
  //           ((atom? lst)
  //             (list (list (quote quote) lst)))
  //           ; unquote rest of list
  //           ((escm-quasiquote-tagged-list? lst (quote unquote))
  //             (if (= level 0)
  //                 (list (cadr lst))
  //                 (list (list (quote list) (quote (quote unquote)) (escm-quasiquote->quote (cadr lst) (- level 1)))))) ; *there*: recursively parse, in nested quasiquote
  //           ; quasiquote vector
  //           ((vector? hd)
  //             (cons (list (quote list) (list (quote list->vector) (escm-quasiquote->quote (vector->list hd) level)))
  //                   (iter (cdr lst))))
  //           ; quasiquote hashmap
  //           ((hashmap? hd)
  //             (cons (list (quote list) (list (quote list->hashmap) (escm-quasiquote->quote (hashmap->list hd) level)))
  //                   (iter (cdr lst))))
  //           ; quote atom
  //           ((atom? hd)
  //             (cons (list (quote list) (list (quote quote) hd))
  //                   (iter (cdr lst))))
  //           ; unquote datum
  //           ((escm-quasiquote-tagged-list? hd (quote unquote))
  //             (if (= level 0)
  //                 (cons (list (quote list) (cadr hd))
  //                       (iter (cdr lst)))
  //                 (cons (list (quote list) (escm-quasiquote->quote hd level)) ; recursively parse, in nested quasiquote (level will be decremented *there*)
  //                       (iter (cdr lst)))))
  //           ; unquote & signal should splice element
  //           ((escm-quasiquote-tagged-list? hd (quote unquote-splicing))
  //             (if (= level 0)
  //                 (cons (cadr hd) ; evaluate datum & append to the expression
  //                       (iter (cdr lst)))
  //                 (cons (list (quote list) (escm-quasiquote->quote hd (- level 1))) ; recursively parse, in nested quasiquote
  //                       (iter (cdr lst)))))
  //           ; nested quasiquote
  //           ((escm-quasiquote-tagged-list? hd (quote quasiquote))
  //             (cons (list (quote list) (escm-quasiquote->quote hd (+ level 1))) ; recursively parse, in nested quasiquote
  //                   (iter (cdr lst))))
  //           ; quasiquote expression
  //           (else
  //             (cons (list (quote list) (escm-quasiquote->quote hd level))
  //                   (iter (cdr lst))))))
  //   (cons (quote append) (iter lst)))
  //
  // (define-syntax quasiquote
  //   (lambda (x) 
  //     (if (and (pair? x) (eq? (car x) 'quasiquote))
  //         (escm-quasiquote->quote x 1)
  //         (escm-quasiquote->quote x 0))))
  public static class Quasiquote extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "quasiquote";
    }

    public Datum signature() {
      return Pair.List(QUASIQUOTE,new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nReturns <obj> as a data structure. Any <unquote>d <obj>s are evaluated first,\nand <unquote-splicing>ed <obj>s will be evaluated and spliced into the list.\n\nNOTE: Never quasiquote cyclic vectors/hashmaps during tricky (likely incorrect)\n      macro use! <quasiquote> has to deep copy vectors and hashmaps at runtime\n      to avoid sharing state in a single compiled function body between threads.\n\nNote that the reader will expand:\n  \"`<obj>\" => \"(quasiquote <obj>)\"\n  \",<obj>\" => \"(unquote <obj>)\"\n  \",@<obj>\" => \"(unquote-splicing <obj>)\"";
    }

    private static boolean isTaggedList(Pair lst, Symbol tag) {
      return lst.car().eq(tag) && !(lst.cdr() instanceof Nil);
    }

    private static Datum iterate(Datum lst, int level) {
      boolean lstIsPair = lst instanceof Pair;
      Datum hd = lstIsPair ? ((Pair)lst).car() : Void.VALUE;
      // finished parsing expression (proper list)
      if(lst instanceof Nil) {
        return Nil.VALUE;
      // quasiquote vector
      } else if(lst instanceof Vector) {
        return Pair.List(Pair.List(LIST_TO_VECTOR,convertQuasiquoteToQuote(((Vector)lst).toList(),level)));
      // quasiquote hashmap
      } else if(lst instanceof Hashmap) {
        return Pair.List(Pair.List(LIST_TO_HASHMAP,convertQuasiquoteToQuote(((Hashmap)lst).toList(),level)));
      // finished parsing expression (dotted list)
      } else if(!lstIsPair) {
        return Pair.List(Pair.List(QUOTE,lst));
      }
      // unquote rest of list
      Pair lstPair = (Pair)lst;
      if(isTaggedList(lstPair,UNQUOTE)) {
        if(level == 0) {
          return Pair.List(((Pair)lstPair.cdr()).car());
        } else {
          return Pair.List(Pair.List(LIST,Pair.List(QUOTE,UNQUOTE),convertQuasiquoteToQuote(((Pair)lstPair.cdr()).car(),level-1))); // *there*: recursively parse, in nested quasiquote
        }
      }
      // quasiquote vector
      if(hd instanceof Vector) {
        return new Pair(
          Pair.List(LIST,Pair.List(LIST_TO_VECTOR,convertQuasiquoteToQuote(((Vector)hd).toList(),level))),
          iterate(lstPair.cdr(),level));
      }
      // quasiquote hashmap
      if(hd instanceof Hashmap) {
        return new Pair(
          Pair.List(LIST,Pair.List(LIST_TO_HASHMAP,convertQuasiquoteToQuote(((Hashmap)hd).toList(),level))),
          iterate(lstPair.cdr(),level));
      }
      // quote atom
      if(!(hd instanceof Pair)) {
        return new Pair(Pair.List(LIST,Pair.List(QUOTE,hd)),iterate(lstPair.cdr(),level));
      }
      // unquote datum
      Pair hdPair = (Pair)hd;
      if(isTaggedList(hdPair,UNQUOTE)) {
        if(level == 0) {
          return new Pair(Pair.List(LIST,((Pair)hdPair.cdr()).car()),iterate(lstPair.cdr(),level));
        } else {
          return new Pair(
            Pair.List(LIST,convertQuasiquoteToQuote(hd,level)), // recursively parse, in nested quasiquote (level will be decremented *there*)
            iterate(lstPair.cdr(),level));
        }
      }
      // unquote & signal should splice element
      if(isTaggedList(hdPair,UNQUOTE_SPLICING)) {
        if(level == 0) {
          return new Pair(
            ((Pair)hdPair.cdr()).car(), // evaluate datum & append to the expression
            iterate(lstPair.cdr(),level));
        } else {
          return new Pair(
            Pair.List(LIST,convertQuasiquoteToQuote(hd,level-1)), // recursively parse, in nested quasiquote
            iterate(lstPair.cdr(),level));
        }
      }
      // nested quasiquote
      if(isTaggedList(hdPair,QUASIQUOTE)) {
        return new Pair(
          Pair.List(LIST,convertQuasiquoteToQuote(hd,level+1)), // recursively parse, in nested quasiquote
          iterate(lstPair.cdr(),level));
      // quasiquote expression
      } else {
        return new Pair(Pair.List(LIST,convertQuasiquoteToQuote(hd,level)),iterate(lstPair.cdr(),level));
      }
    }

    private static Datum convertQuasiquoteToQuote(Datum lst, int level) {
      return new Pair(APPEND,iterate(lst,level));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(quasiquote <obj>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum lst = parameters.get(0);
      if(lst instanceof Pair && ((Pair)lst).car().eq(QUASIQUOTE)) {
        return convertQuasiquoteToQuote(lst,1);
      } else {
        return convertQuasiquoteToQuote(lst,0);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (case <value> ((<key> ...) <expr> ...) ...) 
  // (case <value> ((<key> ...) <expr> ...) ... ((<key> ...) => <callable>) ...) 
  // (case <value> ((<key> ...) <expr> ...) ... (else <expr> ...))
  //
  // (define-syntax case
  //   (lambda (value . clauses)
  //     (define (rest-of-clause c)
  //       (if (and (= (length c) 3) (eq? (cadr c) (quote =>)))
  //           (cdr c)
  //           (list (cons (quote begin) (cdr c)))))
  //     (define cached-value (gensym 'case-key-value))
  //     (define converted-clauses
  //       (map (lambda (c) 
  //             (if (list? (car c))
  //                 (cons (list (quote member) cached-value (cons (quote list) (car c))) 
  //                       (rest-of-clause c))
  //                 c))
  //            clauses))
  //     (list (quote let) (list (list cached-value value))
  //       (cons (quote cond) converted-clauses))))
  public static class Case extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "case";
    }

    public Datum signature() {
      return Pair.List(new Symbol("case"),new Symbol("<value>"),
              Pair.List(Pair.List(new Symbol("<key>"),Signature.VARIADIC),new Symbol("<expr>"),Signature.VARIADIC),
              Pair.List(Pair.List(new Symbol("<key>"),Signature.VARIADIC),ARROW,new Symbol("<callable>")),
              Pair.List(ELSE,new Symbol("<expr>"),Signature.VARIADIC),
              Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nConditional \"switch\" dispatch. If <value> matches any key in \"<key> ...\",\nexecute <body>. With '=>' syntax, (member <value> (list <key> ...)) is passed\nto <callable> iff <value> matches any key in \"<key> ...\".\n<else> matches against all <value>s.";
    }

    private static Datum restOfClause(Pair clause) throws Exception {
      if(Cond.isArrowClause(clause)) {
        return clause.cdr();
      } else {
        return Pair.List(new Pair(BEGIN,clause.cdr()));
      }
    }

    private static Datum getConvertedClauses(Symbol cachedKey, ArrayList<Datum> parameters) throws Exception {
      Datum clauses = Nil.VALUE;
      for(int i = parameters.size()-1; i > 0; --i) {
        Datum clause = parameters.get(i);
        if(!(clause instanceof Pair))
          throw new Exceptionf("'(case <value> ((<key> ...) <expr> ...) ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        Pair p = (Pair)clause;
        Datum matches = p.car();
        if(Pair.isList(matches)) {
          clauses = new Pair(new Pair(Pair.List(MEMBER,cachedKey,new Pair(LIST,matches)),restOfClause(p)),clauses);
        } else {
          clauses = new Pair(clause,clauses);
        }
      }
      return clauses;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(case <value> ((<key> ...) <expr> ...) ...) missing <value>: %s", Exceptionf.profileArgs(parameters));
      Symbol cachedKey = UniqueSymbol.generate("case-key-value");
      return Pair.List(LET,Pair.List(Pair.List(cachedKey,parameters.get(0))),new Pair(COND,getConvertedClauses(cachedKey,parameters)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (let* ((<var> <value>) ...) <body> ...)
  //
  // (define-syntax let*
  //   (lambda (bindings . body)
  //     (fold-right (lambda (binding acc) (list (quote let) (list binding) acc))
  //                 (cons (quote begin) body)
  //                 bindings)))
  public static class LetStar extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "let*";
    }

    public Datum signature() {
      return Pair.List(new Symbol("let*"),
        Pair.List(Pair.List(new Symbol("<symbol>"),new Symbol("<obj>")),Signature.VARIADIC),
        new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nEquivalent to <let>, but later <symbol>s may refer\nto earlier ones.";
    }

    private static Datum generateNestedLambdaCalls(Symbol baseLetSymbol, Datum bindings, Datum internalLet, ArrayList<Datum> parameters) throws Exception {
      if(!(bindings instanceof Pair)) return internalLet;
      Pair p = (Pair)bindings;
      internalLet = generateNestedLambdaCalls(baseLetSymbol,p.cdr(),internalLet,parameters);
      return Pair.List(baseLetSymbol,Pair.List(p.car()),internalLet);
    }

    public static Datum logic(String primitiveName, Symbol baseLetSymbol, ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(%s ((<symbol> <value>) ...) <body> ...) missing <bindings>: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      if(!Pair.isList(bindings))
        throw new Exceptionf("'(%s ((<symbol> <value>) ...) <body> ...) invalid <bindings>: %s", primitiveName, Exceptionf.profileArgs(parameters));
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      return generateNestedLambdaCalls(baseLetSymbol,bindings,new Pair(BEGIN,body),parameters);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic("let*",LET,parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (letrec ((<var> <value>) ...) <body> ...)
  //
  // (define-syntax letrec
  //   (lambda (bindings . body)
  //     (append 
  //       (cons (quote let) 
  //         (cons (map (lambda (param) (list param #f)) (map car bindings))
  //             (map (lambda (binding) (cons (quote set!) binding)) bindings)))
  //       body)))
  public static class LetRec extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "letrec";
    }

    public Datum signature() {
      return Pair.List(LETREC,
        Pair.List(Pair.List(new Symbol("<symbol>"),new Symbol("<obj>")),Signature.VARIADIC),
        new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nLike <let>, but <value> may be a recursive function that\ncalls <symbol>.";
    }

    private static escm.util.Pair<Datum,Datum> getParamsDefaultAndActualSettings(Datum bindings, ArrayList<Datum> parameters) throws Exception {
      if(!(bindings instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)bindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(letrec ((<symbol> <value>) ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Datum bindingName = ((Pair)binding).car();
      if(!(bindingName instanceof Symbol))
        throw new Exceptionf("'(letrec ((<symbol> <value>) ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      escm.util.Pair<Datum,Datum> defaultAndActualSettings = getParamsDefaultAndActualSettings(p.cdr(),parameters);
      return new escm.util.Pair<Datum,Datum>(
        new Pair(Pair.List(bindingName,Boolean.FALSE),defaultAndActualSettings.first),
        new Pair(new Pair(SET_BANG,binding),defaultAndActualSettings.second));
    }

    private static Datum convertLetrecToLet(Datum bindings, Datum body, ArrayList<Datum> parameters) throws Exception {
      escm.util.Pair<Datum,Datum> defaultAndActualSettings = getParamsDefaultAndActualSettings(bindings,parameters);
      return Pair.binaryAppend(new Pair(LET,new Pair(defaultAndActualSettings.first,defaultAndActualSettings.second)),body);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(letrec ((<symbol> <value>) ...) <body> ...) missing <bindings>: %s", Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      if(!Pair.isList(bindings))
        throw new Exceptionf("'(letrec ((<symbol> <value>) ...) <body> ...) invalid <bindings>: %s", Exceptionf.profileArgs(parameters));
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      return convertLetrecToLet(bindings,body,parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (letrec* ((<var> <value>) ...) <body> ...)
  //
  // (define-syntax letrec*
  //   (lambda (bindings . body)
  //     (fold-right (lambda (binding acc) (list (quote letrec) (list binding) acc))
  //                 (cons (quote begin) body)
  //                 bindings)))
  public static class LetRecStar extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "letrec*";
    }

    public Datum signature() {
      return Pair.List(new Symbol("letrec*"),
        Pair.List(Pair.List(new Symbol("<symbol>"),new Symbol("<obj>")),Signature.VARIADIC),
        new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nEquivalent to <letrec>, but later <symbol>s may refer\nto earlier ones.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return LetStar.logic("letrec*",LETREC,parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (unless <condition> <body> ...)
  //
  // (define-syntax unless
  //   (lambda (condition . body)
  //     (list 'if condition
  //          #void
  //          (cons 'begin body))))
  public static class Unless extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "unless";
    }

    public Datum signature() {
      return Pair.List(new Symbol("unless"),new Symbol("<condition>"),new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nExecute \"<body> ...\" if <condition> is not true. Can be combined with the\n'*import*' global variable to mimic Python's \"if __name__=='__main__':\"\npattern:\n\n  (unless *import*\n    <execute-main-escheme-code-here> ...)\n\nOpposite of <when>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(unless <condition> <body> ...) missing <condition>: %s", Exceptionf.profileArgs(parameters));
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      return Pair.List(IF,parameters.get(0),Void.VALUE,new Pair(BEGIN,body));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (when <condition> <body> ...)
  //
  // (define-syntax when
  //   (lambda (condition . body)
  //     (list 'if condition
  //       (cons 'begin body))))
  public static class When extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "when";
    }

    public Datum signature() {
      return Pair.List(new Symbol("when"),new Symbol("<condition>"),new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nExecute \"<body> ...\" if <condition> is true. Opposite of <unless>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(when <condition> <body> ...) missing <condition>: %s", Exceptionf.profileArgs(parameters));
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      return Pair.List(IF,parameters.get(0),new Pair(BEGIN,body));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (while (<condition> <return-expr> ...) <body> ...)
  //
  // (define-syntax while
  //   (lambda (condition-returns . body)
  //     (define compiled-condition (compile (car condition-returns)))
  //     (define compiled-body (apply append (map compile body)))
  //     (define compiled-returns (apply append (map compile (cdr condition-returns))))
  //     (append
  //       (cons 'bytecode compiled-condition)
  //       (append
  //         (cons (list 'ifn (+ (length compiled-body) 2)) compiled-body)
  //         (append 
  //           (list
  //             (list 'jump (- 0 (length compiled-condition) (length compiled-body) 1))
  //             (list 'load #void))
  //           compiled-returns)))))
  public static class While extends PrimitiveSyntaxCallable {
    public java.lang.String escmName() {
      return "while";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(WHILE,
          Pair.List(new Symbol("<condition>")),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(WHILE,
          Pair.List(new Symbol("<condition>"),new Symbol("<return-expr>"),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\nExecute \"<body> ...\" while <condition> is true.\nReturn \"<return-expr> ...\" (defaults to #void) upon completion.\nNote that <while> uses true iteration (not recursion!)";
    }
    
    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() < 1 || !(parameters.get(0) instanceof Pair))
        throw new Exceptionf("'(while (<condition> <return-expr> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Pair conditionReturns = (Pair)parameters.get(0);
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      return Compiler.run(conditionReturns.car(),this.definitionEnvironment,(compiledCondition) -> () -> {
        return Fn.applyAppendMapCompile(this.definitionEnvironment,body,Nil.VALUE,(compiledBody) -> () -> {
          return Fn.applyAppendMapCompile(this.definitionEnvironment,conditionReturns.cdr(),Nil.VALUE,(compiledReturns) -> () -> {
            return continuation.run(Pair.binaryAppend(
              new Pair(BYTECODE,compiledCondition),
              Pair.binaryAppend(
                new Pair(Pair.List(IFN,new Exact(((OrderedCollection)compiledBody).length()+2)),compiledBody),
                Pair.binaryAppend(
                  Pair.List(
                    Pair.List(JUMP,new Exact(0-((OrderedCollection)compiledBody).length()-((OrderedCollection)compiledCondition).length()-1)),
                    Pair.List(LOAD,Void.VALUE)),
                  compiledReturns))));
          });
        });
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (do ((<var> <initial-val> <update-expr>) ...) ; contents are optional 
  //     (<break-condition> <return-expr> ...)     ; contents are optional 
  //     <body> ...)                               ; body is optional 
  //
  // (define-syntax do
  //   (lambda (var-bindings break-returns . body)
  //     (define vars (map car var-bindings))
  //     (define vals (map cadr var-bindings))
  //     (define updates 
  //       (map (lambda (e) (list 'set! (car e) (caddr e)))
  //            (filter (lambda (e) (= (length e) 3)) var-bindings)))
  //     (define break-cond
  //       (if (pair? break-returns)
  //           (car break-returns)
  //           #f))
  //     (define return-exprs
  //       (if (pair? break-returns)
  //           (cdr break-returns)
  //           (quote ())))
  //     (define loop-procedure-name (gensym 'do-loop-name))
  //     (list 'letrec (list (list loop-procedure-name 
  //                 (list 'lambda vars
  //                   (list 'if break-cond
  //                       (cons 'begin return-exprs)
  //                       (append (cons 'begin body) updates (list (cons loop-procedure-name vars)))))))
  //               (cons loop-procedure-name vals))))
  public static class Do extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "do";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("do"),
          Pair.List(),
          Pair.List()
          ,new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("do"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>")),Signature.VARIADIC),
          Pair.List(),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("do"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("do"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(new Symbol("<break-condition>")),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("do"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(new Symbol("<break-condition>"),new Symbol("<return-expression>"),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\n(do ((<var> <initial-val> <update-expr>) ...) \n    (<break-condition> <return-expr> ...) \n    <body> ...)\n\nExecute \"<body> ...\" while <break-condition> is #f. Once <break-condition> is\n#t, return \"<return-expr> ...\". \"<var>\" is set to \"<initial-val>\" at first, then\nto \"<update-expr>\" repeatedly after each iteration.\n\nNote: \n  1. \"<update-expr>\" is optional\n  2. If \"<update-expr>\" is ommited, \"<var> <initial-val>\" is optional\n  3. \"<return-expr> ...\" is optional\n  4. If \"<return-expr> ...\" is ommited, \"<break-condition>\" is optional\n  5. \"<body> ...\" is optional\n\nHence the most minimal form of \"do\" is \"(do () ())\" (an infinite loop).";
    }

    private static escm.util.Pair<Datum,Datum> getVarsAndVals(Datum varBindings, ArrayList<Datum> parameters) throws Exception {
      if(!(varBindings instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)varBindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(do ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Pair bindingPair = (Pair)binding;
      Datum value = bindingPair.cdr();
      if(value instanceof Pair) value = ((Pair)value).car();
      escm.util.Pair<Datum,Datum> tails = getVarsAndVals(p.cdr(),parameters);
      return new escm.util.Pair<Datum,Datum>(
        new Pair(bindingPair.car(),tails.first),
        new Pair(value,tails.second));
    }

    private static Datum getUpdates(Datum varBindings, ArrayList<Datum> parameters) throws Exception {
      if(!(varBindings instanceof Pair)) return Nil.VALUE;
      Pair p = (Pair)varBindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(do ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Pair bindingPair = (Pair)binding;
      if(bindingPair.length() == 3) {
        return new Pair(Pair.List(SET_BANG,bindingPair.car(),((Pair)((Pair)bindingPair.cdr()).cdr()).car()),getUpdates(p.cdr(),parameters));
      } else {
        return getUpdates(p.cdr(),parameters);
      }
    }

    private static Datum getBreakCondition(Datum breakReturns) throws Exception {
      if(breakReturns instanceof Pair) return ((Pair)breakReturns).car();
      return Boolean.FALSE;
    }

    private static Datum getReturnExprs(Datum breakReturns) throws Exception {
      if(breakReturns instanceof Pair) return ((Pair)breakReturns).cdr();
      return Nil.VALUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2 || !Pair.isList(parameters.get(0)) || !Pair.isList(parameters.get(1)))
        throw new Exceptionf("'(do ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum varBindings = parameters.get(0);
      escm.util.Pair<Datum,Datum> varsAndVals = getVarsAndVals(varBindings,parameters);
      Datum updates = getUpdates(varBindings,parameters);
      Datum breakReturns = parameters.get(1);
      Datum breakCondition = getBreakCondition(breakReturns);
      Datum returnExprs = getReturnExprs(breakReturns);
      Symbol loopProcedureName = UniqueSymbol.generate("do-loop-name");
      Datum body = Lambda.getAllExpressionsAfter(parameters,1);
      return Pair.List(LETREC,
        Pair.List(Pair.List(
          loopProcedureName,
          Pair.List(LAMBDA,varsAndVals.first,
            Pair.List(IF,breakCondition,
              new Pair(BEGIN,returnExprs),
              Pair.binaryAppend(
                new Pair(BEGIN,body),
                Pair.binaryAppend(
                  updates,
                  Pair.List(new Pair(loopProcedureName,varsAndVals.first)))))))),
        new Pair(loopProcedureName,varsAndVals.second));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (for ((<var> <initial-val> <update-expr>) ...) ; contents are optional 
  //      (<break-condition> <return-expr> ...)     ; contents are optional 
  //      <body> ...)                               ; body is optional 
  //
  // (define-syntax for
  //   (lambda (var-bindings break-returns . body)
  //     (def vars (map car var-bindings))
  //     (def vals (map cadr var-bindings))
  //     (def updates 
  //       (map #(ls 'set! (car %1) (caddr %1))
  //            (filter #(= (len %1) 3) var-bindings)))
  //     (def break-cond
  //       (if (pair? break-returns)
  //           (car break-returns)
  //           #f))
  //     (def return-exprs
  //       (if (pair? break-returns)
  //           (cdr break-returns)
  //           '()))
  //     (cons 
  //       'begin ; we want <for> to have the same scope semantics as <while>
  //       (append
  //         (map #(ls 'def %1 %2) vars vals)
  //         (ls (cons 'while
  //               (cons 
  //                 (cons (ls 'not break-cond) return-exprs)
  //                 (append body updates))))))))
  public static class For extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "for";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("for"),
          Pair.List(),
          Pair.List()
          ,new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("for"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>")),Signature.VARIADIC),
          Pair.List(),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("for"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("for"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(new Symbol("<break-condition>")),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("for"),
          Pair.List(Pair.List(new Symbol("<var>"),new Symbol("<initial-value>"),new Symbol("<update-expression>")),Signature.VARIADIC),
          Pair.List(new Symbol("<break-condition>"),new Symbol("<return-expression>"),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\n(for ((<var> <initial-val> <update-expr>) ...) \n     (<break-condition> <return-expr> ...) \n     <body> ...)\n\nExecute \"<body> ...\" while <break-condition> is #f. Once <break-condition> is\n#t, return \"<return-expr> ...\". \"<var>\" is set to \"<initial-val>\" at first, then\nto \"<update-expr>\" repeatedly after each iteration.\n\nNote: \n  1. \"<update-expr>\" is optional\n  2. If \"<update-expr>\" is ommited, \"<var> <initial-val>\" is optional\n  3. \"<return-expr> ...\" is optional\n  4. If \"<return-expr> ...\" is ommited, \"<break-condition>\" is optional\n  5. \"<body> ...\" is optional\n\nHence the most minimal form of \"for\" is \"(for () ())\" (an infinite loop).\n\nNote that this mirror's R4RS Scheme's \"do\" macro, but using true iteration \ninternally.";
    }

    private static Datum getVarDefinitions(escm.util.Pair<Datum,Datum> varsAndVals) throws Exception {
      if(!(varsAndVals.first instanceof Pair) || !(varsAndVals.second instanceof Pair)) return Nil.VALUE;
      Pair varPair = (Pair)varsAndVals.first;
      Pair valPair = (Pair)varsAndVals.second;
      return new Pair(
        Pair.List(DEFINE,varPair.car(),valPair.car()),
        getVarDefinitions(new escm.util.Pair<Datum,Datum>(varPair.cdr(),valPair.cdr())));
    }

    private static escm.util.Pair<Datum,Datum> getVarsAndVals(Datum varBindings, ArrayList<Datum> parameters) throws Exception {
      if(!(varBindings instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)varBindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(for ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Pair bindingPair = (Pair)binding;
      Datum value = bindingPair.cdr();
      if(value instanceof Pair) value = ((Pair)value).car();
      escm.util.Pair<Datum,Datum> tails = getVarsAndVals(p.cdr(),parameters);
      return new escm.util.Pair<Datum,Datum>(
        new Pair(bindingPair.car(),tails.first),
        new Pair(value,tails.second));
    }

    private static Datum getUpdates(Datum varBindings, ArrayList<Datum> parameters) throws Exception {
      if(!(varBindings instanceof Pair)) return Nil.VALUE;
      Pair p = (Pair)varBindings;
      Datum binding = p.car();
      if(!(binding instanceof Pair))
        throw new Exceptionf("'(for ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid binding: %s", Exceptionf.profileArgs(parameters));
      Pair bindingPair = (Pair)binding;
      if(bindingPair.length() == 3) {
        return new Pair(Pair.List(SET_BANG,bindingPair.car(),((Pair)((Pair)bindingPair.cdr()).cdr()).car()),getUpdates(p.cdr(),parameters));
      } else {
        return getUpdates(p.cdr(),parameters);
      }
    }

    private static Datum getBreakCondition(Datum breakReturns) throws Exception {
      if(breakReturns instanceof Pair) return ((Pair)breakReturns).car();
      return Boolean.FALSE;
    }

    private static Datum getReturnExprs(Datum breakReturns) throws Exception {
      if(breakReturns instanceof Pair) return ((Pair)breakReturns).cdr();
      return Nil.VALUE;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 2 || !Pair.isList(parameters.get(0)) || !Pair.isList(parameters.get(1)))
        throw new Exceptionf("'(for ((<var> <initial-val> <update-expr>) ...) (<break-condition> <return-expr> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum varBindings = parameters.get(0);
      escm.util.Pair<Datum,Datum> varsAndVals = getVarsAndVals(varBindings,parameters);
      Datum defns = getVarDefinitions(varsAndVals);
      Datum updates = getUpdates(varBindings,parameters);
      Datum breakReturns = parameters.get(1);
      Datum breakCondition = getBreakCondition(breakReturns);
      Datum returnExprs = getReturnExprs(breakReturns);
      Datum body = Lambda.getAllExpressionsAfter(parameters,1);
      return new Pair(
        BEGIN,
        Pair.binaryAppend(
          defns,
          Pair.List(
            new Pair(
              WHILE,
              new Pair(
                new Pair(Pair.List(NOT,breakCondition),returnExprs),
                Pair.binaryAppend(body,updates))))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (-<> <expression> ...)
  //   => Example: (-<> (* 2 2) (+ <> <>) (* <> <>)) ; => 64
  //
  // (define-syntax -<> ; Note: the "<>" value is cached!
  //   (fn
  //     ((a) a)
  //     ((a op) (list (list (quote lambda) (list (quote <>)) op) a))
  //     ((a op . ops) 
  //       (cons (quote -<>) (cons (list (list (quote lambda) (list (quote <>)) op) a) ops)))))
  public static class ArrowWand extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "-<>";
    }

    public Datum signature() {
      return Pair.List(ARROW_WAND,new Symbol("<expression>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nExecute each expression, with \"<>\" bound as the result of the last expression.\nFor example:\n  ; The below results in 64:\n  (-<> (* 2 2)\n       (+ <> <>)\n       (* <> <>))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(-<> <expression> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        return parameters.get(0);
      } else if(n == 2) {
        return Pair.List(Pair.List(LAMBDA,Pair.List(ARROW_WAND_ARG),parameters.get(1)),parameters.get(0));
      } else {
        Datum ops = Nil.VALUE;
        for(int i = parameters.size()-1; i > 1; --i) {
          ops = new Pair(parameters.get(i),ops);
        }
        return new Pair(ARROW_WAND,new Pair(Pair.List(Pair.List(LAMBDA,Pair.List(ARROW_WAND_ARG),parameters.get(1)),parameters.get(0)),ops));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (curry (<param> ...) <optional-docstring> <body> ...)
  //   => APPLICATION: Both ((K 1) 2) and (K 1 2) are valid!
  //   => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED lambda
  //            IE: (curry (x . xs) x) ; INVALID!
  //
  // NOTE: The EScheme code below doesn't account for parsing <optional-docstring>.
  // =====
  //
  // (define-syntax curry 
  //   (lambda (params . body)
  //     (define curried-lambdas (gensym 'curry-lambdas))
  //     (cond ((null? params) 
  //             (cons 'lambda (cons '() body)))
  //           ((null? (cdr params))
  //             (list 'lambda '(x . xs)
  //               (list 'fold '(lambda (f a) (f a)) 
  //                     (cons 'lambda (cons params body))
  //                     '(cons x xs))))
  //           (else
  //             (list 'let (list (list curried-lambdas 
  //               (list 'lambda (list (car params)) (cons 'curry (cons (cdr params) body)))))
  //                 (list 'lambda '(x . xs)
  //                   (list 'fold '(lambda (f a) (f a)) 
  //                         curried-lambdas
  //                         '(cons x xs))))))))
  public static class Curry extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "curry";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(CURRY,
          Pair.List(new Symbol("<parameter>"),Signature.VARIADIC),
          new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(CURRY,
          Pair.List(new Symbol("<parameter>"),Signature.VARIADIC),
          new Symbol("<docstring>"),new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\n<lambda> alternative to create a curried procedure! Suppose\n(define fcn (curry (a b) a)). <fcn> may be invoked as either\n(fcn 1 2) or ((fcn 1) 2).\n\n  => NOTE: It is UNDEFINED BEHAVIOR to have a VARIADIC CURRIED procedure!\n\nOptionally include <docstring> to yield further details on the\nprocedure's intended purpose in the <help> menu.";
    }

    private static Datum FA = Pair.List(new Symbol("f"),new Symbol("a"));
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 1)
        throw new Exceptionf("'(curry (<param> ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum params = parameters.get(0);
      escm.type.String docstring = Fn.parseOptionalDocstring(parameters,n,1);
      int priorBodyIdx = docstring == null ? 0 : 1;
      Datum body = Lambda.getAllExpressionsAfter(parameters,priorBodyIdx);
      Symbol curriedLambdas = UniqueSymbol.generate("curry-lambdas");
      Symbol x = UniqueSymbol.generate("curry-x");
      Symbol xs = UniqueSymbol.generate("curry-xs");
      if(!(params instanceof Pair)) {
        if(docstring != null) body = new Pair(docstring,body);
        return new Pair(LAMBDA,new Pair(Nil.VALUE,body));
      } 
      Pair paramsPair = (Pair)params;
      if(!(paramsPair.cdr() instanceof Pair)) {
        Datum foldExpr = Pair.List(FOLD,Pair.List(LAMBDA,FA,FA),new Pair(LAMBDA,new Pair(params,body)),Pair.List(CONS,x,xs));
        if(docstring != null) {
          return Pair.List(LAMBDA,new Pair(x,xs),docstring,foldExpr);
        } else {
          return Pair.List(LAMBDA,new Pair(x,xs),foldExpr);
        }
      } else {
        Datum foldExpr = Pair.List(FOLD,Pair.List(LAMBDA,FA,FA),curriedLambdas,Pair.List(CONS,x,xs));
        if(docstring != null) {
          return Pair.List(LET,
            Pair.List(Pair.List(
              curriedLambdas,
              Pair.List(LAMBDA,Pair.List(paramsPair.car()),new Pair(CURRY,new Pair(paramsPair.cdr(),new Pair(docstring,body)))))),
            Pair.List(LAMBDA,new Pair(x,xs),docstring,foldExpr));
        } else {
          return Pair.List(LET,
            Pair.List(Pair.List(
              curriedLambdas,
              Pair.List(LAMBDA,Pair.List(paramsPair.car()),new Pair(CURRY,new Pair(paramsPair.cdr(),body))))),
            Pair.List(LAMBDA,new Pair(x,xs),foldExpr));
        }
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (let-values (((<var> ...) <'values'-expression>) ...) <body> ...)
  //
  // (define-syntax let-values
  //   (lambda (bindings . body)
  //     (if (null? bindings)
  //         (list (cons 'lambda (cons '() body)))
  //         (list 'call-with-values
  //           (list 'lambda '() (cadar bindings))
  //           (list 'lambda (caar bindings)
  //             (cons 'let-values 
  //               (cons (cdr bindings) 
  //                     body)))))))
  public static class LetValues extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "let-values";
    }

    public Datum signature() {
      return Pair.List(LET_VALUES,
        Pair.List(Pair.List(Pair.List(new Symbol("<variable>"),Signature.VARIADIC),new Symbol("<'values'-expression>")),Signature.VARIADIC),
        new Symbol("<body>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nWrapper to locally bind results of expressions that yield a <values> object.\nExpands to nested <call-with-values> expressions.\n\nSee the <values> and <call-with-values> <help> entries for more information\nand examples on using <values> expressions.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(let-values (((<var> ...) <'values'-expression>) ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum bindings = parameters.get(0);
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      if(!(bindings instanceof Pair)) {
        return Pair.List(new Pair(LAMBDA,new Pair(Nil.VALUE,body)));
      } else {
        Pair bindingsPair = (Pair)bindings;
        Datum binding = bindingsPair.car();
        if(!(binding instanceof Pair))
          throw new Exceptionf("'(let-values (((<var> ...) <'values'-expression>) ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        Pair bindingPair = (Pair)binding;
        if(!(bindingPair.cdr() instanceof Pair))
          throw new Exceptionf("'(let-values (((<var> ...) <'values'-expression>) ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(CALL_WITH_VALUES,
          Pair.List(LAMBDA,Nil.VALUE,((Pair)bindingPair.cdr()).car()),
          Pair.List(LAMBDA,bindingPair.car(),new Pair(LET_VALUES,new Pair(bindingsPair.cdr(),body))));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (guard (<raised-var> (<condition> <expr> ...) ...) <body> ...)
  // (guard (<raised-var> (<condition> <expr> ...) ... (else <expr> ...)) <body> ...)
  //   See the below for an implementation of <with-exception-handler>, <raise>, & <guard>:
  //     https://srfi.schemers.org/srfi-34/srfi-34.html
  //
  // (define (escm-guard-aux-parse-else exprs)
  //   (cons 'begin (cdar exprs)))
  //
  // (define (escm-guard-aux-parse-test reraise exprs)
  //   (define test (caar exprs))
  //   (define results (cdar exprs))
  //   (define clauses (cdr exprs))
  //   (define temp (gensym 'guard-temp))
  //   (list 'let (list (list temp test))
  //     (list 'if temp
  //         (if (null? results) temp (cons 'begin results))
  //         (if (null? clauses) reraise (cons 'escm-guard-aux (cons reraise clauses))))))
  //
  // (define-syntax escm-guard-aux
  //   (lambda (reraise . exprs)
  //     (if (eq? (caar exprs) 'else)
  //         (escm-guard-aux-parse-else exprs)
  //         (escm-guard-aux-parse-test reraise exprs))))
  //
  // (define-syntax guard 
  //   (lambda (var-and-clauses . exprs) ; (guard (var clause ...) e1 e2 ...)
  //     (define var (car var-and-clauses))
  //     (define clauses (cdr var-and-clauses))
  //     (define guard-k (gensym 'guard-k))
  //     (define condition (gensym 'guard-condition))
  //     (define handler-k (gensym 'guard-handler-k))
  //     (define args (gensym 'guard-args))
  //     (list (list 'call-with-current-continuation
  //        (list 'lambda (list guard-k)
  //          (list 'with-exception-handler
  //           (list 'lambda (list condition)
  //             (list (list 'call-with-current-continuation
  //                (list 'lambda (list handler-k)
  //                  (list guard-k
  //                   (list 'lambda '()
  //                     (list 'let (list (list var condition)) ; clauses may SET! var
  //                       (cons 'escm-guard-aux 
  //                         (cons 
  //                           (list handler-k (list 'lambda '() (list 'raise condition))) 
  //                           clauses)))))))))
  //           (list 'lambda '()
  //             (list 'call-with-values
  //              (cons 'lambda (cons '() exprs))
  //              (list 'lambda args
  //                (list guard-k (list 'lambda '() (list 'apply 'values args))))))))))))
  public static class EscmGuardAux extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "escm-guard-aux";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-guard-aux"),new Symbol("<reraise>"),new Symbol("<expression>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Core\nUsed by <guard> to generate a guared clause using <reraise> and \"<expression> ...\".";
    }

    public Datum parseElse(Pair bodyCar) throws Exception {
      return new Pair(BEGIN,bodyCar.cdr());
    }

    public Datum parseTest(Datum reraise, Pair bodyCar, Pair body) throws Exception {
      Datum test = bodyCar.car();
      Datum results = bodyCar.cdr();
      Datum clauses = body.cdr();
      Symbol temp = UniqueSymbol.generate("guard-temp");
      return Pair.List(LET,Pair.List(Pair.List(temp,test)),
        Pair.List(IF,temp,
          results instanceof Nil ? (Datum)temp : (Datum)(new Pair(BEGIN,results)),
          clauses instanceof Nil ? reraise : (Datum)(new Pair(ESCM_GUARD_AUX,new Pair(reraise,clauses)))));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(escm-guard-aux <reraise> <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum reraise = parameters.get(0);
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      if(!(body instanceof Pair) || !(((Pair)body).car() instanceof Pair))
        throw new Exceptionf("'(escm-guard-aux <reraise> <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Pair bodyCar = (Pair)((Pair)body).car();
      if(bodyCar.car().eq(ELSE)) {
        return parseElse(bodyCar);
      } else {
        return parseTest(reraise,bodyCar,(Pair)body);
      }
    }
  }


  public static class Guard extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "guard";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("guard"),
                  Pair.List(new Symbol("<raised-var>"),
                            Pair.List(new Symbol("<condition>"),new Symbol("<expression>"),Signature.VARIADIC),
                            Signature.VARIADIC),
                  new Symbol("<body>"),Signature.VARIADIC),
        Pair.List(new Symbol("guard"),
                  Pair.List(new Symbol("<raised-var>"),
                            Pair.List(new Symbol("<condition>"),new Symbol("<expression>"),Signature.VARIADIC),
                            Signature.VARIADIC,
                            Pair.List(ELSE,new Symbol("<expression>"),Signature.VARIADIC)),
                  new Symbol("<body>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Core\nExecutes \"<body> ...\" while guarding against a <raise>d exception.\nThe <raise>d exception value is bound to <raised-var>, and\n<raised-var> is then passed to the <cond>-style\n\"(<condition> <expression> ...)\" clauses. If a condition is satisfied, then\n\"<expression> ...\" is executed. <else> matches against all exception types.\n\nFor example:\n  (guard (condition\n           (else\n            (display \"condition: \")\n            (write condition)\n            (newline)\n            'exception))\n    (+ 1 (raise 'an-error)))";
    }

    private static Symbol parseRaisedVar(Datum raisedVarAndClauses, ArrayList<Datum> parameters) throws Exception {
      if(!(raisedVarAndClauses instanceof Pair))
        throw new Exceptionf("'(guard (<raised-var> (<condition> <expr> ...) ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum raisedVar = ((Pair)raisedVarAndClauses).car();
      if(!(raisedVar instanceof Symbol))
        throw new Exceptionf("'(guard (<raised-var> (<condition> <expr> ...) ...) <body> ...) <raised-var> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      return (Symbol)raisedVar;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(guard (<raised-var> (<condition> <expr> ...) ...) <body> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum raisedVarAndClauses = parameters.get(0);
      Symbol raisedVar = parseRaisedVar(raisedVarAndClauses,parameters);
      Datum clauses = ((Pair)raisedVarAndClauses).cdr();
      Datum body = Lambda.getAllExpressionsAfter(parameters,0);
      Symbol guardK = UniqueSymbol.generate("guard-k");
      Symbol condition = UniqueSymbol.generate("guard-condition");
      Symbol handlerK = UniqueSymbol.generate("guard-handler-k");
      Symbol args = UniqueSymbol.generate("guard-args");
      return Pair.List(Pair.List(CALL_CC,
        Pair.List(LAMBDA,Pair.List(guardK),
          Pair.List(WITH_EXCEPTION_HANDLER,
            Pair.List(LAMBDA,Pair.List(condition),
              Pair.List(Pair.List(CALL_CC,
                Pair.List(LAMBDA,Pair.List(handlerK),
                  Pair.List(guardK,
                    Pair.List(LAMBDA,Nil.VALUE,
                      Pair.List(LET,Pair.List(Pair.List(raisedVar,condition)), // clauses may SET! var
                        new Pair(ESCM_GUARD_AUX,
                          new Pair(
                            Pair.List(handlerK,Pair.List(LAMBDA,Nil.VALUE,Pair.List(RAISE,condition))),
                            clauses))))))))),
            Pair.List(LAMBDA,Nil.VALUE,
              Pair.List(CALL_WITH_VALUES,
                new Pair(LAMBDA,new Pair(Nil.VALUE,body)),
                Pair.List(LAMBDA,args,
                  Pair.List(guardK,Pair.List(LAMBDA,Nil.VALUE,Pair.List(APPLY,VALUES,args))))))))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-parameter <symbol> <obj>)
  //
  // (define-syntax define-parameter
  //   (lambda (sym val)
  //     (list 'escm-define-parameter (list 'quote sym) val)))
  public static class DefineParameter extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-parameter";
    }

    public Datum signature() {
      return Pair.List(new Symbol("define-parameter"),new Symbol("<symbol>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nDefines a parameter variable. Parameter variables are stored in the\n'meta-environment', which all module global environments inherit from.\nThus parameter variable states are shared across modules, and are global in\nscope.\n\nSee <set-parameter!> to set pre-existing parameters.\nSee <get-parameter> to explicitely evaluate a parameter variable.\nSee the <parameter?> <help> entry to determine if a variable is a parameter.\n\nNote that *dosync-lock* is a parameter variable!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(define-parameter <symbol> <obj>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(ESCM_DEFINE_PARAMETER,Pair.List(QUOTE,parameters.get(0)),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (set-parameter! <symbol> <obj>)
  //
  // (define-syntax set-parameter!
  //   (lambda (sym val)
  //     (list 'escm-set-parameter! (list 'quote sym) val)))
  public static class SetParameterBang extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "set-parameter!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("set-parameter!"),new Symbol("<symbol>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nSets an existing parameter variable. Parameter variables are stored in the\n'meta-environment', which all module global environments inherit from.\nThus parameter variable states are shared across modules, and are global in\nscope.\n\nSee <define-parameter> to create new parameters.\nSee <get-parameter> to explicitely evaluate a parameter variable.\nSee the <parameter?> <help> entry to determine if a variable is a parameter.\n\nNote that *dosync-lock* is a parameter variable!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(set-parameter! <symbol> <obj>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(ESCM_SET_PARAMETER_BANG,Pair.List(QUOTE,parameters.get(0)),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (get-parameter <symbol>)
  //
  // (define-syntax get-parameter
  //   (lambda (sym)
  //     (list 'escm-get-parameter (list 'quote sym))))
  public static class GetParameter extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "get-parameter";
    }

    public Datum signature() {
      return Pair.List(new Symbol("get-parameter"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nGet a parameter variable's value. Parameter variables are stored in the\n'meta-environment', which all module global environments inherit from.\nThus parameter variable states are shared across modules, and are global in\nscope.\n\nNote that parameter variables can also just be referenced by name, though\nbeware of shadowing them with a regular <define>!\n\nSee <define-parameter> to create new parameters.\nSee <set-parameter!> to set pre-existing parameters.\nSee the <parameter?> <help> entry to determine if a variable is a parameter.\n\nNote that *dosync-lock* is a parameter variable!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(get-parameter <symbol>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(ESCM_GET_PARAMETER,Pair.List(QUOTE,parameters.get(0)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (parameter? <symbol>)
  //
  // (define-syntax parameter?
  //   (lambda (sym)
  //     (list 'escm-parameter? (list 'quote sym))))
  public static class IsParameterP extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "parameter?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("parameter?"),new Symbol("<symbol>"));
    }

    public String docstring() {
      return "@help:Syntax:Core\nDetermine if <name> is a parameter variable. See the <define-parameter>\n<help> entry to learn more about what parameter variables are.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(parameter? <symbol>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(ESCM_IS_PARAMETER_P,Pair.List(QUOTE,parameters.get(0)));
    }
  }
}