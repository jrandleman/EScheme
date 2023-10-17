// Author: Jordan Randleman - escm.primitive.syntax.ModulePrimitives
// Purpose:
//    Java primitives for object-oriented syntax macros. All of this logic
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
import escm.type.Keyword;
import escm.type.bool.Boolean;
import escm.vm.type.primitive.PrimitiveSyntax;
import escm.vm.type.callable.Signature;
import escm.primitive.SystemPrimitives;

public class ModulePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Keywords
  public static Keyword IMPORT = new Keyword("import");
  public static Keyword AS = new Keyword("as");


  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Symbols
  public static Symbol ESCM_LOAD_MODULE = new Symbol("escm-load-module");
  public static Symbol ESCM_RELOAD_MODULE = new Symbol("escm-reload-module");
  public static Symbol DOT = new Symbol(".");


  ////////////////////////////////////////////////////////////////////////////
  // (import <module-path-symbol>)
  // (import <module-path-symbol> :as <module-alias-symbol>)
  // (import <filepath-string> <module-path-symbol>)
  // (import <filepath-string> <module-path-symbol> :as <module-alias-symbol>)
  //
  // (define-syntax import
  //   (fn ((module-path)
  //         (list 'define (escm-get-module-name module-path) (list 'escm-load-module (list 'quote module-path))))
  //       ((filepath-string module-path)
  //         (list 'define (escm-get-module-name module-path) (list 'escm-load-module filepath-string (list 'quote module-path))))
  //       ((module-path as-keyword module-alias)
  //         (list 'define module-alias (list 'escm-load-module (list 'quote module-path))))
  //       ((filepath-string module-path as-keyword module-alias)
  //         (list 'define module-alias (list 'escm-load-module filepath-string (list 'quote module-path))))))
  public static class Import extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "import";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("import"),new Symbol("<module-path-symbol>")),
        Pair.List(new Symbol("import"),new Symbol("<module-path-symbol>"),new Keyword("as"),new Symbol("<module-alias-symbol>")),
        Pair.List(new Symbol("import"),new Symbol("<filepath-string>"),new Symbol("<module-path-symbol>")),
        Pair.List(new Symbol("import"),new Symbol("<filepath-string>"),new Symbol("<module-path-symbol>"),new Keyword("as"),new Symbol("<module-alias-symbol>")));
    }

    public String docstring() {
      return "@help:Syntax:Modules\nImport <module-path-symbol> as a module variable in the current environment,\nwhere <module-path-symbol> represents an Escheme (or <serialize>d!) file.\n\nModule variables are named <module-path-symbol> by default, though there are\n2 caveats:\n\n  1. <module-alias-symbol> is provided: this overrides the default name and\n     will be what the module is loaded into the environment as.\n\n  2. <module-path-symbol> is a dotted list of symbols: this is how we denote\n     access to a module that is in a different folder than the current\n     directory. For example, suppose we have the following directory layout:\n\n       Root\n       |____ Folder1\n       |     |_______ Module.scm\n       |\n       |____ Folder2\n             |_______ Main.scm\n\n     For 'Main.scm' to import 'Module.scm', it would contain:\n\n       (import Root.Folder1.Module) ; within 'Main.scm'\n\n     In this example, the module variable would be named 'Module'. Note that\n     the file extension of the target module file is left out from the 'import'\n     expression.\n\nWith regards to locating the file pointed to by <module-path-symbol>, EScheme\nwill first attempt to find it along the file path tree from the location that\ninvoked the 'import' expression. Note that this makes executing '(import #path Module)\nredundant, in contrast to the 'load' function.\n\nImported modules are also cached across modules, so every instance of '(import Module)\nwill reference the same 'Module' module object. Note that you can force a module\nto be reloaded via the <reload> special form.\n\nAutomatically sets the '*import*' global variable to #t.\n\nLastly, note that the concept of 'parameter' variables exist in order to\nhave global state shared across modules. See the <define-parameter> and\n<parameter?> <help> entries for more details.\n\nSee the <from> <help> entry for an alternative to <import> that extracts specific\nfields from the <module-path-symbol> module, without adding the module itself to\nyour current environment's namespace.\n\nSee the <module> entry in <Types> for more details on EScheme's module system.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 1 || n > 4)
        throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      if(n == 1) {
        Datum modulePath = parameters.get(0);
        if(!(modulePath instanceof Symbol))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) non-symbol <module-path>: %s", Exceptionf.profileArgs(parameters));
        Symbol moduleName = SystemPrimitives.EscmGetModuleName.logic((Symbol)modulePath);
        return Pair.List(CorePrimitives.DEFINE,moduleName,Pair.List(ESCM_LOAD_MODULE,Pair.List(CorePrimitives.QUOTE,modulePath)));
      } else if(n == 2) {
        Datum filepath = parameters.get(0);
        Datum modulePath = parameters.get(1);
        if(!(modulePath instanceof Symbol))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) non-symbol <module-path>: %s", Exceptionf.profileArgs(parameters));
        Symbol moduleName = SystemPrimitives.EscmGetModuleName.logic((Symbol)modulePath);
        return Pair.List(CorePrimitives.DEFINE,moduleName,Pair.List(ESCM_LOAD_MODULE,filepath,Pair.List(CorePrimitives.QUOTE,modulePath)));
      } else if(n == 3) {
        Datum modulePath = parameters.get(0);
        if(!(modulePath instanceof Symbol))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) non-symbol <module-path>: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(1).eq(AS)))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        Datum moduleAlias = parameters.get(2);
        if(!(moduleAlias instanceof Symbol))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) non-symbol <module-alias>: %s", Exceptionf.profileArgs(parameters));
        return Pair.List(CorePrimitives.DEFINE,moduleAlias,Pair.List(ESCM_LOAD_MODULE,Pair.List(CorePrimitives.QUOTE,modulePath)));
      } else { // if(n == 4)
        Datum filepath = parameters.get(0);
        Datum modulePath = parameters.get(1);
        if(!(modulePath instanceof Symbol))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) non-symbol <module-path>: %s", Exceptionf.profileArgs(parameters));
        if(!(parameters.get(2).eq(AS)))
          throw new Exceptionf("'(import <optional-filepath> <module-path> :as <optional-module-alias>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
        Datum moduleAlias = parameters.get(3);
        return Pair.List(CorePrimitives.DEFINE,moduleAlias,Pair.List(ESCM_LOAD_MODULE,filepath,Pair.List(CorePrimitives.QUOTE,modulePath)));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (reload <module-alias-symbol>)
  //
  // (define-syntax reload
  //   (lambda (module-alias-symbol)
  //     (list 'set! module-alias-symbol (list 'escm-reload-module module-alias-symbol))))
  public static class Reload extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "reload";
    }

    public Datum signature() {
      return Pair.List(new Symbol("reload"),new Symbol("<module-alias-symbol>"));
    }

    public String docstring() {
      return "@help:Syntax:Modules\nForcefully reload <module-alias-symbol>. Note that <import> caches modules by default.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(reload <module-alias>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Datum moduleAlias = parameters.get(0);
      if(!(moduleAlias instanceof Symbol))
        throw new Exceptionf("'(reload <module-alias>) non-symbol <module-alias>: %s", Exceptionf.profileArgs(parameters));
      return Pair.List(CorePrimitives.SET_BANG,moduleAlias,Pair.List(ESCM_RELOAD_MODULE,moduleAlias));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (from <module-path-symbol> :import <var1-symbol> <var2-symbol> ...)
  // (from <module-path-symbol> :import <var1-symbol> <var2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
  // (from <filepath-string> <module-path-symbol> :import <var1-symbol> <var2-symbol> ...)
  // (from <filepath-string> <module-path-symbol> :import <var1-symbol> <var2-symbol> ... :as <alias1-symbol> <alias2-symbol> ...)
  //
  // (define (escm-from-parse-arguments a b c xs)
  //   (if (eq? c :import)
  //       (list a b xs)
  //       (list #f a (cons c xs))))
  //
  // (define (escm-from-get-objects-and-aliases xs)
  //   (let ((objs '()) (aliases '()) (prior-as? #t))
  //     (for-each
  //       (lambda (x)
  //         (cond ((eq? x :as) (set! prior-as? #f))
  //               (prior-as? (set! objs (cons x objs)))
  //               (else (set! aliases (cons x aliases)))))
  //       xs)
  //     (if (null? aliases)
  //         (cons objs objs)
  //         (cons objs aliases))))
  //
  // (define-syntax from
  //   (lambda (a b c . xs)
  //     (define args (escm-from-parse-arguments a b c xs))
  //     (let ((filepath-string (car args)) (module-path (cadr args)) (objs-and-aliases (caddr args)))
  //       (define fields (escm-from-get-objects-and-aliases objs-and-aliases))
  //       (define hidden-module-name (gensym))
  //       (cons 'begin
  //         (cons
  //           (list 
  //             'define 
  //             hidden-module-name 
  //             (cons 'escm-load-module 
  //               (if filepath-string 
  //                   (list filepath-string (list 'quote module-path)) 
  //                   (list (list 'quote module-path)))))
  //           (map 
  //             (lambda (obj alias)
  //               (list 'define alias (append hidden-module-name '. obj)))
  //             (car fields) 
  //             (cdr fields)))))))
  public static class From extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "from";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("from"),new Symbol("<module-path-symbol>"),new Keyword("import"),new Symbol("<variable-symbol>"),Signature.VARIADIC),
        Pair.List(new Symbol("from"),new Symbol("<module-path-symbol>"),new Keyword("import"),new Symbol("<variable-symbol>"),Signature.VARIADIC,new Keyword("as"),new Symbol("<alias-symbol>"),Signature.VARIADIC),
        Pair.List(new Symbol("from"),new Symbol("<filepath-string>"),new Symbol("<module-path-symbol>"),new Keyword("import"),new Symbol("<variable-symbol>"),Signature.VARIADIC),
        Pair.List(new Symbol("from"),new Symbol("<filepath-string>"),new Symbol("<module-path-symbol>"),new Keyword("import"),new Symbol("<variable-symbol>"),Signature.VARIADIC,new Keyword("as"),new Symbol("<alias-symbol>"),Signature.VARIADIC));
    }

    public String docstring() {
      return "@help:Syntax:Modules\nImport '<var1-sym> <var2-sym> ...' from <module-path-sym>, without\nexposing the module itself as a variable within the current environment.\n\nEquivalent to:\n  \n  (begin\n    (import <module-path-sym> :as <hidden-name>)\n    (define <var1-sym> <hidden-name>.<var1-sym>)\n    (define <var2-sym> <hidden-name>.<var2-sym>)\n    ...)\n\nOr, if '<alias1-sym> ...' is provided:\n\n  (begin\n    (import <module-path-sym> :as <hidden-name>)\n    (define <alias1-sym> <hidden-name>.<var1-sym>)\n    (define <alias2-sym> <hidden-name>.<var2-sym>)\n    ...)\n\nIf given <filepath-str>, <from> simply adds it to the above 'import'\nstatement. See the <import> <help> entry for more details on how EScheme\nloads modules, and see the <module> <help> entry for general details of\nthe overall module system itself.\n\nLike <import>, also automatically sets the '*import*' global variable to #t.";
    }

    private static ArrayList<Datum> parseArgs(ArrayList<Datum> parameters) {
      ArrayList<Datum> args = new ArrayList<Datum>();
      if(parameters.get(2).eq(IMPORT)) {
        args.add(parameters.get(0));
        args.add(parameters.get(1));
        args.add(CorePrimitives.Lambda.getAllExpressionsAfter(parameters,2));
      } else {
        args.add(Boolean.FALSE);
        args.add(parameters.get(0));
        args.add(new Pair(parameters.get(2),CorePrimitives.Lambda.getAllExpressionsAfter(parameters,2)));
      }
      return args;
    }

    private static escm.util.Pair<Datum,Datum> parseObjsAndAliases(ArrayList<Datum> parameters, boolean priorAs, Datum objsAndAliasesList) throws Exception {
      if(!(objsAndAliasesList instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)objsAndAliasesList;
      Datum x = p.car();
      if(x.eq(AS)) return parseObjsAndAliases(parameters,false,p.cdr());
      if(!(x instanceof Symbol)) {
        if(priorAs) {
          throw new Exceptionf("'(from <optional-filepath> <module-path> :import <obj> ... :as <optional-alias> ...) non-symbol <obj>: %s", Exceptionf.profileArgs(parameters));
        } else {
          throw new Exceptionf("'(from <optional-filepath> <module-path> :import <obj> ... :as <alias> ...) non-symbol <alias>: %s", Exceptionf.profileArgs(parameters));
        }
      }
      escm.util.Pair<Datum,Datum> tails = parseObjsAndAliases(parameters,priorAs,p.cdr());
      if(priorAs) return new escm.util.Pair<Datum,Datum>(new Pair(x,tails.first),tails.second);
      return new escm.util.Pair<Datum,Datum>(tails.first,new Pair(x,tails.second));
    }

    private static Datum getObjectAndAliasDefinitions(ArrayList<Datum> parameters, Symbol hiddenModuleName, Datum objsAndAliasesList) throws Exception {
      escm.util.Pair<Datum,Datum> objsAndAliases = parseObjsAndAliases(parameters,true,objsAndAliasesList);
      if(objsAndAliases.second instanceof Nil) objsAndAliases.second = objsAndAliases.first;
      Datum definitions = Nil.VALUE;
      while(objsAndAliases.first instanceof Pair && objsAndAliases.second instanceof Pair) {
        Pair objs = (Pair)objsAndAliases.first;
        Pair aliases = (Pair)objsAndAliases.second;
        Symbol obj = (Symbol)objs.car();
        Symbol alias = (Symbol)aliases.car();
        definitions = new Pair(Pair.List(CorePrimitives.DEFINE,alias,new Symbol(hiddenModuleName.value()+DOT.value()+obj.value())),definitions);
        objsAndAliases.first = objs.cdr();
        objsAndAliases.second = aliases.cdr();
      }
      return definitions;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 3)
        throw new Exceptionf("'(from <optional-filepath> <module-path> :import <obj> ... :as <optional-alias> ...) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      Symbol hiddenModuleName = UniqueSymbol.generate("from-hidden-module");
      ArrayList<Datum> args = parseArgs(parameters);
      Datum filepath = args.get(0);
      Datum modulePath = args.get(1);
      Datum aliasDefinitions = getObjectAndAliasDefinitions(parameters,hiddenModuleName,args.get(2));
      Datum loadModuleParameters = null;
      if(filepath.isTruthy()) {
        loadModuleParameters = Pair.List(filepath,Pair.List(CorePrimitives.QUOTE,modulePath));
      } else {
        loadModuleParameters = Pair.List(Pair.List(CorePrimitives.QUOTE,modulePath));
      }
      return new Pair(CorePrimitives.BEGIN,
        new Pair(Pair.List(CorePrimitives.DEFINE,hiddenModuleName,new Pair(ESCM_LOAD_MODULE,loadModuleParameters)),
          aliasDefinitions));
    }
  }
}