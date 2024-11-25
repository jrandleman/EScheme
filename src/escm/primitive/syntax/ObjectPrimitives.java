// Author: Jordan Randleman - escm.primitive.syntax.ObjectPrimitives
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

public class ObjectPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Keywords
  public static Keyword EXTENDS = new Keyword("extends");
  public static Keyword IMPLEMENTS = new Keyword("implements");
  public static Keyword STATIC = new Keyword("static");


  ////////////////////////////////////////////////////////////////////////////
  // Static Helper Symbols
  public static Symbol NEW = new Symbol("new");
  public static Symbol ESCM_OO_CLASS = new Symbol("escm-oo-class");
  public static Symbol CLASS = new Symbol("class");
  public static Symbol AND = new Symbol("and");
  public static Symbol IS_OBJECTP = new Symbol("object?");
  public static Symbol OO_ISP = new Symbol("oo-is?");
  public static Symbol ESCM_OO_INTERFACE = new Symbol("escm-oo-interface");
  public static Symbol INTERFACE = new Symbol("interface");
  public static Symbol SUPER = new Symbol("super");
  public static Symbol SELF = new Symbol("self");
  public static Symbol ESCM_OO_SUPER_BANG = new Symbol("escm-oo-super!");


  ////////////////////////////////////////////////////////////////////////////
  // (class <optional-name-keyword> (:extends <super>) (:implements <interface> ...) <optional-docstring> <prop> ...)
  //
  // <prop> ::= (<name> <value>)
  //          | ((<method-name> <param> ...) <body> ...)
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <super> MUST be an expression that evals to a class type.
  // <interface> MUST be an expression that evals to an interface type.
  // 
  // NOTE: The EScheme code below doesn't account for parsing <name-keyword>: a new
  // ===== internal preemptive-naming trick used by <define-class>, it allows us to 
  //       pre-bind our method names upon initial class construction, rather than
  //       only later when we bind a symbol to the class object in the environment. 
  //       * This allows us to reduce the number of method allocations by half!
  //       It also doesn't parse <optional-docstring>, nor keyword types.
  //
  //
  // ; Returns the super object if exists, else #f 
  // (define (escm-oo-get-class-super x)
  //   (if (pair? x) ; if extending first
  //       (if (eq? :extends (caar x))
  //           (cadar x)
  //           (if (and (pair? (cdr x)) (eq? :extends (caadr x))) ; if extending after implementing
  //               (car (cdadr x))
  //               #f))
  //       #f))
  //
  // ; Returns the list of implemented interfaces if exists, else NIL
  // (define (escm-oo-get-class-interfaces x)
  //   (if (pair? x) ; if implementing first
  //       (if (eq? :implements (caar x))
  //           (cdar x)
  //           (if (and (pair? (cdr x)) (eq? :implements (caadr x))) ; if implementing after extending
  //               (cdadr x)
  //               '()))
  //       '()))
  //
  // ; Returns the list of static & instance properties
  // (define (escm-oo-get-class-properties x)
  //   (if (pair? x)
  //       (if (or (eq? :implements (caar x)) (eq? :extends (caar x)))
  //           (if (and (pair? (cdr x)) (or (eq? :implements (caadr x)) (eq? :extends (caadr x))))
  //               (cddr x)
  //               (cdr x))
  //           x)
  //       '()))
  //
  // ; Return whether the property (stripped of :static) is an inlined method
  // (define (escm-oo-class-inlined-method? property)
  //   (pair? (car property)))
  //
  // ; Given an inlined method property (stripped of :static), returns it expanded as a lambda property
  // (define (escm-oo-class-expand-inlined-method inlined-method-property)
  //   (list (caar inlined-method-property) 
  //     (cons 'lambda 
  //       (cons
  //         (cdar inlined-method-property) 
  //         (cdr inlined-method-property)))))
  //
  // ; Given a list of properties stripped of :static, convert all inlined methods to lambdas
  // (define (escm-oo-class-expand-property-inlined-methods properties)
  //   (map 
  //     (lambda (property) 
  //       (if (escm-oo-class-inlined-method? property)
  //           (escm-oo-class-expand-inlined-method property)
  //           property))
  //     properties))
  //
  // ; Determine if the given property is static
  // (define (escm-oo-static-property? property)
  //   (and (pair? property) (eq? (car property) :static)))
  //
  // ; Returns a list of static properties stripped of :static, else NIL
  // ;   => Also converts inlined methods to lambdas!
  // (define (escm-oo-get-class-static-properties properties)
  //   (escm-oo-class-expand-property-inlined-methods
  //     (map cdr (filter escm-oo-static-property? properties))))
  //
  // ; Returns a list of non-static properties, else NIL
  // ;   => Also converts inlined methods to lambdas!
  // (define (escm-oo-get-class-non-static-properties properties)
  //   (escm-oo-class-expand-property-inlined-methods
  //     (filter (lambda (property) (not (escm-oo-static-property? property))) properties)))
  //
  // ; Determine if given property is the class constructor
  // (define (escm-oo-class-constructor? property)
  //   (eq? (car property) 'new))
  //
  // ; Get property list w/o the constructor
  // (define (escm-oo-get-class-instance-properties instance-properties)
  //   (filter (lambda (property) (not (escm-oo-class-constructor? property))) instance-properties))
  //
  // ; Get class constructor function
  // (define (escm-oo-get-class-constructor-procedure instance-properties)
  //   (define ctor-list (filter escm-oo-class-constructor? instance-properties))
  //   (if (null? ctor-list)
  //       #f
  //       (cadar ctor-list)))
  //
  // ; Get property names as a list of quoted symbols
  // (define (escm-oo-get-property-names properties)
  //   (map (lambda (p) (list 'quote (car p))) properties))
  //
  // ; Get property values
  // (define (escm-oo-get-property-values properties)
  //   (map cadr properties))
  //
  // ; <class> macro to create anonymous classes!
  // (define-syntax class
  //   (lambda x
  //     ; Extract "is-a"'s
  //     (define class-super (escm-oo-get-class-super x))
  //     (define class-interfaces (escm-oo-get-class-interfaces x))
  //     ; Extract "has-a"'s
  //     (define class-properties (escm-oo-get-class-properties x))
  //     (define class-non-static-properties (escm-oo-get-class-non-static-properties class-properties))
  //     (define class-static-properties (escm-oo-get-class-static-properties class-properties))
  //     (define class-instance-properties (escm-oo-get-class-instance-properties class-non-static-properties))
  //     (define class-constructor-procedure (escm-oo-get-class-constructor-procedure class-non-static-properties))
  //     ; Generate the call to an internal primitive that makes our class!
  //     (list 'escm-oo-class class-super (cons 'list class-interfaces)
  //       class-constructor-procedure 
  //       (cons 'list (escm-oo-get-property-names class-static-properties))
  //       (cons 'list (escm-oo-get-property-values class-static-properties))
  //       (cons 'list (escm-oo-get-property-names class-instance-properties))
  //       (cons 'list (escm-oo-get-property-values class-instance-properties)))))
  public static class ClassMacro extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "class";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(CLASS,
          Pair.List(EXTENDS,new Symbol("<super>")),Pair.List(IMPLEMENTS,new Symbol("<interface>"),Signature.VARIADIC),
          new Symbol("<optional-docstring>"),
          Pair.List(new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Pair.List(new Keyword("static"),new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(new Keyword("static"),Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Signature.VARIADIC
        ));
    }

    public String docstring() {
      return "@help:Syntax:Objects\nCreates an anonymous class. See <define-class> to bind a name to a class\nin one expression. See <object-system> in <Topics> for more high-level object\norientation details.\n\nRegarding inter-class/interface relations: \n  1. <super> MUST be an expression that evals to a class type.\n  2. <interface> MUST be an expression that evals to an interface type.\n  3. Both :extends and :implements are optional.\n\nOptionally include <docstring> to detail information on the class in <help>.\nMethods support keyword runtime types exactly like <lambda>.\n\n\nSimilar to Java, we support single inheritance for classes and multiple\ninheritance for interfaces.\n\nThe \"new\" pseudo-method acts as the class constructor (doesn't correlate \nto a real method). Invoke the class constructor by calling the class \nobject as if it were a procedure.\n\nThe \"->procedure\" method overloads applications for objects, making them\na \"functor\" (function object).\n\nThe \"name\" field is automatically defined for classes and interfaces.\nThe \"class\" field is automatically defined for objects.\n\nUse \"self\" and \"super\" in methods to refer to the calling object and the\nclass's super object respectively. Qualification of \"self.\" is required\nwhen referring to class fields in a method.\n  (define n 0) ; the \"global n\"\n  (define-class C\n    (n 1)      ; the \"local n\"\n    ((method)\n      n        ; refers to the \"global n\"\n      self.n)) ; refers to the \"local n\"\n\n\nRefer to a field via: object.property1.property2\nRefer to a method as: (object.inner-obj.method <arg> ...)\n\nObjects (classes and interfaces for static methods) support the following:\n  (define object.property <value>) ; define <property> as a new field\n  (set! object.property <value>)   ; set the existing <property> field\n\n\nUse the \":static\" keyword qualifier to indicate that the field/method belongs\nto the class, rather than one of its instances.\n  (define-class C (:static VALUE 10))\n  C.VALUE ; 10\n\n\nInitialization of the super object's non-nullary constructor is achieved\nvia the \"(super! <arg> ...)\" macro. Alternatively use the \"apply-super!\"\nmacro to initialize the super object with a list of values.\n\n  =================================================================\n  * NOTE THAT \"super!\", IF USED, MUST BE THE FIRST EXPRESSION IN A \n    CONSTRUCTOR IN ORDER TO AVOID INVOKING UNDEFINED BEHAVIOR !!!!\n  =================================================================\n\n  ((new)\n    (super! 42) ; valid\n    (define self.value 314)) \n\n  ((new)\n    (define self.value 314)\n    (super! 42)) ; undefined behavior\n\n\nFor example:\n  (define-class Rectangle\n    (length 0)\n    (width 0)\n    ((new l w)\n      (set! self.length l)\n      (set! self.width w))\n    ((area)\n      (* self.length self.width))\n    ((perimeter)\n      (* 2 (+ self.length self.width))))\n\n\n  (define-class Square (:extends Rectangle)\n    ((new l)\n      (super! l l))) ; init the super object\n\n\n  (define s (Square 5))\n\n  (display (s.area)) ; Square \"inherited\" <area> method. Prints 25\n\n\n\nClass reflection is provided by primtive functions prefixed with \"oo-\".\n\nSee <meta-object> in <Intrinsic-Types> for more type details.";
    }

    static private String CLASS_MACRO_SIGNATURE = "(class <super-expr> <interfaces-expr> <prop> ...)";

    // Helper triplet class
    public static class Triple<T,U,V> {
      public T first;
      public U second;
      public V third;
      public Triple(T t, U u, V v) {
        first = t;
        second = u;
        third = v;
      }
    }

    // Extract <name-keyword> if exists, else #f
    public static Datum parseOptionalClassOrInterfaceName(ArrayList<Datum> parameters) {
      if(parameters.size() > 0) {
        Datum d = parameters.get(0);
        if(d instanceof Keyword) {
          parameters.remove(0);
          return d;
        }
      }
      return Boolean.FALSE;
    }

    // If <clause> is a list starting with <key> & has 1+ following items
    public static boolean validKeywordClause(Keyword key, Datum clause) {
      if(!(clause instanceof Pair)) return false;
      Pair p = (Pair)clause;
      return p.car().eq(key) && p.cdr() instanceof Pair;
    }

    // Returns the super object (or #f), list of implemented interfaces (or NIL), and docstring (or #f)
    public static Triple<Datum,Datum,Datum> getSuperAndInterfacesAndDocstring(Datum x) {
      if(!(x instanceof Pair)) return new Triple<Datum,Datum,Datum>(Boolean.FALSE,Nil.VALUE,Boolean.FALSE);
      Pair p = (Pair)x;
      Datum superObj = Boolean.FALSE;
      Datum interfacesList = Nil.VALUE;
      Datum docstring = Boolean.FALSE;
      Datum firstClause = p.car();
      if(validKeywordClause(EXTENDS,firstClause)) {
        superObj = ((Pair)((Pair)firstClause).cdr()).car();
        if(p.cdr() instanceof Pair) {
          p = (Pair)p.cdr();
          Datum nextClause = p.car();
          if(validKeywordClause(IMPLEMENTS,nextClause)) {
            interfacesList = ((Pair)nextClause).cdr();
            if(p.cdr() instanceof Pair) {
              Datum thirdClause = ((Pair)p.cdr()).car();
              if(thirdClause instanceof escm.type.String) {
                docstring = thirdClause;
              }
            }
          } else if(nextClause instanceof escm.type.String) {
            docstring = nextClause;
          }
        }
      } else if(validKeywordClause(IMPLEMENTS,firstClause)) {
        interfacesList = ((Pair)firstClause).cdr();
        if(p.cdr() instanceof Pair) {
          p = (Pair)p.cdr();
          Datum nextClause = p.car();
          if(validKeywordClause(EXTENDS,nextClause)) {
            superObj = ((Pair)((Pair)nextClause).cdr()).car();
            if(p.cdr() instanceof Pair) {
              Datum thirdClause = ((Pair)p.cdr()).car();
              if(thirdClause instanceof escm.type.String) {
                docstring = thirdClause;
              }
            }
          } else if(nextClause instanceof escm.type.String) {
            docstring = nextClause;
          }
        }
      } else if(firstClause instanceof escm.type.String) {
        docstring = firstClause;
      }
      return new Triple<Datum,Datum,Datum>(superObj,interfacesList,docstring);
    }

    // Returns the list of static & instance properties
    public static Datum getProperties(Triple<Datum,Datum,Datum> superAndInterfacesAndDocstring, Datum x) {
      if(!(x instanceof Pair)) return Nil.VALUE;
      int propertyOffset = 0;
      if(superAndInterfacesAndDocstring.first.isTruthy()) ++propertyOffset;
      if(superAndInterfacesAndDocstring.second instanceof Pair) ++propertyOffset;
      if(superAndInterfacesAndDocstring.third.isTruthy()) ++propertyOffset;
      if(propertyOffset == 3) {
        return ((Pair)((Pair)((Pair)x).cdr()).cdr()).cdr();
      } else if(propertyOffset == 2) {
        return ((Pair)((Pair)x).cdr()).cdr();
      } else if(propertyOffset == 1) {
        return ((Pair)x).cdr();
      } else {
        return x;
      }
    }

    // Return whether the property (stripped of :static) is an inlined method
    public static boolean isInlinedMethod(Datum property) {
      if(!(property instanceof Pair)) return false;
      Pair pproperty = (Pair)property;
      Datum first = pproperty.car();
      if(first instanceof Pair) return true; // immediately found parameters, no return type!
      Datum rest = pproperty.cdr();
      return first instanceof Keyword && rest instanceof Pair && ((Pair)rest).car() instanceof Pair;
    }

    // Given an inlined method property (stripped of :static), returns it expanded as a lambda property
    private static class InlinedMethodComponents {
      public Keyword returnType = null;
      public Pair bindings = null;
      public Datum body = null;
      public InlinedMethodComponents(Keyword returnType, Pair bindings, Datum body) {
        this.returnType = returnType;
        this.bindings = bindings;
        this.body = body;
      }
    }

    private static InlinedMethodComponents getReturnTypeAndParams(Pair inlinedMethodProperty) {
      Datum first = inlinedMethodProperty.car();
      if(first instanceof Pair) 
        return new InlinedMethodComponents(null,(Pair)first,inlinedMethodProperty.cdr());
      Pair rest = (Pair)inlinedMethodProperty.cdr();
      return new InlinedMethodComponents((Keyword)first,(Pair)rest.car(),rest.cdr());
    }

    public static Datum expandInlinedMethod(Pair inlinedMethodProperty) {
      InlinedMethodComponents method = getReturnTypeAndParams(inlinedMethodProperty);
      if(method.returnType == null) {
        return Pair.List(method.bindings.car(),
          new Pair(CorePrimitives.LAMBDA,new Pair(method.bindings.cdr(),method.body)));
      }
      return Pair.List(method.bindings.car(),
        new Pair(CorePrimitives.LAMBDA,new Pair(method.returnType,new Pair(method.bindings.cdr(),method.body))));
    }

    // Determine if the given property is static
    public static boolean isStaticProperty(Datum property) {
      if(!(property instanceof Pair)) return false;
      Pair p = (Pair)property;
      return p.car().eq(STATIC) && p.cdr() instanceof Pair;
    }

    // Returns a pair of lists of static & non-static properties stripped of :static, else NIL
    //   => Also converts inlined methods to lambdas!
    public static escm.util.Pair<Datum,Datum> getInterfaceProperties(Datum properties) {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      escm.util.Pair<Datum,Datum> tails = getInterfaceProperties(p.cdr());
      if(isStaticProperty(p.car())) {
        Datum property = ((Pair)p.car()).cdr();
        if(isInlinedMethod(property)) {
          return new escm.util.Pair<Datum,Datum>(new Pair(expandInlinedMethod((Pair)property),tails.first),tails.second);
        } else {
          return new escm.util.Pair<Datum,Datum>(new Pair(property,tails.first),tails.second);
        }
      } else {
        Datum property = p.car();
        if(isInlinedMethod(property)) {
          return new escm.util.Pair<Datum,Datum>(tails.first,new Pair(expandInlinedMethod((Pair)property),tails.second));
        } else {
          return new escm.util.Pair<Datum,Datum>(tails.first,new Pair(property,tails.second));
        }
      }
    }

    // Determine if given property is the class constructor
    public static boolean isConstructor(Datum property) {
      if(!(property instanceof Pair)) return false;
      Pair p = (Pair)property;
      return p.car().eq(NEW) && p.cdr() instanceof Pair;
    }

    // Returns a pair of the constructor procedure & property list
    //   => Yields #f if no constructor was found
    public static escm.util.Pair<Datum,Datum> parseConstructorProcedureAndInstanceProperties(Datum properties) {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Boolean.FALSE,Nil.VALUE);
      Pair p = (Pair)properties;
      escm.util.Pair<Datum,Datum> ctorAndTail = parseConstructorProcedureAndInstanceProperties(p.cdr());
      if(isConstructor(p.car())) {
        return new escm.util.Pair<Datum,Datum>(((Pair)((Pair)p.car()).cdr()).car(),ctorAndTail.second);
      } else {
        return new escm.util.Pair<Datum,Datum>(ctorAndTail.first,new Pair(p.car(),ctorAndTail.second));
      }
    }

    public static boolean isValidProperty(Datum property) {
      return property instanceof Pair && ((Pair)property).length() == 2;
    }

    // Returns pair of property names as a list of quoted symbols & property values
    public static escm.util.Pair<Datum,Datum> parsePropertyNamesAndValues(ArrayList<Datum> parameters, Datum properties, String macroSignature) throws Exception {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      if(!isValidProperty(p.car()))
        throw new Exceptionf("'%s invalid <prop> %s: %s", macroSignature, p.car().profile(), Exceptionf.profileArgs(parameters));
      Pair prop = (Pair)p.car();
      escm.util.Pair<Datum,Datum> tails = parsePropertyNamesAndValues(parameters,p.cdr(),macroSignature);
      return new escm.util.Pair<Datum,Datum>(
        new Pair(Pair.List(CorePrimitives.QUOTE,prop.car()),tails.first),
        new Pair(((Pair)prop.cdr()).car(),tails.second));
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      // Extract optional name (provided by <define-class>)
      Datum className = parseOptionalClassOrInterfaceName(parameters);
      // Extract "is-a"'s
      Datum x = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      Triple<Datum,Datum,Datum> superAndInterfacesAndDocstring = getSuperAndInterfacesAndDocstring(x);
      // Extract "has-a"'s
      Datum properties = getProperties(superAndInterfacesAndDocstring,x);
      escm.util.Pair<Datum,Datum> staticAndNonStaticProps = getInterfaceProperties(properties);
      escm.util.Pair<Datum,Datum> ctorAndInstanceProps = parseConstructorProcedureAndInstanceProperties(staticAndNonStaticProps.second);
      escm.util.Pair<Datum,Datum> staticPropNamesAndVals = parsePropertyNamesAndValues(parameters,staticAndNonStaticProps.first,CLASS_MACRO_SIGNATURE);
      escm.util.Pair<Datum,Datum> instancePropNamesAndVals = parsePropertyNamesAndValues(parameters,ctorAndInstanceProps.second,CLASS_MACRO_SIGNATURE);
      // Generate the call to an internal primitive that makes our class!
      return Pair.List(ESCM_OO_CLASS,className,superAndInterfacesAndDocstring.third,
        superAndInterfacesAndDocstring.first,new Pair(CorePrimitives.LIST,superAndInterfacesAndDocstring.second),
        ctorAndInstanceProps.first,
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.second),
        new Pair(CorePrimitives.LIST,instancePropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,instancePropNamesAndVals.second));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-class <name> (:extends <super>) (:implements <interface> ...) <optional-docstring> <prop> ...)
  //
  // <prop> ::= (<name> <value>)
  //          | ((<method-name> <param> ...) <body> ...)
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <super> MUST be an expression that evals to a class type.
  // <interface> MUST be an expression that evals to an interface type.
  // 
  // NOTE: The EScheme code below doesn't account for generating the <name-keyword>
  // ===== argument passed to <class>. See <class>'s comment description above for
  //       more details. It also doesn't parse <optional-docstring>.
  //
  //
  // (define-syntax define-class
  //   (lambda (name . class-components)
  //     (define obj (gensym (append name '? '-obj)))
  //     (list 'begin
  //       (list 'define (list (append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
  //       (list 'define name (cons 'class class-components)))))
  public static class DefineClass extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-class";
    }

    public String docstring() {
      return "@help:Syntax:Objects\nSimple wrapper macro combining <define> and <class> to bind <class-name>.\nAlso generates a (<class-name>? <obj>) predicate procedure!\n\nOptionally include <docstring> to detail information on the class in <help>.\nMethods support keyword runtime types exactly like <lambda>.\n\nAliased by <defclass>.\n\nSee <object-system> in <Topics> for more high-level object orientation details.\nSee <class> for more detailed object orientation details. See <meta-object> in\n<Intrinsic-Types> for more type details.";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("define-class"),new Symbol("<class-name>"),
          Pair.List(EXTENDS,new Symbol("<super>")),Pair.List(IMPLEMENTS,new Symbol("<interface>"),Signature.VARIADIC),
          new Symbol("<optional-docstring>"),
          Pair.List(new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Pair.List(new Keyword("static"),new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(new Keyword("static"),Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Signature.VARIADIC
        ));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(define-class <name> <super-expr> <interfaces-expr> <prop> ...) missing <name>: %s", Exceptionf.profileArgs(parameters));
      Datum nameDatum = parameters.get(0);
      if(!(nameDatum instanceof Symbol))
        throw new Exceptionf("'(define-class <name> <super-expr> <interfaces-expr> <prop> ...) <name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      Symbol name = (Symbol)nameDatum;
      Symbol obj = UniqueSymbol.generate(name.value()+"?-obj");
      parameters.set(0,new Keyword(name.value()));
      return Pair.List(CorePrimitives.BEGIN,
        Pair.List(CorePrimitives.DEFINE,Pair.List(new Symbol(name.value()+"?"),obj),
          Pair.List(AND,Pair.List(IS_OBJECTP,obj),Pair.List(OO_ISP,obj,name))), // predicate generation!
        Pair.List(CorePrimitives.DEFINE,name,ClassMacro.logic(parameters)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (interface <optional-name-keyword> (:extends <interface> ...) <optional-docstring> <prop> ...)
  //
  // <prop> ::= <name>
  //          | (<method-name> <param> ...) ; typed method signature
  //          | (<method-name> (<param> ...) ...) ; multiple typed method signatures
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <interface> MUST be an expression that evals to an interface type.
  // 
  // NOTE: The EScheme code below doesn't account for parsing <name-keyword>: a new
  // ===== internal preemptive-naming trick used by <define-interface>, it allows us
  //       to pre-bind our method names upon initial interface construction, rather
  //       than only later when we bind a symbol to the interface object in the 
  //       environment. 
  //       * This allows us to reduce the number of method allocations by half!
  //       It also doesn't parse <optional-docstring>, nor typed method signatures.
  //
  //
  // ; Returns the list of implemented interfaces if exists, else NIL
  // (define (escm-oo-get-interface-interfaces x)
  //   (if (and (pair? x) (pair? (car x)) (eq? :extends (caar x)))
  //       (cdar x)
  //       '()))
  //
  // ; Returns the list of static & instance properties
  // (define (escm-oo-get-interface-properties x)
  //   (if (pair? x)
  //       (if (and (pair? (car x)) (eq? :extends (caar x)))
  //           (cdr x)
  //           x)
  //       '()))
  //
  // ; Returns a list of non-static properties, else NIL
  // (define (escm-oo-get-interface-instance-properties properties)
  //   (filter (lambda (property) (not (escm-oo-static-property? property))) properties))
  //
  // ; <interface> macro to create anonymous interfaces!
  // (define-syntax interface
  //   (lambda x
  //     ; Extract "is-a"'s
  //     (define interface-interfaces (escm-oo-get-interface-interfaces x))
  //     ; Extract "has-a"'s
  //     (define interface-properties (escm-oo-get-interface-properties x))
  //     (define interface-static-properties (escm-oo-get-class-static-properties interface-properties))
  //     (define interface-instance-properties (escm-oo-get-interface-instance-properties interface-properties))
  //     ; Generate the call to an internal primitive that makes our interface!
  //     (list 'escm-oo-interface (cons 'list interface-interfaces)
  //                              (cons 'list (escm-oo-get-property-names interface-static-properties))
  //                              (cons 'list (escm-oo-get-property-values interface-static-properties))
  //                              (cons 'list (map (lambda (p) (list 'quote p)) interface-instance-properties)))))
  public static class InterfaceMacro extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "interface";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("interface"),
          Pair.List(EXTENDS,new Symbol("<interface>"),Signature.VARIADIC),
          new Symbol("<optional-docstring>"),
          new Symbol("<field-name>"),
          Pair.List(Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC)),
          Pair.List(new Keyword("static"),new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(new Keyword("static"),Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Signature.VARIADIC
        ));
    }

    public String docstring() {
      return "@help:Syntax:Objects\nCreates an anonymous interface. Similar to classes, BUT cannot be instantiated\nvia a constructor.\n  * Required property names are denoted by symbols (see <field-name>).\n  * Required method signatures are denoted by procedures without bodies\n    (see <method-name>).\n\nUse :extends to optionally inherit required fields from other interface objects.\n\nOptionally include <docstring> to detail information on the interface in <help>.\nStatic and required methods support keyword runtime types exactly like <lambda>.\n\nSee <object-system> in <Topics> for more high-level object orientation details.\nSee <class> for more detailed object orientation details. See <meta-object> in\n<Intrinsic-Types> for more type details.\n\nFor example:\n  (define-interface IHasName\n    (:static VALUE 10)\n    name)\n\n  (define-interface IHasAge\n    age\n    (:num (get-age))\n    (:void (set-age! :num age)))\n\n  (define-interface IPerson (:extends IHasName IHasAge))\n\n  (define-class Person (:implements IPerson)\n    (name \"\")\n    (age 0)\n    (:num (get-age)\n      self.age)\n    (:void (set-age! :num age)\n      (set! self.age age)))";
    }

    static private String INTERFACE_MACRO_SIGNATURE = "(interface <interfaces-expr> <prop> ...)";

    // Class to store parsed interface properties
    private static class InterfaceProperties {
      public Datum staticProps;
      public Datum instanceSymbols;
      public Datum instanceMethodSignatures;
      public InterfaceProperties(Datum staticProps, Datum instanceSymbols, Datum instanceMethodSignatures) {
        this.staticProps = staticProps;
        this.instanceSymbols = instanceSymbols;
        this.instanceMethodSignatures = instanceMethodSignatures;
      }
    }

    // Returns the list of implemented interfaces if exists, else NIL
    public static escm.util.Pair<Datum,Datum> getInterfacesAndDocstring(Datum x) {
      if(!(x instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Boolean.FALSE);
      Datum interfaces = Nil.VALUE;
      Datum docstring = Boolean.FALSE;
      Pair p = (Pair)x;
      Datum firstClause = p.car();
      if(ClassMacro.validKeywordClause(EXTENDS,firstClause)) {
        interfaces = ((Pair)firstClause).cdr();
        if(p.cdr() instanceof Pair) {
          Datum nextClause = ((Pair)p.cdr()).car();
          if(nextClause instanceof escm.type.String) {
            docstring = nextClause;
          }
        }
      } else if(firstClause instanceof escm.type.String) {
        docstring = firstClause;
      }
      return new escm.util.Pair<Datum,Datum>(interfaces,docstring);
    }

    // Returns the list of static & instance properties
    public static Datum getProperties(escm.util.Pair<Datum,Datum> interfacesListAndDocstring, Datum x) {
      if(!(x instanceof Pair)) return Nil.VALUE;
      int propertyOffset = 0;
      if(!(interfacesListAndDocstring.first instanceof Nil)) ++propertyOffset;
      if(interfacesListAndDocstring.second.isTruthy()) ++propertyOffset;
      if(propertyOffset == 2) {
        return ((Pair)((Pair)x).cdr()).cdr();
      } else if(propertyOffset == 1) {
        return ((Pair)x).cdr();
      } else {
        return x;
      }
    }

    // Returns a pair of lists of static & non-static properties stripped of :static, else NIL
    //   => Also converts static/type-signature inlined methods to lambdas!
    //   => Also yields instance properties as quoted values!
    //   => Also yields type-signatures for required instance methods!
    public static InterfaceProperties parseInterfaceProperties(ArrayList<Datum> parameters, Datum properties) throws Exception {
      if(!(properties instanceof Pair)) return new InterfaceProperties(Nil.VALUE,Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      InterfaceProperties tails = parseInterfaceProperties(parameters,p.cdr());
      if(ClassMacro.isStaticProperty(p.car())) {
        Datum property = ((Pair)p.car()).cdr();
        if(ClassMacro.isInlinedMethod(property)) {
          return new InterfaceProperties(
            new Pair(ClassMacro.expandInlinedMethod((Pair)property),tails.staticProps),
            tails.instanceSymbols,
            tails.instanceMethodSignatures
          );
        } else {
          return new InterfaceProperties(
            new Pair(property,tails.staticProps),
            tails.instanceSymbols,
            tails.instanceMethodSignatures
          );
        }
      } else {
        Datum property = p.car();
        if(property instanceof Symbol) {
          return new InterfaceProperties(
            tails.staticProps,
            new Pair(Pair.List(CorePrimitives.QUOTE,property),tails.instanceSymbols),
            tails.instanceMethodSignatures
          );
        } else { // if (is a type-signature method)
          if(ClassMacro.isInlinedMethod(property)) {
            return new InterfaceProperties(
              tails.staticProps,
              tails.instanceSymbols,
              new Pair(ClassMacro.expandInlinedMethod((Pair)property),tails.instanceMethodSignatures)
            );
          } else {
            return new InterfaceProperties(
              tails.staticProps,
              tails.instanceSymbols,
              new Pair(property,tails.instanceMethodSignatures)
            );
          }
        }
      }
    }

    public static Datum logic(ArrayList<Datum> parameters) throws Exception {
      // Extract optional name (provided by <define-interface>)
      Datum interfaceName = ClassMacro.parseOptionalClassOrInterfaceName(parameters);
      // Extract "is-a"'s
      Datum x = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      escm.util.Pair<Datum,Datum> interfacesListAndDocstring = getInterfacesAndDocstring(x);
      // Extract "has-a"'s
      Datum properties = getProperties(interfacesListAndDocstring,x);
      InterfaceProperties interfaceProps = parseInterfaceProperties(parameters,properties);
      escm.util.Pair<Datum,Datum> staticPropNamesAndVals = ClassMacro.parsePropertyNamesAndValues(parameters,interfaceProps.staticProps,INTERFACE_MACRO_SIGNATURE);
      escm.util.Pair<Datum,Datum> methodSignaturePropNamesAndVals = ClassMacro.parsePropertyNamesAndValues(parameters,interfaceProps.instanceMethodSignatures,INTERFACE_MACRO_SIGNATURE);
      // Generate the call to an internal primitive that makes our interface!
      return Pair.List(ESCM_OO_INTERFACE,interfaceName,interfacesListAndDocstring.second,
        new Pair(CorePrimitives.LIST,interfacesListAndDocstring.first),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.second),
        new Pair(CorePrimitives.LIST,methodSignaturePropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,methodSignaturePropNamesAndVals.second),
        new Pair(CorePrimitives.LIST,interfaceProps.instanceSymbols));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      return logic(parameters);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-interface <name> (:extends <interface> ...) <optional-docstring> <prop> ...)
  //
  // <prop> ::= <name>
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <interface> MUST be an expression that evals to an interface type.
  // 
  // NOTE: The EScheme code below doesn't account for generating the <name-keyword>
  // ===== argument passed to <interface>. See <interface>'s comment description
  //       above for more details. It also doesn't parse <optional-docstring>.
  //
  //
  // (define-syntax define-interface
  //   (lambda (name . interface-components)
  //     (define obj (gensym (append name '? '-obj)))
  //     (list 'begin
  //       (list 'define (list (append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
  //       (list 'define name (cons 'interface interface-components)))))
  public static class DefineInterface extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-interface";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("define-interface"),new Symbol("<interface-name>"),
          Pair.List(EXTENDS,new Symbol("<interface>"),Signature.VARIADIC),
          new Symbol("<optional-docstring>"),
          new Symbol("<field-name>"),
          Pair.List(Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC)),
          Pair.List(new Keyword("static"),new Symbol("<field-name>"),new Symbol("<default-value>")),
          Pair.List(new Keyword("static"),Pair.List(new Symbol("<method-name>"),new Symbol("<parameter>"),Signature.VARIADIC),new Symbol("<body>"),Signature.VARIADIC),
          Signature.VARIADIC
        ));
    }

    public String docstring() {
      return "@help:Syntax:Objects\nSimple wrapper macro combining <define> and <interface> to bind <interface-name>.\nAlso generates a (<interface-name>? <obj>) predicate procedure!\n\nOptionally include <docstring> to detail information on the interface in <help>.\nStatic and required methods support keyword runtime types exactly like <lambda>.\n\nAliased by <definterface>.\n\nSee <object-system> in <Topics> for more high-level object orientation details.\nSee <class> for more detailed object orientation details. See <meta-object> in\n<Intrinsic-Types> for more type details.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(define-interface <name> <interfaces-expr> <prop> ...) missing <name>: %s", Exceptionf.profileArgs(parameters));
      Datum nameDatum = parameters.get(0);
      if(!(nameDatum instanceof Symbol))
        throw new Exceptionf("'(define-interface <name> <interfaces-expr> <prop> ...) <name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      Symbol name = (Symbol)nameDatum;
      Symbol obj = UniqueSymbol.generate(name.value()+"?-obj");
      parameters.set(0,new Keyword(name.value()));
      return Pair.List(CorePrimitives.BEGIN,
        Pair.List(CorePrimitives.DEFINE,Pair.List(new Symbol(name.value()+"?"),obj),
          Pair.List(AND,Pair.List(IS_OBJECTP,obj),Pair.List(OO_ISP,obj,name))), // predicate generation!
        Pair.List(CorePrimitives.DEFINE,name,InterfaceMacro.logic(parameters)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (super! <param> ...)
  //
  // Helper macro to initialize an object's super object.
  //
  // NOTE: _MUST_ BE USED AS THE _FIRST_ STATEMENT IN OBJECT CONSTRUCTORS.
  //        -:- -:- ANY OTHER USE RESULTS IN UNDEFINED BEHAVIOR -:- -:-
  //
  //
  // (define-syntax super!
  //   (lambda params
  //     (cons 'set! (cons 'super (list (cons 'escm-oo-super! (cons 'self params)))))))
  public static class SuperBang extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "super!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("super!"),new Symbol("<parameter>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Objects\nInitialize the super object via its non-nullary constructor.\n\nONLY valid as the FIRST expression in a class constructor:\nany other use risks undefined behavior!\n\nSee <object-system> in <Topics> for more high-level object orientation details.\nSee <class> for more detailed object orientation details. See <meta-object> in\n<Intrinsic-Types> for more type details.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum params = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      return new Pair(CorePrimitives.SET_BANG,new Pair(SUPER,Pair.List(new Pair(ESCM_OO_SUPER_BANG,new Pair(SELF,params)))));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (apply-super! <param-list>)
  //
  // Helper macro to initialize an object's super object.
  //
  // NOTE: _MUST_ BE USED AS THE _FIRST_ STATEMENT IN OBJECT CONSTRUCTORS.
  //        -:- -:- ANY OTHER USE RESULTS IN UNDEFINED BEHAVIOR -:- -:-
  //
  //
  // (define-syntax apply-super!
  //   (lambda (params-list) 
  //     (cons 'set! (cons 'super (list (list 'apply 'escm-oo-super! (list 'append '(list self) params-list)))))))
  public static class ApplySuperBang extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "apply-super!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("apply-super!"),new Symbol("<parameter>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Syntax:Objects\nAlternative to \"super!\" that initializes the super object by\napplying its <new> constructor to <list-of-args>.\n\nONLY valid as the FIRST expression in a class constructor:\nany other use risks undefined behavior!\n\nSee <object-system> in <Topics> for more high-level object orientation details.\nSee <class> for more detailed object orientation details. See <meta-object> in\n<Intrinsic-Types> for more type details.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(apply-super! <param-list>) invalid syntax: %s", Exceptionf.profileArgs(parameters));
      return new Pair(CorePrimitives.SET_BANG,new Pair(SUPER,
        Pair.List(Pair.List(CorePrimitives.APPLY,
          ESCM_OO_SUPER_BANG,
          Pair.List(CorePrimitives.APPEND,Pair.List(CorePrimitives.LIST,SELF),parameters.get(0))))));
    }
  }
}