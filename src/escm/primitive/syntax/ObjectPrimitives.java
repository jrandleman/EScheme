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
import escm.util.Exceptionf;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.Keyword;
import escm.type.bool.Boolean;
import escm.vm.type.PrimitiveSyntax;

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
  // (class (:extends <super>) (:implements <interface> ...) <prop> ...)
  //
  // <prop> ::= (<name> <value>)
  //          | ((<method-name> <param> ...) <body> ...)
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <super> MUST be an expression that evals to a class type.
  // <interface> MUST be an expression that evals to an interface type.
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

    // If <clause> is a list starting with <key> & has 1+ following items
    public static boolean validKeywordClause(Keyword key, Datum clause) {
      if(!(clause instanceof Pair)) return false;
      Pair p = (Pair)clause;
      return p.car().eq(key) && p.cdr() instanceof Pair;
    }

    // Returns the super object (or #f), and list of implemented interfaces (or NIL)
    public static escm.util.Pair<Datum,Datum> getSuperAndInterfaces(Datum x) {
      if(!(x instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Boolean.FALSE,Nil.VALUE);
      Pair p = (Pair)x;
      Datum superObj = Boolean.FALSE;
      Datum interfacesList = Nil.VALUE;
      Datum firstClause = p.car();
      if(validKeywordClause(EXTENDS,firstClause)) {
        superObj = ((Pair)((Pair)firstClause).cdr()).car();
        if(p.cdr() instanceof Pair) {
          Datum nextClause = ((Pair)p.cdr()).car();
          if(validKeywordClause(IMPLEMENTS,nextClause))
            interfacesList = ((Pair)nextClause).cdr();
        }
      } else if(validKeywordClause(IMPLEMENTS,firstClause)) {
        interfacesList = ((Pair)firstClause).cdr();
        if(p.cdr() instanceof Pair) {
          Datum nextClause = ((Pair)p.cdr()).car();
          if(validKeywordClause(EXTENDS,nextClause))
            superObj = ((Pair)((Pair)nextClause).cdr()).car();
        }
      }
      return new escm.util.Pair<Datum,Datum>(superObj,interfacesList);
    }

    // Returns the list of static & instance properties
    public static Datum getProperties(Datum superObj, Datum interfacesList, Datum x) {
      if(!(x instanceof Pair)) return Nil.VALUE;
      boolean hasSuper = superObj.isTruthy();
      boolean hasInterfaces = interfacesList instanceof Pair;
      if(hasSuper && hasInterfaces) {
        return ((Pair)((Pair)x).cdr()).cdr();
      } else if(hasSuper || hasInterfaces) {
        return ((Pair)x).cdr();
      } else {
        return x;
      }
    }

    // Return whether the property (stripped of :static) is an inlined method
    public static boolean isInlinedMethod(Datum property) {
      return property instanceof Pair && ((Pair)property).car() instanceof Pair;
    }

    // Given an inlined method property (stripped of :static), returns it expanded as a lambda property
    public static Datum expandInlinedMethod(Pair inlinedMethodProperty) {
      Pair bindings = (Pair)inlinedMethodProperty.car();
      return Pair.List(bindings.car(),new Pair(CorePrimitives.LAMBDA,new Pair(bindings.cdr(),inlinedMethodProperty.cdr())));
    }

    // Determine if the given property is static
    public static boolean isStaticProperty(Datum property) {
      if(!(property instanceof Pair)) return false;
      Pair p = (Pair)property;
      return p.car().eq(STATIC) && p.cdr() instanceof Pair;
    }

    // Returns a pair of lists of static & non-static properties stripped of :static, else NIL
    //   => Also converts inlined methods to lambdas!
    public static escm.util.Pair<Datum,Datum> getStaticAndNonStaticProperties(Datum properties) {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      escm.util.Pair<Datum,Datum> tails = getStaticAndNonStaticProperties(p.cdr());
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
    public static escm.util.Pair<Datum,Datum> parsePropertyNamesAndValues(ArrayList<Datum> parameters, Datum properties) throws Exception {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      if(!isValidProperty(p.car()))
        throw new Exceptionf("'(class <super-expr> <interfaces-expr> <prop> ...) invalid <prop> %s: %s", p.car().profile(), Exceptionf.profileArgs(parameters));
      Pair prop = (Pair)p.car();
      escm.util.Pair<Datum,Datum> tails = parsePropertyNamesAndValues(parameters,p.cdr());
      return new escm.util.Pair<Datum,Datum>(
        new Pair(Pair.List(CorePrimitives.QUOTE,prop.car()),tails.first),
        new Pair(((Pair)prop.cdr()).car(),tails.second));
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum x = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      // Extract "is-a"'s
      escm.util.Pair<Datum,Datum> superAndInterfaces = getSuperAndInterfaces(x);
      // Extract "has-a"'s
      Datum properties = getProperties(superAndInterfaces.first,superAndInterfaces.second,x);
      escm.util.Pair<Datum,Datum> staticAndNonStaticProps = getStaticAndNonStaticProperties(properties);
      escm.util.Pair<Datum,Datum> ctorAndInstanceProps = parseConstructorProcedureAndInstanceProperties(staticAndNonStaticProps.second);
      escm.util.Pair<Datum,Datum> staticPropNamesAndVals = parsePropertyNamesAndValues(parameters,staticAndNonStaticProps.first);
      escm.util.Pair<Datum,Datum> instancePropNamesAndVals = parsePropertyNamesAndValues(parameters,ctorAndInstanceProps.second);
      // Generate the call to an internal primitive that makes our class!
      return Pair.List(ESCM_OO_CLASS,superAndInterfaces.first,new Pair(CorePrimitives.LIST,superAndInterfaces.second),
        ctorAndInstanceProps.first,
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.second),
        new Pair(CorePrimitives.LIST,instancePropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,instancePropNamesAndVals.second));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-class <name> (:extends <super>) (:implements <interface> ...) <prop> ...)
  //
  // <prop> ::= (<name> <value>)
  //          | ((<method-name> <param> ...) <body> ...)
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <super> MUST be an expression that evals to a class type.
  // <interface> MUST be an expression that evals to an interface type.
  //
  //
  // (define-syntax define-class
  //   (lambda (name . class-components)
  //     (define obj (gensym (symbol-append name '? '-obj)))
  //     (list 'begin
  //       (list 'define (list (symbol-append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
  //       (list 'define name (cons 'class class-components)))))
  public static class DefineClass extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-class";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(define-class <name> <super-expr> <interfaces-expr> <prop> ...) missing <name>: %s", Exceptionf.profileArgs(parameters));
      Datum nameDatum = parameters.get(0);
      if(!(nameDatum instanceof Symbol))
        throw new Exceptionf("'(define-class <name> <super-expr> <interfaces-expr> <prop> ...) <name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      Symbol name = (Symbol)nameDatum;
      Symbol obj = UniqueSymbol.generate(name.value()+"?-obj");
      Datum classComponents = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      return Pair.List(CorePrimitives.BEGIN,
        Pair.List(CorePrimitives.DEFINE,Pair.List(new Symbol(name.value()+"?"),obj),
          Pair.List(AND,Pair.List(IS_OBJECTP,obj),Pair.List(OO_ISP,obj,name))), // predicate generation!
        Pair.List(CorePrimitives.DEFINE,name,new Pair(CLASS,classComponents)));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (interface (:extends <interface> ...) <prop> ...)
  //
  // <prop> ::= <name>
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <interface> MUST be an expression that evals to an interface type.
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

    // Returns the list of implemented interfaces if exists, else NIL
    public static Datum getInterfaces(Datum x) {
      if(!(x instanceof Pair)) return Nil.VALUE;
      Datum firstClause = ((Pair)x).car();
      if(ClassMacro.validKeywordClause(EXTENDS,firstClause)) return ((Pair)firstClause).cdr();
      return Nil.VALUE;
    }

    // Returns the list of static & instance properties
    public static Datum getProperties(Datum interfacesList, Datum x) {
      if(!(x instanceof Pair)) return Nil.VALUE;
      if(interfacesList instanceof Pair) {
        return ((Pair)x).cdr();
      } else {
        return x;
      }
    }

    // Returns a pair of lists of static & non-static properties stripped of :static, else NIL
    //   => Also converts static inlined methods to lambdas!
    //   => Also yields instance properties as quoted values!
    public static escm.util.Pair<Datum,Datum> getStaticAndNonStaticProperties(ArrayList<Datum> parameters, Datum properties) throws Exception {
      if(!(properties instanceof Pair)) return new escm.util.Pair<Datum,Datum>(Nil.VALUE,Nil.VALUE);
      Pair p = (Pair)properties;
      escm.util.Pair<Datum,Datum> tails = getStaticAndNonStaticProperties(parameters,p.cdr());
      if(ClassMacro.isStaticProperty(p.car())) {
        Datum property = ((Pair)p.car()).cdr();
        if(ClassMacro.isInlinedMethod(property)) {
          return new escm.util.Pair<Datum,Datum>(new Pair(ClassMacro.expandInlinedMethod((Pair)property),tails.first),tails.second);
        } else {
          return new escm.util.Pair<Datum,Datum>(new Pair(property,tails.first),tails.second);
        }
      } else {
        Datum property = p.car();
        if(!(property instanceof Symbol))
          throw new Exceptionf("'(interface <interfaces-expr> <prop> ...) non-static <prop> %s isn't a symbol: %s", property.profile(), Exceptionf.profileArgs(parameters));
        return new escm.util.Pair<Datum,Datum>(tails.first,new Pair(Pair.List(CorePrimitives.QUOTE,property),tails.second));
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      Datum x = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,-1);
      // Extract "is-a"'s
      Datum interfacesList = getInterfaces(x);
      // Extract "has-a"'s
      Datum properties = getProperties(interfacesList,x);
      escm.util.Pair<Datum,Datum> staticAndNonStaticProps = getStaticAndNonStaticProperties(parameters,properties);
      escm.util.Pair<Datum,Datum> staticPropNamesAndVals = ClassMacro.parsePropertyNamesAndValues(parameters,staticAndNonStaticProps.first);
      // Generate the call to an internal primitive that makes our interface!
      return Pair.List(ESCM_OO_INTERFACE,new Pair(CorePrimitives.LIST,interfacesList),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.first),
        new Pair(CorePrimitives.LIST,staticPropNamesAndVals.second),
        new Pair(CorePrimitives.LIST,staticAndNonStaticProps.second));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // (define-interface <name> (:extends <interface> ...) <prop> ...)
  //
  // <prop> ::= <name>
  //          | (:static <name> <value>)
  //          | (:static (<method-name> <param> ...) <body> ...)
  //
  // <interface> MUST be an expression that evals to an interface type.
  //
  //
  // (define-syntax define-interface
  //   (lambda (name . interface-components)
  //     (define obj (gensym (symbol-append name '? '-obj)))
  //     (list 'begin
  //       (list 'define (list (symbol-append name '?) obj) (list 'and (list 'object? obj) (list 'oo-is? obj name))) ; predicate generation!
  //       (list 'define name (cons 'interface interface-components)))))
  public static class DefineInterface extends PrimitiveSyntax {
    public java.lang.String escmName() {
      return "define-interface";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(define-interface <name> <interfaces-expr> <prop> ...) missing <name>: %s", Exceptionf.profileArgs(parameters));
      Datum nameDatum = parameters.get(0);
      if(!(nameDatum instanceof Symbol))
        throw new Exceptionf("'(define-interface <name> <interfaces-expr> <prop> ...) <name> isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      Symbol name = (Symbol)nameDatum;
      Symbol obj = UniqueSymbol.generate(name.value()+"?-obj");
      Datum classComponents = CorePrimitives.Lambda.getAllExpressionsAfter(parameters,0);
      return Pair.List(CorePrimitives.BEGIN,
        Pair.List(CorePrimitives.DEFINE,Pair.List(new Symbol(name.value()+"?"),obj),
          Pair.List(AND,Pair.List(IS_OBJECTP,obj),Pair.List(OO_ISP,obj,name))), // predicate generation!
        Pair.List(CorePrimitives.DEFINE,name,new Pair(INTERFACE,classComponents)));
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