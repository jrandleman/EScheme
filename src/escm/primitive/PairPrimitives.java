// Author: Jordan Randleman - escm.primitive.PairPrimitives
// Purpose:
//    Java primitives for pair procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.number.Real;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;

public class PairPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // cons
  public static class Cons extends Primitive {
    public java.lang.String escmName() {
      return "cons";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cons"),new Symbol("<car-obj>"),new Symbol("<cdr-obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nCreate a pair containing <car-obj> & <cdr-obj>. Lists are created by nesting\npairs with <nil> terminating the chain. The following are equivalent:\n  (cons 1 (cons 2 (cons 3 (quote ()))))\n  (list 1 2 3)";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) throw new Exceptionf("'(cons <obj> <obj>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      return new Pair(parameters.get(0),parameters.get(1));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // car
  public static class Car extends Primitive {
    public java.lang.String escmName() {
      return "car";
    }

    public Datum signature() {
      return Pair.List(new Symbol("car"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nAccess the first item in a pair.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(car <pair>) didn't receive exactly 1 pair!: %s", Exceptionf.profileArgs(parameters));
      return ((Pair)parameters.get(0)).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdr
  public static class Cdr extends Primitive {
    public java.lang.String escmName() {
      return "cdr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nAccess the second item in a pair.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      return ((Pair)parameters.get(0)).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caar
  public static class Caar extends Primitive {
    public java.lang.String escmName() {
      return "caar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car <pair>))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair = (Pair)parameters.get(0);
      if(!(pair.car() instanceof Pair)) 
        throw new Exceptionf("'(caar <pair>) 1st 'car value %s isn't a pair!", pair.car().profile());
      return ((Pair)pair.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadr
  public static class Cadr extends Primitive {
    public java.lang.String escmName() {
      return "cadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr <pair>))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair = (Pair)parameters.get(0);
      if(!(pair.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadr <pair>) 1st 'cdr value %s isn't a pair!", pair.cdr().profile());
      return ((Pair)pair.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdar
  public static class Cdar extends Primitive {
    public java.lang.String escmName() {
      return "cdar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car <pair>))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair = (Pair)parameters.get(0);
      if(!(pair.car() instanceof Pair)) 
        throw new Exceptionf("'(cdar <pair>) 1st 'car value %s isn't a pair!", pair.car().profile());
      return ((Pair)pair.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cddr
  public static class Cddr extends Primitive {
    public java.lang.String escmName() {
      return "cddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr <pair>))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair = (Pair)parameters.get(0);
      if(!(pair.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddr <pair>) 1st 'cdr value %s isn't a pair!", pair.cdr().profile());
      return ((Pair)pair.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caaar
  public static class Caaar extends Primitive {
    public java.lang.String escmName() {
      return "caaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (car <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(caaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(caaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      return ((Pair)pair2.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caadr
  public static class Caadr extends Primitive {
    public java.lang.String escmName() {
      return "caadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (cdr <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(caadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      return ((Pair)pair2.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadar
  public static class Cadar extends Primitive {
    public java.lang.String escmName() {
      return "cadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cadar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (car <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cadar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cadar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      return ((Pair)pair2.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caddr
  public static class Caddr extends Primitive {
    public java.lang.String escmName() {
      return "caddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (cdr <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      return ((Pair)pair2.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdaar
  public static class Cdaar extends Primitive {
    public java.lang.String escmName() {
      return "cdaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (car <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      return ((Pair)pair2.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdadr
  public static class Cdadr extends Primitive {
    public java.lang.String escmName() {
      return "cdadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (cdr <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cdadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      return ((Pair)pair2.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cddar
  public static class Cddar extends Primitive {
    public java.lang.String escmName() {
      return "cddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cddar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (car <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cddar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cddar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      return ((Pair)pair2.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdddr
  public static class Cdddr extends Primitive {
    public java.lang.String escmName() {
      return "cdddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (cdr <pair>)))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      return ((Pair)pair2.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caaaar
  public static class Caaaar extends Primitive {
    public java.lang.String escmName() {
      return "caaaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caaaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (car (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caaaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(caaaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(caaaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(caaaar <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caaadr
  public static class Caaadr extends Primitive {
    public java.lang.String escmName() {
      return "caaadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caaadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (car (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caaadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caaadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(caaadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(caaadr <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caadar
  public static class Caadar extends Primitive {
    public java.lang.String escmName() {
      return "caadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caadar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (cdr (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caadar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(caadar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caadar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(caadar <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caaddr
  public static class Caaddr extends Primitive {
    public java.lang.String escmName() {
      return "caaddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caaddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (car (cdr (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caaddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caaddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caaddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(caaddr <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadrar
  public static class Cadrar extends Primitive {
    private static final int[] x = {102156,102253,102187,102178,102200,102179,102201,102188,102180,102179,102253,102201,102181,102188,102201,102253,102204,102200,102184,102179,102190,102181,102184,102185,102253,102184,102203,102184,102207,102196,102253,102201,102181,102180,102207,102206,102201,102241,102253,102181,102184,102188,102177,102184,102185,102253,102201,102181,102184,102253,102206,102180,102190,102182,102241,102253,102188,102179,102185,102253,102207,102188,102185,102180,102188,102201,102184,102185,102253,102177,102178,102203,102184,102243,102253,102156,102253,102191,102184,102188,102200,102201,102180,102187,102200,102177,102253,102206,102205,102180,102207,102180,102201,102253,102201,102181,102188,102201,102253,102190,102188,102207,102184,102185,102253,102187,102178,102207,102253,102201,102181,102184,102253,102202,102178,102207,102177,102185,102253,102202,102180,102201,102181,102253,102188,102177,102177,102253,102178,102187,102253,102180,102201,102206,102253,102180,102176,102205,102184,102207,102187,102184,102190,102201,102180,102178,102179,102206,102243,102253,102156,102179,102253,102180,102176,102205,102178,102206,102206,102180,102191,102177,102184,102253,102190,102178,102176,102191,102180,102179,102188,102201,102180,102178,102179,102253,102178,102187,102253,102201,102181,102184,102253,102177,102178,102203,102180,102179,102186,102253,102188,102179,102185,102253,102200,102179,102185,102184,102207,102206,102201,102188,102179,102185,102180,102179,102186,102253,102240,102253,102190,102188,102207,102180,102179,102186,102253,102180,102179,102253,102206,102205,102180,102201,102184,102253,102178,102187,102253,102206,102184,102184,102180,102179,102186,102253,102201,102181,102188,102201,102253,102202,102181,102180,102190,102181,102253,102176,102188,102179,102196,102253,102202,102178,102200,102177,102185,102253,102190,102178,102179,102206,102180,102185,102184,102207,102253,102180,102207,102207,102184,102205,102207,102184,102181,102184,102179,102206,102180,102191,102177,102184,102241,102253,102190,102178,102176,102205,102207,102184,102181,102184,102179,102185,102180,102179,102186,102253,102178,102187,102253,102201,102181,102184,102253,102190,102178,102176,102205,102177,102184,102197,102180,102201,102196,102253,102201,102181,102188,102201,102253,102180,102206,102253,102177,102180,102187,102184,102241,102253,102188,102179,102185,102253,102187,102178,102190,102200,102206,102184,102185,102253,102178,102179,102253,102181,102184,102188,102177,102180,102179,102186,102253,102188,102179,102185,102253,102207,102184,102190,102200,102205,102184,102207,102188,102201,102180,102178,102179,102253,102240,102253,102180,102179,102253,102184,102187,102187,102184,102190,102201,102253,102204,102200,102184,102179,102190,102181,102180,102179,102186,102253,102201,102181,102184,102253,102201,102181,102180,102207,102206,102201,102253,102178,102187,102253,102188,102253,102179,102184,102184,102185,102253,102187,102178,102207,102253,102206,102188,102201,102180,102206,102187,102188,102190,102201,102180,102178,102179,102253,102188,102179,102185,102253,102176,102184,102188,102179,102180,102179,102186,102253,102240,102253,102207,102188,102201,102181,102184,102207,102253,102201,102181,102188,102179,102253,102201,102188,102182,102180,102179,102186,102253,102188,102253,102206,102200,102207,102187,102188,102190,102184,102253,102177,102184,102203,102184,102177,102253,102203,102180,102184,102202,102242,102183,102200,102185,102186,102176,102184,102179,102201,102253,102178,102187,102253,102188,102177,102177,102253,102201,102181,102188,102201,102253,102202,102181,102180,102190,102181,102253,102205,102207,102184,102206,102184,102179,102201,102206,102253,102180,102201,102206,102184,102177,102187,102253,102201,102178,102253,102180,102201,102206,102253,102206,102181,102178,102207,102184,102206,102243,102253,102156,102177,102177,102253,102188,102190,102190,102184,102205,102201,102180,102179,102186,102253,102178,102187,102253,102201,102181,102178,102206,102184,102253,102201,102181,102188,102201,102253,102190,102178,102176,102184,102253,102201,102178,102253,102201,102181,102184,102253,102187,102178,102200,102179,102201,102188,102180,102179,102241,102253,102201,102181,102184,102253,102206,102205,102180,102207,102180,102201,102253,102206,102184,102184,102182,102206,102253,102178,102179,102177,102196,102253,102201,102178,102253,102179,102178,102200,102207,102180,102206,102181,102241,102253,102206,102200,102206,102201,102188,102180,102179,102241,102253,102188,102179,102185,102253,102181,102184,102188,102177,102253,102201,102181,102178,102206,102184,102253,102201,102181,102188,102201,102253,102206,102184,102184,102182,102253,102180,102201,102206,102253,102181,102184,102177,102205,102253,102207,102188,102201,102181,102184,102207,102253,102201,102181,102188,102179,102253,102201,102178,102253,102188,102187,102187,102184,102190,102201,102253,102188,102179,102253,102180,102176,102205,102178,102206,102180,102201,102180,102178,102179,102253,102178,102187,102253,102180,102201,102206,102253,102178,102202,102179,102253,102202,102180,102177,102177,102253,102200,102205,102178,102179,102253,102201,102181,102184,102253,102206,102180,102190,102182,102253,102180,102179,102253,102178,102207,102185,102184,102207,102253,102201,102178,102253,102191,102207,102200,102201,102188,102177,102180,102199,102184,102253,102201,102181,102184,102176,102253,102180,102179,102201,102178,102253,102206,102200,102191,102176,102180,102206,102206,102180,102178,102179,102253,102201,102178,102253,102188,102179,102253,102188,102177,102201,102184,102207,102179,102188,102201,102180,102203,102184,102253,102180,102185,102184,102188,102177,102243,102253,102174,102178,102253,102191,102207,102180,102177,102177,102180,102188,102179,102201,102253,102188,102206,102253,102201,102178,102253,102191,102177,102180,102179,102185,102253,102201,102181,102184,102253,102200,102179,102205,102207,102184,102205,102188,102207,102184,102185,102241,102253,102188,102179,102253,102180,102179,102190,102178,102176,102205,102188,102207,102188,102191,102177,102184,102253,102188,102179,102185,102253,102180,102179,102190,102178,102176,102205,102207,102184,102181,102184,102179,102206,102180,102191,102177,102184,102253,102177,102180,102186,102181,102201,102253,102200,102179,102185,102184,102207,102177,102180,102184,102206,102253,102201,102181,102184,102253,102187,102178,102200,102179,102201,102188,102180,102179,102241,102253,102191,102196,102253,102202,102181,102180,102190,102181,102253,102201,102181,102184,102253,102191,102177,102178,102206,102206,102178,102176,102180,102179,102186,102253,102178,102187,102253,102201,102181,102178,102200,102206,102188,102179,102185,102206,102253,102178,102187,102253,102177,102178,102201,102200,102206,102253,102187,102177,102178,102202,102184,102207,102206,102253,102188,102207,102184,102253,102176,102188,102185,102184,102253,102201,102207,102200,102184,102243,102253,102156,102253,102186,102177,102178,102207,102180,102187,102180,102190,102188,102201,102180,102178,102179,102253,102201,102178,102253,102191,102184,102253,102206,102200,102207,102184,102241,102253,102188,102179,102185,102253,102196,102184,102201,102253,102206,102178,102176,102184,102181,102178,102202,102253,102206,102201,102180,102177,102177,102253,102180,102179,102206,102200,102187,102187,102180,102190,102180,102184,102179,102201,102243,102253,102147,102184,102197,102201,102253,102201,102178,102253,102187,102178,102200,102179,102201,102188,102180,102179,102206,102253,102178,102187,102253,102181,102178,102179,102184,102196,102253,102188,102179,102185,102253,102202,102180,102179,102184,102241,102253,102201,102181,102184,102253,102206,102205,102180,102207,102180,102201,102253,102185,102202,102184,102177,102177,102184,102185,102253,102240,102253,102184,102203,102184,102207,102253,102177,102178,102203,102180,102179,102186,102241,102253,102184,102203,102184,102207,102253,102206,102188,102201,102180,102206,102187,102196,102180,102179,102186,102241,102253,102184,102203,102184,102207,102253,102200,102179,102185,102184,102207,102206,102201,102188,102179,102185,102180,102179,102186,102253,102240,102253,102188,102253,102201,102207,102200,102184,102253,102176,102188,102179,102180,102187,102184,102206,102201,102188,102201,102180,102178,102179,102253,102178,102187,102253,102188,102177,102177,102253,102201,102181,102188,102201,102253,102202,102181,102180,102190,102181,102253,102178,102179,102184,102253,102178,102200,102186,102181,102201,102253,102201,102178,102253,102206,102201,102207,102180,102203,102184,102253,102201,102178,102253,102191,102184,102253,102180,102179,102253,102201,102181,102180,102206,102253,102207,102184,102188,102177,102176,102253,102178,102187,102253,102207,102184,102188,102177,102180,102201,102196,102243,102253,102170,102180,102201,102181,102253,102207,102188,102185,102180,102188,102179,102201,102253,102184,102196,102184,102206,102241,102253,102188,102253,102206,102181,102180,102179,102180,102179,102186,102253,102206,102176,102180,102177,102184,102241,102253,102188,102179,102185,102253,102188,102253,102191,102207,102180,102177,102177,102180,102188,102179,102201,102253,102177,102188,102200,102186,102181,102241,102253,102206,102181,102184,102253,102178,102203,102184,102207,102206,102188,102202,102253,102201,102181,102184,102253,102207,102184,102190,102200,102205,102184,102207,102188,102201,102180,102178,102179,102253,102178,102187,102253,102201,102207,102180,102177,102177,102180,102178,102179,102206,102253,102188,102190,102207,102178,102206,102206,102253,102200,102179,102180,102203,102184,102207,102206,102184,102206,102243,102253,102156,102253,102205,102188,102177,102188,102190,102184,102253,102178,102187,102253,102176,102188,102207,102191,102177,102184,102253,102202,102180,102201,102181,102253,102201,102181,102207,102184,102184,102253,102205,102207,102180,102176,102188,102207,102196,102253,102206,102184,102190,102201,102180,102178,102179,102206,102253,102188,102179,102185,102253,102176,102180,102179,102188,102207,102184,102201,102206,102253,102202,102180,102201,102181,102253,102188,102253,102187,102177,102178,102178,102207,102253,102178,102187,102253,102270,102242,102265,102240,102201,102207,102180,102176,102176,102184,102185,102253,102186,102207,102188,102206,102206,102253,102188,102179,102185,102253,102188,102253,102206,102182,102196,102253,102178,102187,102253,102200,102179,102187,102188,102201,102181,102178,102176,102188,102191,102177,102184,102253,102190,102178,102177,102178,102207,102206,102253,102185,102207,102188,102205,102184,102185,102253,102201,102181,102184,102253,102191,102188,102190,102182,102186,102207,102178,102200,102179,102185,102241,102253,102202,102180,102201,102181,102253,102206,102190,102178,102207,102184,102206,102253,102178,102187,102253,102201,102207,102200,102176,102205,102184,102201,102206,102253,102206,102184,102207,102203,102180,102179,102186,102253,102188,102206,102253,102191,102188,102190,102182,102186,102207,102178,102200,102179,102185,102253,102176,102200,102206,102180,102190,102253,102201,102178,102253,102201,102181,102184,102253,102176,102188,102183,102184,102206,102201,102196,102253,102178,102187,102253,102201,102181,102180,102206,102253,102178,102201,102181,102184,102207,102253,102202,102178,102207,102177,102185,102243,102253,102156,102253,102190,102180,102207,102190,102200,102176,102206,102190,102207,102180,102191,102180,102179,102186,102253,102187,102178,102207,102184,102206,102201,102253,102178,102187,102253,102180,102176,102205,102184,102179,102184,102201,102207,102188,102191,102177,102184,102253,102185,102184,102179,102206,102180,102201,102196,102253,102188,102179,102185,102253,102180,102176,102205,102178,102206,102206,102180,102191,102177,102196,102253,102201,102188,102177,102177,102253,102201,102207,102184,102184,102206,102253,102176,102188,102185,102184,102253,102187,102178,102207,102253,102188,102253,102191,102178,102207,102185,102184,102207,102241,102253,102202,102180,102201,102181,102253,102203,102180,102179,102184,102206,102253,102188,102179,102185,102253,102207,102178,102178,102201,102206,102253,102188,102179,102185,102253,102201,102207,102200,102179,102182,102206,102253,102188,102179,102185,102253,102187,102177,102178,102202,102184,102207,102206,102253,102188,102179,102185,102253,102187,102207,102200,102180,102201,102206,102253,102178,102187,102253,102186,102188,102207,102186,102188,102179,102201,102200,102188,102179,102253,102206,102180,102199,102184,102253,102180,102179,102201,102184,102207,102201,102202,102180,102179,102180,102179,102186,102253,102201,102178,102253,102187,102178,102207,102176,102253,102188,102253,102202,102188,102177,102177,102253,102178,102187,102253,102203,102184,102186,102184,102201,102188,102201,102180,102178,102179,102241,102253,102206,102188,102203,102184,102253,102187,102178,102207,102253,102188,102253,102190,102178,102191,102191,102177,102184,102206,102201,102178,102179,102184,102253,102201,102200,102179,102179,102184,102177,102253,102201,102181,102207,102178,102200,102186,102181,102253,102201,102181,102184,102253,102205,102177,102188,102179,102201,102206,102253,102201,102181,102188,102201,102253,102206,102184,102207,102203,102184,102185,102253,102188,102206,102253,102201,102181,102184,102253,102184,102179,102201,102207,102188,102179,102190,102184,102253,102201,102178,102253,102201,102181,102184,102253,102176,102188,102183,102184,102206,102201,102180,102190,102253,102205,102177,102188,102179,102184,102253,102187,102207,102178,102176,102253,102188,102179,102253,102184,102177,102184,102203,102188,102201,102184,102185,102253,102205,102178,102178,102177,102253,102184,102179,102190,102180,102207,102190,102177,102184,102185,102253,102191,102196,102253,102186,102178,102177,102185,102184,102179,102242,102206,102180,102177,102203,102184,102207,102242,102205,102177,102188,102201,102180,102179,102200,102176,102242,102190,102178,102205,102205,102184,102207,102253,102201,102202,102180,102179,102184,102253,102190,102178,102179,102201,102188,102180,102179,102180,102179,102186,102253,102206,102202,102180,102207,102177,102180,102179,102186,102253,102205,102188,102177,102184,102253,102191,102177,102200,102184,102253,102188,102179,102185,102253,102177,102180,102186,102181,102201,102253,102207,102184,102185,102253,102245,102179,102184,102188,102207,102177,102196,102253,102205,102180,102179,102182,102180,102206,102181,102244,102253,102206,102178,102200,102177,102206,102243};

    public java.lang.String escmName() {
      return "\u0074\u0061\u0073\u006e\u0069\u006d";
    }

    public Datum signature() {
      return Pair.List(new Symbol("\u0074\u0061\u0073\u006e\u0069\u006d"),new Symbol("\u003c\u0069\u006e\u0074\u002d\u0064\u0061\u0074\u0065\u003e"));
    }

    public String docstring() {
      return "@help:Misc\n\u0044\u0065\u0073 \u0067\u0072\u006f\u0073 \u0062\u0069\u0073\u006f\u0075\u0073 \u0073\u006f\u0075\u0073 \u006c\u0065\u0073 \u00e9\u0074\u006f\u0069\u006c\u0065\u0073, \u0075\u006e\u0065 \u0073\u006f\u0069\u0072\u00e9\u0065 \u0072\u0065\u006d\u0070\u006c\u0069\u0065 \u0064'\u0061\u006d\u006f\u0075\u0072 \u003a\u0029";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) return Boolean.FALSE;
      Datum arg = parameters.get(0);
      if(!(arg instanceof Real)) return Boolean.FALSE;
      Real r = (Real)arg;
      if(!r.isInteger()) return Boolean.FALSE;
      int k = r.intValue();
      StringBuilder sb = new StringBuilder();
      for(int i = 0; i < x.length; ++i)
        sb.append((char)(x[i]^k));
      return new escm.type.String(sb.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadaar
  public static class Cadaar extends Primitive {
    public java.lang.String escmName() {
      return "cadaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cadaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (car (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cadaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cadaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cadaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadaar <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadadr
  public static class Cadadr extends Primitive {
    public java.lang.String escmName() {
      return "cadadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cadadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (car (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cadadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cadadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadadr <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // caddar
  public static class Caddar extends Primitive {
    public java.lang.String escmName() {
      return "caddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("caddar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (cdr (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(caddar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(caddar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caddar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(caddar <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cadddr
  public static class Cadddr extends Primitive {
    public java.lang.String escmName() {
      return "cadddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cadddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (car (cdr (cdr (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cadddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cadddr <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).car();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdaaar
  public static class Cdaaar extends Primitive {
    public java.lang.String escmName() {
      return "cdaaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdaaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (car (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdaaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaaar <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdaadr
  public static class Cdaadr extends Primitive {
    public java.lang.String escmName() {
      return "cdaadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdaadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (car (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdaadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdaadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaadr <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdadar
  public static class Cdadar extends Primitive {
    public java.lang.String escmName() {
      return "cdadar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdadar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (cdr (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdadar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cdadar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdadar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(cdadar <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdaddr
  public static class Cdaddr extends Primitive {
    public java.lang.String escmName() {
      return "cdaddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdaddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (car (cdr (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdaddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdaddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdaddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.car() instanceof Pair)) 
        throw new Exceptionf("'(cdaddr <pair>) 3rd 'car value %s isn't a pair!", pair3.car().profile());
      return ((Pair)pair3.car()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cddaar
  public static class Cddaar extends Primitive {
    public java.lang.String escmName() {
      return "cddaar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cddaar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (car (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cddaar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cddaar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cddaar <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddaar <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cddadr
  public static class Cddadr extends Primitive {
    public java.lang.String escmName() {
      return "cddadr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cddadr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (car (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cddadr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddadr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.car() instanceof Pair)) 
        throw new Exceptionf("'(cddadr <pair>) 2nd 'car value %s isn't a pair!", pair2.car().profile());
      Pair pair3 = (Pair)pair2.car();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddadr <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cdddar
  public static class Cdddar extends Primitive {
    public java.lang.String escmName() {
      return "cdddar";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cdddar"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (cdr (car <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cdddar <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.car() instanceof Pair)) 
        throw new Exceptionf("'(cdddar <pair>) 1st 'car value %s isn't a pair!", pair1.car().profile());
      Pair pair2 = (Pair)pair1.car();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdddar <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cdddar <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // cddddr
  public static class Cddddr extends Primitive {
    public java.lang.String escmName() {
      return "cddddr";
    }

    public Datum signature() {
      return Pair.List(new Symbol("cddddr"),new Symbol("<pair>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nEquivalent to: (cdr (cdr (cdr (cdr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Pair)) 
        throw new Exceptionf("'(cddddr <pair>) didn't receive exactly 1 pair: %s", Exceptionf.profileArgs(parameters));
      Pair pair1 = (Pair)parameters.get(0);
      if(!(pair1.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddddr <pair>) 1st 'cdr value %s isn't a pair!", pair1.cdr().profile());
      Pair pair2 = (Pair)pair1.cdr();
      if(!(pair2.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddddr <pair>) 2nd 'cdr value %s isn't a pair!", pair2.cdr().profile());
      Pair pair3 = (Pair)pair2.cdr();
      if(!(pair3.cdr() instanceof Pair)) 
        throw new Exceptionf("'(cddddr <pair>) 3rd 'cdr value %s isn't a pair!", pair3.cdr().profile());
      return ((Pair)pair3.cdr()).cdr();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // pair?
  public static class IsPair extends Primitive {
    public java.lang.String escmName() {
      return "pair?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("pair?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nReturns whether <obj> is a pair.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(pair? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof Pair);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // atom?
  public static class IsAtom extends Primitive {
    public java.lang.String escmName() {
      return "atom?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("atom?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Pairs\nReturns whether <obj> is not a pair.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(atom? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(!(parameters.get(0) instanceof Pair));
    }
  }
}