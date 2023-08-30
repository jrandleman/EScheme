// Author: Jordan Randleman - escm.primitive.PairPrimitives
// Purpose:
//    Java primitives for pair procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.Void;
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
      return "Create a pair containing <car-obj> & <cdr-obj>. Lists are created by nesting\npairs with <nil> terminating the chain. The following are equivalent:\n  (cons 1 (cons 2 (cons 3 (quote ()))))\n  (list 1 2 3)";
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
      return "Access the first item in a pair.";
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
      return "Access the second item in a pair.";
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
      return "Equivalent to: (car (car <pair>))";
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
      return "Equivalent to: (car (cdr <pair>))";
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
      return "Equivalent to: (cdr (car <pair>))";
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
      return "Equivalent to: (cdr (cdr <pair>))";
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
      return "Equivalent to: (car (car (car <pair>)))";
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
      return "Equivalent to: (car (car (cdr <pair>)))";
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
      return "Equivalent to: (car (cdr (car <pair>)))";
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
      return "Equivalent to: (car (cdr (cdr <pair>)))";
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
      return "Equivalent to: (cdr (car (car <pair>)))";
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
      return "Equivalent to: (cdr (car (cdr <pair>)))";
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
      return "Equivalent to: (cdr (cdr (car <pair>)))";
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
      return "Equivalent to: (cdr (cdr (cdr <pair>)))";
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
      return "Equivalent to: (car (car (car (car <pair>))))";
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
      return "Equivalent to: (car (car (car (cdr <pair>))))";
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
      return "Equivalent to: (car (car (cdr (car <pair>))))";
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
      return "Equivalent to: (car (car (cdr (cdr <pair>))))";
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
  // cadarr
  public static class Cadarr extends Primitive {
    private static final int[] x = {65,32,102,111,117,110,116,97,105,110,32,116,104,97,116,32,113,117,101,110,99,104,101,100,32,101,118,101,114,121,32,116,104,105,114,115,116,44,32,104,101,97,108,101,100,32,116,104,101,32,115,105,99,107,44,32,97,110,100,32,114,97,100,105,97,116,101,100,32,108,111,118,101,46,32,65,32,98,101,97,117,116,105,102,117,108,32,115,112,105,114,105,116,32,116,104,97,116,32,99,97,114,101,100,32,102,111,114,32,116,104,101,32,119,111,114,108,100,32,119,105,116,104,32,97,108,108,32,111,102,32,105,116,115,32,105,109,112,101,114,102,101,99,116,105,111,110,115,46,32,65,110,32,105,109,112,111,115,115,105,98,108,101,32,99,111,109,98,105,110,97,116,105,111,110,32,111,102,32,116,104,101,32,108,111,118,105,110,103,32,97,110,100,32,117,110,100,101,114,115,116,97,110,100,105,110,103,32,45,32,99,97,114,105,110,103,32,105,110,32,115,112,105,116,101,32,111,102,32,115,101,101,105,110,103,32,116,104,97,116,32,119,104,105,99,104,32,109,97,110,121,32,119,111,117,108,100,32,99,111,110,115,105,100,101,114,32,105,114,114,101,112,114,101,104,101,110,115,105,98,108,101,44,32,99,111,109,112,114,101,104,101,110,100,105,110,103,32,111,102,32,116,104,101,32,99,111,109,112,108,101,120,105,116,121,32,116,104,97,116,32,105,115,32,108,105,102,101,44,32,97,110,100,32,102,111,99,117,115,101,100,32,111,110,32,104,101,97,108,105,110,103,32,97,110,100,32,114,101,99,117,112,101,114,97,116,105,111,110,32,45,32,105,110,32,101,102,102,101,99,116,32,113,117,101,110,99,104,105,110,103,32,116,104,101,32,116,104,105,114,115,116,32,111,102,32,97,32,110,101,101,100,32,102,111,114,32,115,97,116,105,115,102,97,99,116,105,111,110,32,97,110,100,32,109,101,97,110,105,110,103,32,45,32,114,97,116,104,101,114,32,116,104,97,110,32,116,97,107,105,110,103,32,97,32,115,117,114,102,97,99,101,32,108,101,118,101,108,32,118,105,101,119,47,106,117,100,103,109,101,110,116,32,111,102,32,97,108,108,32,116,104,97,116,32,119,104,105,99,104,32,112,114,101,115,101,110,116,115,32,105,116,115,101,108,102,32,116,111,32,105,116,115,32,115,104,111,114,101,115,46,32,65,108,108,32,97,99,99,101,112,116,105,110,103,32,111,102,32,116,104,111,115,101,32,116,104,97,116,32,99,111,109,101,32,116,111,32,116,104,101,32,102,111,117,110,116,97,105,110,44,32,116,104,101,32,115,112,105,114,105,116,32,115,101,101,107,115,32,111,110,108,121,32,116,111,32,110,111,117,114,105,115,104,44,32,115,117,115,116,97,105,110,44,32,97,110,100,32,104,101,97,108,32,116,104,111,115,101,32,116,104,97,116,32,115,101,101,107,32,105,116,115,32,104,101,108,112,32,114,97,116,104,101,114,32,116,104,97,110,32,116,111,32,97,102,102,101,99,116,32,97,110,32,105,109,112,111,115,105,116,105,111,110,32,111,102,32,105,116,115,32,111,119,110,32,119,105,108,108,32,117,112,111,110,32,116,104,101,32,115,105,99,107,32,105,110,32,111,114,100,101,114,32,116,111,32,98,114,117,116,97,108,105,122,101,32,116,104,101,109,32,105,110,116,111,32,115,117,98,109,105,115,115,105,111,110,32,116,111,32,97,110,32,97,108,116,101,114,110,97,116,105,118,101,32,105,100,101,97,108,46,32,83,111,32,98,114,105,108,108,105,97,110,116,32,97,115,32,116,111,32,98,108,105,110,100,32,116,104,101,32,117,110,112,114,101,112,97,114,101,100,44,32,97,110,32,105,110,99,111,109,112,97,114,97,98,108,101,32,97,110,100,32,105,110,99,111,109,112,114,101,104,101,110,115,105,98,108,101,32,108,105,103,104,116,32,117,110,100,101,114,108,105,101,115,32,116,104,101,32,102,111,117,110,116,97,105,110,44,32,98,121,32,119,104,105,99,104,32,116,104,101,32,98,108,111,115,115,111,109,105,110,103,32,111,102,32,116,104,111,117,115,97,110,100,115,32,111,102,32,108,111,116,117,115,32,102,108,111,119,101,114,115,32,97,114,101,32,109,97,100,101,32,116,114,117,101,46,32,65,32,103,108,111,114,105,102,105,99,97,116,105,111,110,32,116,111,32,98,101,32,115,117,114,101,44,32,97,110,100,32,121,101,116,32,115,111,109,101,104,111,119,32,115,116,105,108,108,32,105,110,115,117,102,102,105,99,105,101,110,116,46,32,78,101,120,116,32,116,111,32,102,111,117,110,116,97,105,110,115,32,111,102,32,104,111,110,101,121,32,97,110,100,32,119,105,110,101,44,32,116,104,101,32,115,112,105,114,105,116,32,100,119,101,108,108,101,100,32,45,32,101,118,101,114,32,108,111,118,105,110,103,44,32,101,118,101,114,32,115,97,116,105,115,102,121,105,110,103,44,32,101,118,101,114,32,117,110,100,101,114,115,116,97,110,100,105,110,103,32,45,32,97,32,116,114,117,101,32,109,97,110,105,102,101,115,116,97,116,105,111,110,32,111,102,32,97,108,108,32,116,104,97,116,32,119,104,105,99,104,32,111,110,101,32,111,117,103,104,116,32,116,111,32,115,116,114,105,118,101,32,116,111,32,98,101,32,105,110,32,116,104,105,115,32,114,101,97,108,109,32,111,102,32,114,101,97,108,105,116,121,46,32,87,105,116,104,32,114,97,100,105,97,110,116,32,101,121,101,115,44,32,97,32,115,104,105,110,105,110,103,32,115,109,105,108,101,44,32,97,110,100,32,97,32,98,114,105,108,108,105,97,110,116,32,108,97,117,103,104,44,32,115,104,101,32,111,118,101,114,115,97,119,32,116,104,101,32,114,101,99,117,112,101,114,97,116,105,111,110,32,111,102,32,116,114,105,108,108,105,111,110,115,32,97,99,114,111,115,115,32,117,110,105,118,101,114,115,101,115,46,32,65,32,112,97,108,97,99,101,32,111,102,32,109,97,114,98,108,101,32,119,105,116,104,32,116,104,114,101,101,32,112,114,105,109,97,114,121,32,115,101,99,116,105,111,110,115,32,97,110,100,32,109,105,110,97,114,101,116,115,32,119,105,116,104,32,97,32,102,108,111,111,114,32,111,102,32,51,47,52,45,116,114,105,109,109,101,100,32,103,114,97,115,115,32,97,110,100,32,97,32,115,107,121,32,111,102,32,117,110,102,97,116,104,111,109,97,98,108,101,32,99,111,108,111,114,115,32,100,114,97,112,101,100,32,116,104,101,32,98,97,99,107,103,114,111,117,110,100,44,32,119,105,116,104,32,115,99,111,114,101,115,32,111,102,32,116,114,117,109,112,101,116,115,32,115,101,114,118,105,110,103,32,97,115,32,98,97,99,107,103,114,111,117,110,100,32,109,117,115,105,99,32,116,111,32,116,104,101,32,109,97,106,101,115,116,121,32,111,102,32,116,104,105,115,32,111,116,104,101,114,32,119,111,114,108,100,46,32,65,32,99,105,114,99,117,109,115,99,114,105,98,105,110,103,32,102,111,114,101,115,116,32,111,102,32,105,109,112,101,110,101,116,114,97,98,108,101,32,100,101,110,115,105,116,121,32,97,110,100,32,105,109,112,111,115,115,105,98,108,121,32,116,97,108,108,32,116,114,101,101,115,32,109,97,100,101,32,102,111,114,32,97,32,98,111,114,100,101,114,44,32,119,105,116,104,32,118,105,110,101,115,32,97,110,100,32,114,111,111,116,115,32,97,110,100,32,116,114,117,110,107,115,32,97,110,100,32,102,108,111,119,101,114,115,32,97,110,100,32,102,114,117,105,116,115,32,111,102,32,103,97,114,103,97,110,116,117,97,110,32,115,105,122,101,32,105,110,116,101,114,116,119,105,110,105,110,103,32,116,111,32,102,111,114,109,32,97,32,119,97,108,108,32,111,102,32,118,101,103,101,116,97,116,105,111,110,44,32,115,97,118,101,32,102,111,114,32,97,32,99,111,98,98,108,101,115,116,111,110,101,32,116,117,110,110,101,108,32,116,104,114,111,117,103,104,32,116,104,101,32,112,108,97,110,116,115,32,116,104,97,116,32,115,101,114,118,101,100,32,97,115,32,116,104,101,32,101,110,116,114,97,110,99,101,32,116,111,32,116,104,101,32,109,97,106,101,115,116,105,99,32,112,108,97,110,101,32,102,114,111,109,32,97,110,32,101,108,101,118,97,116,101,100,32,112,111,111,108,32,101,110,99,105,114,99,108,101,100,32,98,121,32,103,111,108,100,101,110,47,115,105,108,118,101,114,47,112,108,97,116,105,110,117,109,47,99,111,112,112,101,114,32,116,119,105,110,101,32,99,111,110,116,97,105,110,105,110,103,32,115,119,105,114,108,105,110,103,32,112,97,108,101,32,98,108,117,101,32,97,110,100,32,108,105,103,104,116,32,114,101,100,32,40,110,101,97,114,108,121,32,112,105,110,107,105,115,104,41,32,115,111,117,108,115,46};

    public java.lang.String escmName() {
      return "tasnim";
    }

    public Datum signature() {
      return Pair.List(new Symbol("tasnim"));
    }

    public String docstring() {
      return "Equivalent to: (car (cdr (car (cr <pair>))))";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      StringBuilder sb = new StringBuilder();
      for(int i = 0; i < x.length; ++i)
        sb.append((char)x[i]);
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
      return "Equivalent to: (car (cdr (car (car <pair>))))";
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
      return "Equivalent to: (car (cdr (car (cdr <pair>))))";
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
      return "Equivalent to: (car (cdr (cdr (car <pair>))))";
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
      return "Equivalent to: (car (cdr (cdr (cdr <pair>))))";
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
      return "Equivalent to: (cdr (car (car (car <pair>))))";
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
      return "Equivalent to: (cdr (car (car (cdr <pair>))))";
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
      return "Equivalent to: (cdr (car (cdr (car <pair>))))";
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
      return "Equivalent to: (cdr (car (cdr (cdr <pair>))))";
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
      return "Equivalent to: (cdr (cdr (car (car <pair>))))";
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
      return "Equivalent to: (cdr (cdr (car (cdr <pair>))))";
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
      return "Equivalent to: (cdr (cdr (cdr (car <pair>))))";
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
      return "Equivalent to: (cdr (cdr (cdr (cdr <pair>))))";
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
      return "Returns whether <obj> is a pair.";
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
      return "Returns whether <obj> is not a pair.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) throw new Exceptionf("'(atom? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(!(parameters.get(0) instanceof Pair));
    }
  }
}