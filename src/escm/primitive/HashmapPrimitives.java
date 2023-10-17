// Author: Jordan Randleman - escm.primitive.HashmapPrimitives
// Purpose:
//    Java primitives for hashmap procedures.

package escm.primitive;
import java.util.ArrayList;
import java.util.Objects;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.Void;
import escm.type.bool.Boolean;
import escm.type.number.Exact;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.callable.Signature;

public class HashmapPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // hashmap
  public static class Hashmap extends Primitive {
    public java.lang.String escmName() {
      return "hashmap";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap"),new Symbol("<key>"),new Symbol("<value>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nConstruct a hashmap containing key-value associations of \"<key> <value> ...\".\nCreate hashmap literals via the {<key> <value> ...} syntax.\nHashmaps are applicable to a key to get their entry: (<hashmap> <key>)";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() % 2 != 0)
        throw new Exceptionf("'(hashmap <key> <value> ...) didn't receive an even number of args: %s", Exceptionf.profileArgs(parameters));
      escm.type.Hashmap h = new escm.type.Hashmap();
      for(int i = 0, n = parameters.size(); i < n; i += 2) {
        h.set(parameters.get(i),parameters.get(i+1));
      }
      return h;
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-keys
  public static class HashmapKeys extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-keys";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-keys"),new Symbol("<hashmap>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nGet the keys of <hashmap> as a list.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-keys <hashmap>) didn't receive exactly 1 hashmap: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Hashmap)parameters.get(0)).keys();
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-values
  public static class HashmapValues extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-values";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-values"),new Symbol("<hashmap>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nGet the values of <hashmap> as a list.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-values <hashmap>) didn't receive exactly 1 hashmap: %s", Exceptionf.profileArgs(parameters));
      return ((escm.type.Hashmap)parameters.get(0)).values();
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-key?
  public static class IsHashmapKey extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-key?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-key?"),new Symbol("<hashmap>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nReturns whether <obj> is a key in <hashmap>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(hashmap-key? <hashmap> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum h = parameters.get(0);
      if(!(h instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-key? <hashmap> <obj>) 1st arg %s isn't a hashmap: %s", h.profile(), Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Hashmap)h).hasKey(parameters.get(1)));
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-val?
  public static class IsHashmapVal extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-val?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-val?"),new Symbol("<hashmap>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nReturns whether <obj> is a value in <hashmap>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(hashmap-val? <hashmap> <obj>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum h = parameters.get(0);
      if(!(h instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-val? <hashmap> <obj>) 1st arg %s isn't a hashmap: %s", h.profile(), Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Hashmap)h).hasVal(parameters.get(1)));
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-set!
  public static class HashmapSetBang extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-set!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-set!"),new Symbol("<hashmap>"),new Symbol("<key>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nAssociate <value> to <key> in <hashmap>.\nReturns whether replaced an existing value.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 3)
        throw new Exceptionf("'(hashmap-set! <hashmap> <key> <value>) expects exactly 3 args: %s", Exceptionf.profileArgs(parameters));
      Datum h = parameters.get(0);
      if(!(h instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-set! <hashmap> <key> <value>) 1st arg %s isn't a hashmap: %s", h.profile(), Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Hashmap)h).set(parameters.get(1),parameters.get(2)));
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-delete!
  public static class HashmapDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-delete!"),new Symbol("<hashmap>"),new Symbol("<key>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nDelete <key>'s association in <hashmap>. Returns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(hashmap-delete! <hashmap> <key>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum h = parameters.get(0);
      if(!(h instanceof escm.type.Hashmap))
        throw new Exceptionf("'(hashmap-delete! <hashmap> <key>) 1st arg %s isn't a hashmap: %s", h.profile(), Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(((escm.type.Hashmap)h).del(parameters.get(1)));
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-merge
  public static class HashmapMerge extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-merge";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-merge"),new Symbol("<hashmap>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nReturns a new <hashmap> combining all of the associations in \"<hashmap> ...\".";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(hashmap-merge <hashmap> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.Hashmap> hs = new ArrayList<escm.type.Hashmap>();
      for(Datum p : parameters) {
        if(!(p instanceof escm.type.Hashmap))
          throw new Exceptionf("'(hashmap-merge <hashmap> ...) invalid non-hashmap %s given: %s", p.profile(), Exceptionf.profileArgs(parameters));
        hs.add((escm.type.Hashmap)p);
      }
      return escm.type.Hashmap.merge(hs);
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap-merge!
  public static class HashmapMergeBang extends Primitive {
    public java.lang.String escmName() {
      return "hashmap-merge!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap-merge!"),new Symbol("<hashmap>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nCombine all of the associations in \"<merged-hashmap> ...\" into <hashmap>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(hashmap-merge! <hashmap> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      ArrayList<escm.type.Hashmap> hs = new ArrayList<escm.type.Hashmap>();
      Datum h =  parameters.get(0);
      if(!(h instanceof escm.type.Hashmap))
          throw new Exceptionf("'(hashmap-merge! <hashmap> ...) invalid non-hashmap %s given: %s", h.profile(), Exceptionf.profileArgs(parameters));
      for(int i = 1, n = parameters.size(); i < n; ++i) {
        Datum p = parameters.get(i);
        if(!(p instanceof escm.type.Hashmap))
          throw new Exceptionf("'(hashmap-merge! <hashmap> ...) invalid non-hashmap %s given: %s", p.profile(), Exceptionf.profileArgs(parameters));
        hs.add((escm.type.Hashmap)p);
      }
      ((escm.type.Hashmap)h).addAll(hs);
      return Void.VALUE;
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashmap?
  public static class IsHashmap extends Primitive {
    public java.lang.String escmName() {
      return "hashmap?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashmap?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nReturns whether <obj> is a hashmap.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(hashmap? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof escm.type.Hashmap);
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // hashcode
  public static class Hashcode extends Primitive {
    public java.lang.String escmName() {
      return "hashcode";
    }

    public Datum signature() {
      return Pair.List(new Symbol("hashcode"),new Symbol("<obj>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Hashmaps\nReturns the hashcode of \"<obj> ...\" combined with one another.\nUnused by any of the other hashmap primitives, but hey, it\ncould be useful to expose to users.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() < 1)
        throw new Exceptionf("'(hashcode <obj> ...) expects at least 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.size() == 1) return new Exact(parameters.get(0).hashCode());
      return new Exact(Objects.hash(parameters.toArray()));
    }
  }
}