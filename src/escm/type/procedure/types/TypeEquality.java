// Author: Jordan Randleman - escm.type.procedure.types.TypeEquality
// Purpose:
//    EScheme <callable> type annotation comparison utility functions.
//    See doc/types.md for more information on using EScheme's types.
//    Provides:
//      1. <sameType(key1,env1,key2,env2)>: Determine if types match.

package escm.type.procedure.types;
import java.util.ArrayList;
import java.util.HashSet;
import escm.util.error.Exceptionf;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.procedure.types.TypeTree.ValueGenerator;
import escm.type.oo.EscmClass;
import escm.type.oo.EscmInterface;
import escm.vm.util.Environment;
import escm.vm.util.ObjectAccessChain;

public class TypeEquality {
  ////////////////////////////////////////////////////////////////////////////
  // Type Equality Syntax Tree
  public static abstract class TypeEqualityNode {} // only supports ".equals()"


  private static class TypeEqualityCompound extends TypeEqualityNode {
    ArrayList<TypeEqualityNode> orTypes;

    public TypeEqualityCompound(ArrayList<TypeEqualityNode> orTypes) {
      this.orTypes = orTypes;
    }

    public boolean equals(Object o) {
      if(o instanceof TypeEqualityCompound) {
        HashSet<Integer> seen = new HashSet<Integer>();
        ArrayList<TypeEqualityNode> ts = ((TypeEqualityCompound)o).orTypes;
        if(ts.size() != orTypes.size()) return false;
        for(int i = 0, n = orTypes.size(); i < n; ++i) {
          int j = 0;
          for(; j < n; ++j) {
            if(!seen.contains(j) && orTypes.get(i).equals(ts.get(j))) {
              seen.add(j);
              break;
            }
          }
          if(j == n) return false;
        }
        return true;
      }
      return false;
    }
  }


  private static class TypeEqualityContainer extends TypeEqualityNode {
    String name;
    TypeEqualityNode keyType;
    TypeEqualityNode valueType;

    public TypeEqualityContainer(String name, TypeEqualityNode keyType, TypeEqualityNode valueType) {
      this.name = name;
      this.keyType = keyType;
      this.valueType = valueType;
    }

    public boolean equals(Object o) {
      if(o instanceof TypeEqualityContainer) {
        TypeEqualityContainer oc = (TypeEqualityContainer)o;
        TypeEqualityNode kt = oc.keyType;
        TypeEqualityNode vt = oc.valueType;
        if(!name.equals(oc.name)) return false;
        if((kt == null || keyType == null) && kt != keyType) return false;
        if(kt != null && keyType != null && !kt.equals(keyType)) return false;
        if((vt == null || valueType == null) && vt != valueType) return false;
        if(vt != null && valueType != null && !vt.equals(valueType)) return false;
        return true;
      }
      return false;
    }
  }


  private static class TypeEqualityPrimitive extends TypeEqualityNode {
    String name;

    public TypeEqualityPrimitive(String name) {
      this.name = name;
    }

    public boolean equals(Object o) {
      return o instanceof TypeEqualityPrimitive && name.equals(((TypeEqualityPrimitive)o).name);
    }
  }


  private static class TypeEqualityClass extends TypeEqualityNode {
    EscmClass eclass;

    public TypeEqualityClass(EscmClass eclass) {
      this.eclass = eclass;
    }

    public boolean equals(Object o) {
      return o instanceof TypeEqualityClass && eclass.eq(((TypeEqualityClass)o).eclass);
    }
  }


  private static class TypeEqualityInterface extends TypeEqualityNode {
    EscmInterface eface;

    public TypeEqualityInterface(EscmInterface eface) {
      this.eface = eface;
    }

    public boolean equals(Object o) {
      return o instanceof TypeEqualityInterface && eface.eq(((TypeEqualityInterface)o).eface);
    }
  }

  
  ////////////////////////////////////////////////////////////////////////////
  // Parser TypeEqualityNode Value Generator
  private static class TypeEqualityNodeGenerator implements ValueGenerator<TypeEqualityNode> {
    /////////////////////////////////////
    // Primitive TypeEqualityNodes
    public TypeEqualityNode primitiveAny() {
      return new TypeEqualityPrimitive("any");
    }

    public TypeEqualityNode primitiveNum() {
      return new TypeEqualityPrimitive("num");
    }
    public TypeEqualityNode primitiveInt() {
      return new TypeEqualityPrimitive("int");
    }
    public TypeEqualityNode primitiveFlo() {
      return new TypeEqualityPrimitive("flo");
    }
    public TypeEqualityNode primitiveReal() {
      return new TypeEqualityPrimitive("real");
    }
    public TypeEqualityNode primitiveExact() {
      return new TypeEqualityPrimitive("exact");
    }
    public TypeEqualityNode primitiveInexact() {
      return new TypeEqualityPrimitive("inexact");
    }

    public TypeEqualityNode primitiveStr() {
      return new TypeEqualityPrimitive("str");
    }
    public TypeEqualityNode primitiveChar() {
      return new TypeEqualityPrimitive("char");
    }
    public TypeEqualityNode primitiveKey() {
      return new TypeEqualityPrimitive("key");
    }
    public TypeEqualityNode primitiveBool() {
      return new TypeEqualityPrimitive("bool");
    }
    public TypeEqualityNode primitiveSym() {
      return new TypeEqualityPrimitive("sym");
    }
    public TypeEqualityNode primitiveVoid() {
      return new TypeEqualityPrimitive("void");
    }

    public TypeEqualityNode primitiveNil() {
      return new TypeEqualityPrimitive("nil");
    }
    public TypeEqualityNode primitiveAtom() {
      return new TypeEqualityPrimitive("atom");
    }

    public TypeEqualityNode primitiveThread() {
      return new TypeEqualityPrimitive("thread");
    }
    public TypeEqualityNode primitiveMutex() {
      return new TypeEqualityPrimitive("mutex");
    }

    public TypeEqualityNode primitiveFn() {
      return new TypeEqualityPrimitive("fn");
    }
    public TypeEqualityNode primitiveProcedure() {
      return new TypeEqualityPrimitive("procedure");
    }
    public TypeEqualityNode primitiveSyntax() {
      return new TypeEqualityPrimitive("syntax");
    }

    public TypeEqualityNode primitiveMetaobj() {
      return new TypeEqualityPrimitive("metaobj");
    }
    public TypeEqualityNode primitiveObject() {
      return new TypeEqualityPrimitive("object");
    }
    public TypeEqualityNode primitiveClass() {
      return new TypeEqualityPrimitive("class");
    }
    public TypeEqualityNode primitiveInterface() {
      return new TypeEqualityPrimitive("interface");
    }

    public TypeEqualityNode primitiveDottable() {
      return new TypeEqualityPrimitive("dottable");
    }
    public TypeEqualityNode primitiveModule() {
      return new TypeEqualityPrimitive("module");
    }

    public TypeEqualityNode primitivePort() {
      return new TypeEqualityPrimitive("port");
    }
    public TypeEqualityNode primitiveIport() {
      return new TypeEqualityPrimitive("iport");
    }
    public TypeEqualityNode primitiveOport() {
      return new TypeEqualityPrimitive("oport");
    }

    public TypeEqualityNode primitiveTypeAlias() {
      return new TypeEqualityPrimitive("type-alias");
    }

    /////////////////////////////////////
    // Container TypeEqualityNodes
    public TypeEqualityNode containerVec(TypeEqualityNode valType) {
      return new TypeEqualityContainer("vec", null, valType);
    }
    public TypeEqualityNode containerMap(TypeEqualityNode keyType, TypeEqualityNode valType) {
      return new TypeEqualityContainer("map", keyType, valType);
    }
    public TypeEqualityNode containerMap(TypeEqualityNode valType) {
      return new TypeEqualityContainer("map", null, valType);
    }
    public TypeEqualityNode containerPair(TypeEqualityNode keyType, TypeEqualityNode valType) {
      return new TypeEqualityContainer("pair", keyType, valType);
    }
    public TypeEqualityNode containerPair(TypeEqualityNode valType) {
      return new TypeEqualityContainer("pair", null, valType);
    }
    public TypeEqualityNode containerList(TypeEqualityNode valType) {
      return new TypeEqualityContainer("list", null, valType);
    }
    public TypeEqualityNode containerAC(TypeEqualityNode valType) {
      return new TypeEqualityContainer("ac", null, valType);
    }
    public TypeEqualityNode containerOC(TypeEqualityNode valType) {
      return new TypeEqualityContainer("oc", null, valType);
    }


    /////////////////////////////////////
    // User Type (Class, Interface, Alias) TypeEqualityNodes
    public TypeEqualityNode userType(String type, String name, Environment env) throws Exception {
      Datum classOrInterfaceOrAlias = env.nullableGet(name);
      if(classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s",name,type);
      }
      if(classOrInterfaceOrAlias instanceof TypeAlias) {
        TypeAlias alias = (TypeAlias)classOrInterfaceOrAlias;
        return TypeTree.walk(alias.typeKeyword(),alias.definitionEnvironment(),TYPE_EQUALITY_NODE_GENERATOR);
      } else if(classOrInterfaceOrAlias instanceof EscmClass) {
        return new TypeEqualityClass((EscmClass)classOrInterfaceOrAlias);
      } else if(classOrInterfaceOrAlias instanceof EscmInterface) {
        return new TypeEqualityInterface((EscmInterface)classOrInterfaceOrAlias);
      } else {
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s",name,type);
      }
    }
    public TypeEqualityNode moduleUserType(String type, String name, ObjectAccessChain chain, Environment env) throws Exception {
      Datum classOrInterfaceOrAlias = chain.nullableLoadWithState(env);
      if(classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s",name,type);
      }
      if(classOrInterfaceOrAlias instanceof TypeAlias) {
        TypeAlias alias = (TypeAlias)classOrInterfaceOrAlias;
        return TypeTree.walk(alias.typeKeyword(),alias.definitionEnvironment(),TYPE_EQUALITY_NODE_GENERATOR);
      } else if(classOrInterfaceOrAlias instanceof EscmClass) {
        return new TypeEqualityClass((EscmClass)classOrInterfaceOrAlias);
      } else if(classOrInterfaceOrAlias instanceof EscmInterface) {
        return new TypeEqualityInterface((EscmInterface)classOrInterfaceOrAlias);
      } else {
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s",name,type);
      }
    }


    /////////////////////////////////////
    // Compound Type
    public TypeEqualityNode compound(ArrayList<TypeEqualityNode> orTypes) {
      return new TypeEqualityCompound(orTypes);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Type Comparison
  private static TypeEqualityNodeGenerator TYPE_EQUALITY_NODE_GENERATOR = new TypeEqualityNodeGenerator();


  public static TypeEqualityNode tree(Keyword key, Environment env) throws Exception {
    return TypeTree.walk(key,env,TYPE_EQUALITY_NODE_GENERATOR);
  }


  public static boolean sameType(Keyword key1, Environment env1, Keyword key2, Environment env2) throws Exception {
    return tree(key1,env1).equals(tree(key2,env2));
  }
}