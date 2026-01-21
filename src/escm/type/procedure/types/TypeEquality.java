// Author: Jordan Randleman - escm.type.procedure.types.TypeEquality
// Purpose:
//    EScheme <callable> type annotation comparison utility functions.
//    See doc/types.md for more information on using EScheme's types.
//    Provides:
//      1. <Node>: Type tree node class (supports ".equals()").
//      2. <tree(key,env)>: Keyword type tree node generator.
//      3. <sameType(key1,env1,key2,env2)>: Determine if types match.

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
  public static abstract class Node {
  } // only supports ".equals()"

  private static class CompoundNode extends Node {
    ArrayList<Node> orTypes;

    public CompoundNode(ArrayList<Node> orTypes) {
      this.orTypes = orTypes;
    }

    public boolean equals(Object o) {
      if (o instanceof CompoundNode) {
        HashSet<Integer> seen = new HashSet<Integer>();
        ArrayList<Node> ts = ((CompoundNode) o).orTypes;
        if (ts.size() != orTypes.size())
          return false;
        for (int i = 0, n = orTypes.size(); i < n; ++i) {
          int j = 0;
          for (; j < n; ++j) {
            if (!seen.contains(j) && orTypes.get(i).equals(ts.get(j))) {
              seen.add(j);
              break;
            }
          }
          if (j == n)
            return false;
        }
        return true;
      }
      return false;
    }
  }

  private static class ContainerNode extends Node {
    String name;
    Node keyType;
    Node valueType;

    public ContainerNode(String name, Node keyType, Node valueType) {
      this.name = name;
      this.keyType = keyType;
      this.valueType = valueType;
    }

    public boolean equals(Object o) {
      if (o instanceof ContainerNode) {
        ContainerNode oc = (ContainerNode) o;
        Node kt = oc.keyType;
        Node vt = oc.valueType;
        if (!name.equals(oc.name))
          return false;
        if ((kt == null || keyType == null) && kt != keyType)
          return false;
        if (kt != null && keyType != null && !kt.equals(keyType))
          return false;
        if ((vt == null || valueType == null) && vt != valueType)
          return false;
        if (vt != null && valueType != null && !vt.equals(valueType))
          return false;
        return true;
      }
      return false;
    }
  }

  private static class PrimitiveNode extends Node {
    String name;

    public PrimitiveNode(String name) {
      this.name = name;
    }

    public boolean equals(Object o) {
      return o instanceof PrimitiveNode && name.equals(((PrimitiveNode) o).name);
    }
  }

  private static class ClassNode extends Node {
    EscmClass eclass;

    public ClassNode(EscmClass eclass) {
      this.eclass = eclass;
    }

    public boolean equals(Object o) {
      return o instanceof ClassNode && eclass.eq(((ClassNode) o).eclass);
    }
  }

  private static class InterfaceNode extends Node {
    EscmInterface eface;

    public InterfaceNode(EscmInterface eface) {
      this.eface = eface;
    }

    public boolean equals(Object o) {
      return o instanceof InterfaceNode && eface.eq(((InterfaceNode) o).eface);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Parser Node Value Generator
  private static class NodeGenerator implements ValueGenerator<Node> {
    /////////////////////////////////////
    // Primitive Nodes
    public Node primitiveAny() {
      return new PrimitiveNode("any");
    }

    public Node primitiveNum() {
      return new PrimitiveNode("num");
    }

    public Node primitiveInt() {
      return new PrimitiveNode("int");
    }

    public Node primitiveFlo() {
      return new PrimitiveNode("flo");
    }

    public Node primitiveReal() {
      return new PrimitiveNode("real");
    }

    public Node primitiveExact() {
      return new PrimitiveNode("exact");
    }

    public Node primitiveInexact() {
      return new PrimitiveNode("inexact");
    }

    public Node primitiveBigInt() {
      return new PrimitiveNode("bigint");
    }

    public Node primitiveStr() {
      return new PrimitiveNode("str");
    }

    public Node primitiveChar() {
      return new PrimitiveNode("char");
    }

    public Node primitiveKey() {
      return new PrimitiveNode("key");
    }

    public Node primitiveBool() {
      return new PrimitiveNode("bool");
    }

    public Node primitiveSym() {
      return new PrimitiveNode("sym");
    }

    public Node primitiveVoid() {
      return new PrimitiveNode("void");
    }

    public Node primitiveNil() {
      return new PrimitiveNode("nil");
    }

    public Node primitiveAtom() {
      return new PrimitiveNode("atom");
    }

    public Node primitiveThread() {
      return new PrimitiveNode("thread");
    }

    public Node primitiveMutex() {
      return new PrimitiveNode("mutex");
    }

    public Node primitiveFn() {
      return new PrimitiveNode("fn");
    }

    public Node primitiveProcedure() {
      return new PrimitiveNode("procedure");
    }

    public Node primitiveSyntax() {
      return new PrimitiveNode("syntax");
    }

    public Node primitiveMetaobj() {
      return new PrimitiveNode("metaobj");
    }

    public Node primitiveObject() {
      return new PrimitiveNode("object");
    }

    public Node primitiveClass() {
      return new PrimitiveNode("class");
    }

    public Node primitiveInterface() {
      return new PrimitiveNode("interface");
    }

    public Node primitiveDottable() {
      return new PrimitiveNode("dottable");
    }

    public Node primitiveModule() {
      return new PrimitiveNode("module");
    }

    public Node primitivePort() {
      return new PrimitiveNode("port");
    }

    public Node primitiveIport() {
      return new PrimitiveNode("iport");
    }

    public Node primitiveOport() {
      return new PrimitiveNode("oport");
    }

    public Node primitiveTypeAlias() {
      return new PrimitiveNode("type-alias");
    }

    /////////////////////////////////////
    // Container Nodes
    public Node containerVec(Node valType) {
      return new ContainerNode("vec", null, valType);
    }

    public Node containerMap(Node keyType, Node valType) {
      return new ContainerNode("map", keyType, valType);
    }

    public Node containerMap(Node valType) {
      return new ContainerNode("map", null, valType);
    }

    public Node containerPair(Node keyType, Node valType) {
      return new ContainerNode("pair", keyType, valType);
    }

    public Node containerPair(Node valType) {
      return new ContainerNode("pair", null, valType);
    }

    public Node containerList(Node valType) {
      return new ContainerNode("list", null, valType);
    }

    public Node containerAC(Node valType) {
      return new ContainerNode("ac", null, valType);
    }

    public Node containerOC(Node valType) {
      return new ContainerNode("oc", null, valType);
    }

    /////////////////////////////////////
    // User Type (Class, Interface, Alias) Nodes
    public Node userType(String type, String name, Environment env) throws Exception {
      Datum classOrInterfaceOrAlias = env.nullableGet(name);
      if (classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s", name, type);
      }
      if (classOrInterfaceOrAlias instanceof TypeAlias) {
        TypeAlias alias = (TypeAlias) classOrInterfaceOrAlias;
        return TypeTree.walk(alias.typeKeyword(), alias.definitionEnvironment(), NODE_GENERATOR);
      } else if (classOrInterfaceOrAlias instanceof EscmClass) {
        return new ClassNode((EscmClass) classOrInterfaceOrAlias);
      } else if (classOrInterfaceOrAlias instanceof EscmInterface) {
        return new InterfaceNode((EscmInterface) classOrInterfaceOrAlias);
      } else {
        throw new Exceptionf("Invalid Class/Interface/Alias Type \"%s\": %s", name, type);
      }
    }

    public Node moduleUserType(String type, String name, ObjectAccessChain chain, Environment env) throws Exception {
      Datum classOrInterfaceOrAlias = chain.nullableLoadWithState(env);
      if (classOrInterfaceOrAlias == null) {
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s", name, type);
      }
      if (classOrInterfaceOrAlias instanceof TypeAlias) {
        TypeAlias alias = (TypeAlias) classOrInterfaceOrAlias;
        return TypeTree.walk(alias.typeKeyword(), alias.definitionEnvironment(), NODE_GENERATOR);
      } else if (classOrInterfaceOrAlias instanceof EscmClass) {
        return new ClassNode((EscmClass) classOrInterfaceOrAlias);
      } else if (classOrInterfaceOrAlias instanceof EscmInterface) {
        return new InterfaceNode((EscmInterface) classOrInterfaceOrAlias);
      } else {
        throw new Exceptionf("Invalid Module Class/Interface/Alias Type \"%s\": %s", name, type);
      }
    }

    /////////////////////////////////////
    // Compound Type
    public Node compound(ArrayList<Node> orTypes) {
      return new CompoundNode(orTypes);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Type Equality Tree Creation
  private static NodeGenerator NODE_GENERATOR = new NodeGenerator();

  public static Node tree(Keyword key, Environment env) throws Exception {
    return TypeTree.walk(key, env, NODE_GENERATOR);
  }

  public static Node tree(String key, Environment env) throws Exception {
    return TypeTree.walk(key, env, NODE_GENERATOR);
  }

  ////////////////////////////////////////////////////////////////////////////
  // Type Comparison
  public static boolean sameType(Keyword key1, Environment env1, Keyword key2, Environment env2) throws Exception {
    return tree(key1, env1).equals(tree(key2, env2));
  }

  public static boolean sameType(String key1, Environment env1, String key2, Environment env2) throws Exception {
    return tree(key1, env1).equals(tree(key2, env2));
  }
}