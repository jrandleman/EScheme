// Author: Jordan Randleman - escm.primitive.lib.help.fs.ObjectNode
// Purpose:
//    Class representing an object leaf node in our fs (file system) tree!

package escm.primitive.lib.help.fs;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;
import escm.primitive.HelpPrimitives;

public class ObjectNode extends HelpNode {
  public final String name;
  public final FolderNode parent; // points to enclosing folder
  public final Datum obj;

  ObjectNode(FolderNode parent, String name, Datum obj) {
    this.parent = parent;
    this.name = name;
    this.obj = obj;
  }

  public FolderNode getParent() {
    return parent;
  }

  public String getName() {
    return name;
  }

  public String getPath() {
    ArrayList<String> path = new ArrayList<String>();
    path.add(':'+name);
    FolderNode iter = parent;
    while(iter != null) {
      path.add(':'+iter.name);
      iter = iter.parent;
    }
    StringBuilder sb = new StringBuilder();
    for(int i = path.size()-1; i >= 0; --i) {
      sb.append(path.get(i));
    }
    return sb.toString();
  }

  public Datum toDatum() {
    return new Pair(new Keyword(name),new escm.type.String(toString()));
  }

  public String toMarkdown(int depth) {
    String name = HelpPrimitives.Help.getObjectName(obj);
    if(name == null) return "";
    String docs = HelpPrimitives.Help.getObjectDocstring(obj,0);
    if(docs == null) return "";
    String sigs = HelpPrimitives.Help.getObjectSignatures(obj,0);
    String bolds = HelpNode.bolds(depth);
    StringBuilder sb = new StringBuilder();
    sb.append("\n-------------------------------------------------------------------------------\n");
    sb.append(bolds+" `"+name+"`\n");
    if(sigs != null) {
      sb.append("\n"+bolds+"# Signatures:\n");
      sb.append("```scheme\n");
      sb.append(sigs);
      sb.append("\n```\n");
    }
    sb.append("\n"+bolds+"# Description:\n");
    sb.append("```\n"+docs+"\n```\n");
    return sb.toString();
  }

  public String toString() {
    return HelpPrimitives.Help.describeObject(obj);
  }

  public void print() {
    System.out.print(toString());
    System.out.flush();
  }
}