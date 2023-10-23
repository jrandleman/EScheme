// Author: Jordan Randleman - escm.primitive.lib.help.fs.ObjectNode
// Purpose:
//    Class representing an object leaf node in our fs (file system) tree!

package escm.primitive.lib.help.fs;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
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
    return new Keyword(name);
  }

  public void print() {
    HelpPrimitives.Help.describeObject(obj);
  }
}