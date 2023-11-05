// Author: Jordan Randleman - escm.primitive.lib.help.fs.TopicNode
// Purpose:
//    Class representing a topic leaf node in our fs (file system) tree!

package escm.primitive.lib.help.fs;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;

public class TopicNode extends HelpNode {
  public final String name;
  public final FolderNode parent; // points to enclosing folder
  public final String description;

  TopicNode(FolderNode parent, String name, String description) {
    this.parent = parent;
    this.name = name;
    this.description = description;
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
    String bolds = HelpNode.bolds(depth);
    StringBuilder sb = new StringBuilder();
    sb.append("\n-------------------------------------------------------------------------------\n");
    sb.append(bolds+" `"+name+"`\n\n");
    sb.append(bolds+"# Description:\n");
    sb.append("```\n"+description+"\n```\n");
    return sb.toString();
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("===============================================================================\n");
    sb.append("Entry: "+name+"\n");
    for(int i = 0, n = name.length()+7; i < n; ++i) {
      sb.append('*');
    }
    sb.append("\n\nDescription:\n  "+String.join("\n  ",description.split("\n"))+"\n");
    sb.append("===============================================================================\n");
    return sb.toString();
  }

  public void print() {
    System.out.print(toString());
    System.out.flush();
  }
}