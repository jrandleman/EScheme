// Author: Jordan Randleman - escm.primitive.lib.help.fs.FolderNode
// Purpose:
//    Class representing a branch node in our fs (file system) tree!

package escm.primitive.lib.help.fs;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;
import escm.type.Datum;
import escm.type.Keyword;
import escm.type.Pair;
import escm.type.Nil;

public class FolderNode extends HelpNode {
  //////////////////////////////////////////////////////////////////////////
  // <HelpNode> Implementation
  public final String name;
  public final FolderNode parent; // points to enclosing folder; topmost points to :home <folder>, who's parent is <null>
  public final ConcurrentHashMap<String,HelpNode> children = new ConcurrentHashMap<String,HelpNode>();

  FolderNode(FolderNode parent, String name) {
    this.parent = parent;
    this.name = name;
  }

  public FolderNode() { // create the home directory
    this.parent = null;
    this.name = HOME_DIRECTORY_FOLDER_NAME;
  }

  public void addObject(String[] dir, String name, Datum obj) {
    FolderNode iter = createDirectory(dir,name);
    if(iter == null) return;
    ObjectNode objNode = new ObjectNode(iter,name,obj);
    iter.children.put(name,objNode);
  }

  public void addTopic(String[] dir, String name, String topic) {
    FolderNode iter = createDirectory(dir,name);
    if(iter == null) return;
    TopicNode topicNode = new TopicNode(iter,name,topic);
    iter.children.put(name,topicNode);
  }

  public HelpNode get(String name) { // returns <null> if DNE
    return children.get(name);
  }

  public HelpNode get(String[] names) { // returns <null> if DNE
    if(names.length == 0) return null;
    HelpNode item = this;
    for(int i = 0, n = names.length-1; i < n && item != null; ++i) {
      if(item instanceof FolderNode) {
        item = ((FolderNode)item).children.get(names[i]);
      } else {
        return null; // found non-<FolderNode> too early in the chain
      }
    }
    if(item == null) return null;
    return ((FolderNode)item).children.get(names[names.length-1]);
  }

  public FolderNode getParent() {
    return parent;
  }

  public FolderNode getShellParent() {
    if(parent == null) return this;
    return parent;
  }

  public String getPath() {
    ArrayList<String> path = new ArrayList<String>();
    FolderNode iter = this;
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
    Datum treeDatum = Nil.VALUE;
    for(ConcurrentHashMap.Entry<String,HelpNode> entry : children.entrySet()) {
      treeDatum = new Pair(entry.getValue().toDatum(),treeDatum);
    }
    return new Pair(new Keyword(name),treeDatum);
  }

  public String toString() {
    ArrayList<String> options = new ArrayList<String>();
    for(ConcurrentHashMap.Entry<String,HelpNode> entry : children.entrySet()) {
      options.add(entry.getKey());
    }
    Collections.sort(options);
    return getOptionsMenu(options);
  }

  public void print() {
    System.out.print(toString());
    System.out.flush();
  }


  //////////////////////////////////////////////////////////////////////////
  // Create a Directory Chain to <name> with <dir>. Returns <null> on fail.
  private FolderNode createDirectory(String[] dir, String name) {
    if(name.startsWith(RESERVED_PREFIX_1) || name.startsWith(RESERVED_PREFIX_2)) return null;
    FolderNode iter = this;
    for(int i = 0; i < dir.length; ++i) {
      HelpNode entry = iter.children.get(dir[i]);
      if(entry != null) {
        if(!(entry instanceof FolderNode)) return null; // failed to add
        iter = (FolderNode)entry;
      } else {
        iter = new FolderNode(iter,dir[i]);
        iter.parent.children.put(dir[i],iter);
      }
    }
    return iter;
  }


  //////////////////////////////////////////////////////////////////////////
  // Internal File System Navigator Support Function
  private static final int HELP_MENU_COLUMNS = 5;

  private static String getOptionsMenu(ArrayList<String> options) {
    StringBuilder output = new StringBuilder();
    // Get the lengths of each menu column
    int[] col_lengths = new int[HELP_MENU_COLUMNS];
    int totalOptions = options.size();
    for(int col = 0; col < HELP_MENU_COLUMNS; ++col) {
      for(int i = col; i < totalOptions; i += HELP_MENU_COLUMNS) {
        int entryLength = options.get(i).length();
        if(col_lengths[col] < entryLength) {
          col_lengths[col] = entryLength;
        }
      }
    }
    // Print the menu
    if(totalOptions > 0) output.append("\n  ");
    for(int i = 0; i < totalOptions; ++i) {
      String option = options.get(i);
      StringBuilder sb = new StringBuilder();
      sb.append(option);
      sb.append("  ");
      for(int space = 0, n = col_lengths[i%HELP_MENU_COLUMNS]-option.length(); space < n; ++space) {
        sb.append(' ');
      }
      output.append(sb.toString());
      if((i+1)%HELP_MENU_COLUMNS == 0) {
        output.append("\n  ");
      } else if(i+1 == totalOptions) {
        output.append('\n');
      }
    }
    return output.toString();
  }
}