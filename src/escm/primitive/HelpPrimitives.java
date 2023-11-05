// Author: Jordan Randleman - escm.primitive.HelpPrimitives
// Purpose:
//    Java primitive to help explain language features.
//
//      => NOTE: <help> only prints to STDOUT & accepts from STDIN
//         -> eg NOT (current-output-port) & (current-input-port)

package escm.primitive;
import java.util.ArrayList;
import java.util.Collections;
import java.util.TreeMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Keyword;
import escm.type.Void;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.type.primitive.Primitive;
import escm.vm.util.Environment;
import escm.primitive.MetaPrimitives;
import escm.primitive.lib.help.fs.HelpNode;
import escm.primitive.lib.help.fs.FolderNode;

public class HelpPrimitives {
  public static class Help extends Primitive {
    //////////////////////////////////////////////////////////////////////////
    // Terminate Help Exception
    private static class TerminateHelpMenuException extends Exception {
      public TerminateHelpMenuException() {
        super("Terminating the help menu.");
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Interactive DB Shell
    private static final int NOT_A_PARENT_DIRECTORY = -1;


    private static void helpCommand() {
      System.out.println("\n===============================================================================");
      System.out.println("EScheme's help menu, by default, will interpret user inputs as help file");
      System.out.println("names to navigate to from within the current directory. Use .. to denote the");
      System.out.println("parent directory, and . for the current directory.\n");
      System.out.println("The help menu also supports a limited number of keyword commands: behaviors");
      System.out.println("that <help> can execute to perform special actions. These include:\n");
      System.out.println("  1. :quit | Ends the current <help> session. Equivalent to typing <EOF>");
      System.out.println("  2. :help | Prints this menu");
      System.out.println("  3. :~    | Changes the current directory back to the home directory\n");
      System.out.println("Type a file name below to navigate to its help entry:");
    }


    private static FolderNode goHome(FolderNode root) {
      FolderNode parent = root.getParent();
      while(parent != null) {
        root = parent;
        parent = root.getParent();
      }
      return root;
    }


    private static FolderNode executeCommand(FolderNode root, String cmd) throws Exception {
      switch(cmd) {
        case ":quit": {
          throw new TerminateHelpMenuException();
        }
        case ":help": {
          helpCommand();
          root.print();
          return root;
        }
        case ":~": {
          return goHome(root);
        }
        default: {
          System.err.printf("help> Invalid keyword command: %s\n", cmd);
          return root;
        }
      }
    }


    // Returns <NOT_A_PARENT_DIRECTORY> if <cmd> isn't only periods
    private static int gotoParentDirectory(String cmd) {
      int ancestryCount = NOT_A_PARENT_DIRECTORY;
      for(int i = 0; i < cmd.length(); ++i) {
        if(cmd.charAt(i) == '.') {
          ++ancestryCount;
        } else {
          return NOT_A_PARENT_DIRECTORY;
        }
      }
      return ancestryCount;
    }


    private static FolderNode openFile(FolderNode root, String cmd) throws Exception {
      int ancestryCount = gotoParentDirectory(cmd);
      if(ancestryCount != NOT_A_PARENT_DIRECTORY) {
        for(; ancestryCount > 0; --ancestryCount) {
          root = root.getShellParent();
        }
        root.print();
        return root;
      }
      for(ConcurrentHashMap.Entry<String,HelpNode> entry : root.children.entrySet()) {
        if(entry.getKey().equals(cmd)) {
          HelpNode target = entry.getValue();
          target.print();
          if(target instanceof FolderNode) {
            return (FolderNode)target;
          } else {
            return root;
          }
        }
      }
      System.err.printf("help> Invalid file (not in the current directory): %s\n", cmd);
      root.print();
      return null;
    }


    private static boolean isCommand(String cmd) {
      return cmd.charAt(0) == ':';
    }


    private static FolderNode executeProcess(FolderNode root, String cmd) throws Exception {
      if(isCommand(cmd)) {
        return executeCommand(root,cmd);
      } else {
        return openFile(root,cmd);
      }
    }


    public static String[] preprocessCommand(String prompt) {
      prompt = prompt.trim();
      if(isCommand(prompt)) return new String[]{prompt};
      return prompt.split(":");
    }


    static boolean isValidCommand(String[] cmds) {
      if(cmds.length == 0) return false;
      for(int i = 0; i < cmds.length; ++i) {
        if(cmds[i].length() == 0) return false;
      }
      return true;
    }


    // Returns the root of the next prompt's execution location
    private static FolderNode prompt(FolderNode root) throws Exception {
      System.out.println("");
      while(true) {
        System.out.print("help["+root.getPath()+"]> ");
        System.out.flush();
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String input = br.readLine();
        if(input == null) {
          System.out.println("");
          throw new TerminateHelpMenuException();
        }
        String[] cmds = preprocessCommand(input);
        if(isValidCommand(cmds)) {
          for(int i = 0; i < cmds.length; ++i) {
            FolderNode newRoot = executeProcess(root,cmds[i]);
            if(newRoot == null) return prompt(root); // error message printed
            if(i+1 < cmds.length) System.out.println("\n  -----------------------------------------------------------------------------");
            root = newRoot;
          }
          return root;
        }
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Interactive DB Traversal
    private static synchronized void launchInteractiveMenu(Environment env) throws Exception {
      FolderNode home = HelpNode.createHomeDirectory(env);
      System.out.println("\n===============================================================================");
      System.out.println("Welcome to EScheme's help menu!");
      System.out.println("");
      System.out.println("Type :quit to quit, :help for a list of commands, .. to go to the parent");
      System.out.println("directory, . for the current directory, or any of the options below for");
      System.out.println("their help entries:");
      home.print();
      while(true) {
        home = prompt(home);
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Static DB Traversal
    //   => Returns <null> if <target> is an invalid help path, as would be
    //      entered in the interactive menu's home directory!
    private static String getInteractiveMenuEntry(Environment env, String target) throws Exception {
      FolderNode home = HelpNode.createHomeDirectory(env);
      String[] cmds = preprocessCommand(target);
      if(isValidCommand(cmds)) {
        HelpNode item = home.get(cmds);
        if(item == null) return null;
        return item.toString();
      }
      return null;
    }


    //////////////////////////////////////////////////////////////////////////
    // Analyze a Given Object
    private static String getTab(int tabSpaceWidth) {
      StringBuilder tab = new StringBuilder();
      for(; tabSpaceWidth > 0; --tabSpaceWidth) {
        tab.append(" ");
      }
      return tab.toString();
    }


    private static boolean hasMultipleSignatures(Datum sigs) {
      return Pair.isListPair(sigs) && ((Pair)sigs).car() instanceof Pair;
    }


    private static String padNewlinesWithTabs(String docs, String tab) {
      return tab+String.join("\n"+tab,docs.split("\n"));
    }


    // Might return <null> on fail
    public static String getObjectName(Datum obj) {
      Datum name = MetaPrimitives.CallableName.logic(obj);
      if(name instanceof Symbol) return ((Symbol)name).value();
      return null;
    }


    // Might return <null> on fail
    public static String getObjectSignatures(Datum obj, int tabSpaceWidth) {
      String tab = getTab(tabSpaceWidth);
      Datum sigs = MetaPrimitives.CallableSignature.logic(obj);
      if(sigs instanceof Boolean) return null;
      if(hasMultipleSignatures(sigs)) {
        StringBuilder sb = new StringBuilder();
        while(sigs instanceof Pair) {
          Pair p = (Pair)sigs;
          sb.append(padNewlinesWithTabs(p.car().pprint(),tab));
          sigs = p.cdr();
          if(sigs instanceof Pair) sb.append("\n");
        }
        return sb.toString();
      } else {
        return padNewlinesWithTabs(sigs.pprint(),tab);
      }
    }


    // Might return <null> on fail
    public static String getObjectDocstring(Datum obj, int tabSpaceWidth) {
      String tab = getTab(tabSpaceWidth);
      Datum docs = MetaPrimitives.DocStringExtractor.helpLogic(obj);
      if(docs instanceof escm.type.String && ((escm.type.String)docs).value().length() > 0) {
        return padNewlinesWithTabs(((escm.type.String)docs).value(),tab);
      } else {
        return tab+"Given item: "+obj.profile();
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Describe a Given Object
    private static void printObjectName(StringBuilder sb, Datum obj) {
      String nameValue = getObjectName(obj);
      if(nameValue != null) {
        sb.append("Name: ");
        sb.append(nameValue+"\n");
        for(int i = 0, n = nameValue.length()+6; i < n; ++i) {
          sb.append('*');
        }
        sb.append("\n\n");
      }
    }


    private static void printObjectSignatures(StringBuilder sb, Datum obj) {
      String sigs = getObjectSignatures(obj,2);
      if(sigs != null) {
        sb.append("Signatures:\n");
        sb.append(sigs);
        sb.append("\n\n");
      }
    }


    private static void printObjectDocstring(StringBuilder sb, Datum obj) {
      String docs = getObjectDocstring(obj,2);
      if(docs != null) {
        sb.append("Description:\n");
        sb.append(docs);
        sb.append("\n");
      }
    }


    public static synchronized String describeObject(Datum obj) {
      StringBuilder sb = new StringBuilder();
      sb.append("===============================================================================\n");
      printObjectName(sb,obj);
      printObjectSignatures(sb,obj);
      printObjectDocstring(sb,obj);
      sb.append("===============================================================================\n");
      return sb.toString();
    }


    //////////////////////////////////////////////////////////////////////////
    // Main Dispatch
    public java.lang.String escmName() {
      return "help";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("help")),
        Pair.List(new Symbol("help"),new Symbol("<path-string>")),
        Pair.List(new Symbol("help"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:Meta\nObj Argument:\n  Get information about <obj>.\n\nString Argument:\n  Get result of typing in <path-string> to the interactive help menu.\n\nNo Arguments:\n  Launch the interactive help menu. The help menu consists of folders, which\n  in turn hold descriptions of various variables in EScheme's environment.\n\n    * Note that the <help> menu always operates relative to the program's\n      original <stdin> and <stdout> streams, rather than the values of\n      <current-input-port> and <current-output-port>.\n\n  Type folder names in the input prompt to enter them, and use \":\" as a \n  separator to enter multiple folders (e.g. \"folder1:folder2:folder3\").\n  Type . for the current directory, .. for the parent, ... for the \n  grandparent, etc.\n\n  Type :quit to quit, :~ to return to the home directory, or :help for more \n  information.\n\n  Procedures, classes, and interfaces that want to explain how they work in \n  the <help> menu should use <docstring> syntax. Within the <docstring>, the\n  \"@help\" syntax can be used to place the variable within a certain \n  subdirectory of the help menu. For example:\n\n    (define (fact n)\n      \"\n      @help:Procedures:Numbers\n      The factorial function. Accepts an int arg.\n      \"\n      (if (< n 2)\n          1\n          (* n (fact (- n 1)))))\n\n  would put fact's <help> entry in the Numbers directory, which itself is in\n  the Procedures directory. Docstring entries without the \"@help\" syntax\n  are placed in the \"Misc\" directory.\n\nSee <define-help> to register topic documents in help's file tree.\nSee <help-directory> to get help's files as an EScheme data structure.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n > 1)
        throw new Exceptionf("'(help <optional-obj>) received more than 1 arg: %s", Exceptionf.profileArgs(parameters));
      try {
        // Launch the interactive menu
        if(n == 0) {
          launchInteractiveMenu(this.definitionEnvironment);
        // Or inspect an item
        } else {
          // Given a string: get the <help> menu result of navigating the given path
          Datum obj = parameters.get(0);
          if(obj instanceof escm.type.String) {
            String entry = getInteractiveMenuEntry(this.definitionEnvironment,((escm.type.String)obj).value());
            if(entry == null) return Boolean.FALSE;
            return new escm.type.String(entry);
          // Given an obj: print out information about the given object
          } else {
            System.out.print(describeObject(obj));
            System.out.flush();
          }
        }
      } catch(TerminateHelpMenuException e) {
        // Ignore this exception, thrown when user enters :quit or ^D (EOF)
      }
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // define-help
  public static class DefineHelp extends Primitive {
    //////////////////////////////////////////////////////////////////////////
    // Global Topic Storage (accessed by <escm.primitive.lib.help.fs.HelpNode>)
    public static final ConcurrentHashMap<String,String> TOPICS = new ConcurrentHashMap<String,String>();


    //////////////////////////////////////////////////////////////////////////
    // Main Execution
    public java.lang.String escmName() {
      return "define-help";
    }

    public Datum signature() {
      return Pair.List(new Symbol("define-help"),new Symbol("<path:name-string>"),new Symbol("<docstring>"));
    }

    public String docstring() {
      return "@help:Procedures:Meta\nRegister the <docstring> topic document in help's file tree, at path <path> in\nfile <name>. Note that <path> denotes a set of folder names seperated by <:>.\n\nAliased by <defhelp>. Use <help> to see defined topic documents.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(help <path:name-string> <docstring>) expects exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum path = parameters.get(0);
      Datum docs = parameters.get(1);
      if(!(path instanceof escm.type.String))
        throw new Exceptionf("'(help <path:name-string> <docstring>) <path:name> %s isn't a string: %s", path.profile(), Exceptionf.profileArgs(parameters));
      if(!(docs instanceof escm.type.String))
        throw new Exceptionf("'(help <path:name-string> <docstring>) <docstring> %s isn't a string: %s", docs.profile(), Exceptionf.profileArgs(parameters));
      String pathStr = ((escm.type.String)path).value();
      String docsStr = ((escm.type.String)docs).value();
      if(!Help.isValidCommand(Help.preprocessCommand(pathStr)))
        throw new Exceptionf("'(help <path:name-string> <docstring>) <path:name> %s has invalid folder names: %s", docs.profile(), Exceptionf.profileArgs(parameters));
      TOPICS.put(pathStr.trim(),docsStr);
      return Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // help-directory
  public static class HelpDirectory extends Primitive {
    public java.lang.String escmName() {
      return "help-directory";
    }

    public Datum signature() {
      return Pair.List(new Symbol("help-directory"));
    }

    public String docstring() {
      return "@help:Procedures:Meta\nGet the entire <help> menu directory as an EScheme data structure. A folder\nis a list that starts with its name as a keyword, followed by its child files.\nA topic is a pair with a keyword name <car> and a docstring <cdr>.\n\nUse <help> to explore this data structure interactively via the command line.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(help-directory) doesn't accept arguments: %s", Exceptionf.profileArgs(parameters));
      return HelpNode.createHomeDirectory(this.definitionEnvironment).toDatum();
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // help-markdown
  public static class HelpMarkdown extends Primitive {
    public java.lang.String escmName() {
      return "help-markdown";
    }

    public Datum signature() {
      return Pair.List(new Symbol("help-markdown"));
    }

    public String docstring() {
      return "@help:Procedures:Meta\nGet the entire <help> menu directory as a markdown string. Folder contents are\norganized by topics then subfolders, with each section being alphabetically\nlisted.\n\nUse <help> to explore this markdown interactively via the command line.";
    }

    private static String getTableOfContents(HelpNode root, String indents) {
      if(root instanceof FolderNode) {
        String name = root.getName();
        if(name.equals(HelpNode.UNCATEGORIZED_VARIABLES_FOLDER_NAME)) return "";
        StringBuilder sb = new StringBuilder();
        String link = name.equals("Syntax") ? "Syntax-1" : name; // clashes with <syntax?> from <:~:Procedures>
        sb.append(indents+"* ["+name+"](#"+link+")\n");
        for(Map.Entry<String,HelpNode> entry : ((FolderNode)root).getOrderedChildren().entrySet()) {
          sb.append(getTableOfContents(entry.getValue(),indents+"  "));
        }
        return sb.toString();
      } else {
        // return indents+"* `"+root.getName()+"`\n";
        return ""; // no longer printing anything in ToC for non-folders
      }
    }

    private static String getTableOfContents(TreeMap<String,HelpNode> contents) {
      StringBuilder toc = new StringBuilder("-------------------------------------------------------------------------------\n");
      toc.append("# Table of Contents\n\n");
      for(ConcurrentHashMap.Entry<String,HelpNode> entry : contents.entrySet()) {
        toc.append(getTableOfContents(entry.getValue(),""));
      }
      return toc.toString();
    }

    private static String getMarkdown(TreeMap<String,HelpNode> contents) {
      StringBuilder md = new StringBuilder();
      for(ConcurrentHashMap.Entry<String,HelpNode> entry : contents.entrySet()) {
        md.append(entry.getValue().toMarkdown(1));
      }
      return md.toString();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0)
        throw new Exceptionf("'(help-markdown) doesn't accept arguments: %s", Exceptionf.profileArgs(parameters));
      TreeMap<String,HelpNode> contents = HelpNode.createHomeDirectory(this.definitionEnvironment).getOrderedChildren();
      return new escm.type.String(getTableOfContents(contents)+"\n"+getMarkdown(contents));
    }
  }
}