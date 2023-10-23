// Author: Jordan Randleman - escm.primitive.HelpPrimitives
// Purpose:
//    Java primitive to help explain language features.
//
//      => NOTE: <help> only prints to STDOUT & accepts from STDIN
//         -> eg NOT (current-output-port) & (current-input-port)

package escm.primitive;
import java.util.ArrayList;
import java.util.Collections;
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
    // Describe a Given Object
    private static void printObjectName(Datum obj) {
      Datum name = MetaPrimitives.CallableName.logic(obj);
      if(name instanceof Symbol) {
        String nameValue = ((Symbol)name).value();
        System.out.print("Name: ");
        System.out.println(nameValue);
        for(int i = 0, n = nameValue.length()+6; i < n; ++i) {
          System.out.print('*');
        }
        System.out.println("\n");
      }
    }


    private static boolean hasMultipleSignatures(Datum sigs) {
      return Pair.isListPair(sigs) && ((Pair)sigs).car() instanceof Pair;
    }


    private static String padNewlinesWithTabs(String docs) {
      return "  "+String.join("\n  ",docs.split("\n"));
    }


    private static void printObjectSignatures(Datum obj) {
      Datum sigs = MetaPrimitives.CallableSignature.logic(obj);
      if(!(sigs instanceof Boolean)) {
        System.out.println("Signatures:");
        if(hasMultipleSignatures(sigs)) {
          while(sigs instanceof Pair) {
            Pair p = (Pair)sigs;
            System.out.println(padNewlinesWithTabs(p.car().pprint()));
            sigs = p.cdr();
          }
        } else {
          System.out.println(padNewlinesWithTabs(sigs.pprint()));
        }
        System.out.println("");
      }
    }


    private static void printObjectDocstring(Datum obj) {
      Datum docs = MetaPrimitives.DocStringExtractor.helpLogic(obj);
      System.out.println("Description:");
      if(docs instanceof escm.type.String && ((escm.type.String)docs).value().length() > 0) {
        System.out.println(padNewlinesWithTabs(((escm.type.String)docs).value()));
      } else {
        System.out.println("  Given item: "+obj.profile());
      }
    }


    public static synchronized void describeObject(Datum obj) {
      System.out.println("===============================================================================");
      printObjectName(obj);
      printObjectSignatures(obj);
      printObjectDocstring(obj);
      System.out.println("===============================================================================");
    }


    //////////////////////////////////////////////////////////////////////////
    // Main Dispatch
    public java.lang.String escmName() {
      return "help";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("help")),
        Pair.List(new Symbol("help"),new Symbol("<obj>")));
    }

    public String docstring() {
      return "@help:Procedures:Meta\nObj Argument:\n  Get information about <obj>.\n\nNo Arguments:\n  Launch the interactive help menu. The help menu consists of folders, which\n  in turn hold descriptions of various variables in EScheme's environment.\n\n  Type folder names in the input prompt to enter them, and use \":\" as a \n  separator to enter multiple folders (e.g. \"folder1:folder2:folder3\").\n  Type . for the current directory, .. for the parent, ... for the \n  grandparent, etc.\n\n  Type :quit to quit, :~ to return to the home directory, or :help for more \n  information.\n\n  Procedures, classes, and interfaces that want to explain how they work in \n  the <help> menu should use <docstring> syntax. Within the <docstring>, the\n  \"@help\" syntax can be used to place the variable within a certain \n  subdirectory of the help menu. For example:\n\n    (define (fact n)\n      \"\n      @help:Procedures:Numbers\n      The factorial function. Accepts an int arg.\n      \"\n      (if (< n 2)\n          1\n          (* n (fact (- n 1)))))\n\n  would put fact's <help> entry in the Numbers directory, which itself is in\n  the Procedures directory. Docstring entries without the \"@help\" syntax\n  are placed in the \"Misc\" directory.\n\nSee <define-help> to register topic documents in help's file tree.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1)
        throw new Exceptionf("'(help <optional-obj>) received more than 1 arg: %s", Exceptionf.profileArgs(parameters));
      try {
        if(parameters.size() == 0) {
          launchInteractiveMenu(this.definitionEnvironment);
        } else {
          describeObject(parameters.get(0));
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
      return "@help:Procedures:Meta\nRegister the <docstring> topic document in help's file tree, at path <path> in\nfile <name>. Note that <path> denotes a set of folder names seperated by <:>.\n\nAliased by <defhelp>.";
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
}