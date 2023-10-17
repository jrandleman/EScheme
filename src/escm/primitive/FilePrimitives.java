// Author: Jordan Randleman - escm.primitive.FilePrimitives
// Purpose:
//    Java primitives to manipulate the file system.

package escm.primitive;
import java.util.ArrayList;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.StandardCopyOption;
import java.io.File;
import escm.type.Datum;
import escm.type.Symbol;
import escm.type.Pair;
import escm.type.number.Real;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.vm.Reader;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.callable.Signature;
import escm.vm.util.SourceInformation;

public class FilePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // file-read
  public static class FileRead extends Primitive {
    public java.lang.String escmName() {
      return "file-read";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-read"),new Symbol("<filename-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet the contents of <filename-string> as an EScheme data structure.";
    }
    
    private static Datum convertReadExpressionsToReadExpression(ArrayList<Datum> contents) {
      if(contents.size() == 1) return contents.get(0);
      Datum expression = escm.type.Nil.VALUE;
      for(int i = contents.size()-1; i >= 0; --i)
        expression = new escm.type.Pair(contents.get(i),expression);
      return new escm.type.Pair(new escm.type.Symbol("begin"),expression);
    }

    public static String slurpFile(String filename, String callerName) throws Exception {
      try {
        return Files.readString(Path.of(filename));
      } catch(Exception e) {
        throw new Exceptionf("'%s couldn't read from file \"%s\"", callerName, filename);
      }
    }

    public static ArrayList<Datum> readBufferAsArrayList(String filename, String buffer) throws Exception {
      SourceInformation source = new SourceInformation(AbsolutePath.logic(filename),1,1);
      ArrayList<Datum> contents = new ArrayList<Datum>();
      buffer = buffer.stripTrailing();
      if(buffer.length() == 0) return contents;
      Integer n = buffer.length();
      escm.util.Pair<Datum,Integer> result = Reader.nullableRead(buffer,source,Reader.GIVE_DETAILED_INCOMPLETE_ERRORS);
      if(result.first != null) contents.add(result.first);
      buffer = buffer.substring(result.second);
      while(result.second != n && buffer.length() > 0) {
        n = buffer.length();
        result = Reader.nullableRead(buffer,source,Reader.GIVE_DETAILED_INCOMPLETE_ERRORS);
        if(result.first != null) contents.add(result.first);
        buffer = buffer.substring(result.second);
      }
      return contents;
    }

    public static Datum readBuffer(String filename, String buffer) throws Exception {
      ArrayList<Datum> contents = readBufferAsArrayList(filename,buffer);
      if(contents.size() == 0) return escm.type.Void.VALUE;
      return convertReadExpressionsToReadExpression(contents);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-read <filename-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      String filename = ((escm.type.String)parameters.get(0)).value();
      return readBuffer(filename,slurpFile(filename,"file-read"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-read-string
  public static class FileReadString extends Primitive {
    public java.lang.String escmName() {
      return "file-read-string";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-read-string"),new Symbol("<filename-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet the contents of <filename-string> as a string.";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-read-string <filename-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(FileRead.slurpFile(((escm.type.String)parameters.get(0)).value(),"file-read-string"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-write
  public static class FileWrite extends Primitive {
    public java.lang.String escmName() {
      return "file-write";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-write"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nWrite <obj> to <filename-string> using machine-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public static void writeStringToFile(String filename, String str, String callerName) throws Exception {
      try {
        Files.writeString(Path.of(filename),str);
      } catch(Exception e) {
        throw new Exceptionf("'%s couldn't write to file \"%s\"", filename, callerName);
      }
    }

    public static void appendStringToFile(String filename, String str, String callerName) throws Exception {
      try {
        Files.writeString(Path.of(filename),str,new StandardOpenOption[]{StandardOpenOption.CREATE,StandardOpenOption.APPEND,StandardOpenOption.WRITE});
      } catch(Exception e) {
        throw new Exceptionf("'%s couldn't write to file \"%s\"", filename, callerName);
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-write <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      writeStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).write(),"file-write");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-write+
  public static class FileWritePlus extends Primitive {
    public java.lang.String escmName() {
      return "file-write+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-write+"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nAppend <obj> to <filename-string> using machine-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-write+ <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.appendStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).write(),"file-write+");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-display
  public static class FileDisplay extends Primitive {
    public java.lang.String escmName() {
      return "file-display";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-display"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nWrite <obj> to <filename-string> using human-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-display <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.writeStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).display(),"file-display");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-display+
  public static class FileDisplayPlus extends Primitive {
    public java.lang.String escmName() {
      return "file-display+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-display+"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nAppend <obj> to <filename-string> using human-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-display+ <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.appendStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).display(),"file-display+");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-pretty-print
  public static class FilePprint extends Primitive {
    public java.lang.String escmName() {
      return "file-pretty-print";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-pretty-print"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nWrite <obj> to <filename-string> using indented machine-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-pretty-print <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.writeStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).pprint(),"file-pretty-print");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-pretty-print+
  public static class FilePprintPlus extends Primitive {
    public java.lang.String escmName() {
      return "file-pretty-print+";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-pretty-print+"),new Symbol("<filename-string>"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nAppend <obj> to <filename-string> using indented machine-readable syntax.\nNote that this will infinitely loop for cyclical structures!";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-pretty-print+ <filename-string> <obj>) didn't receive exactly 1 string & 1 obj: %s", Exceptionf.profileArgs(parameters));
      FileWrite.appendStringToFile(((escm.type.String)parameters.get(0)).value(),parameters.get(1).pprint()+"\n","file-pretty-print+");
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path?
  public static class IsPathP extends Primitive {
    public java.lang.String escmName() {
      return "path?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("path?"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns whether <path-string> is a path (directory or file) that exists on the\nsystem.";
    }

    public static boolean logic(Path p) {
      try {
        return Files.exists(p);
      } catch (Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path? <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory?
  public static class IsDirectoryP extends Primitive {
    public java.lang.String escmName() {
      return "directory?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("directory?"),new Symbol("<directory-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns whether <directory-path-string> is a directory path.";
    }

    public static boolean logic(Path p) {
      try {
        return Files.isDirectory(p);
      } catch(Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory? <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file?
  public static class IsFileP extends Primitive {
    public java.lang.String escmName() {
      return "file?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file?"),new Symbol("<file-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns whether <file-path-string> is a file path.";
    }

    public static boolean logic(Path p) {
      try {
        return Files.exists(p) == true && Files.isDirectory(p) == false;
      } catch (Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file? <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-delete!
  public static class FileDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "file-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-delete!"),new Symbol("<file-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nDeletes <file-path-string>. Returns the deletion's success status.";
    }

    public static boolean logic(Path p) {
      try {
        return IsFileP.logic(p) && Files.deleteIfExists(p);
      } catch (Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-delete! <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-delete!
  public static class DirectoryDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "directory-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("directory-delete!"),new Symbol("<directory-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nDeletes <directory-path-string> if its empty. Returns the deletion's success status.";
    }

    public static boolean logic(Path p) {
      if(!IsDirectoryP.logic(p)) return false;
      try {
        return Files.deleteIfExists(p);
      } catch(Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-delete! <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-recursive-delete!
  public static class DirectoryRecursiveDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "directory-recursive-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("directory-recursive-delete!"),new Symbol("<directory-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nDeletes <directory-path-string> and its contents. Returns the deletion's success status.";
    }

    private static boolean deleteDirectoryContents(Path dirPath) throws Exception {
      boolean succeeded = true;
      for(File entry : (new File(dirPath.toString())).listFiles()) {
        Path p = entry.toPath();
        if(IsDirectoryP.logic(p)) {
          succeeded = succeeded && deleteDirectoryContents(p);
        }
        try {
          succeeded = succeeded && Files.deleteIfExists(p);
        } catch (Exception e) {
          return false;
        }
      }
      return succeeded;
    }

    public static boolean logic(Path p) {
      try {
        return IsDirectoryP.logic(p) && deleteDirectoryContents(p) && Files.deleteIfExists(p);
      } catch (Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-recursive-delete! <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-delete!
  public static class PathDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "path-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("path-delete!"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nDeletes <path-string> by dispatching to <file-delete!> or <directory-delete!>.\nReturns the deletion's success status.";
    }

    public static boolean logic(Path p) {
      try {
        return Files.exists(p) && Files.deleteIfExists(p);
      } catch (Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-delete! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-recursive-delete!
  public static class PathRecursiveDeleteBang extends Primitive {
    public java.lang.String escmName() {
      return "path-recursive-delete!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("path-recursive-delete!"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nDeletes <path-string> by dispatching to <file-delete!> or <directory-recursive-delete!>.\nReturns the deletion's success status.";
    }

    public static boolean logic(Path p) {
      try {
        if(IsDirectoryP.logic(p)) {
          return DirectoryRecursiveDeleteBang.logic(p);
        } else if(IsFileP.logic(p)) {
          return Files.deleteIfExists(p);
        }
        return false;
      } catch(Exception e) {
        return false;
      }
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-recursive-delete! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-entries
  public static class DirectoryEntries extends Primitive {
    public java.lang.String escmName() {
      return "directory-entries";
    }

    public Datum signature() {
      return Pair.List(new Symbol("directory-entries"),new Symbol("<directory-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns a list of the directory entry paths in <directory-path-string>.\nReturns \"#f\" if <directory-path-string> doesn't denote a directory.";
    }

    public static Datum logic(Path p) throws Exception {
      if(IsDirectoryP.logic(p) == false) return Boolean.FALSE;
      Datum lis = escm.type.Nil.VALUE;
      for(File entry : (new File(p.toString())).listFiles()) {
        lis = new escm.type.Pair(new escm.type.String(entry.toString()),lis);
      }
      return lis;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-entries <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return logic(Path.of(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-entries*
  public static class DirectoryEntriesStar extends Primitive {
    public java.lang.String escmName() {
      return "directory-entries*";
    }

    public Datum signature() {
      return Pair.List(new Symbol("directory-entries*"),new Symbol("<directory-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns a list of the directory entry paths in <directory-path-string>, WITHOUT dot-files.\nNote that \"dot-file\" here refers to file names starting with \".\".\nReturns \"#f\" if <directory-path-string> doesn't denote a directory.";
    }

    public static Datum logic(Path p) throws Exception {
      if(IsDirectoryP.logic(p) == false) return Boolean.FALSE;
      Datum lis = escm.type.Nil.VALUE;
      for(File entry : (new File(p.toString())).listFiles()) {
        String filename = entry.toPath().getFileName().toString();
        if(filename.length() != 0 && filename.charAt(0) != '.') {
          lis = new escm.type.Pair(new escm.type.String(entry.toString()),lis);
        }
      }
      return lis;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-entries* <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return logic(Path.of(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // current-directory
  public static class CurrentDirectory extends Primitive {
    public java.lang.String escmName() {
      return "current-directory";
    }

    public Datum signature() {
      return Pair.List(new Symbol("current-directory"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet the current working directory's absolute path as a string.\n\nNote that <current-directory> refers to the directory that launched\nthe current process. Use #path if you instead want to refer to the\ndirectory of the current file.";
    }

    public static String logic() throws Exception {
      return Path.of("").toAbsolutePath().toString();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 0) 
        throw new Exceptionf("'(current-directory) doesn't accept any args: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(logic());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path
  public static class GeneratePath extends Primitive {
    public java.lang.String escmName() {
      return "path";
    }

    public Datum signature() {
      return Pair.List(new Symbol("path"),new Symbol("<string>"),Signature.VARIADIC);
    }

    public String docstring() {
      return "@help:Procedures:Files\nCreate a path by combining \"<string> ...\" with <*file-separator*> between each\ncomponent. Yields an absolute path. Passing no arguments is equivalent to\n(current-directory).";
    }

    private static boolean hasTerminalSeparator(String s) {
      return s.endsWith(File.separator);
    }

    private static String withoutStartingSeparator(String s) {
      if(s.startsWith(File.separator)) return s.substring(File.separator.length());
      return s;
    }

    private static String withoutTerminalSeparator(String s) {
      if(s.endsWith(File.separator)) return s.substring(0,s.length()-File.separator.length());
      return s;
    }

    public static String logic(String[] pathStrings) throws Exception {
      if(pathStrings.length == 0) return CurrentDirectory.logic();
      String path = pathStrings[0];
      if(!hasTerminalSeparator(path)) path += File.separator;
      for(int i = 1; i < pathStrings.length; ++i) {
        String s = withoutStartingSeparator(pathStrings[i]);
        if(s.length() == 0 || hasTerminalSeparator(s)) {
          path += s;
        } else {
          path += s+File.separator;
        }
      }
      return AbsolutePath.logic(withoutTerminalSeparator(path));
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int totalArgs = parameters.size();
      String[] args = new String[totalArgs];
      for(int i = 0; i < totalArgs; ++i) {
        Datum arg = parameters.get(i);
        if(!(arg instanceof escm.type.String))
          throw new Exceptionf("'(path <string> ...) arg %s isn't a string: %s", arg.profile(), Exceptionf.profileArgs(parameters));
        args[i] = ((escm.type.String)arg).value();
      }
      return new escm.type.String(logic(args));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-parent
  public static class PathParent extends Primitive {
    public java.lang.String escmName() {
      return "path-parent";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("path-parent"),new Symbol("<path-string>")),
        Pair.List(new Symbol("path-parent"),new Symbol("<path-string>"),new Symbol("<positive-integer>")));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet <path-string>'s parent path. Returns #f if <path-string> doesn't have a\nparent. <positive-integer> denotes how many parents in the chain to go up\n(1 by default).";
    }

    // Returns <null> on failure
    public static String logic(String pathString) {
      try {
        Path p = Path.of(pathString).getParent();
        if(p == null) return null;
        return p.toString();
      } catch(Exception e) {
        return null;
      }
    }

    private static int parseCompositionCount(int n, ArrayList<Datum> parameters) throws Exception {
      if(n == 1) return 1;
      Datum d = parameters.get(1);
      if(!(d instanceof Real))
        throw new Exceptionf("'(path-parent <path-string> <optional-positive-int>) 2nd arg isn't a positive int: %s", Exceptionf.profileArgs(parameters));
      Real compositionCount = (Real)d;
      if(!compositionCount.isInteger() || !compositionCount.isPositive())
        throw new Exceptionf("'(path-parent <path-string> <optional-positive-int>) 2nd arg isn't a positive int: %s", Exceptionf.profileArgs(parameters));
      return compositionCount.intValue();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      int n = parameters.size();
      if(n < 1 || n > 2 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-parent <path-string> <optional-positive-int>) incorrect args: %s", Exceptionf.profileArgs(parameters));
      int compositionCount = parseCompositionCount(n,parameters);
      String parent = ((escm.type.String)parameters.get(0)).value();
      while(compositionCount > 0) {
        parent = logic(parent);
        if(parent == null) return Boolean.FALSE;
        --compositionCount;
      }
      return new escm.type.String(parent);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-file
  public static class PathFile extends Primitive {
    public java.lang.String escmName() {
      return "path-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("path-file"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet <path-string>'s file name. Returns #f if <path-string> is empty.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-file <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      Path p = Path.of(((escm.type.String)parameters.get(0)).value()).getFileName();
      if(p == null) return Boolean.FALSE;
      return new escm.type.String(p.toString());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-directory
  public static class MakeDirectory extends Primitive {
    public java.lang.String escmName() {
      return "make-directory";
    }

    public Datum signature() {
      return Pair.List(new Symbol("make-directory"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nCreate <path-string> as a directory.\nFails if any directory midway in <path-string> doesn't exist.\nReturns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(make-directory <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf((new File(((escm.type.String)parameters.get(0)).value())).mkdir());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-directory!
  public static class MakeDirectoryBang extends Primitive {
    public java.lang.String escmName() {
      return "make-directory!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("make-directory!"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nCreate <path-string> as a directory.\nCreates intermediate directories as needed while creating <path-string>.\nReturns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(make-directory! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf((new File(((escm.type.String)parameters.get(0)).value())).mkdirs());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // absolute-path
  public static class AbsolutePath extends Primitive {
    public java.lang.String escmName() {
      return "absolute-path";
    }

    public Datum signature() {
      return Pair.List(new Symbol("absolute-path"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns the absolute path of <path-string>.";
    }

    public static String logic(String pathStr) {
      try {
        return Path.of(pathStr).toFile().getCanonicalPath().toString();
      } catch(Exception e1) {
        try {
          return Path.of(pathStr).toFile().getAbsolutePath();
        } catch(Exception e2) {
          return pathStr;
        }
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(absolute-path <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(logic(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // absolute-path?
  public static class IsAbsolutePath extends Primitive {
    public java.lang.String escmName() {
      return "absolute-path?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("absolute-path?"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns whether <path-string> is an absolute path. Note that this is different\nfrom <path?>: checks path structure, not whether it exists on the current\nsystem. Effectively: (equal? <path-string> (absolute-path <path-string>))";
    }

    public static boolean logic(String path) {
      try {
        File fp = Path.of(path).toFile();
        try {
          return path.equals(fp.getCanonicalPath().toString()) || path.equals(fp.getAbsolutePath());
        } catch(Exception e1) {
          try {
            return path.equals(fp.getAbsolutePath());
          } catch(Exception e2) {
            return false;
          }
        }
      } catch(Exception e3) {
        return false;
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(absolute-path? <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-extension
  public static class FileExtension extends Primitive {
    public java.lang.String escmName() {
      return "file-extension";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-extension"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nGet the file extension of <path-string>. Returns #f if non-existant.";
    }

    public static String logic(String pathStr) {
      Path p = Path.of(pathStr).getFileName();
      if(p == null) return null;
      String pString = p.toString();
      int idx = pString.lastIndexOf('.');
      if(idx == -1) return null;
      return pString.substring(idx+1);
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-extension <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      String ext = logic(((escm.type.String)parameters.get(0)).value());
      if(ext == null) return Boolean.FALSE;
      return new escm.type.String(ext);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-has-extension?
  public static class FileHasExtensionP extends Primitive {
    public java.lang.String escmName() {
      return "file-has-extension?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-has-extension?"),new Symbol("<path-string>"),new Symbol("<extension-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns whether <path-string> has <extension-string> as its file extension.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(file-has-extension? <path-string> <extension-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum pathStrDatum = parameters.get(0);
      if(!(pathStrDatum instanceof escm.type.String))
        throw new Exceptionf("'(file-has-extension? <path-string> <extension-string>) path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum extStrDatum = parameters.get(1);
      if(!(extStrDatum instanceof escm.type.String))
        throw new Exceptionf("'(file-has-extension? <path-string> <extension-string>) extension isn't a string: %s", Exceptionf.profileArgs(parameters));
      String ext = FileExtension.logic(((escm.type.String)pathStrDatum).value());
      if(ext == null) return Boolean.FALSE;
      return Boolean.valueOf(((escm.type.String)extStrDatum).value().equals(ext));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // swap-file-extension
  public static class SwapFileExtension extends Primitive {
    public java.lang.String escmName() {
      return "swap-file-extension";
    }

    public Datum signature() {
      return Pair.List(new Symbol("swap-file-extension"),new Symbol("<path-string>"),new Symbol("<new-extension-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns <path-string> with <new-extension-string> as its file extension.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(swap-file-extension <path-string> <new-extension-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum pathStrDatum = parameters.get(0);
      if(!(pathStrDatum instanceof escm.type.String))
        throw new Exceptionf("'(swap-file-extension <path-string> <new-extension-string>) path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum extStrDatum = parameters.get(1);
      if(!(extStrDatum instanceof escm.type.String))
        throw new Exceptionf("'(swap-file-extension <path-string> <new-extension-string>) extension isn't a string: %s", Exceptionf.profileArgs(parameters));
      String pathStr = ((escm.type.String)pathStrDatum).value();
      String extStr = ((escm.type.String)extStrDatum).value();
      Path p = Path.of(pathStr).getFileName();
      if(p == null) return new escm.type.String(pathStr+"."+extStr);
      String pString = p.toString();
      int idx = pString.lastIndexOf('.');
      if(idx == -1) return new escm.type.String(pathStr+"."+extStr);
      return new escm.type.String(pString.substring(0,idx)+"."+extStr);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // remove-file-extension
  public static class RemoveFileExtension extends Primitive {
    public java.lang.String escmName() {
      return "remove-file-extension";
    }

    public Datum signature() {
      return Pair.List(new Symbol("remove-file-extension"),new Symbol("<path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturns <path-string> without its extension, including the \".\".\nReturns <path-string> if it doesn't have an extension or is an invalid path.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(remove-file-extension <path-string>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum pathStrDatum = parameters.get(0);
      if(!(pathStrDatum instanceof escm.type.String))
        throw new Exceptionf("'(remove-file-extension <path-string>) path isn't a string: %s", Exceptionf.profileArgs(parameters));
      String pathStr = ((escm.type.String)pathStrDatum).value();
      Path p = Path.of(pathStr).getFileName();
      if(p == null) return pathStrDatum;
      String pString = p.toString();
      int idx = pString.lastIndexOf('.');
      if(idx == -1) return pathStrDatum;
      return new escm.type.String(pString.substring(0,idx));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-size
  public static class FileSize extends Primitive {
    public java.lang.String escmName() {
      return "file-size";
    }

    public Datum signature() {
      return Pair.List(new Symbol("file-size"),new Symbol("<file-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nReturn the size of <file-path-string> in bytes.\nPlatform-dependant result if <file-path-string> is a directory.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-size <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      try {
        return new escm.type.number.Exact(Files.size(Path.of(((escm.type.String)parameters.get(0)).value())));
      } catch (Exception e) {
        throw new Exceptionf("'(file-size <file-path-string>) couldn't get file size: %s", Exceptionf.profileArgs(parameters));
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // move-file
  public static class MoveFile extends Primitive {
    public java.lang.String escmName() {
      return "move-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("move-file"),new Symbol("<old-path-string>"),new Symbol("<new-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nMove <old-path-string> to be at <new-path-string> instead.\nTriggers an error if <new-path-string> already exists, or if\nany of the intermediate directories in <new-path-string> don't exist.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(move-file <old-path-string> <new-path-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum oldPathDatum = parameters.get(0);
      if(!(oldPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(move-file <old-path-string> <new-path-string>) old-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum newPathDatum = parameters.get(1);
      if(!(newPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(move-file <old-path-string> <new-path-string>) new-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      try {
        Files.move(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(((escm.type.String)newPathDatum).value()));
      } catch (Exception e) {
        throw new Exceptionf("'(move-file <old-path-string> <new-path-string>) couldn't move path: %s", Exceptionf.profileArgs(parameters));
      }
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // move-file!
  public static class MoveFileBang extends Primitive {
    public java.lang.String escmName() {
      return "move-file!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("move-file!"),new Symbol("<old-path-string>"),new Symbol("<new-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nMove <old-path-string> to be at <new-path-string> instead.\nReplaces <new-path-string> if it already exists, and creates\nany intermediate directories in <new-path-string> as needed.\nReturns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(move-file! <old-path-string> <new-path-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum oldPathDatum = parameters.get(0);
      if(!(oldPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(move-file! <old-path-string> <new-path-string>) old-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum newPathDatum = parameters.get(1);
      if(!(newPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(move-file! <old-path-string> <new-path-string>) new-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      // Create intermediate directories as needed
      String newPathString = ((escm.type.String)newPathDatum).value();
      Path directoriesToCreate = Path.of(AbsolutePath.logic(newPathString)).getParent();
      if(directoriesToCreate != null && !IsDirectoryP.logic(directoriesToCreate)) {
        if((new File(directoriesToCreate.toString())).mkdirs() == false) return Boolean.FALSE;
      }
      // Move the file once directories are made
      try {
        Files.move(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(newPathString),StandardCopyOption.REPLACE_EXISTING);
      } catch (Exception e) {
        throw new Exceptionf("'(move-file! <old-path-string> <new-path-string>) couldn't move! path: %s", Exceptionf.profileArgs(parameters));
      }
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // copy-file
  public static class CopyFile extends Primitive {
    public java.lang.String escmName() {
      return "copy-file";
    }

    public Datum signature() {
      return Pair.List(new Symbol("copy-file"),new Symbol("<old-path-string>"),new Symbol("<new-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nCopy <old-path-string> to <new-path-string>.\nTriggers an error if <new-path-string> already exists, or if\nany of the intermediate directories in <new-path-string> don't exist.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(copy-file <old-path-string> <new-path-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum oldPathDatum = parameters.get(0);
      if(!(oldPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(copy-file <old-path-string> <new-path-string>) old-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum newPathDatum = parameters.get(1);
      if(!(newPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(copy-file <old-path-string> <new-path-string>) new-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      try {
        Files.copy(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(((escm.type.String)newPathDatum).value()));
      } catch (Exception e) {
        throw new Exceptionf("'(copy-file <old-path-string> <new-path-string>) couldn't copy file: %s", Exceptionf.profileArgs(parameters));
      }
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // copy-file!
  public static class CopyFileBang extends Primitive {
    public java.lang.String escmName() {
      return "copy-file!";
    }

    public Datum signature() {
      return Pair.List(new Symbol("copy-file!"),new Symbol("<old-path-string>"),new Symbol("<new-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Files\nCopy <old-path-string> to <new-path-string>.\nReplaces <new-path-string> if it already exists, and creates\nany intermediate directories in <new-path-string> as needed.\nReturns whether succeeded.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(copy-file! <old-path-string> <new-path-string>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum oldPathDatum = parameters.get(0);
      if(!(oldPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(copy-file! <old-path-string> <new-path-string>) old-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      Datum newPathDatum = parameters.get(1);
      if(!(newPathDatum instanceof escm.type.String))
        throw new Exceptionf("'(copy-file! <old-path-string> <new-path-string>) new-path isn't a string: %s", Exceptionf.profileArgs(parameters));
      // Create intermediate directories as needed
      String newPathString = ((escm.type.String)newPathDatum).value();
      Path directoriesToCreate = Path.of(AbsolutePath.logic(newPathString)).getParent();
      if(directoriesToCreate != null && !IsDirectoryP.logic(directoriesToCreate)) {
        if((new File(directoriesToCreate.toString())).mkdirs() == false) return Boolean.FALSE;
      }
      // Move the file once directories are made
      try {
        Files.copy(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(newPathString),StandardCopyOption.REPLACE_EXISTING);
      } catch (Exception e) {
        throw new Exceptionf("'(copy-file! <old-path-string> <new-path-string>) couldn't copy! file: %s", Exceptionf.profileArgs(parameters));
      }
      return Boolean.TRUE;
    }
  }
}