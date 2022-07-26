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
import escm.type.bool.Boolean;
import escm.util.Exceptionf;
import escm.vm.Reader;
import escm.vm.type.Primitive;
import escm.vm.util.SourceInformation;

public class FilePrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // file-read
  public static class FileRead implements Primitive {
    public java.lang.String escmName() {
      return "file-read";
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
  public static class FileReadString implements Primitive {
    public java.lang.String escmName() {
      return "file-read-string";
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-read-string <filename-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.String(FileRead.slurpFile(((escm.type.String)parameters.get(0)).value(),"file-read-string"));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-write
  public static class FileWrite implements Primitive {
    public java.lang.String escmName() {
      return "file-write";
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
  public static class FileWritePlus implements Primitive {
    public java.lang.String escmName() {
      return "file-write+";
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
  public static class FileDisplay implements Primitive {
    public java.lang.String escmName() {
      return "file-display";
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
  public static class FileDisplayPlus implements Primitive {
    public java.lang.String escmName() {
      return "file-display+";
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
  public static class FilePprint implements Primitive {
    public java.lang.String escmName() {
      return "file-pretty-print";
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
  public static class FilePprintPlus implements Primitive {
    public java.lang.String escmName() {
      return "file-pretty-print+";
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
  public static class IsPathP implements Primitive {
    public java.lang.String escmName() {
      return "path?";
    }

    public static boolean logic(Path p) {
      return Files.exists(p);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path? <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory?
  public static class IsDirectoryP implements Primitive {
    public java.lang.String escmName() {
      return "directory?";
    }

    public static boolean logic(Path p) {
      return Files.isDirectory(p);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory? <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file?
  public static class IsFileP implements Primitive {
    public java.lang.String escmName() {
      return "file?";
    }

    public static boolean logic(Path p) {
      return Files.exists(p) == true && Files.isDirectory(p) == false;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file? <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // file-delete!
  public static class FileDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "file-delete!";
    }

    public static boolean logic(Path p) throws Exception {
      if(!IsFileP.logic(p)) return false;
      return Files.deleteIfExists(p);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-delete! <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-delete!
  public static class DirectoryDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "directory-delete!";
    }

    public static boolean logic(Path p) throws Exception {
      if(!IsDirectoryP.logic(p)) return false;
      return Files.deleteIfExists(p);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-delete! <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-recursive-delete!
  public static class DirectoryRecursiveDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "directory-recursive-delete!";
    }

    private static boolean deleteDirectoryContents(Path dirPath) throws Exception {
      boolean succeeded = true;
      for(File entry : (new File(dirPath.toString())).listFiles()) {
        Path p = entry.toPath();
        if(IsDirectoryP.logic(p)) {
          succeeded = succeeded && deleteDirectoryContents(p);
        }
        succeeded = succeeded && Files.deleteIfExists(p);
      }
      return succeeded;
    }

    public static boolean logic(Path p) throws Exception {
      if(!IsDirectoryP.logic(p)) return false;
      return deleteDirectoryContents(p) && Files.deleteIfExists(p);
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(directory-recursive-delete! <directory-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-delete!
  public static class PathDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "path-delete!";
    }

    public static boolean logic(Path p) throws Exception {
      if(Files.exists(p)) return Files.deleteIfExists(p);
      return false;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-delete! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-recursive-delete!
  public static class PathRecursiveDeleteBang implements Primitive {
    public java.lang.String escmName() {
      return "path-recursive-delete!";
    }

    public static boolean logic(Path p) throws Exception {
      if(IsDirectoryP.logic(p)) {
        return DirectoryRecursiveDeleteBang.logic(p);
      } else if(IsFileP.logic(p)) {
        return Files.deleteIfExists(p);
      }
      return false;
    }
    
    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-recursive-delete! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // directory-entries
  public static class DirectoryEntries implements Primitive {
    public java.lang.String escmName() {
      return "directory-entries";
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
  public static class DirectoryEntriesStar implements Primitive {
    public java.lang.String escmName() {
      return "directory-entries*";
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
  public static class CurrentDirectory implements Primitive {
    public java.lang.String escmName() {
      return "current-directory";
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
  // path-parent
  public static class PathParent implements Primitive {
    public java.lang.String escmName() {
      return "path-parent";
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

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(path-parent <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      String parent = logic(((escm.type.String)parameters.get(0)).value());
      if(parent == null) return Boolean.FALSE;
      return new escm.type.String(parent);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // path-file
  public static class PathFile implements Primitive {
    public java.lang.String escmName() {
      return "path-file";
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
  public static class MakeDirectory implements Primitive {
    public java.lang.String escmName() {
      return "make-directory";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(make-directory <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf((new File(((escm.type.String)parameters.get(0)).value())).mkdir());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // make-directory!
  public static class MakeDirectoryBang implements Primitive {
    public java.lang.String escmName() {
      return "make-directory!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(make-directory! <path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf((new File(((escm.type.String)parameters.get(0)).value())).mkdirs());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // absolute-path
  public static class AbsolutePath implements Primitive {
    public java.lang.String escmName() {
      return "absolute-path";
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
  public static class IsAbsolutePath implements Primitive {
    public java.lang.String escmName() {
      return "absolute-path?";
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
  public static class FileExtension implements Primitive {
    public java.lang.String escmName() {
      return "file-extension";
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
  public static class FileHasExtensionP implements Primitive {
    public java.lang.String escmName() {
      return "file-has-extension?";
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
  public static class SwapFileExtension implements Primitive {
    public java.lang.String escmName() {
      return "swap-file-extension";
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
  public static class RemoveFileExtension implements Primitive {
    public java.lang.String escmName() {
      return "remove-file-extension";
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
  public static class FileSize implements Primitive {
    public java.lang.String escmName() {
      return "file-size";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(file-size <file-path-string>) didn't receive exactly 1 string: %s", Exceptionf.profileArgs(parameters));
      return new escm.type.number.Exact(Files.size(Path.of(((escm.type.String)parameters.get(0)).value())));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // move-file
  public static class MoveFile implements Primitive {
    public java.lang.String escmName() {
      return "move-file";
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
      Files.move(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(((escm.type.String)newPathDatum).value()));
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // move-file!
  public static class MoveFileBang implements Primitive {
    public java.lang.String escmName() {
      return "move-file!";
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
      Path directoriesToCreate = Path.of(newPathString).getParent();
      if(directoriesToCreate != null) {
        if((new File(directoriesToCreate.toString())).mkdirs() == false) return Boolean.FALSE;
      }
      // Move the file once directories are made
      Files.move(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(newPathString),StandardCopyOption.REPLACE_EXISTING);
      return Boolean.TRUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // copy-file
  public static class CopyFile implements Primitive {
    public java.lang.String escmName() {
      return "copy-file";
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
      Files.copy(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(((escm.type.String)newPathDatum).value()));
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // copy-file!
  public static class CopyFileBang implements Primitive {
    public java.lang.String escmName() {
      return "copy-file!";
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
      Path directoriesToCreate = Path.of(newPathString).getParent();
      if(directoriesToCreate != null) {
        if((new File(directoriesToCreate.toString())).mkdirs() == false) return Boolean.FALSE;
      }
      // Move the file once directories are made
      Files.copy(Path.of(((escm.type.String)oldPathDatum).value()),Path.of(newPathString),StandardCopyOption.REPLACE_EXISTING);
      return Boolean.TRUE;
    }
  }
}