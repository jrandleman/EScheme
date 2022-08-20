// Author: Jordan Randleman - escm.primitive.SerializationPrimitives
// Purpose:
//    Java primitives for boolean operations.

package escm.primitive;
import java.util.ArrayList;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.Void;
import escm.type.bool.Boolean;
import escm.type.port.Eof;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.Compiler;
import escm.vm.Assembler;
import escm.vm.Interpreter;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;
import escm.vm.runtime.GlobalState;
import escm.primitive.SerializationPrimitives_util.InstructionSet;

public class SerializationPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // Serialized File Header
  public static final byte[] SERIALIZED_FILE_HEADER = "\10SERIALIZED\22ESCHEME\21".getBytes();


  ////////////////////////////////////////////////////////////////////////////
  // Private Linked List for CPS Serialization
  private static class InstructionsList {
    // Fields
    public ArrayList<Instruction> car = null;
    public InstructionsList cdr = null;

    // Constructor
    public InstructionsList(ArrayList<Instruction> car, InstructionsList cdr) {
      this.car = car;
      this.cdr = cdr;
    }

    // Merge this list (in reverse) into a single node
    public ArrayList<Instruction> mergeInReverse() {
      ArrayList<Instruction> arr = new ArrayList<Instruction>();
      InstructionsList reversed = null;
      InstructionsList iter = this;
      while(iter != null) {
        reversed = new InstructionsList(iter.car,reversed);
        iter = iter.cdr;
      }
      while(reversed != null) {
        arr.addAll(reversed.car);
        reversed = reversed.cdr;
      }
      return arr;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // serialized?
  public static class IsSerializedP implements Primitive {
    public java.lang.String escmName() {
      return "serialized?";
    }

    private static boolean hasSerializationHeader(byte[] readHeader, int charsRead) {
      if(charsRead < SERIALIZED_FILE_HEADER.length || charsRead > readHeader.length) return false;
      for(int i = 0; i < SERIALIZED_FILE_HEADER.length; ++i) {
        if(readHeader[i] != SERIALIZED_FILE_HEADER[i]) return false;
      }
      return true;
    }


    public static boolean logic(String filePath) {
      if(FilePrimitives.IsFileP.logic(Path.of(filePath)) == false) return false;
      try {
        FileInputStream fileIn = new FileInputStream(filePath);
        ObjectInputStream in = new ObjectInputStream(fileIn);
        byte[] header_buffer = new byte[SERIALIZED_FILE_HEADER.length];
        int read_length = in.read(header_buffer,0,header_buffer.length);
        boolean hasHeader = hasSerializationHeader(header_buffer,read_length);
        in.close();
        fileIn.close();
        return hasHeader;
      } catch(Exception e) {
        return false;
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(serialized? <file-path-string>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(logic(((escm.type.String)parameters.get(0)).value()));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // serialize
  public static class Serialize implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "serialize";
    }

    private static Trampoline.Bounce getEscmInstructions(InstructionsList list, int i, int n, ArrayList<Datum> escmExprs, Trampoline.Continuation continuation) throws Exception { 
      if(i >= n) {
        if(list == null) return continuation.run(new InstructionSet());
        return continuation.run(new InstructionSet(list.mergeInReverse()));
      }
      return Compiler.run(escmExprs.get(i),(compiledExpr) -> () -> {
        ArrayList<Instruction> compiledAsm = Assembler.run(compiledExpr);
        return Interpreter.run(new ExecutionState(GlobalState.globalEnvironment,compiledAsm),(evalResult) -> () -> {
          if(evalResult instanceof Eof)
            return continuation.run(new InstructionSet((new InstructionsList(compiledAsm,list)).mergeInReverse()));
          return getEscmInstructions(new InstructionsList(compiledAsm,list),i+1,n,escmExprs,continuation);
        });
      });
    }

    private static void writeEscmSerializedInstructions(ArrayList<Instruction> instructions, String serializePath) throws Exception {
      FileOutputStream fileOut = new FileOutputStream(serializePath);
      ObjectOutputStream out = new ObjectOutputStream(fileOut);
      out.write(SERIALIZED_FILE_HEADER);
      out.writeObject(instructions);
      out.close();
      fileOut.close();
    }

    public static Trampoline.Bounce logic(String escmPathStr, String serializePath, Trampoline.Continuation continuation) throws Exception {
      Path escmPath = Path.of(escmPathStr);
      if(FilePrimitives.IsFileP.logic(escmPath) == false)
        throw new Exceptionf("'(serialize <escm-file-path> <serialized-file-path>) 1st arg \"%s\" isn't a file!", escmPathStr);
      String escmContents = Files.readString(escmPath);
      ArrayList<Datum> escmExprs = FilePrimitives.FileRead.readBufferAsArrayList(escmPathStr,escmContents);
      return getEscmInstructions(null,0,escmExprs.size(),escmExprs,(instructionSet) -> () -> {
        if(!(instructionSet instanceof InstructionSet))
          throw new Exceptionf("'(serialize <escm-file-path> <serialized-file-path>) compiling \"%s\" didn't yield an instruction set!", escmPathStr);
        ArrayList<Instruction> instructions = ((InstructionSet)instructionSet).value();
        writeEscmSerializedInstructions(instructions,serializePath);
        return continuation.run(Void.VALUE);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(serialize <escm-file-path> <serialized-file-path>) didn't receive 2 strings: %s", Exceptionf.profileArgs(parameters));
      return logic(((escm.type.String)parameters.get(0)).value(), ((escm.type.String)parameters.get(1)).value(), continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-serialized
  public static class LoadSerialized implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-serialized";
    }

    private static ArrayList<Instruction> getEscmInstructions(String serializePath) throws Exception {
      if(IsSerializedP.logic(serializePath) == false)
        throw new Exceptionf("'(load-serialized <serialized-file-path>) 1st arg \"%s\" isn't a serialized file!", serializePath);
      FileInputStream fileIn = new FileInputStream(serializePath);
      ObjectInputStream in = new ObjectInputStream(fileIn);
      in.skipBytes(SERIALIZED_FILE_HEADER.length);
      Object readObject = in.readObject();
      in.close();
      fileIn.close();
      if(!(readObject instanceof ArrayList))
        throw new Exceptionf("'(load-serialized <serialized-file-path>) file \"%s\" didn't contain a serialized instruction set!", serializePath);
      ArrayList readObjectArrayList = (ArrayList)readObject;
      ArrayList<Instruction> instructions = new ArrayList<Instruction>(readObjectArrayList.size());
      for(Object obj : readObjectArrayList) {
        if(!(obj instanceof Instruction))
          throw new Exceptionf("'(load-serialized <serialized-file-path>) file \"%s\" didn't contain a serialized instruction set!", serializePath);
        instructions.add((Instruction)obj);
      }
      return instructions;
    }

    public static Trampoline.Bounce logic(String serializePath, Trampoline.Continuation continuation) throws Exception {
      ArrayList<Instruction> instructions = getEscmInstructions(serializePath);
      return Interpreter.run(new ExecutionState(GlobalState.globalEnvironment,instructions),(evalResult) -> () -> {
        if(evalResult instanceof Eof) return continuation.run(Void.VALUE);
        return continuation.run(evalResult);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(load-serialized <serialized-file-path>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      return logic(((escm.type.String)parameters.get(0)).value(),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // load-once-serialized
  public static class LoadOnceSerialized implements PrimitiveCallable {
    public java.lang.String escmName() {
      return "load-once-serialized";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof escm.type.String)) 
        throw new Exceptionf("'(load-once-serialized <serialized-file-path>) didn't receive 1 string: %s", Exceptionf.profileArgs(parameters));
      String filePath = FilePrimitives.AbsolutePath.logic(((escm.type.String)parameters.get(0)).value());
      if(SystemPrimitives.LoadOnce.notLoadedYet(filePath)) {
        SystemPrimitives.LoadOnce.registerLoadedFile(filePath);
        return LoadSerialized.logic(filePath,continuation);
      }
      return continuation.run(Void.VALUE);
    }
  }
}