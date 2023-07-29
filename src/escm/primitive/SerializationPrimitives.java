// Author: Jordan Randleman - escm.primitive.SerializationPrimitives
// Purpose:
//    Java primitives for serialization operations.
//    Also contains loading logic for serialized files via <loadSerializedFile()>.

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
import escm.vm.util.Environment;
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
  public static class IsSerializedP extends Primitive {
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
  public static class Serialize extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "serialize";
    }

    private static Trampoline.Bounce getDeserializedEscmInstructions(InstructionsList list, int i, int n, ArrayList<Datum> escmExprs, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception { 
      if(i >= n) {
        if(list == null) return continuation.run(new InstructionSet());
        return continuation.run(new InstructionSet(list.mergeInReverse()));
      }
      return Compiler.run(escmExprs.get(i),definitionEnvironment,(compiledExpr) -> () -> {
        ArrayList<Instruction> compiledAsm = Assembler.run(compiledExpr);
        return Interpreter.run(new ExecutionState(definitionEnvironment,compiledAsm),(evalResult) -> () -> {
          if(evalResult instanceof Eof)
            return continuation.run(new InstructionSet((new InstructionsList(compiledAsm,list)).mergeInReverse()));
          return getDeserializedEscmInstructions(new InstructionsList(compiledAsm,list),i+1,n,escmExprs,definitionEnvironment,continuation);
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

    public static Trampoline.Bounce logic(String prmName, String escmPathStr, String serializePath, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
      Path escmPath = Path.of(escmPathStr);
      if(FilePrimitives.IsFileP.logic(escmPath) == false)
        throw new Exceptionf("'(%s <escm-file-path> <serialized-file-path>) 1st arg \"%s\" isn't a file!", prmName, escmPathStr);
      String escmContents = Files.readString(escmPath);
      ArrayList<Datum> escmExprs = FilePrimitives.FileRead.readBufferAsArrayList(escmPathStr,escmContents);
      return getDeserializedEscmInstructions(null,0,escmExprs.size(),escmExprs,definitionEnvironment,(instructionSet) -> () -> {
        if(!(instructionSet instanceof InstructionSet))
          throw new Exceptionf("'(%s <escm-file-path> <serialized-file-path>) compiling \"%s\" didn't yield an instruction set!", prmName, escmPathStr);
        ArrayList<Instruction> instructions = ((InstructionSet)instructionSet).value();
        writeEscmSerializedInstructions(instructions,serializePath);
        return continuation.run(Void.VALUE);
      });
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(serialize <escm-file-path> <serialized-file-path>) didn't receive 2 strings: %s", Exceptionf.profileArgs(parameters));
      return logic("serialize", ((escm.type.String)parameters.get(0)).value(), ((escm.type.String)parameters.get(1)).value(), this.definitionEnvironment, continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // serialize-module
  public static class SerializeModule extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "serialize-module";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2 || !(parameters.get(0) instanceof escm.type.String) || !(parameters.get(1) instanceof escm.type.String)) 
        throw new Exceptionf("'(serialize-module <escm-file-path> <serialized-file-path>) didn't receive 2 strings: %s", Exceptionf.profileArgs(parameters));
      Environment isolatedEnvironment = SystemPrimitives.EscmLoadModule.getModuleEnvironment();
      return Serialize.logic("serialize-module", ((escm.type.String)parameters.get(0)).value(), ((escm.type.String)parameters.get(1)).value(), isolatedEnvironment, continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // Logic to load a serialized file
  private static ArrayList<Instruction> getDeserializedEscmInstructions(String primitiveName, String serializePath) throws Exception {
    FileInputStream fileIn = new FileInputStream(serializePath);
    ObjectInputStream in = new ObjectInputStream(fileIn);
    in.skipBytes(SERIALIZED_FILE_HEADER.length);
    Object readObject = in.readObject();
    in.close();
    fileIn.close();
    if(!(readObject instanceof ArrayList))
      throw new Exceptionf("'(%s <filename>) serialized file \"%s\" didn't contain a valid serialized instruction set!", primitiveName, serializePath);
    ArrayList readObjectArrayList = (ArrayList)readObject;
    ArrayList<Instruction> instructions = new ArrayList<Instruction>(readObjectArrayList.size());
    for(Object obj : readObjectArrayList) {
      if(!(obj instanceof Instruction))
        throw new Exceptionf("'(%s <filename>) serialized file \"%s\" didn't contain a valid serialized instruction set!", primitiveName, serializePath);
      instructions.add((Instruction)obj);
    }
    return instructions;
  }

  // @PRECONDITION: IsSerializedP.logic(serializePath)
  public static Trampoline.Bounce loadSerializedFile(String primitiveName, String serializePath, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
    ArrayList<Instruction> instructions = getDeserializedEscmInstructions(primitiveName,serializePath);
    return Interpreter.run(new ExecutionState(definitionEnvironment,instructions),(evalResult) -> () -> {
      if(evalResult instanceof Eof) return continuation.run(Void.VALUE);
      return continuation.run(evalResult);
    });
  }
}