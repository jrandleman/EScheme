// Author: Jordan Randleman - escm.primitive.SerializationPrimitives
// Purpose:
//    Java primitives for boolean operations.

package escm.primitive;
import java.util.ArrayList;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import escm.type.Datum;
import escm.type.Void;
import escm.type.port.Eof;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.Compiler;
import escm.vm.Assembler;
import escm.vm.Interpreter;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.type.PrimitiveCallable;
import escm.vm.runtime.GlobalState;
import escm.primitive.SerializationPrimitives_util.InstructionSet;

public class SerializationPrimitives {
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
      if(FilePrimitives.IsFileP.logic(Path.of(serializePath)) == false)
        throw new Exceptionf("'(load-serialized <serialized-file-path>) 1st arg \"%s\" isn't a file!", serializePath);
      FileInputStream fileIn = new FileInputStream(serializePath);
      ObjectInputStream in = new ObjectInputStream(fileIn);
      Object readObject = in.readObject();
      in.close();
      fileIn.close();
      if(!(readObject instanceof ArrayList))
        throw new Exceptionf("'(load-serialized <serialized-file-path>) file \"%s\" didn't contain a serialized instruction set!", serializePath);
      ArrayList readObjectArrayList = (ArrayList)readObject;
      ArrayList<Instruction> instructions = new ArrayList<Instruction>();
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
}