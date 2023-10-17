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
import escm.type.Pair;
import escm.type.Symbol;
import escm.type.Void;
import escm.type.bool.Boolean;
import escm.type.port.Eof;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.vm.Compiler;
import escm.vm.Assembler;
import escm.vm.Interpreter;
import escm.vm.util.Instruction;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;
import escm.vm.runtime.GlobalState;
import escm.primitive.lib.serialization.InstructionSet;

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

    public Datum signature() {
      return Pair.List(new Symbol("serialized?"),new Symbol("<file-path-string>"));
    }

    public String docstring() {
      return "@help:Procedures:Utilities\nReturns whether <file-path> is a serialized file created by <serialize> or\n<serialize-module>.";
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

    public Datum signature() {
      return Pair.List(new Symbol("serialize"),new Symbol("<escm-file-path>"),new Symbol("<serialized-file-path>"));
    }

    public String docstring() {
      return "@help:Procedures:Utilities\nLoad and serialize the contents of <escm-file-path> into <serialized-file-path>.\n\nSerialization works by storing the Java bytecode of the assembled instruction\nset object holding <escm-file-path>'s EScheme bytecode in <serialized-file-path>.\nThe contents must first be evaluated in order to expand macros properly during\ncompilation.\n\nTotally optional to use, this facility is provided in order to serve as a portable \nobfuscation tactic to generate a rough equivalent to \"EScheme binaries\". \n\nHowever, it is important to note that - strangely enough - serialized files tend to \nload _SLOWER_ than regular EScheme files. Despite allowing us to circumvent the \nreading, compilation, and assembly of EScheme code, it turns out that EScheme's \nfacilities to do so are all actually so fast as to beat out Java's native \ndeserialization functions!\n\nHence, only serialize your EScheme code if you'd like to make the file unreadable\nfor humans.\n\nAll EScheme objects serialize just as you'd imagine, save for Threads and Ports.\nBy default, Java doesn't enable these items to be serialized since they rely on\nsystem-dependant components that don't translate to a different machine's processes.\nHowever, EScheme allows such to occur using the following rules:\n\n  1. Threads\n     * Threads serialize to their default, \"pre-run\" state. This means that\n       serialized threads save their name and runnable-callable. It does _NOT_\n       save any information about the executing sub-process if serialized mid-run.\n  2. Output-Ports\n     * Output-Ports serialize their absolute file path, and always deserialize\n       without appending (as if created via <open-output-file>).\n       - This is because deserialization re-loads the entire script containing\n         the port, and hence any writing operations conducted by the script ought\n         to reoccur during the script's loading process.\n     * Hence Output-Ports _MUST_ be serialized & deserialized on a machine with the\n       same directory layout, to avoid errors with the stored absolute-path!\n  3. Input-Ports\n     * Input-Ports serialize their absolute file path, and always deserialize with\n       their current line & column both set to 1 (reading from the start of the file).\n     * Hence Input-Ports _MUST_ be serialized & deserialized on a machine with the\n       same directory layout, to avoid errors with the stored absolute-path!\n\nOF NOTE: The semantics of thread/port serialization won't be an issue for 99.99999%\n*******  of serialized programs that use macros in a remotely sane way.\n\n         Only by intentionally returning a value that contains an initialized thread\n         or port datum from a macro can one tease out this behavior.\n\n         Given that most macros only return code as data structures (rather than\n         data values), these semantics are only something worth thinking about\n         if you're intentionally employing some genuinely tricky (& almost\n         certainly questionable) EScheme meta-programming techniques.\n\nUse <serialize-module> to instead load <escm-file-path> (as part of its \ncompilation) in a unique environment, rather than in the current one (as \ndone by <serialize>).\n\nLoad serialized EScheme code with the <load> primitive or <import> macro.";
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

    private static Trampoline.Bounce logic(String prmName, String escmPathStr, String serializePath, Environment definitionEnvironment, Trampoline.Continuation continuation) throws Exception {
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

    public Datum signature() {
      return Pair.List(new Symbol("serialize-module"),new Symbol("<escm-file-path>"),new Symbol("<serialized-file-path>"));
    }

    public String docstring() {
      return "@help:Procedures:Utilities\nOperates exactly like <serialize>, but loads <escm-file-path> in a seperate\nenvironment rather than the current one.\n\nSee <serialize> for details on the serialization process, and <serialized?>\nto learn how to check whether a file is serialized or not.\n\nLoad serialized EScheme code with the <load> primitive or <import> macro.";
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