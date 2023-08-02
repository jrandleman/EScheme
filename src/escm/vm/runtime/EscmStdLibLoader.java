// Author: Jordan Randleman - escm.vm.runtime.EscmStdLibLoader
// Purpose:
//   Interpret the <src/stdlib.scm> file.

package escm.vm.runtime;
import java.util.ArrayList;
import java.util.Stack;
import java.nio.file.Files;
import java.io.File;
import java.nio.file.Path;
import escm.type.Datum;
import escm.util.Pair;
import escm.util.Trampoline;
import escm.primitive.FilePrimitives;
import escm.primitive.SystemPrimitives;
import escm.vm.util.SourceInformation;
import escm.vm.util.Environment;
import escm.vm.runtime.installerGenerated.EscmPath;

public class EscmStdLibLoader {
  public static void load(Environment definitionEnvironment) throws Exception {
    String stdlibPath = EscmPath.VALUE+File.separator+"src"+File.separator+"stdlib.scm";
    String escmCode = Files.readString(Path.of(stdlibPath));
    // Note that we know our stdlib don't store any continuations using call/cc
    //   upon loading, so we can afford evaluating it with a dummy continuation.
    Trampoline.Continuation terminalContinuation = (ignored) -> () -> Trampoline.LAST_BOUNCE_SIGNAL;
    ArrayList<Datum> exprs = FilePrimitives.FileRead.readBufferAsArrayList(stdlibPath,escmCode);
    Trampoline.resolve(SystemPrimitives.Load.evalEachExpression(exprs,0,EscmCallStack.newCallStack(),definitionEnvironment,terminalContinuation));
  }
}
