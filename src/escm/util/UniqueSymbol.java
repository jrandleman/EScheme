// Author: Jordan Randleman - escm.util.UniqueSymbol
// Purpose:
//    Class to provide unique-symbol generation. Used by <gensym>.
//    Leverages <EscmThread> for a thread-local global counter.

package escm.util;
import java.math.BigInteger;
import escm.type.Symbol;
import escm.vm.runtime.EscmThread;

public class UniqueSymbol {
  private static String generateUniqueSymbolValue() {
    EscmThread currentThread = (EscmThread)Thread.currentThread();
    BigInteger currentCount = currentThread.uniqueCounter;
    currentThread.uniqueCounter = currentThread.uniqueCounter.add(BigInteger.ONE);
    return "escm-gensym-" + currentThread.getId() + "-" + currentCount;
  }

  public static escm.type.Symbol generate() {
    return new Symbol(generateUniqueSymbolValue());
  }

  public static escm.type.Symbol generate(String nameForReadability) {
    return new Symbol(generateUniqueSymbolValue() + "-" + nameForReadability);
  }
};