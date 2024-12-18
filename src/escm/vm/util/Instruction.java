// Author: Jordan Randleman - escm.vm.util.Instruction
// Purpose:
//    Class to denote an escm instruction.

package escm.vm.util;
import java.io.Serializable;
import escm.type.Datum;

public class Instruction implements Serializable {
  ////////////////////////////////////////////////////////////////////////////
  // Static Instruction Constants
  public static final int DEFINE      = 0;
  public static final int SET         = 1;

  public static final int IFN         = 2;
  public static final int JUMP        = 3;

  public static final int QUOTE       = 4;
  public static final int LOAD        = 5;

  public static final int CALL        = 6;

  public static final int PUSH        = 7;
  public static final int POP         = 8;

  public static final int RETURN      = 9;

  public static final int DEFINEDP    = 10;

  public static final int DEFINE_TYPE = 11;


  ////////////////////////////////////////////////////////////////////////////
  // Static Pseudo-Instruction Constants
  // => Generated by the compiler
  // => Converted to <load> instances by the assembler
  public static class Pseudo {
    public static final int LOAD_CLOSURE = 12;
  }


  ////////////////////////////////////////////////////////////////////////////
  // Instruction Designation & Argument
  public final int operation;
  public final Datum argument;


  ////////////////////////////////////////////////////////////////////////////
  // Constructor
  public Instruction(int operation, Datum argument) {
    this.operation = operation;
    this.argument = argument;
  }
}