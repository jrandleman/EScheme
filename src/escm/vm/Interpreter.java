// Author: Jordan Randleman - escm.vm.Interpreter
// Purpose:
//    Wrapper class containing the EScheme bytecode interpreter logic.
//    Invoke via "Interpreter.run()".

package escm.vm;
import java.util.ArrayList;
import java.util.Stack;
import java.util.Collections;
import escm.type.Datum;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.vm.type.Callable;
import escm.vm.type.Instruction;
import escm.vm.type.ExecutionState;
import escm.vm.type.Environment;
import escm.vm.type.ObjectAccessChain;

public class Interpreter {
  ////////////////////////////////////////////////////////////////////////////
  // Application helper
  private static Trampoline.Bounce executeApplication(Stack<Datum> stack, escm.type.Number applicationItemCount, Trampoline.Continuation continuation) throws Exception {
    // Get number of application items
    int count = applicationItemCount.intValue();
    if(count == 0)
      throw new Exception("VM ERROR: Invalid application count 0 in \"call\" instruction!");
    // Extract application items
    int absCount = Math.abs(count);
    Datum procedure = null;
    ArrayList<Datum> args = new ArrayList<Datum>(absCount-1);
    try {
      if(count < 0) {
        procedure = stack.pop();
        for(int i = 0; i < absCount-1; ++i)
          args.add(stack.pop());
      } else {
        for(int i = absCount-2; i >= 0; --i)
          args.add(stack.pop());
        Collections.reverse(args);
        procedure = stack.pop();
      }
    } catch(java.util.EmptyStackException e) {
      throw new Exceptionf("VM ERROR: Insufficient args for \"call\" instruction (need %d have %d)!", absCount, ((procedure == null) ? 0 : 1) + args.size());
    }
    
    // Validate procedure as such
    if(!(procedure instanceof Callable))
      throw new Exceptionf("VM ERROR: Can't apply non-callable %s", procedure.profile());

    // Return the application's result
    return ((Callable)procedure).callWith(args,continuation);
  }


  ////////////////////////////////////////////////////////////////////////////
  // Core interpretation loop
  // => PRECONDITION: <state.instructions> was generated by the assembler
  // => CVR = current value register, CII = current instruction index
  // => Trampolines continuations!
  public static Trampoline.Bounce run(ExecutionState state, Trampoline.Continuation continuation) throws Exception {
    int totalInstructions = state.instructions.size();
    while(state.cii < totalInstructions) {
      Instruction instruction = state.instructions.get(state.cii);
      switch(instruction.operation) {
        //////////////////////////////////////////////////////////////////////
        // (define <symbol>)
        case Instruction.DEFINE: {
          if(instruction.argument instanceof escm.type.Symbol) {
            state.env.define(((escm.type.Symbol)instruction.argument).value(),state.cvr);
          } else { // instruction.argument instanceof ObjectAccessChain
            ((ObjectAccessChain)instruction.argument).define(state,state.cvr);
          }
          state.cvr = escm.type.Void.VALUE;
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (set! <symbol>)
        case Instruction.SET: {
          if(instruction.argument instanceof escm.type.Symbol) {
            state.env.set(((escm.type.Symbol)instruction.argument).value(),state.cvr);
          } else { // instruction.argument instanceof ObjectAccessChain
            ((ObjectAccessChain)instruction.argument).set(state,state.cvr);
          }
          state.cvr = escm.type.Void.VALUE;
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (defined? <symbol>)
        case Instruction.DEFINEDP: {
          if(instruction.argument instanceof escm.type.Symbol) {
            state.cvr = escm.type.Boolean.valueOf(state.env.has(((escm.type.Symbol)instruction.argument).value()));
          } else { // instruction.argument instanceof ObjectAccessChain
            state.cvr = escm.type.Boolean.valueOf(((ObjectAccessChain)instruction.argument).has(state));
          }
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (ifn <number>)
        case Instruction.IFN: {
          if(state.cvr.isTruthy()) {
            ++state.cii;
          } else {
            state.cii += ((escm.type.Number)instruction.argument).intValue();
          }
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (jump <number>)
        case Instruction.JUMP: {
          state.cii += ((escm.type.Number)instruction.argument).intValue();
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (load <datum>)
        case Instruction.LOAD: {
          state.cvr = instruction.argument.loadWithState(state);
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (load-symbol <symbol>)
        case Instruction.LOAD_SYMBOL: {
          state.cvr = instruction.argument;
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (call <datum>)
        case Instruction.CALL: {
          state.cvr = instruction.argument.loadWithState(state);
          ++state.cii;
          if(!(state.cvr instanceof escm.type.Number))
            throw new Exceptionf("VM ERROR: Invalid application count %s in \"call\" instruction!", state.cvr.profile());
          return executeApplication(
            state.stack,
            (escm.type.Number)state.cvr,
            (value) -> () -> Interpreter.run(state.getContinuationState(value),continuation)
          );
        }

        //////////////////////////////////////////////////////////////////////
        // (push) (push <datum>)
        case Instruction.PUSH: {
          if(instruction.argument == null) {
            state.stack.push(state.cvr);
          } else {
            state.stack.push(instruction.argument.loadWithState(state));
          }
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (pop)
        case Instruction.POP: {
          if(state.stack.empty())
            throw new Exception("VM ERROR: Invalid \"pop\" instruction, stack is empty!");
          state.cvr = state.stack.pop();
          ++state.cii;
          break;
        }

        //////////////////////////////////////////////////////////////////////
        // (return) (return <datum>)
        case Instruction.RETURN: {
          if(instruction.argument == null) {
            return () -> continuation.run(state.cvr);
          } else {
            return () -> continuation.run(instruction.argument.loadWithState(state));
          }
        }

        //////////////////////////////////////////////////////////////////////
        // Invalid instruction!
        default: {
          throw new Exceptionf("VM ERROR: Invalid instruction index %d!", state.cii);
        }
      }
    }
    Datum finalCvrValue = state.cvr;
    return () -> continuation.run(finalCvrValue);
  }
}