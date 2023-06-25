// Author: Jordan Randleman - escm.primitive.MetaPrimitives
// Purpose:
//    Java primitives for metaprogramming procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Symbol;
import escm.util.Exceptionf;
import escm.util.Trampoline;
import escm.util.UniqueSymbol;
import escm.vm.Compiler;
import escm.vm.Assembler;
import escm.vm.Interpreter;
import escm.vm.type.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.type.Primitive;
import escm.vm.type.PrimitiveCallable;

public class MetaPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // apply
  public static class Apply extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "apply";
    }

    public static ArrayList<Datum> convertListToArrayList(Datum lis) {
      ArrayList<Datum> aList = new ArrayList<Datum>();
      while(lis instanceof Pair) {
        Pair lisPair = (Pair)lis;
        aList.add(lisPair.car());
        lis = lisPair.cdr();
      }
      return aList;
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 2) 
        throw new Exceptionf("'(apply <callable> <arg-list>) didn't receive exactly 2 args (procedure & arg-list): %s", Exceptionf.profileArgs(parameters));
      Datum procedure = parameters.get(0);
      Datum arguments = parameters.get(1);
      if(!(procedure instanceof Callable)) 
        throw new Exceptionf("'(apply <callable> <arg-list>) 1st arg %s isn't a procedure!", procedure.profile());
      if(!Pair.isList(arguments)) 
        throw new Exceptionf("'(apply <callable> <arg-list>) 2nd arg %s isn't an arg list!", arguments.profile());
      ArrayList<Datum> args = convertListToArrayList(arguments);
      return ((Callable)procedure).callWith(args,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // compile
  public static class Compile extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "compile";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(compile <escm-code-as-data>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Compiler.run(parameters.get(0),this.definitionEnvironment,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // eval-bytecode
  public static class EvalBytecode extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "eval-bytecode";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eval-bytecode <escm-bytecode-as-data>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Interpreter.run(new ExecutionState(this.definitionEnvironment,Assembler.run(parameters.get(0))),continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // eval
  public static class Eval extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "eval";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eval <escm-code-as-data>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      Datum data = parameters.get(0);
      return Compiler.run(data,this.definitionEnvironment,(compiled) -> () -> {
        return Interpreter.run(new ExecutionState(this.definitionEnvironment,Assembler.run(compiled)),continuation);
      });
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // gensym
  public static class Gensym extends Primitive {
    public java.lang.String escmName() {
      return "gensym";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() > 1)
        throw new Exceptionf("'(gensym <optional-name-symbol>) received more than 1 arg: %s", Exceptionf.profileArgs(parameters));
      if(parameters.size() == 0) return UniqueSymbol.generate();
      Datum name = parameters.get(0);
      if(!(name instanceof Symbol))
        throw new Exceptionf("'(gensym <optional-name-symbol>) <name> arg isn't a symbol: %s", Exceptionf.profileArgs(parameters));
      return UniqueSymbol.generate(((Symbol)name).value());
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-define-syntax
  public static class EscmDefineSyntax extends Primitive {
    public java.lang.String escmName() {
      return "escm-define-syntax";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(escmn-define-syntax <symbol> <callable>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum name = parameters.get(0);
      if(!(name instanceof Symbol))
        throw new Exceptionf("'(escmn-define-syntax <symbol> <callable>) 1st arg %s isn't a symbol: %s", name.profile(), Exceptionf.profileArgs(parameters));
      Datum value = parameters.get(1);
      if(!(value instanceof Callable))
        throw new Exceptionf("'(escmn-define-syntax <symbol> <callable>) 2nd arg %s isn't a callable: %s", value.profile(), Exceptionf.profileArgs(parameters));
      String macroName = ((Symbol)name).value();
      Compiler.MACRO_REGISTRY.put(macroName,(Callable)value.loadWithName(macroName));
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // expand-syntax
  public static class ExpandSyntax extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "expand-syntax";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) didn't receive exactly 1 symbol: %s", Exceptionf.profileArgs(parameters));
      Datum macroExpr = parameters.get(0);
      if(!(macroExpr instanceof Pair) || !(((Pair)macroExpr).car() instanceof Symbol))
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) given arg isn't a macro expression: %s", Exceptionf.profileArgs(parameters));
      Pair macroExprList = (Pair)macroExpr;
      String macroName = ((Symbol)macroExprList.car()).value();
      if(!Compiler.MACRO_REGISTRY.containsKey(macroName))
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) given arg isn't a macro expression: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Datum> macroArgs = MetaPrimitives.Apply.convertListToArrayList(macroExprList.cdr());
      return Compiler.MACRO_REGISTRY.get(macroName).callWith(macroArgs,(expandedExpr) -> () -> continuation.run(expandedExpr));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // delete-syntax!
  public static class DeleteSyntaxBang extends Primitive {
    public java.lang.String escmName() {
      return "delete-syntax!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(delete-syntax! <macro-name>) didn't receive exactly 1 symbol: %s", Exceptionf.profileArgs(parameters));
      return escm.type.bool.Boolean.valueOf(Compiler.MACRO_REGISTRY.remove(((Symbol)parameters.get(0)).value()) != null);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // syntax?
  public static class IsSyntax extends Primitive {
    public java.lang.String escmName() {
      return "syntax?";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1 || !(parameters.get(0) instanceof Symbol))
        throw new Exceptionf("'(syntax? <macro-name>) didn't receive exactly 1 symbol: %s", Exceptionf.profileArgs(parameters));
      return escm.type.bool.Boolean.valueOf(Compiler.MACRO_REGISTRY.containsKey(((Symbol)parameters.get(0)).value()));
    }
  }
}