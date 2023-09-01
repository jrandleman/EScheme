// Author: Jordan Randleman - escm.primitive.MetaPrimitives
// Purpose:
//    Java primitives for metaprogramming procedures.

package escm.primitive;
import java.util.ArrayList;
import escm.type.Datum;
import escm.type.Pair;
import escm.type.Nil;
import escm.type.Symbol;
import escm.type.procedure.SyntaxProcedure;
import escm.type.procedure.Procedure;
import escm.type.bool.Boolean;
import escm.util.error.Exceptionf;
import escm.util.Trampoline;
import escm.util.UniqueSymbol;
import escm.vm.Compiler;
import escm.vm.Assembler;
import escm.vm.Interpreter;
import escm.vm.type.callable.Callable;
import escm.vm.util.ExecutionState;
import escm.vm.util.Environment;
import escm.vm.util.ObjectAccessChain;
import escm.vm.type.callable.DocString;
import escm.vm.type.primitive.Primitive;
import escm.vm.type.primitive.PrimitiveCallable;

public class MetaPrimitives {
  ////////////////////////////////////////////////////////////////////////////
  // callable-signature
  public static class CallableSignature extends Primitive {
    public java.lang.String escmName() {
      return "callable-signature";
    }

    public Datum signature() {
      return Pair.List(new Symbol("callable-signature"),new Symbol("<callable>"));
    }

    public String docstring() {
      return "Get the <callable> call signature as EScheme data, or #f if unavailable.\n\nFor unary parameter lists: returns the parameter clause as a list of symbols.\nFor binary+ parameter lists: returns a list of parameter clauses.\n\nNote that the symbols '... & '. denote variadic parameters.\n\nAn argument list may be used to denote a default argument value.";
    }

    public static Datum logic(Datum obj) {
      if(!FunctionalPrimitives.IsCallable.logic(obj)) return Boolean.FALSE;
      return ((Callable)obj).signature();
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(callable-signature <callable>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // callable-name
  public static class CallableName extends Primitive {
    public java.lang.String escmName() {
      return "callable-name";
    }

    public Datum signature() {
      return Pair.List(new Symbol("callable-name"),new Symbol("<callable>"));
    }

    public String docstring() {
      return "Get the <callable>'s name as EScheme data, or #f if unavailable.";
    }

    public static Datum logic(Datum obj) {
      if(!FunctionalPrimitives.IsCallable.logic(obj)) return Boolean.FALSE;
      if(obj instanceof Procedure) return new Symbol(((Procedure)obj).readableName());
      Datum sig = ((Callable)obj).signature();
      if(sig instanceof Pair) {
        Datum head = ((Pair)sig).car();
        if(head instanceof Pair) head = ((Pair)head).car();
        if(head instanceof Symbol) return head;
      }
      return Boolean.FALSE;
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(callable-name <callable>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0));
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // docstring
  public static class DocStringExtractor extends Primitive {
    public java.lang.String escmName() {
      return "docstring";
    }

    public Datum signature() {
      return Pair.List(new Symbol("docstring"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Get the <obj>'s docstring if available, else #f.\n\nDocstrings are used to give details on objects passed to the <help> function,\nand **MUST** be writted as string literals to register as valid syntax (e.g.\nfor procedures, classes, interfaces, etc.).\n\nNote that docstring newlines and tabs should be added by users exactly as\nthey'd like their docstring to render in the <help> menu: no additional\nformatting will be done!";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(docstring <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      String docs = ((DocString)parameters.get(0)).docstring().trim();
      if(docs.length() > 0) return new escm.type.String(docs);
      return Boolean.FALSE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // apply
  public static class Apply extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "apply";
    }


    public Datum signature() {
      return Pair.List(new Symbol("apply"),new Symbol("<callable>"),new Symbol("<arg-list>"));
    }

    public String docstring() {
      return "Apply <callable> to the arguments stored in the <arg-list> list.";
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

    public Datum signature() {
      return Pair.List(new Symbol("compile"),new Symbol("<escm-code-as-data>"));
    }

    public String docstring() {
      return "Convert <escm-code-as-data> into a list of its equivalent bytecode instructions.";
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

    public Datum signature() {
      return Pair.List(new Symbol("eval-bytecode"),new Symbol("<escm-bytecode-as-data>"));
    }

    public String docstring() {
      return "Evaluate <escm-bytecode-as-data> to produce a result.";
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

    public Datum signature() {
      return Pair.List(new Symbol("eval"),new Symbol("<escm-code-as-data>"));
    }

    public String docstring() {
      return "Evaluate <escm-code-as-data> to produce a result. Equivalent to:\n  (eval-bytecode (compile <escm-code-as-data>))";
    }

    public static Trampoline.Bounce logic(Datum data, Environment env, Trampoline.Continuation continuation) throws Exception {
      return Compiler.run(data,env,(compiled) -> () -> Interpreter.run(new ExecutionState(env,Assembler.run(compiled)),continuation));
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1) 
        throw new Exceptionf("'(eval <escm-code-as-data>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return logic(parameters.get(0),this.definitionEnvironment,continuation);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // gensym
  public static class Gensym extends Primitive {
    public java.lang.String escmName() {
      return "gensym";
    }

    public Datum signature() {
      return Pair.List(
        Pair.List(new Symbol("gensym")),
        Pair.List(new Symbol("gensym"),new Symbol("<name-symbol>")));
    }

    public String docstring() {
      return "Returns a fresh, unique symbol. Guarentees that the symbol is unique across\nthreads as well. Used extensively by macros. Pass a <name-symbol> to improve\nreadability when printing generated symbols.";
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
  // syntax?
  public static class IsSyntax extends Primitive {
    public java.lang.String escmName() {
      return "syntax?";
    }

    public Datum signature() {
      return Pair.List(new Symbol("syntax?"),new Symbol("<obj>"));
    }

    public String docstring() {
      return "Returns whether <obj> is a syntax object. Syntax objects are created when\nmacros are evaluated as a procedure argument.";
    }

    private static SyntaxProcedure isModuleMacro(Environment definitionEnvironment, Symbol moduleMacro) {
      try {
        Datum value = (new ObjectAccessChain(moduleMacro)).loadWithState(definitionEnvironment);
        if(!(value instanceof SyntaxProcedure)) return null;
        return (SyntaxProcedure)value;
      } catch(Exception e) {
        return null;
      }
    }

    // Returns <null> if false, <SyntaxProcedure> if true
    public static SyntaxProcedure logic(Environment definitionEnvironment, Symbol name) {
      try {
        // Check for module macro invocation (e.g. `Module.myMacro`)
        if(ObjectAccessChain.is(name))
          return isModuleMacro(definitionEnvironment,name);
        // Check for variable macro invocation (e.g. `myMacro`)
        if(!definitionEnvironment.has(name)) return null;
        Datum value = definitionEnvironment.get(name);
        if(value instanceof SyntaxProcedure) return (SyntaxProcedure)value;
        return null;
      } catch(Exception e) {
        return null;
      }
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(syntax? <obj>) didn't receive exactly 1 arg: %s", Exceptionf.profileArgs(parameters));
      return Boolean.valueOf(parameters.get(0) instanceof SyntaxProcedure);
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // escm-define-syntax
  public static class EscmDefineSyntax extends Primitive {
    public java.lang.String escmName() {
      return "escm-define-syntax";
    }

    public Datum signature() {
      return Pair.List(new Symbol("escm-define-syntax"),new Symbol("<symbol>"),new Symbol("<callable>"));
    }

    public String docstring() {
      return "Expanded to by <define-syntax> with its quoted <name-symbol> as <symbol>.";
    }

    public Datum callWith(ArrayList<Datum> parameters) throws Exception {
      if(parameters.size() != 2)
        throw new Exceptionf("'(escm-define-syntax <symbol> <callable>) didn't receive exactly 2 args: %s", Exceptionf.profileArgs(parameters));
      Datum name = parameters.get(0);
      if(!(name instanceof Symbol))
        throw new Exceptionf("'(escm-define-syntax <symbol> <callable>) 1st arg %s isn't a symbol: %s", name.profile(), Exceptionf.profileArgs(parameters));
      Datum value = parameters.get(1);
      if(!(value instanceof Callable))
        throw new Exceptionf("'(escm-define-syntax <symbol> <callable>) 2nd arg %s isn't a callable: %s", value.profile(), Exceptionf.profileArgs(parameters));
      Symbol nameSymbol = (Symbol)name;
      this.definitionEnvironment.define(nameSymbol,new SyntaxProcedure(nameSymbol.value(),(Callable)value));
      return escm.type.Void.VALUE;
    }
  }


  ////////////////////////////////////////////////////////////////////////////
  // expand-syntax
  public static class ExpandSyntax extends PrimitiveCallable {
    public java.lang.String escmName() {
      return "expand-syntax";
    }

    public Datum signature() {
      return Pair.List(new Symbol("expand-syntax"),new Symbol("<quoted-macro-expression>"));
    }

    public String docstring() {
      return "Returns the expansion of <quoted-macro-expression> by executing its macro.";
    }

    public Trampoline.Bounce callWith(ArrayList<Datum> parameters, Trampoline.Continuation continuation) throws Exception {
      if(parameters.size() != 1)
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) didn't receive exactly 1 symbol: %s", Exceptionf.profileArgs(parameters));
      Datum macroExpr = parameters.get(0);
      if(!(macroExpr instanceof Pair) || !(((Pair)macroExpr).car() instanceof Symbol))
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) given arg isn't a macro expression: %s", Exceptionf.profileArgs(parameters));
      Pair macroExprList = (Pair)macroExpr;
      Symbol macroName = (Symbol)macroExprList.car();
      SyntaxProcedure macro = IsSyntax.logic(this.definitionEnvironment,macroName);
      if(macro == null)
        throw new Exceptionf("'(expand-syntax <quoted-macro-expression>) given arg isn't a macro expression: %s", Exceptionf.profileArgs(parameters));
      ArrayList<Datum> macroArgs = Apply.convertListToArrayList(macroExprList.cdr());
      return macro.callWith(macroArgs,continuation);
    }
  }
}