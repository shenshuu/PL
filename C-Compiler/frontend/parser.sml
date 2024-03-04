open Lexer;

signature Parser =
sig
    type program
    type function
    type statement
    type exp
	     
    val printAST : program -> unit
    val parse : Token list -> program	  
end

structure Parser =
struct 
exception ProgramParseError of string
exception FunctionParseError of string
exception StatementParseError of string
exception ExpParseError of string

(* AST Node definitions *)
type id = string
	      
datatype program = Program of function
     and function = Function of (id * statement) | NullFunc
     and statement = Statement of exp | NullStm
     and exp = Const of int | NullExp

			    
(* Helper function to print AST *)
fun printAST prog =
    let
	fun stringMultiply (s, n) =
	    if n <= 0 then "" else s ^ stringMultiply(s, n-1)
						     
	fun printProg (prog,level) =
	    case prog of
		Program f =>  (print("Program(\n");
			       print(stringMultiply("  ", level));
			       printFunc(f, level+1);
			       print(")\n"))
	and printFunc (f,level) =
	    case f of
		Function(name,body) => (print("Function(\n");
					print(stringMultiply("  ", level));
					print("name=\"" ^ name ^ "\"\n");
					print(stringMultiply("  ", level));
					print("body=");
					printStm(body, level+1);
					print(stringMultiply("  ", level-1));
					print(")\n"))
	and printStm (s,level) =
	    case s of
		Statement e => (print("Return(\n");
				print(stringMultiply("  ", level));
				printExp(e, level+1);
				print(stringMultiply("  ", level-1));
				print(")\n"))
	and printExp (e,level) =
	    case e of 
		Const n => print("Const(" ^ Int.toString n ^ ")\n")
    in
	printProg (prog,1)
    end
		 

fun parse tokens =
    let
	fun parseFunc (tokens, i, func) =
	    case (tokens, i) of
		(Keyword("int")::tokens', 0) => parseFunc(tokens', i+1, func)
	      | (Keyword("main")::tokens', 1) => parseFunc(tokens', i+1, Function("main", NullStm))
	      | (OpenParen::tokens', 2) => parseFunc(tokens', i+1, func)
	      | (Keyword("void")::tokens', 3) => parseFunc(tokens', i+1, func)
	      | (CloseParen::tokens', 4) => parseFunc(tokens', i+1, func)
	      | (OpenBrace::tokens', 5) => parseFunc(tokens', i+1, func)
	      | (_::_, 6) => (let val (stm, rest) = parseStm(tokens, 0, NullStm)
			      in parseFunc(rest, i+1, Function("main", stm)) end)
	      | (CloseBrace::tokens', 7) => (func, tokens')
	      | _ => raise FunctionParseError "cannot parse function from given tokens"
	and parseStm (tokens, i, stm) =
	    case (tokens, i) of
		(Keyword("return")::tokens', 0) => (let val (e, rest) = parseExp(tokens', 0, NullExp)
						    in (Statement e, rest) end)
	      | _ => raise StatementParseError "cannot parse statement from given tokens"
	and parseExp (tokens, i, e) =
	    case (tokens, i) of
		(Constant(n)::tokens', 0) => parseExp(tokens', i+1, Const n)
	      | (Semicolon::tokens', 1) => (e, tokens')
	      | _ => raise ExpParseError "cannot parse exp from given tokens"
    in
	case tokens of
	    Keyword("int")::tokens' => (let val (func, rest) = parseFunc(tokens, 0, NullFunc)
					in if null rest then Program func else parse rest end)
	 | _ => raise ProgramParseError "cannot parse invalid program"
								    
    end
end
