open Lexer;

signature Parser =
sig
    type program
    type function
    type statement
    type exp
	     
    val print_AST : program -> unit
    val parse : Token list -> program	  
				  
end

structure Parser =
struct 
exception ProgramParseError
exception FunctionParseError
exception StatementParseError
exception ExpParseError

(* AST Node definitions *)
type id = string
	      
datatype program = Program of function
     and function = Function of (id * statement)
     and statement = Statement of exp
     and exp = Const of int

			    
(* Helper function to print AST *)
fun print_AST prog =
    let
	fun string_multiply (s, n) =
	    if n <= 0 then "" else s ^ string_multiply(s, n-1)
						     
	fun print_prog (prog,level) =
	    case prog of
		Program f =>  (print("Program(\n");
			       print(string_multiply("  ", level));
			       print_func(f, level+1);
			       print(")\n"))
	and print_func (f,level) =
	    case f of
		Function(name,body) => (print("Function(\n");
					print(string_multiply("  ", level));
					print("name=\"" ^ name ^ "\"\n");
					print(string_multiply("  ", level));
					print("body=");
					print_stm(body, level+1);
					print(string_multiply("  ", level-1));
					print(")\n"))
	and print_stm (s,level) =
	    case s of
		Statement e => (print("Return(\n");
				print(string_multiply("  ", level));
				print_exp(e, level+1);
				print(string_multiply("  ", level-1));
				print(")\n"))
	and print_exp (e,level) =
	    case e of 
		Const n => print("Const(" ^ Int.toString n ^ ")\n")
    in
	print_prog (prog,1)
    end
		 

fun parse tokens =
    let
	fun parse_func (tokens, i, name, body) =
	    case (tokens, i, name, body) of
		(Keyword("int")::tokens', 0, NONE, NONE) => parse_func(tokens', i+1, name, body)
	      | (Keyword("main")::tokens', 1, NONE, NONE) => parse_func(tokens', i+1, SOME "main", NONE)
	      | (OpenParen::tokens', 2, _, NONE) => parse_func(tokens', i+1, name, body)
	      | (Keyword("void")::tokens', 3, _, NONE) => parse_func(tokens', i+1, name, body)
	      | (CloseParen::tokens', 4, _, NONE) => parse_func(tokens', i+1, name, body)
	      | (OpenBrace::tokens', 5, _, NONE) => parse_func(tokens', i+1, name, body)
	      | (_::_, 6, _, NONE) => (let val (stm, rest) = parse_stm(tokens, 0, NONE)
						       in parse_func(rest, i+1, name, SOME stm) end)
	      | (CloseBrace::tokens', 7, SOME name, SOME body) => (Function(name, body), tokens')
	      | _ => (print("cannot parse function from given tokens");
		      raise FunctionParseError)
	and parse_stm (tokens, i, stm) =
	    case (tokens, i, stm) of
		(Keyword("return")::tokens', 0, NONE) => (let val (e, rest) = parse_exp(tokens', 0, NONE)
							  in (Statement e, rest) end)
	      | _ => (print("cannot parse statement from given tokens");
		      raise StatementParseError)
	and parse_exp (tokens, i, e) =
	    case (tokens, i, e) of
		(Constant(n)::tokens', 0, NONE) => parse_exp(tokens', i+1, SOME (Const n))
	      | (Semicolon::tokens', 1, SOME e) => (e, tokens')
	      | _ => (print("cannot parse exp from given tokens");
		      raise ExpParseError)
    in
	case tokens of
	    Keyword("int")::tokens' => (let val (func, rest) = parse_func(tokens, 0, NONE, NONE)
					in if null rest then Program func else parse rest end)
	  | _ => (print("cannot parse invalid program");
		  raise ProgramParseError)
								    
    end
end
