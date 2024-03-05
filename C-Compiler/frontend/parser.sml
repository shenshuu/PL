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

fun parse_error_handler (parse_fn_name, xs) =
    let
	val error_msg =  List.foldl (fn (x,acc) => acc ^ " " ^ x) "" xs
    in
	"cannot parse " ^ parse_fn_name  ^ " from: " ^ error_msg
    end
		 

fun parse tokens =
    let
	fun parse_func (tokens, info, i, name, body) =
	    case (tokens, i) of
		(Keyword("int")::tokens', 0) => parse_func(tokens', "int"::info, i+1, name, body)
	      | (Keyword("main")::tokens', 1) => parse_func(tokens', "main"::info, i+1, SOME "main", NONE)
	      | (OpenParen::tokens', 2) => parse_func(tokens', "("::info, i+1, name, body)
	      | (Keyword("void")::tokens', 3) => parse_func(tokens', "void"::info, i+1, name, body)
	      | (CloseParen::tokens', 4) => parse_func(tokens', ")"::info, i+1, name, body)
	      | (OpenBrace::tokens', 5) => parse_func(tokens', "{"::info, i+1, name, body)
	      | (tok::_, 6) => (let val (stm, rest) = parse_stm(tokens, [Lexer.tok_to_string tok], 0, NONE)
			      in parse_func(rest, info, i+1, name, SOME stm) end)
	      | (CloseBrace::tokens', 7) => (case (name, body) of
						 (SOME name', SOME body') => (Function(name', body'), tokens')
					       | _ => (print("cannot parse function with missing info");
						       raise FunctionParseError))
	      | _ => (print(parse_error_handler("function", List.rev info));
		      raise FunctionParseError)
	and parse_stm (tokens, info, i, stm) =
	    case (tokens, i) of
		(Keyword("return")::tokens', 0) => (let val (e, rest) = parse_exp(tokens', ["return"], 0, NONE)
						    in (Statement e, rest) end)
	      | _ => (print(parse_error_handler("statement", List.rev info));
		      raise StatementParseError)
	and parse_exp (tokens, info, i, e) =
	    case (tokens, i, e) of
		(Constant(n)::tokens', 0, NONE) => parse_exp(tokens', (Int.toString n)::info, i+1, SOME (Const n))
	      | (Semicolon::tokens', 1, SOME e) => (e, tokens')
	      | _ =>  (print(parse_error_handler("exp", List.rev info));
		      raise ExpParseError)
    in
	case tokens of
	    Keyword("int")::tokens' => (let val (func, rest) = parse_func(tokens, [], 0, NONE, NONE)
					in if null rest then Program func else parse rest end)
	  | _ => (print(parse_error_handler("program", List.map Lexer.tok_to_string tokens));
		  raise ProgramParseError)
								    
    end
end
