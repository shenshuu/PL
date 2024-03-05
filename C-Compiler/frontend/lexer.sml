signature Lexer =
sig
    type Token
    val lex : TextIO.instream -> Token list
end
    
structure Lexer =
struct
						   
datatype Token = Identifier of string
	       | Constant of int | OpenBrace | CloseBrace
	       | OpenParen | CloseParen | Keyword of string | Semicolon

exception SyntaxError
exception InvalidToken
			     
fun take_while pred cs =
    let
	fun helper (acc,cs) =
	    case cs of
		[] => (List.rev acc,cs)
	      | c::cs' => if pred c
			  then helper(c::acc, cs')
			  else (List.rev acc, c::cs')
    in
	helper([],cs)
    end
	
val keywords = ["void", "int", "main", "return"]
		   
fun lex prog =
    let
	fun validify_tokens toks =
	    case toks of
		[] => []
 	      | tok::[] => tok::[]
	      | (Constant n)::(Identifier id)::toks' => (print("invalid identifier");
	      		  		  		 raise SyntaxError)
	      | tok1::tok2::toks' => tok1::validify_tokens(tok2::toks')
							  
	fun tokenizer (chars,acc) =
	    case chars of
		[] => List.rev acc
	      | #"\n"::cs => tokenizer(cs,acc)
	      | #"\t"::cs => tokenizer(cs,acc)
	      | #" "::cs => tokenizer(cs,acc)
	      | #"{"::cs => tokenizer(cs,OpenBrace::acc)
	      | #"}"::cs => tokenizer(cs,CloseBrace::acc)
	      | #"("::cs => tokenizer(cs,OpenParen::acc)
	      | #")"::cs => tokenizer(cs,CloseParen::acc)
	      | #";"::cs => tokenizer(cs,Semicolon::acc)
	      | c::cs => if Char.isAlpha c
			 then
			     let val (tok_cs,rest) = take_while Char.isAlphaNum (c::cs)
				 val tok = String.implode tok_cs
			     in if List.exists (fn keyword => keyword=tok) keywords
				then tokenizer(rest, Keyword(tok)::acc)
				else tokenizer(rest, Identifier(tok)::acc)
			     end
			 else if Char.isDigit c
			 then
			     let val (tok_cs,rest) = take_while Char.isDigit (c::cs)
			     in case Int.fromString(String.implode tok_cs) of
				    NONE => (print("invalid constant");
					     raise SyntaxError)
				  | SOME n => tokenizer(rest, Constant(n)::acc)
			     end
			 else
			     (print("unable to tokenize characters");
			      raise InvalidToken)
    in
	case TextIO.inputLine prog of
	    NONE => []
	  | SOME line => validify_tokens(tokenizer(String.explode line, [])) @ lex prog
    end
end
