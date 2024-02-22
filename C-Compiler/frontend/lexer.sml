val fileStream = TextIO.openIn "main.c"
		       
fun readAndPrintLines stream =
    case TextIO.inputLine stream of
	NONE => ()
      | SOME line => (
	  print(line ^ "\n");
	  readAndPrintLines stream
      )

fun getChars stream =
    case TextIO.inputLine stream of
	NONE => []
      | SOME line => String.explode(line)::getChars(stream)

			 
datatype Token = Identifier of string | Constant of int | OpenBrace | CloseBrace | OpenParen | CloseParen | Keyword of string | Semicolon

exception SyntaxError of string

	      
fun takeWhile pred cs =
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
	      | (Constant n)::(Identifier id)::toks' => raise SyntaxError "invalid identifier"
	      | tok1::tok2::toks' => tok1::validify_tokens(tok2::toks')
			 
	fun tokenizer (chars,acc) =
	    case chars of
		[] => List.rev acc
	      | #"\n"::cs => tokenizer(cs,acc)
	      | #" "::cs => tokenizer(cs,acc)
	      | #"{"::cs => tokenizer(cs,OpenBrace::acc)
	      | #"}"::cs => tokenizer(cs,CloseBrace::acc)
	      | #"("::cs => tokenizer(cs,OpenParen::acc)
	      | #")"::cs => tokenizer(cs,CloseParen::acc)
	      | #";"::cs => tokenizer(cs,Semicolon::acc)
	      | c::cs => if Char.isAlpha c
			 then
			     let val (tok_cs,rest) = takeWhile Char.isAlphaNum (c::cs)
				 val tok = String.implode tok_cs
			     in if List.exists (fn keyword => keyword=tok) keywords
				then tokenizer(rest, Keyword(tok)::acc)
				else tokenizer(rest, Identifier(tok)::acc)
			     end
			 else if Char.isDigit c
			 then
			     let val (tok_cs,rest) = takeWhile Char.isDigit (c::cs)
			     in case Int.fromString(String.implode tok_cs) of
				    NONE => raise SyntaxError "invalid constant"
				  | SOME n => tokenizer(rest, Constant(n)::acc)
			     end
			 else
			     raise SyntaxError "unknown syntax"		    
    in
	case TextIO.inputLine prog of
	    NONE => []
	  | SOME line => validify_tokens(tokenizer(String.explode line, [])) @ lex prog
    end
	
val tokens = lex fileStream
	
val () = TextIO.closeIn fileStream