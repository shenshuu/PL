fun tokenizePrograms progs =
    case progs of
	p::progs' => (let val fileStream = TextIO.openIn p
		      in (lex fileStream)::tokenizePrograms(progs') end)
      | [] => []

val progs = ["end_before_expr.c", "extra_junk.c"]

val all_tokens = tokenizePrograms progs