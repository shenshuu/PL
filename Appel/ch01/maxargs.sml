type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	      |AssignStm of id * exp
	      | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
	     | OpExp of exp * binop * exp
	     | EseqExp of stm * exp


fun maxargs stm =
    let
	fun maxargsStm stm =
	    case stm of
		CompoundStm(s1,s2) => Int.max(maxargsStm s1, maxargsStm s2)
	      | AssignStm(_,e) => maxargsExp e
	      | PrintStm es => List.foldl Int.max (List.length es) (List.map maxargsExp es)
	and maxargsExp e =
	    case e of
		OpExp(e1,_,e2) => Int.max(maxargsExp e1, maxargsExp e2)
	      | EseqExp(s',e') => Int.max(maxargsStm s', maxargsExp e')
	      | _ => 0
    in
	maxargsStm stm
    end

	
val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
		CompoundStm(AssignStm("b",
				      EseqExp(PrintStm [IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
					      OpExp(NumExp 10, Times, IdExp "a"))),
			    PrintStm [IdExp "b"]))
	       
val nestedPrints =
    CompoundStm(PrintStm [NumExp 1, NumExp 2], AssignStm("a",EseqExp(PrintStm [],
								   EseqExp(PrintStm [NumExp 1, NumExp 2, NumExp 3], IdExp "foo"))))
