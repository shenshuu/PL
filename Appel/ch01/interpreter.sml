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

exception notFound
	
fun lookup (i,table) =
    case table of
	[] => raise notFound
      | (i',v)::tail => if i' = i then v else lookup(i,tail)

fun getOp s =
    case s of
	Plus => op +
      | Minus => op -
      | Times => op *
      | Div => Int.div				    
	 
fun interp prog =
    let
	fun interpStm (stm,env) =
	    case stm of
		CompoundStm(s1,s2) => interpStm(s2, interpStm(s1,env))
	      | AssignStm(i,e) => let val (v, env') = interpExp(e,env)
				  in (i,v)::env' end 
	      | PrintStm exps => let
		                     fun printExp (e,env) =
		                         let val (v,env') = interpExp(e,env)
		                         in
			                   (print (Int.toString v);
			                   print " ";
			                   env')
		                         end
		                 in
		                   List.foldl printExp env exps
		                 end
	and interpExp (e,env) =
	    case e of
		IdExp i => let val v = lookup(i,env)
			   in (v, (i,v)::env) end
	      | NumExp n => (n, env)
	      | OpExp(e1,binop,e2) => let
		                           val (v1,left_env) = interpExp(e1,env)
		                           val (v2,right_env) = interpExp(e2,left_env)
	                                 in
		                           (getOp(binop)(v1,v2), right_env)
	                                 end
	      | EseqExp(stm,e) => interpExp(e, interpStm(stm,env))
    in
	interpStm(prog,[])
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

