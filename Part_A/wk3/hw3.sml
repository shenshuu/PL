(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs


fun longest_string1 xs =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

	  
fun longest_string2 xs =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs


fun longest_string_helper f xs =
    foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" xs

	  
fun longest_string3 xs =
    longest_string_helper (fn (x,y) => x > y) xs

			  
fun longest_string4 xs =
    longest_string_helper (fn (x,y) => x >= y) xs


fun longest_capitalized xs =
    (longest_string1 o only_capitals) xs
			      
	
fun rev_string s =
    (implode o rev o explode) s

	
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs'
					   

fun all_answers f xs =
    let fun helper (xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => helper (xs', acc @ v)
    in helper (xs, []) end


fun count_wildcards p =
    g (fn () => 1) (fn _ => 0) p


fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p
	   

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if s=x then 1 else 0) p


fun check_pat p =
    let
	fun var_names p =
	    case p of
		Variable s => [s] 
	      | ConstructorP(_, p') => var_names p'
	      | TupleP ps => foldl (fn (p', acc) => (var_names p') @ acc) [] ps
	      | _ => []
	fun has_duplicates xs =
	    case xs of
		[] => false
	      | x::xs' => List.exists (fn y => x=y) xs' orelse has_duplicates xs'
    in
	(not o has_duplicates o var_names) p
    end


fun match (v, p) =
    case (v, p) of
	(v, Wildcard) => SOME []
      | (Const num1, ConstP num2) => if num1 = num2 then SOME [] else NONE
      | (v, Variable s) => SOME [(s, v)]
      | (Tuple vs, TupleP ps) => if length vs = length ps
				then all_answers match (ListPair.zip(vs, ps))
				else NONE
      | (Constructor(s, v'), ConstructorP(s', _)) => if s = s'
						     then SOME [(s, v')]
						     else NONE
      | (Unit, UnitP) => SOME []
      | (_, _) => NONE
		      

fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE