(* Dan Grossman, PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (s, xs) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(s,x)
		  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE => NONE
			 | SOME tl => SOME(x::tl)

					  
fun get_substitutions1 (xs, s) =
    case xs of
	[] => []
      | x::xs' => case all_except_option(s, x) of
		      NONE => []
		    | SOME lst => lst @ get_substitutions1(xs', s)
							  
    
fun get_substitutions2 (xs, s) =
    let fun helper (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => case all_except_option(s, x) of
			      NONE => helper(xs', acc)
			    | SOME tl => helper(xs', acc @ tl)
    in helper(xs, []) end
	

fun similar_names (xs, {first=x, middle=y, last=z}) =
    let val names = get_substitutions2(xs, x)
	fun helper xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=y, last=z}::helper(xs')
    in {first=x,middle=y,last=z}::helper(names) end
	

	