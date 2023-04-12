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
	

fun card_color c =
    case c of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | _ => Red
	

fun card_value c =
    case c of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10


fun remove_card (cs, c, e) =
    let fun exists cs =
	    case cs of
		[] => false
	      | x::cs' => c=x orelse exists cs'
    in
	if exists cs
	then case cs of
		 [] => []
	       | x::cs' => if c=x
			   then cs'
			   else x::remove_card(cs', c, e)
	else raise e
    end
	
				     

fun all_same_color cs =
    case cs of
	[] => true
      | c::[] => true
      | head::neck::tail => card_color(head) = card_color(neck)
			    andalso all_same_color tail
						   

fun sum_cards cs =
    let fun helper (cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => helper(cs', acc+card_value(c))
    in helper(cs, 0) end
	

fun score (cs, goal) =
    let val sum = sum_cards cs
	val all_same = all_same_color cs
    in
	case (sum > goal, all_same) of
	    (true, true) => (3 * (sum - goal)) div 2
	  | (true, false) => 3 * (sum - goal)
	  | (false, true) => (goal - sum) div 2
	  | _ => goal - sum
    end
	
fun officiate (cs, ms, goal) =
    let fun helper (held, cs, ms) =
	    case ms of
		[] => score(held, goal)
	      | m::ms' => case m of
			      Discard c => helper(remove_card(held, c, IllegalMove), cs, ms')
			    | Draw => case cs of
					  [] => score(held, goal)     
					| c::cs' => if sum_cards(held) + card_value(c) > goal
						    then score(c::held, goal)
						    else helper (c::held, cs', ms')
    in 
        helper([], cs, ms) 
    end