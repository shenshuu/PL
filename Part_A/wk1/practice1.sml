fun alternate (xs : int list) =
    let fun helper (xs : int list, i : int) =
	    if null xs
	    then 0
	    else (if i mod 2 = 0
		  then hd xs
		  else ~(hd xs)) + helper(tl xs, i+1)
    in helper(xs, 0)
    end

fun min_or_max (xs : int list, f) =
    if null xs
    then NONE
    else
	let fun max_nonempty (xs : int list) =
		if null (tl xs)
		then hd xs
		else
		    let val tl_ans = max_nonempty(tl xs)
		    in if f(hd xs, tl_ans)
		       then hd xs
		       else tl_ans
		    end
	in SOME(max_nonempty(xs))
	end

fun is_greater (x : int, y : int) =
    x > y

fun is_less (x : int, y : int) =
    x < y
	    
fun min_max (xs : int list) =
    (valOf(min_or_max(xs, is_less)), valOf(min_or_max(xs, is_greater)))
	
