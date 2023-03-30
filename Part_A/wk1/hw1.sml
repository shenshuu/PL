fun is_older (d1 : int * int * int, d2 : int * int * int) =
    (#1 d1) < (#1 d2) orelse
    (#1 d1) = (#1 d2) andalso (#2 d1) < (#2 d2) orelse
    (#1 d1) = (#1 d2) andalso (#2 d1) = (#2 d2) andalso (#3 d1) < (#3 d2)

								      
fun number_in_month (ds : (int * int * int) list, m : int) =
    if null ds
    then 0
    else if (#2 (hd ds)) = m
    then 1 + number_in_month (tl ds, m)
    else number_in_month (tl ds, m)

			 
fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

						      
fun dates_in_month (ds : (int * int * int) list, m : int) =
    if null ds
    then []
    else if (#2 (hd ds)) = m
    then (hd ds)::dates_in_month(tl ds, m)
    else dates_in_month(tl ds, m)

		       
fun dates_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

						    
fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

		
fun date_to_string (d : int * int * int) =
    let val months = [
	    "January", "February", "March", "April",
	    "May", "June", "July", "August", "September",
	    "October", "November", "December"
	]
    in
	get_nth(months, (#2 d)) ^ " " ^ Int.toString((#3 d)) ^ ", " ^ Int.toString((#1 d))
    end

	
fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum <= hd xs
    then 0
    else 1 + number_before_reaching_sum(sum-(hd xs), tl xs)

				       
fun what_month (n : int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(n, days)+1 end 

	
fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else what_month(d1)::month_range(d1+1, d2)

				    
fun oldest (ds : (int * int * int) list) =
    if null ds
    then NONE
    else
	let fun nonempty_oldest (ds : (int * int * int) list) =
		if null (tl ds)
		then hd ds
		else
		    let val tl_ans = nonempty_oldest(tl ds)
		    in
			if is_older(hd ds, tl_ans)
			then hd ds
			else tl_ans
		    end
	in SOME(nonempty_oldest(ds)) end

	    
fun exists (xs : int list, x : int) =
    if null xs
    then false
    else (hd xs = x) orelse exists(tl xs, x)

				  
fun remove_duplicates (xs : int list) =
    if null xs
    then []
    else
	if exists(tl xs, hd xs)
	then remove_duplicates(tl xs)
	else (hd xs)::remove_duplicates(tl xs)
	      
	
fun number_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    number_in_months(ds, remove_duplicates(ms))

		    
fun dates_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    dates_in_months(ds, remove_duplicates(ms))

		   
fun reasonable_date (d : int * int * int) =
    let fun is_leap_year () =
	    (#1 d) mod 4 = 0 andalso (#1 d) mod 100 <> 0
	    orelse (#1 d) mod 400 = 0
	fun valid_day_in_month () =
	    if is_leap_year()
	    then (#2 d) = 2 andalso (#3 d) <= 29
	    else (#2 d) = 2 andalso (#3 d) <= 28
		 orelse exists([4,6,9,11], (#2 d)) andalso (#3 d) <= 30									 
		 orelse exists([1,3,5,7,8,10,12], (#2 d)) andalso (#3 d) <= 31

    in
	(#1 d) > 0 andalso (#2 d) > 0 andalso (#3 d) > 0
	andalso (#2 d) <= 12 andalso valid_day_in_month()
    end