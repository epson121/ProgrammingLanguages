(* compute max of integer list *)

fun max(xs : int list) = 
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else 
	(* recursion call to find max on smaller list
         up to the list with 1 or 0 elements *)
	let val ans = max(tl xs)
	in
	    if hd xs > ans
	    then hd xs
	    else ans
	end

fun max1(xs : int list) = 
    if null (tl xs)
    then NONE
    else 
	(* recursion call to find max on smaller list
         up to the list with 1 or 0 elements *)
	let val ans = max1(tl xs)
	in
	    if isSome ans andalso valOf ans > hd xs
	    then ans
	    else SOME(hd xs)
	end

fun max2(xs : int list) =
    if null xs
    then NONE
    else
	let
	    fun max_nonempty(xs : int list) = 
		if null (tl xs)
		then hd xs
		else
		    let val ans = max_nonempty(tl xs)
		    in
			if hd xs > ans
			then hd xs
			else ans
		    end
	in
	    SOME (max_nonempty(xs))
	end
