(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option_helper(s1 : string, s2 : string list) =
    case s2 of
	[] => []
       | x::xs' => if same_string(x, s1)
		   then all_except_option_helper(s1, xs')
		   else x :: all_except_option_helper(s1, xs')


fun contains(list : string list, value : string) =
    case list of
	[] => false
	| x::xs' => if same_string(x, value) 
		    then true
		    else contains(xs', value)



fun all_except_option(s1 : string, s2 : string list) =
    if contains(s2, s1)
    then SOME (all_except_option_helper(s1, s2))
    else NONE


fun get_substitutions1(s1 : string list list, s2 : string) =
    case s1 of
	[] => []
      | x::xs' => case all_except_option(s2, x) of
		      NONE => get_substitutions1(xs', s2)
		      | SOME l => l @ get_substitutions1(xs', s2)

fun get_substitutions2(s1 : string list list, s2 : string) = 
    let fun gs(ls, s, acc) =
	    case ls of 
		[] => acc
		| x::xs' => case all_except_option(s, x) of
				NONE => gs(xs', s, acc)
				| SOME l => gs(xs', s, l @ acc)
    in
	gs(s1, s2, [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
