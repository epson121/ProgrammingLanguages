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


fun similar_names(s1 : string list list, triple) =
    let fun gs(ls, triple, acc) =
	    case ls of
		[] => acc
		| xn::xs' => case triple of 
				 {first=x, middle=y, last=z} => gs(xs', triple, {first=xn,middle=y, last=z}::acc)
    in				
	case triple of
	    {first=x, middle=y, last=z} => triple::gs(get_substitutions2(s1, x), triple, [])
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

fun card_color(card) = 
    case card of 
	(suit, _) => if suit = Clubs orelse suit = Spades
		     then Black
		     else Red

fun card_value(card) =
    case card of
	(_, rank) => case rank of
		     Num x => x
		     | Ace => 11
		     | _ => 10

fun remove_card(cs : card list, c : card, exc) =
    case cs of
	[] => raise exc
     | x::xs' => if x = c then xs' else x::remove_card(xs', c, exc) 

fun all_same_color(cs : card list) =
    case cs of
	[] => true
      | _::[] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color (neck::rest))


fun sum_cards(cs : card list) =
    let fun summator(cl, acc) =
	    case cl of
		[] => acc
		| x::xs' => summator(xs', acc+card_value(x))
    in
	summator(cs, 0)
    end

fun cal_score(cs : card list, goal : int) =
    case sum_cards(cs) > goal of
	true => (3 * (sum_cards(cs) - goal))
      | false => (goal - sum_cards(cs))

fun score(cs : card list, goal : int) =
    case all_same_color(cs) of
	true =>  cal_score(cs, goal) div 2
     | false =>  cal_score(cs, goal)

fun officiate(cards : card list, moves : move list, goal : int) =
    let fun player(cards, moves, held_list, goal) =
	    if score(held_list, goal) > goal
	    then score(held_list, goal)
	    else
		case moves of
		    [] => score(held_list, goal)
		 |  x::xs' => case x of
				  Discard c => player(cards, xs', remove_card(held_list, c, IllegalMove), goal)
				| Draw => case cards of
					      [] => score(held_list, goal)
					    | c::cs' => player(cs', xs', c::held_list, goal) 
    in
	player(cards, moves, [], goal)
    end
