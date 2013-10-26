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



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(l : string list) =
     List.filter(fn x => Char.isUpper(String.sub(x, 0))) l;

fun longest_string1(l : string list) = 
    List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" l;
	   
fun longest_string2(l : string list) = 
    List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" l;

fun longest_string_helper f = 
    fn l => List.foldl(fn (x, y) => if f(String.size(x), String.size(y)) then x else y) "" l;

val longest_string3 = fn l => longest_string_helper (fn (x, y) => x > y) l 

val longest_string4 = fn l => longest_string_helper (fn (x, y) => x >= y) l 

val longest_capitalized = fn l => (longest_string1 o only_capitals ) l

fun rev_string (s : string) =
    (String.implode o List.rev o String.explode) s

fun first_answer f = fn l =>
			let fun check_elements(list) =
				  case list of
				      [] => raise NoAnswer
				   | x::xs' => case f(x) of
						   SOME y => y 
						 | _ => check_elements(xs')
			in 
			    check_elements(l)
			end


fun all_answers f = fn l =>
			let fun check_elements(list, acc) =
				  case list of
				      [] => SOME acc
				   | x::xs' => case f(x) of
						   NONE  => NONE
						 | SOME _ => check_elements(xs', x::acc)
			in  
			    check_elements(l, [])
			end


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

fun count_wildcards pattern = 
    g (fn x => 1) (fn y => 0) pattern

fun count_wild_and_variable_lengths pattern =
    g (fn x => 1) (fn y => String.size(y)) pattern

fun count_some_var(s : string, p : pattern) =
    g (fn x => 0) (fn y => if s = y then 1 else 0) p


fun h p =
    let 
	val r = h
    in
	case p of
	    Variable x        => [x]
	  | TupleP ps         => List.foldl(fn(p, i) => (r p) @ i) [] ps
	  | ConstructorP(_,p) => r p
	  | _                 => []
    end

fun check_if_unique(list : string list) =
    if null (tl list)
    then true
    else 
	if List.exists (fn x => x = hd list) (tl list)
	then false
	else check_if_unique(tl list)

fun check_pat pattern =
    check_if_unique(h pattern)

(* fun match(v : valu, p : pattern) = *)
