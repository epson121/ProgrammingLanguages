(* WRITE A FUNCTION IS_OLDER THAT TAKES TWO DATES AND EVALUATES TO TRUE OR FALSE. IT EVALUATES TO TRUE IF
THE FIRST ARGUMENT IS A DATE THAT COMES BEFORE THE SECOND ARGUMENT. (IF THE TWO DATES ARE THE SAME,
THE RESULT IS FALSE. *)

fun is_older(x1 : (int * int * int), x2 : (int * int * int)) =
    if #1 x1 < #1 x2
    then true
    else 
	if #2 x1 < #2 x2 andalso #1 x1 = #1 x2
	then true
	else
	    if #3 x1 < #3 x2 andalso #1 x1 = #1 x2 andalso #2 x1 = #2 x2
	    then true
	    else
		false

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)

fun number_in_month(xs : (int * int * int) list, n : int) =
    if null xs
    then 0
    else 
	if #2 (hd xs) = n
	then 1 + number_in_month(tl xs, n)
	else 0 + number_in_month(tl xs, n)

(* Write a function number_in_months that takes a list of dates and a list of
months (i.e., an int list) and returns the number of dates in the list of dates
that are in any of the months in the list of months.Assume the list of months 
has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null (tl months)
    then number_in_month(dates, hd months)
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months) 

(* Write a function dates_in_month that takes a list of dates and a month 
(i.e., an int) and returns a list holding the dates from the argument list of 
dates that are in the month. The returned list should contain dates in the 
order they were originally given. *)

fun dates_in_month(xs : (int * int * int) list, n : int) =
    if null xs
    then []
    else 
	if #2 (hd xs) = n
	then (hd xs)::dates_in_month(tl xs, n)
	else dates_in_month(tl xs, n)

(* Write a function dates_in_months that takes a list of dates and a list of 
months (i.e., an int list) and returns a list holding the dates from the 
argument list of dates that are in any of the months in the list of months. 
Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null (tl months)
    then dates_in_month(dates, hd months)
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Write a function get_nth that takes a list of strings and an int n and 
returns the nth element of the list where the head of the list is 1st . 
Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is 
okay. *)

fun get_nth(xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

(* Write a function date_to_string that takes a date and returns a string of 
the form January 20, 2013 (for example). Use the operator ^ for concatenating 
strings and the library function Int.toString for converting an int to a 
string. For producing the month part, do not use a bunch of conditionals. 
Instead, use a list holding 12 strings and your answer to the previous problem.
For consistency, put a comma following the day and use capitalized English 
month names: January, February, March, April, May, June, July, August, 
September, October, November, December. *)

fun date_to_string(date : (int * int * int) ) =
    get_nth(["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November"], #2 date) 
             ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* Write a function number_before_reaching_sum that takes an int called sum, 
which you can assume is positive, and an int list, which you can assume 
contains all positive numbers, and returns an int. You should return an int n 
such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to 
more than the passed in value; it is okay for an exception to occur if this 
is not the case. *)

fun number_before_reaching_sum(sum : int, xs : int list) =
    let
	fun number_before_helper(count : int, counter : int, xs : int list) =
	    if (hd xs + counter) >= sum
	    then count
	    else number_before_helper(count + 1, counter + hd xs, tl xs)
    in
	number_before_helper(0, 0, xs)
    end
     
(* Write a function what_month that takes a day of year (i.e., an int between 
1 and 365) and returns what month that day is in (1 for January, 2 for 
February, etc.). Use a list holding 12 integers and your answer to the previous
problem. *)

fun what_month(day : int) =
    number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 
				     31, 30, 31, 30, 31]) + 1;

(* Write a function month_range that takes two days of the year day1 and day2 
and returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is 
the month of day1+1, ..., and mn is the month of day day2. Note the result 
will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

(* Write a function oldest that takes a list of dates and evaluates to an 
(int*int*int) option. It evaluates to NONE if the list has no dates and 
SOME d if the date d is the oldest date in the list. *)

fun oldest(xs : (int * int * int) list ) =
    if null xs
    then NONE
    else
	let
	    fun oldest_helper(xs : (int * int * int) list, date : (int * int * int)) =
		if null xs
		then true
		else 
		    if is_older(date, hd xs)
		    then oldest_helper(tl xs, date)
		    else false
	    fun oldest_helper_2(xs : (int * int * int) list) =
		if oldest_helper(tl xs, hd xs)
		then hd xs
		else oldest_helper_2(tl xs)
	in
	    SOME (oldest_helper_2(xs))
	end

(* Challenge Problem: Write functions number_in_months_challenge and 
dates_in_months_challenge that are like your solutions to problems 3 and 5 
except having a month in the second argument multiple times has no more 
effect than having it once. (Hint: Remove duplicates, then use previous 
work.) *)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append((tl xs), ys)

fun contains(list : int list, value : int) =
    if null list
    then false
    else
	if hd list = value
	then true
	else contains(tl list, value)

fun remove_duplicates(list : int list, res : int list) =
    if null list
    then res
    else 
	if contains(res, hd list)
	then remove_duplicates(tl list, res)
	else remove_duplicates(tl list, append(res, [hd list]))
    

(* this solution uses several helper functions. "append" is copied from the slides, others are 
my own work *)

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months, []))
    

(* Challenge Problem: Write a function reasonable_date that takes a date and 
determines if it describes a real date in the common era. A “real date” has a 
positive year (year 0 did not exist), a month between 1 and 12, and a day 
appropriate for the month. Solutions should properly handle leap years. Leap years 
are years that are either divisible by 400 or divisible by 4 but not divisible by 
100. (Do not worry about days possibly lost in the conversion to the Gregorian 
calendar in the Late 1500s.) *)

fun get_nth_int(xs : int list, n : int) =
    if n = 1
    then hd xs
    else get_nth_int(tl xs, n-1)

(* lot of if statements :D. I haven't thoroughly tested this, but it worked
for few of my examples. *)

fun reasonable_date(date : (int * int * int)) =
    if #1 date > 0
    then 
	if #2 date > 0 andalso #2 date < 13
	then
	    if #3 date > 0 andalso #3 date <= get_nth_int([31, 29, 31, 30, 31, 30, 
							  31, 31, 30, 31, 30, 31],
							 #2 date)
	    then 
		if #3 date = 29 andalso #2 date = 2
		then
		    if (#1 date mod 400 = 0 orelse #1 date mod 4 = 0) andalso #1 date mod 100 <> 0
		    then true
		    else false
		else true
	    else false
	else false
    else false
