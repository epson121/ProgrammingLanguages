(* TEST EXAMPLES FOR MY SUBMISSION *)

use "hw1.sml";

(* 1 *)
is_older((1,2,3),(2,3,4)) = true;
is_older((2012, 5, 2), (2010, 3, 4)) = false;
is_older((2009, 12, 3), (2014, 8, 5)) = true;

(* 2 *)
number_in_month([(2012,2,28),(2013,12,1)],2) = 1;
number_in_month([(2012,2,28),(2013,12,1)],12) = 1;
number_in_month([(2012,2,28),(2013,12,1)],1) = 0;

(* 3 *)
number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2, 3]) = 2;
number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = 1;

(* 4 *)
dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];

(* 5 *)
dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];

(* 6 *)
get_nth(["hi", "there", "how", "are", "you"], 2) = "there";

(* 7 *)
date_to_string((2013, 6, 1)) = "June 1, 2013";

(* 8 *)
number_before_reaching_sum(10, [1,2,3,4,5]) = 3;

(* 9 *)
what_month(70) = 3;

(* 10 *)
month_range(31, 34) = [1,2,2,2];

(* 11 *)
oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);

(* 12 *)
number_in_months_challenge([(2012,2,28),(2011,3,31),(2011,4,28)], [2, 3, 4, 2, 3, 4]) = 3;
number_in_months_challenge([(2012,2,28),(2011,3,31)], [2, 3, 4, 2, 3, 4]) = 2;
number_in_months_challenge([(2012,2,28)], [2, 3, 4, 2, 3, 4]) = 1;

(* 13 *)
reasonable_date(2012, 12, 30) = true;
reasonable_date(2012, 12, 31) = true;
reasonable_date(2012, 12, 30) = true;
reasonable_date(2008, 2, 29) = true;
reasonable_date(2009, 2, 29) = false;

