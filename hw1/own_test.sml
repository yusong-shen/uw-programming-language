
val test1 = is_older ((1,2,3),(2,3,4)) = true;
val test1_1 = is_older ((2012,2,28),(2011,3,31)) = false;
val test1_2 = is_older ((2011,3,31),(2011,4,28)) = true;

val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val test2_2 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0;
val test2_3 = number_in_month ([(2012,2,28),(2013,12,1), (2014,2,3)],2) = 2;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0;
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5]) = 0;
val test3_3 = number_in_months ([],[5]) = 0;


val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];
val test4_1 = dates_in_month ([],2) = [];
val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1)],3) = [];
val test4_3 = dates_in_month ([(2012,2,28),(2012,2,27),(2013,12,1)],2) = [(2012,2,28), (2012,2,27)];

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];
val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5, 6]) = [];
val test5_2 = dates_in_months ([],[5]) = [];
val test5_3 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = [(2012,2,28)];
val test5_4 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = [];

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_val = number_before_reaching_sum (10, [1,2,3,4,5])
val test8_1 = number_before_reaching_sum (10, [1,7,13,15,19]) = 2
val test8_1_val = number_before_reaching_sum (10, [1,7,13,15,19])

val test9 = what_month 70 = 3
val test9_val = what_month 70
val test9_2 = what_month 365 = 12
val test9_2_val = what_month 365

val test10 = month_range (31, 34) = [1,2,2,2]
val test10_1 = month_range (31, 28) = []
val test10_2 = month_range (31, 31) = [1]

val test11_01 = older((2012,2,28),(2011,3,31)) = (2011,3,31)
val test11_02 = older((2011,3,31),(2011,4,28)) = (2011,3,31)
val test11_03 = older((2012,2,28),(2011,4,28)) = (2011,4,28)
val test11_04 = older((2011,4,28),(2012,2,28)) = (2011,4,28)

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_val = oldest([(2012,2,28),(2011,3,31),(2011,4,28)])
val test11_1 = oldest([(2012,2,28)]) = SOME (2012,2,28)
val test11_2 = oldest([]) = NONE
val test11_3 = oldest([(2011,3,31),(2011,4,28),(2012,2,28)]) = SOME (2011,3,31)
val test11_3_val = oldest([(2011,3,31),(2011,4,28),(2012,2,28)])
