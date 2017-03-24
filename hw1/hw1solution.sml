
(* uw PL hw1 solution*)

(* p1 *)
fun is_older (date1 : (int*int*int), date2 : (int*int*int)) =
  if #1 date1 > #1 date2
  then false
  else if #1 date1 = #1 date2 andalso #2 date1 >  #2 date2
  then false
  else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 >= #3 date2
  then false
  else true

	   
(* p2 *)
fun number_in_month (xs : (int*int*int) list, month : int) =
  if null xs
  then 0
  else if (#2 (hd xs)) = month
  then 1 + number_in_month(tl xs, month)
  else  number_in_month(tl xs, month)

		       
(* p3 *)
fun number_in_months (xs : (int*int*int) list, months : int list) =
  if null xs orelse null months
  then 0
  else
      let val hd_ans = number_in_month(xs, hd months)
      in
	  if null (tl months)
	  then hd_ans
	  else hd_ans + number_in_months(xs, tl months)
      end

(* p4 *)							
fun dates_in_month (xs : (int*int*int) list, month : int) =
  if null xs
  then []
  else if (#2 (hd xs)) = month
  then (hd xs)::dates_in_month(tl xs, month)
  else dates_in_month(tl xs, month)


fun append (xs : (int*int*int) list, ys : (int*int*int) list) =
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)		     
		     
(* p5 *)
fun dates_in_months  (xs : (int*int*int) list, months : int list) =
  if null xs orelse null months
  then []
  else if null (tl months)
  then dates_in_month(xs, hd months)
  else append(dates_in_month(xs, hd months), dates_in_months(xs, tl months))


(* p6 *)						     
fun get_nth (xs : string list, index : int) =
  if index = 1
  then hd xs
  else get_nth (tl xs, index - 1)
	       
(* p7 *)
fun date_to_string (date : (int*int*int)) =
  let val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(month_list, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end
      

(* p8 *)      
fun number_before_reaching_sum (sum : int, xs : int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)

				     
(* p9 *)
fun what_month (day : int) =
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, days_in_months) + 1
  end
      				     

(* p10 *)      
fun month_range (day1 : int, day2 : int) =       
  if day2 < day1
  then []
  else what_month(day1)::month_range(day1 + 1, day2)


fun older (date1 : (int*int*int), date2 : (int*int*int)) =
  if is_older(date1, date2)
  then date1
  else date2
				    
(* p11 *)
fun oldest(xs : (int*int*int) list) =
  if null xs
  then NONE
  else
      let		     
	  fun oldest_nonempty (xs : (int*int*int) list) =
	    if null (tl xs)
	    then hd xs
	    else older(hd xs, oldest_nonempty(tl xs))
      in
	  SOME (oldest_nonempty(xs))
      end
	   


		       
