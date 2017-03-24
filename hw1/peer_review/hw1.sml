fun is_older(a: int*int*int, b: int*int*int) =
  if (#1a <> #1b)
  then #1a < #1b
  else if (#2 a <> #2 b)
  then #2a < #2b
  else #3a < #3b

fun number_in_month(dates: (int*int*int) list, month: int) =
  if(null dates)
  then 0
  else if (#2(hd dates) = month)
  then 1 + number_in_month (tl dates, month)
  else number_in_month(tl dates, month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
  if(null months)
  then 0
  else
  number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
  if(null dates)
  then []
  else if (#2(hd dates) = month)
  then hd dates :: dates_in_month (tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if(null months)
  then []
  else
  dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (xs: string list, nth: int) =
  if nth = 1
  then hd xs
  else get_nth(tl xs, nth -1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date: int*int*int) =
  get_nth(months, #2 date) ^ " " ^  Int.toString(#3 date) ^ ", "  ^  Int.toString(#1 date)

fun number_before_reaching_sum (sum: int, xs: int list) =
  if (sum - hd xs) <= 0
  then 0
  else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month (day: int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, month_days)+1
  end

fun month_range (day1: int, day2: int) =
  if(day1 > day2)
  then []
  else what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates: (int*int*int) list) =
  if null dates
  then NONE
  else
    let
      fun oldest_nonempty(dates: (int*int*int) list) =
        if null (tl dates)
        then hd dates
        else
          let val oldest_date = oldest_nonempty(tl dates)
          in
            if is_older(oldest_date, hd dates)
            then oldest_date
            else hd dates
          end
    in
      SOME (oldest_nonempty dates)
    end

val leap_year_month_days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val normal_year_month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun is_leap_year(year: int) =
  (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 > 0)

fun get_int_nth (xs: int list, nth: int) =
  if(null xs orelse nth < 0)
  then 0
  else if nth = 1
  then hd xs
  else get_int_nth(tl xs, nth -1)

fun reasonable_date(date: (int*int*int)) =
  if (#1 date < 1) orelse (#2 date < 1) orelse (#2 date > 12) orelse (#3 date < 0)
  then false
  else if is_leap_year(#1 date)
  then #3 date <= get_int_nth(leap_year_month_days, #2 date)
  else #3 date <= get_int_nth(normal_year_month_days, #2 date)
