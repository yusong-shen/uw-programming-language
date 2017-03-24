fun is_older (first : (int * int * int), second : (int * int * int)) =
  if #1 first > #1 second
  then false
  else if #1 first < #1 second
  then true
  else if #2 first > #2 second
  then false
  else if #2 first < #2 second
  then true
  else if #3 first >= #3 second
  then false
  else true

fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if (#2 date = month)
  then 1 + number_in_months(tl dates, month)
  else number_in_months(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
  if months = []
  then 0
  else
      number_in_month(dates, hd months) +
      number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if (#2 (hd dates) = month)
  then (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if months = []
  then []
  else
      dates_in_month(dates, hd months) @
      dates_in_months(dates, tl months)

fun get_nth (strings : string list, n : int) =
  if (n = 1)
  then hd strings
  else get_nth (tl strings, n - 1)

fun date_to_string (date : (int * int * int)) =
  let
      val months_name = ["January", "February", "March", "April",
                         "May", "June", "July", "August",
                         "September","October", "November", "December"]
  in
      get_nth (months_name, #2 date) ^
      " " ^
      Int.toString (#3 date) ^
      ", " ^
      Int.toString (#1 date)
  end

fun number_before_reaching_sum (sum : int, numbers : int list) =
  if hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - hd lst, tl lst)

fun what_month(day : int) =
  let
      val months_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, months_days) + 1
  end

fun month_range (day1 :int, day2 : int) =
  let
      fun month_range_helper (this_day : int) =
        if this_day > day2
        then []
        else what_month(this_day) ::
             month_range_helper(this_day + 1)
  in
      month_range_helper (day1)
  end

fun oldest (dates : (int * int * int) list) =
  let
      fun oldest_helper (oldest_now : (int * int * int),
                         remain_dates: (int * int * int) list) =
        if remain_dates = []
        then SOME(oldest_now)
        else if is_older(oldest_now, hd remain_dates)
        then oldest_helper(oldest_now,
                           tl remain_dates)
        else oldest_helper(hd remain_dates,
                           tl remain_dates)
  in
      if dates = []
      then NONE
      else oldest_helper(hd dates, tl dates)
  end


fun remove_duplicates(ints : int lists) =
  if null ints
  then
      []
  else
      let
          (* Check if x is duplicates with elements in remain_elements *)
          fun duplicated (x : int, remain_elements : int list) =
            not null remain_elements
            andalso (x = hd remain_elements orelse (duplicated(x, tl remain_elements)))

          val removed_tl = remove_duplicates(tl ints)
      in
          if duplicated(hd xs, tl_ans)
          then tl_ans
          else (hd xs)::tl_ans
      end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates months)

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates months)

fun reasonable_date(date : (int * int * int)) =
  let
      val leap  = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
      val feb_len = if leap then 29 else 28
      val months_days = [31, feb_len, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      fun get_nth_num (n : int, lst : int list) =
        if n=1
        then hd lst
        else get_nth(tl lst, n-1)
  in
      year > 0
      andalso month >= 1
      andalso month <= 12
      andalso day >= 1
      andalso day <= get_nth_num(lengths, month)
  end
