(* 1 *)
fun is_older(x: int*int*int, y: int*int*int) =
  if #1 x < #1 y
  then true
  else if #1 x > #1 y
  then false
  else (if #2 x < #2 y
        then true
        else if #2 x > #2 y
        then false
        else (if #3 x < #3 y
              then true
              else false));


(* 2 *)
fun number_in_month(xs: (int*int*int) list, x: int) =
  if null xs
  then 0
  else if #2 (hd xs) = x
  then 1 + number_in_month(tl xs, x)
  else number_in_month(tl xs, x);


(* 3 *)
fun number_in_months(xs: (int*int*int) list, ys: int list) =
  if null ys
  then 0
  else number_in_month(xs, hd ys) + number_in_months(xs, tl ys);


(* 4 *)
fun dates_in_month(xs: (int*int*int) list, x: int) =
  if null xs
  then []
  else if #2 (hd xs) = x
  then hd xs :: dates_in_month(tl xs, x)
  else dates_in_month(tl xs, x);


(* 5 *)
fun dates_in_months(xs: (int*int*int) list, ys: int list) =
  if null ys
  then []
  else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys);


(* 6 *)
fun get_nth(xs: string list, n: int) =
  if n = 1
  then hd xs
  else get_nth(tl xs, n - 1);


(* 7 *)
fun date_to_string(x: int*int*int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 x) ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
  end;


(* 8 *)
(*
fun number_before_reaching_sum(x: int, xs: int list) =
  let fun index(sum: int, ys: int list) =
        if sum >= x
        then 0
        else 1 + index(sum + hd ys, tl ys)
  in
      index(0, xs) - 1
  end;
*)

fun number_before_reaching_sum(x: int, xs: int list) =
  if x <= 0
  then ~1
  else 1 + number_before_reaching_sum(x - hd xs, tl xs);


(* 9 *)
fun what_month(x: int) =
  let val int_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(x, int_months) + 1
  end;


(* 10 *)
fun month_range(x: int, y: int) =
  if x > y
  then []
  else what_month x :: month_range(x + 1, y);


(* 10 *)
fun oldest(xs: (int*int*int) list) =
  if null xs
  then NONE
  else let fun older(y, ys) =
             if null ys
             then y
             else (if is_older(y , hd ys)
                   then older(y, tl ys)
                   else older(hd ys, tl ys))
       in
           SOME (older(hd xs, tl xs))
       end;


