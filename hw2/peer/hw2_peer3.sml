(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, sl) =
  let
    fun f(s, pre, post) =
      case post of
        [] => NONE
      | x::xs =>
        if same_string(x, s)
        then SOME(xs @ pre)
        else f(s, x::pre, xs)
  in
    f(s, [], sl)
  end

fun get_substitutions1(subs, s) = 
  case subs of
    [] => []
  | x::xs =>
    case all_except_option(s, x) of
      NONE => [] @ get_substitutions1(xs, s)
    | SOME v => v @ get_substitutions1(xs, s)

fun get_substitutions2(subs, s) =
  let
    fun f(subs, acc) =
      case subs of
        [] => acc
      | x::xs =>
        case all_except_option(s, x) of
          NONE => f(xs, acc)
        | SOME v => f(xs, v @ acc)
  in
    f(subs, [])
  end

fun similar_names (subs, {first, middle, last}) =
  let
    val firstnames = get_substitutions2(subs, first)
    fun f ([], acc) = acc
    | f (x::xs, acc) = f(xs, {first=x, middle=middle, last=last}::acc)
  in
    {first=first, middle=middle, last=last}::f(firstnames, [])
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

fun card_color (Clubs, _) = Black
| card_color (Spades, _) = Black
| card_color (_, _) = Red

fun card_value (_, Num v) = v
| card_value (_, Ace) = 11
| card_value (_, _) = 10

fun remove_card (cs, c, e) =
  let
    fun f ([], acc) = raise e
    | f (c'::cs', acc) = if c' = c then acc @ cs' else f (cs', c'::acc)
  in
    f (cs, [])
  end

fun all_same_color cs =
  let
    fun f (_, []) = true
    | f (NONE, c'::cs') = f (SOME(card_color(c')), cs')
    | f (SOME prev_color, c'::cs') = if prev_color = card_color(c')
                                then f (SOME prev_color, cs')
                                else false
  in
    f (NONE, cs)
  end

fun sum_cards (cs) =
  let
    fun f ([], acc) = acc
    | f (c'::cs', acc) = f (cs', card_value(c') + acc)
  in
    f (cs, 0)
  end

fun score (cs, goal) =
  let
    val total_sum = sum_cards(cs)
    val prelim_score = if total_sum > goal
      then 3 * (total_sum - goal)
      else goal - total_sum
  in
    if all_same_color(cs)
    then prelim_score div 2
    else prelim_score
  end

fun officiate (cs, ms, goal) =
  let
    fun f ([], _, hs) =  score(hs, goal)
    | f (_, [], hs) = score(hs, goal)
    | f (c'::cs', m'::ms', hs) =
      case m' of
        Draw =>
          if sum_cards(c'::hs) > goal
          then score(c'::hs, goal)
          else f (cs', ms', c'::hs)
      | Discard c => f (c'::cs', ms', remove_card(hs, c, IllegalMove))
  in
    f (cs, ms, [])
  end
