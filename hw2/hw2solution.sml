(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* YS, put your solutions for problem 2 here *)
(* part 1 : name substitution *)
(* 1a : takes a string and a string list. Return NONE if the string 
 is not in the list, else return SOME lst where lst is identical to 
    the argument list except the string is not in it. *)
fun all_except_option params = 
    case params of
        (str, []) => NONE
        | (str, x::xs) => if same_string(x, str) 
                        then SOME xs 
                        else case all_except_option(str, xs) of
                            NONE => NONE
                            | SOME y => SOME(x::y)

(* 1b *)
fun get_substitutions1 params = 
    case params of
        ([], str) => []
        | (x::xs, str) => case all_except_option(str, x) of
                        NONE => get_substitutions1(xs, str)
                        | SOME y => y @ get_substitutions1(xs, str)


(* 1c : use a tail-recursive local helper function *)
fun get_substitutions2(list_of_strlist, str) = 
    let
        fun helper(xs, acc) =
            case xs of
                [] => acc
                | x::xs' => case all_except_option(str, x) of
                        NONE => helper(xs', acc)
                        | SOME y => helper(xs', acc @ y)
    in
        helper(list_of_strlist, [])
    end

(* 1d : use a tail-recursive local helper function *)
fun similar_names(lst, {first=a, middle=b, last=c}) = 
    let fun aux(substitutions, acc) =
            case substitutions of
                [] => {first=a, middle=b, last=c}::acc
                | x::xs => aux(xs, {first=x, middle=b, last=c}::acc)
        fun produce_names firsts = 
            aux(firsts, [])
    in
        produce_names(get_substitutions2(lst, a))
    end

(* without a tail-recursive local helper function*)
(*fun similar_names(lst, {first=a, middle=b, last=c}) = 
    let 
        fun produce_names substitutions = 
            case substitutions of
                [] => {first=a, middle=b, last=c}::[]
                | x::xs => produce_names xs @ ({first=x, middle=b, last=c}::[])
    in
        produce_names(get_substitutions2(lst, a))
    end *)   

(* part 2 : solitaire game *)
(* 2a *)
fun card_color card = 
    case card of
        (Clubs, _) => Black
        | (Spades, _) => Black
        | (Diamonds, _) => Red
        | (Hearts, _) => Red


(* 2b *)
fun card_value card = 
    case card of
        (_, Num x) => x
        | (_, Ace) => 11
        | (_, _) => 10


(* 2c *)
fun remove_card params =
    case params of
        ([], c, e) => raise e
        | (x::xs, c, e) => if x = c
                        then xs
                        else x::remove_card(xs, c, e)

(* 2d *)
fun all_same_color cards =
    case cards of
        [] => true
        | _::[] => true
        | head::(neck::xs) => card_color(head) = card_color(neck) andalso all_same_color(neck::xs)

(* 2e *)
fun sum_cards cs =
    let
        fun aux(cs, acc) = 
            case cs of
                [] => acc
                | x::xs => aux(xs, acc + card_value x)
    in
        aux(cs, 0)
    end

(* 2f *)
fun score (cs, goal) =
    let
        val sum = sum_cards(cs)
        val p_score = if goal > sum then goal - sum else 3 * (sum - goal)
        val final_score = if (all_same_color(cs)) then p_score div 2 else p_score
    in
        final_score
    end

(* 2g *)
fun officiate(cards, moves, goal) =
    let
        fun aux(cards, moves, helds, acc) =
            case (cards, moves, acc) of
                (_, [], acc) => acc
                | ([], _, acc) => acc
                | (c::cs, m::ms, acc) => if acc > goal 
                            then acc 
                            else (case m of
                                Draw => aux(remove_card(cards, c, IllegalMove), ms, c::helds, score(c::helds, goal))
                                | Discard(x) => aux(remove_card(cards, x, IllegalMove), ms, helds, acc))
    in
        aux(cards, moves, [], score([], goal))
    end



(* part 3 : challenge *)



