(* Dan Grossman, Coursera Programming A, homework #2, "hw2.sml" *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1. build up to, in part (d), using firstname substitutions to come up with alt names. *)

(* (a) return NONE if string not in string list, else return SOME lst that omits given string *)
fun all_except_option (s: string, sL: string list) =
  let fun remove_string(s_remove, sL_remainder) =
	  case sL_remainder of
	      [] => []
	   |  x :: x' => if same_string(x, s_remove) then x' else
	                 x :: remove_string(s_remove, x')
      val fsL = remove_string(s, sL)
  in
      if fsL = sL then NONE else SOME fsL
  end

(* (b) input sLL, and s, return sL, contains all synonyms for s, but not s itself *)
fun get_substitutions1 (substitutions: string list list, s: string) =
  case substitutions of
      [] => []
    | x :: x' => let val fsL = all_except_option(s,x) in
		     case fsL of
			 NONE => get_substitutions1(x', s)
		       | SOME sL => sL @ get_substitutions1(x', s)
		 end

(* (c) same as previous, but using a tail-recursive local helper fn *)
fun get_substitutions2(substitutions: string list list, s: string) =
  let fun get_substitutions (sLL: string list list, matches: string list) =
	case sLL of
	    [] => matches
	  | x :: x' => let val fsL = all_except_option(s, x) in
			   case fsL of
			       NONE => get_substitutions(x', matches)
			     | SOME sL => get_substitutions(x', matches @ sL)
		       end
  in
      get_substitutions(substitutions, [])
  end

(* (d) take string list list of substitutions (as in prev 2, (b) and (c) ) and a fullname 
of type {first: string, middle: string, last: string} and return list of fullnames,
beginning with original fullname, followed by all fullnames with substitions of first.
Allow duplicates. Hint: Use local helper fn.  *)
fun similar_names (substitutions: string list list, {first=first, middle=middle, last=last}) =
  let val firstnames = first :: get_substitutions2(substitutions, first);
      fun helper (firstnameL: string list) =
	case firstnameL of
	    [] => []
	  | x :: x' => {first = x, middle = middle, last = last} :: helper(x')
  in
      helper(firstnames)
  end
      
(* 2. you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* (a) take a card, return its color, with spades & clubs = black, diamond & hearts = red
       Note: One case expression is enough.  *)
fun card_color (card_suit, _) =
  case card_suit of
      Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red


(* (b) take a card, return its value, with ace=11, other faces=10
     Note: One case expression is enough *)
fun card_value(_, card_rank) =
  case card_rank of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num value => value

(* (c) take a list of cards cs, a card c, and an exception e. Return a list having
       all elements of cs except for the first instance of c.  If c is absent, then 
       raise exception e.  Compare cards with =.  *)
fun remove_card (cs: card list, c: card, e) =
	let fun filter_card_list (card_list: card list) =
	    case card_list of
		  [] => []
	       |  x ::x' => if x = c then x' else
			    x :: filter_card_list(x')
	    val filtered_card_list = filter_card_list(cs)
	in
	    if filtered_card_list = cs then raise e else filtered_card_list
	end


(* (d) take a list of cards and return true if all cards in list are same color. 
      Hint: similar to a fun in the lecture that used patter-matching  *)
fun all_same_color (cards: card list) =
  case cards of
      [] => true
    | first :: [] => true
    | first :: second :: remaining => if card_color(first) = card_color(second) then
					  all_same_color(second :: remaining) else
				      false

(* (e) take a list of cards, return sum of their values. Use local helper fun that is tail recursive. *)
fun sum_cards (card_list: card list) =
  let fun sum_remaining (remaining_list: card list, sum: int) =
	case remaining_list of
	    [] => sum
	 |  x :: x' => sum_remaining(x', sum + card_value(x))
  in
      sum_remaining(card_list, 0)
  end

(* (f) take a card list (held cards) and an int (goal) and compute score as in (e)  *)
fun score (held_cards: card list, goal: int) =
  let val sum = sum_cards(held_cards)
      val temp_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
      val same_color = all_same_color(held_cards)
  in
      if same_color then temp_score div 2 else temp_score
  end

(* (g) Create fun to "run a game." take card list and a move list (what the player
     "does" at each point) and an int (goal).  Return the final score at the end 
   of the game, after processing (some of all of) the moves in the move list, in order.
   Use a local recursive helper fun, taking several args that together represent the
   "current state" of the game.
   * game starts with held_cards being empty list.
   * game ends if no more moves (i.e. payer chose to stop since move list is empty.)
   * If player discards card c, play continues (i.e. recurse) with the held_cards
      not incuding c, and the card list unchanged. If c isn't in held_cards, then raise
      the IllegalMove exception.
   * If player draws and card_list is already empty, game over!  Else if drawing causes
     the sum of held_cards to exceed goal, game over (after drawing). Else play continues
    with a larger held_cardsf, and a smaller card_list.
Sample solution is under 20 lines.  *)
fun officiate (card_list: card list, move_list: move list, goal: int) =
  let fun play (held_cards: card list, moves: move list, remaining_cards: card list) =
	case moves of
	    [] => score(held_cards, goal)
	  | x :: x' => case x of
			   Discard card => play(remove_card(held_cards, card, IllegalMove), x', remaining_cards)
			 | Draw => case remaining_cards of
				       [] => score(held_cards, goal)
				     | y :: y' => let val new_held_cards = y :: held_cards in
						      if sum_cards(new_held_cards) > goal then
							  score(new_held_cards, goal) else
						      play(new_held_cards, x', y')
						  end
  in
      play([], move_list, card_list)
  end
      
