(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1-a *)
fun all_except_option(str,strlist) = 
	let 
		fun aux (str, strlist, listexcept) = 
			case strlist of
				[] => NONE |
				str1::rest =>  if same_string(str, str1) 
                               then SOME (listexcept @ rest) 
                               else aux(str, rest, listexcept @ [str1])
	in
		aux(str, strlist, [])
	end

(* 1-b *)
fun get_substitutions1(substitutions, str) =
            case substitutions of
                [] => [] |
                strlist::morelists => case all_except_option(str, strlist) of 
                                        NONE => get_substitutions1(morelists, str) |
                                        SOME strlist1 => strlist1 @ get_substitutions1(morelists, str)

(* 1-c *)                                        
fun get_substitutions2(substitutions, str) =
    let 
        fun aux(combinedlist, substitutions, str) =
                case substitutions of
                [] => combinedlist |
                strlist::morelists => case all_except_option(str, strlist) of 
                                        NONE => aux(combinedlist, morelists, str) |
                                        SOME all_except_list => aux(combinedlist @ all_except_list, morelists, str)
    in
        aux([],substitutions,str)
    end

(* 1-d *)
fun similar_names(substitutions, name) = 
   let 
      val {first = f: string, middle = m: string, last = l: string} = name
      val all_subs = get_substitutions1(substitutions, f)
      fun aux(strlist, namerec) =
          let 
            val {first = f: string, middle = m: string, last = l: string} = namerec;
          in
            case strlist of 
                [] => [] |
                str::therest=> {first = str, middle = m, last = l}::aux(therest, namerec)
          end
   in  
       name::aux(all_subs , name)
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
(* 2-a *)
fun card_color( c : card )= 
    case c of
        (Spades,_) => Black |
        (Diamonds,_) => Red |
        (Hearts,_)  => Red  |
        (Clubs,_)   => Black 
(* 2-b *)            
fun card_value( c : card )= 
    case c of
        (_,Num n) => n |
        (_,Ace) => 11 |
        (_,_)  => 10  
(* 2-c *)
fun remove_card( cs, c, e )=
    case cs of
        [] => raise e|
        c1::cstail => if c1 = c then cstail else c1::remove_card(cstail, c, e )
        
(* 2-d *)
fun all_same_color( cs )=
    case cs of
        []=> true|
        c1::[] => true|
        c1::(c2::cstail)=> if card_color(c1)<>card_color(c2) then false else all_same_color(c2::cstail)
        
(* 2-e *)
fun sum_cards( cs )=
    let
        fun aux(sum, cs)=
            case cs of
                []=> sum|
                c::cstail=> aux(sum + card_value(c),cstail)
    in
        aux(0, cs)
    end
        
(* 2-f *)
fun score( cs, goal )=
    let 
        val diff = sum_cards( cs ) - goal
        val prescore = if diff>0 then 3*diff else ~diff
    in 
        if all_same_color(cs) then prescore div 2 else prescore
    end

(* 2-g *)
fun officiate ( deck, moves, goal )=
    let 
        fun play( held, deck, moves, goal )=
            case moves of
                [] => score( held, goal ) |
                Discard(c)::movestail => play( remove_card(held, c, IllegalMove), deck, movestail, goal)|
                Draw::movestail => case deck of
                                        [] => score( held, goal )|
                                        c::decktail => if sum_cards( c::held )>goal 
                                                       then score( c::held, goal )
                                                       else play( c::held, decktail, movestail, goal)
    in
        play( [], deck, moves, goal )
    end
        
            