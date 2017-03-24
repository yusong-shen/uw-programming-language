(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


fun only_capitals xs =
  List.filter (fn f => Char.isUpper(String.sub(f,0))) xs

fun longest_string1 xs =
  (* fun g defined as helper function to make code more readable.  Not necessary to do so. *)
  let fun g (acc,x) =
      if (String.size x >= String.size acc) then x else acc
  in
      foldl g "" xs
  end

fun longest_string2 xs =
  (* fun g defined as helper function to make code more readable.  Not necessary to do so. *)
  let fun g (acc,x) =
      if (String.size x > String.size acc) then x else acc
  in
      foldl g "" xs
  end
	
fun longest_string_helper f xs =
  let fun g (acc, x) =
	if f (String.size x, String.size acc)
	then x
	else acc
  in
      foldl g "" xs
  end

val longest_string3 = fn xs =>
  longest_string_helper (fn (a,b) => a >= b) xs

val longest_string4 = fn xs =>
  longest_string_helper (fn (a,b) => a > b) xs

val longest_capitalized = fn xs =>
  (longest_string1 o only_capitals) xs

fun rev_string x =
  (String.implode o List.rev o String.explode) x
			      
fun first_answer f xs =
  case xs of	       
      [] => raise NoAnswer
    | x::xs' => (case f(x) of
		    NONE => first_answer f xs'
		  | SOME v => v)

fun all_answers f xs =
    let fun g([],[]) = SOME []
	      | g([],acc) = SOME acc
	      | g(x::xs', acc) = case f(x) of
			       NONE => NONE
			     | SOME v => g(xs', v @ acc) 
    in
	case xs of
	    [] => SOME []
	  | _  => g(xs,[])
    end

val count_wildcards = g (fn x => 1) (fn y => 0)
			
fun count_wild_and_variable_lengths pttrn =
  let val count = 0
  in
      g (fn y => count + 1) (fn z => count + String.size z) pttrn
  end
      
fun count_some_var (s, pttrn) =
  let val count = 0
  in
      g (fn y => 0) (fn z => if z = s then count + 1 else 0) pttrn
  end
      
fun check_pat pttrn =
  let fun makelist acc pttrn =
	case pttrn of
	  Variable string => string::acc
	 | TupleP (hdp::(hdtlp::tlp)) => (makelist acc hdp)@(makelist acc hdtlp)
	 | ConstructorP (_,p) => makelist acc p
	 | _ => acc 
      fun check_duplicates xs =
	case xs of
	    [] => true
	  | x::xs' => if List.exists (fn y => x=y) xs'
		      then false
		      else check_duplicates xs'
  in
     check_duplicates (makelist [] pttrn)
  end

fun match (value,pttrn) =
  let fun match_helper v p acc =
	case (v,p) of
	    (_,Variable x) => SOME ([(x,v)]@acc)
	  | (_,Wildcard) => SOME []
	  | (Unit,UnitP) => SOME []
	  | (Const int1,ConstP int2) => if int1=int2 then SOME [] else NONE
	  | (Tuple vs, TupleP ps) =>
	    let val pairlist = ListPair.zip (vs,ps)
	    in
		if ((all_answers match pairlist = NONE)
		       orelse ((List.length vs) <> (List.length ps)))
		then NONE
		else SOME (acc@(valOf (all_answers match pairlist)))
	    end
	  | (Constructor (s1,v1), ConstructorP (s2,p1)) =>
	      (if s1 = s2 then match_helper v1 p1 acc else NONE)
	  | _ => NONE
  in
      match_helper value pttrn []
  end
      	    
fun first_match v ps =
 SOME (first_answer (fn f => match(v,f)) ps)
  handle NoAnswer => NONE

