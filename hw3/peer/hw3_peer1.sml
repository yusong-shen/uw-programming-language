fun only_capitals (st_list : string list) =
  List.filter (fn (st : string) => Char.isUpper (String.sub (st, 0))) st_list
	     
fun fold (f, acc, xs) =
 case xs of
	[] => acc
	| x::xs' => fold (f,f(acc,x),xs')
							     
fun longest_string1 (st_list : string list) =
  fold ((fn (st1 : string, st2 : string ) =>
	    if String.size (st1) >= String.size(st2)
	    then st1 else st2), "",  st_list)

fun longest_string2 (st_list : string list) =
  fold ((fn (st1 : string, st2 : string ) =>
	     if String.size (st1) > String.size(st2)
	     then st1 else st2), "",  st_list)

fun longest_string_helper g st_list =
  fold(fn (st1,st2) => if g(String.size(st1),String.size(st2)) then st1 else st2 , "", st_list) 

val longest_string3 =
    longest_string_helper (fn (st1,st2) => st1-st2>=0)

val longest_string4 =
    longest_string_helper (fn (st1,st2) => st1-st2>0)

val  longest_capitalized  = longest_string1 o only_capitals

val rev_string = implode o rev o explode

exception NoAnswer
				     
fun first_answer g xs =
  case xs of
      [] => raise  NoAnswer
    | x::xs' => case g(x) of
		    SOME b => b
		 | _ => first_answer g xs' 



fun all_answers g xs =
   case xs of
      [] => SOME []
    | x::xs' => case g(x) of
		    NONE => NONE
		 |  SOME lst =>  case all_answers g xs' of
				  SOME lst2 => SOME (lst@lst2)
				  | NONE  => NONE

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
  
val count_wildcards =  g (fn(x)=>1) (fn(x)=>0)

val count_wild_and_variable_lengths = g (fn(x)=>1) (fn(x) => String.size(x))

fun count_some_var (s,p) = g (fn(x)=>0) (fn(x)=> if  x = s then 1 else 0 ) p

fun check_pat p =
  let
      fun gen_list r = case r of
			   Variable x => x ::[]
			 | TupleP ps => List.foldl (fn(p,l)=>l @ gen_list p) [] ps
		         | ConstructorP (s1,p) => gen_list(p) 
			 | _ => []
      fun count_rep l = case l of
			    [] => true
                 | x::xs' => if List.exists (fn(y)=>x=y) xs' then false
			     else count_rep xs'
  in
count_rep(gen_list p)
  end
      
fun match (v, p) =
  case (v, p) of
     (_, Wildcard) => SOME []
   | (_, Variable x) => SOME [(x,v)]
   | (Unit, UnitP) => SOME []
   | (Const n, ConstP m) => if n=m then SOME [] else NONE
   | (Tuple vs, TupleP ps) =>  if List.length(vs) = List.length(ps) then all_answers (fn (x,y)=> match(x,y))(ListPair.zip(vs, ps)) else NONE
  | (Constructor (s2,v), ConstructorP (s1,p)) => if s1 = s2 then match(v,p) else NONE 
  | _ => NONE 

fun first_match v ps =
   (case first_answer (fn (x) => match(v, x)) ps of
       lst => SOME lst)
handle NoAnswer => NONE

