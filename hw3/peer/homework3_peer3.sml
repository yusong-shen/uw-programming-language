exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun isUpper s = Char.isUpper(String.sub(s,0))

fun only_capitals xs = List.filter isUpper xs

fun longest_string1(xs) = 
    case xs of 
        [] => ""  
        | x::xs' => List.foldl((fn (s,x) => if String.size s > String.size x then s else x)) "" xs

fun longest_string2(xs) =
	longest_string1 (List.rev(xs))

fun longest_string_helper f = List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper op >

val longest_string4 = longest_string_helper op >=

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
  	[] => raise NoAnswer
  	| x::xs' => case f x of
  		SOME x => x
  		| NONE => first_answer f xs'
		
fun all_answers f [] = SOME []
| all_answers f xs = 
	let
		fun fold f acc ys =
			case ys of
			 [] => SOME acc
			 | _ => NONE
	in
		fold f [] xs
	end


(*Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains*)

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

val count_wildcards = g (fn y => 1) (fn x => 0)

val count_wild_and_variable_lengths = fn p => count_wildcards p + g (fn x => 0) (fn y => String.size y) p

fun count_some_var (s, p) = g (fn x => 0) (fn y => if y = s then 1 else 0) p

fun check_pat p = 
    let 
        fun fold p = 
            case p of 
                Variable x => [x]
              | TupleP tp => List.foldl (fn (pat, acc) => (fold pat) @ acc) [] tp
              | _ => []
        
        fun distinct lst acc = 
            case lst of
                [] => true
              | x :: xs => List.all (fn s => s = x) acc andalso distinct xs (x :: acc)
    in
        distinct (fold p) []
    end

(*11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines. Remember to look above for the
rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier.*)

fun match (v, p) =
    case (v, p) of 
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP pc) => if c = pc then SOME [] else NONE
      | (Tuple val_list, TupleP ps) => all_answers match (ListPair.zip(val_list, ps))
      | (Constructor(s2, v1), ConstructorP(s1,p1)) => if s1=s2 then match(v1, p1) else NONE
      | (_, _) => NONE

(*12. Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
handle-expression. Hints: Sample solution is 3 lines*)

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE(*

check_pat: Called check_pat on input: TupleP[Variable "x",Variable "x"], should have gotten: false but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"], should have gotten: false but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"], should have gotten: false but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: TupleP[ConstP 4,Wildcard,Variable "ba",TupleP[Variable "ab"]], should have gotten: true but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",Variable "x"]), should have gotten: false but your function returned otherwise. [incorrect answer]
check_pat: Called check_pat on input: ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])]), should have gotten: false but your function returned otherwise. [incorrect answer]
match: Called match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]
first_match: Called first_match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],[ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]
prob13 tests failed to run (most likely caused by an incorrect function signature in the submission)
all_answers: Called all_answers on: ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~], should have gotten: SOME([\",#,$,%,&,',(,),*,+,,,-,.,/,0,1,2,3,4,5,6,7,8,9,:,;,<,=,>,?,@,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,[,\\,],^,_,`,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,{,|,},~]) but your function returned something else [incorrect answer]
all_answers: Called all_answers on: [the,walrus,and,the,carpenter,talked,of,many,things,,of,shoes,and,ships,and,ceiling,wax,,of,Cabbages,and,Kings.], should have gotten: SOME([K,i,n,g,s,.,a,n,d,C,a,b,b,a,g,e,s,o,f,w,a,x,,,c,e,i,l,i,n,g,a,n,d,s,h,i,p,s,a,n,d,s,h,o,e,s,o,f,t,h,i,n,g,s,,,m,a,n,y,o,f,t,a,l,k,e,d,c,a,r,p,e,n,t,e,r,t,h,e,a,n,d,w,a,l,r,u,s,t,h,e]) but your function returned something else [incorrect answer]
all_answers: Called all_answers on: [this,list,has,no,capital,letters], should have gotten: SOME([l,e,t,t,e,r,s,c,a,p,i,t,a,l,n,o,h,a,s,l,i,s,t,t,h,i,s]) but your function returned something else [incorrect answer]
all_answers: Called all_answers on: [Alabama,Alaska,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Hawaii,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,NorthCarolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,southCarolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming], should have gotten: SOME([W,y,o,m,i,n,g,W,i,s,c,o,n,s,i,n,W,e,s,t, ,V,i,r,g,i,n,i,a,W,a,s,h,i,n,g,t,o,n,V,i,r,g,i,n,i,a,V,e,r,m,o,n,t,U,t,a,h,T,e,x,a,s,T,e,n,n,e,s,s,e,e,S,o,u,t,h, ,D,a,k,o,t,a,s,o,u,t,h,C,a,r,o,l,i,n,a,R,h,o,d,e, ,I,s,l,a,n,d,P,e,n,n,s,y,l,v,a,n,i,a,O,r,e,g,o,n,O,k,l,a,h,o,m,a,O,h,i,o,N,o,r,t,h, ,D,a,k,o,t,a,N,o,r,t,h,C,a,r,o,l,i,n,a,N,e,w, ,Y,o,r,k,N,e,w, ,M,e,x,i,c,o,N,e,w, ,J,e,r,s,e,y,N,e,w, ,H,a,m,p,s,h,i,r,e,N,e,v,a,d,a,N,e,b,r,a,s,k,a,M,o,n,t,a,n,a,M,i,s,s,o,u,r,i,M,i,s,s,i,s,s,i,p,p,i,M,i,n,n,e,s,o,t,a,M,i,c,h,i,g,a,n,m,a,s,s,a,c,h,u,s,e,t,t,s,M,a,r,y,l,a,n,d,M,a,i,n,e,L,o,u,i,s,i,a,n,a,K,e,n,t,u,c,k,y,K,a,n,s,a,s,I,o,w,a,I,n,d,i,a,n,a,I,l,l,i,n,o,i,s,I,d,a,h,o,H,a,w,a,i,i,G,e,o,r,g,i,a,F,l,o,r,i,d,a,D,e,l,a,w,a,r,e,C,o,n,n,e,c,t,i,c,u,t,C,o,l,o,r,a,d,o,C,a,l,i,f,o,r,n,i,a,A,r,k,a,n,s,a,s,A,r,i,z,o,n,a,A,l,a,s,k,a,A,l,a,b,a,m,a]) but your function returned something else [incorrect answer]
all_answers: Called all_answers on: ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~], should have gotten: SOME([\",#,$,%,&,',(,),*,+,,,-,.,/,0,1,2,3,4,5,6,7,8,9,:,;,<,=,>,?,@,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,[,\\,],^,_,`,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,{,|,},~]) but your function returned something else [incorrect answer]*)