(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)
(* p1 *)
fun only_capitals lst =
  List.filter (fn x => Char.isUpper (String.sub (x, 0))) lst

(* p2 *)
fun longest_string1 (lst: string list) =
  List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" lst

(* p3 *)
fun longest_string2 (lst: string list) =
  List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" lst

(* p4 *)
fun longest_string_helper f lst =
  List.foldl (fn (x, y) => if f (x, y) then x else y) "" lst

fun longest_string3 (lst: string list) =
  longest_string_helper (fn (x, y) => String.size x > String.size y) lst

fun longest_string4 (lst: string list) = 
  longest_string_helper (fn (x, y) => String.size x >= String.size y) lst


(* p5 *)
(* worse style for SML
fun longest_capitalized lst =
  longest_string1 (only_capitals lst)
*)

val longest_capitalized = longest_string1 o only_capitals


(* p6 *)
(*val rev_string = String.implode o List.rev o String.explode *)

infix |>
fun x |> f = f x

fun rev_string x = x |> String.explode |> List.rev |> String.implode

(* p7 *)
fun first_answer f lst =
  case lst of
    [] => raise NoAnswer
    | x::xs => (case f x of
              NONE => first_answer f xs
              | SOME v => v)


(* p8 *)
fun all_answers f lst =
  let
    fun helper f lst acc =
      case lst of
        [] => SOME acc
        | x::xs => (case f x of
              NONE => NONE
              | SOME v => helper f xs (acc @ v))
  in
    helper f lst []
  end


(* part 2 : using following type to solve problem *)
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
(* p9a *)
val count_wildcards = g (fn x => 1) (fn x => 0)


(* p9b *)
val count_wild_and_variable_lengths = 
  g (fn x => 1) (fn x => String.size x)  

(* p9c *)
fun count_some_var (str, p) = 
  g (fn x => 0) (fn x => if x = str then 1 else 0) p


(* p10 *)

fun check_pat p =
  let
    fun getVars p =
      case p of
          Variable x => x::[]
        | TupleP ps => List.foldl (fn (v, vs) => vs @ (getVars v)) [] ps
        | ConstructorP(_,p) => getVars p
        | _ => []
    fun check_distinct lst = 
      case lst of
        [] => true
        | x::xs => if List.exists (fn y => y = x) xs then false else check_distinct xs
  in
    check_distinct (getVars p)
  end

(* p11 *)
fun match (v, p) =
    case p of
        Variable x => SOME [(x, v)]
      | UnitP =>
        (case v of
             Unit => SOME []
           | _ => NONE)
      | Wildcard => SOME []
      | ConstP k =>
        (case v of
             Const(v) => if k = v then SOME [] else NONE
           | _ => NONE)
      | TupleP ps =>
        (case v of
             Tuple(vs) => if List.length vs = List.length ps
                          then all_answers match (ListPair.zip(vs, ps))
                          else NONE
           | _ => NONE)
      | ConstructorP(s1,pp) =>
        (case v of
             Constructor(s2,vv) =>
             if s1 = s2 then match(vv,pp) else NONE
           | _ => NONE)

(* p12 *)
fun first_match v ps =
  SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE

