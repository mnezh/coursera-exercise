(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a Write a function all_except_option, which takes a string
and a string list. Return NONE if the string is not in the list,
else return SOME lst where lst is identical to the argument list
except the string is not in it.  You may assume the string is in
the list at most once.  Use same_string , provided to you,
to compare strings.  Sample solution is around 8 lines. *)
fun all_except_option(str, lst) =
  let
    fun helper ([], true, accumulator) = SOME accumulator
    | helper ([], false, _) = NONE
    | helper (head::rest, met, accumulator) = helper(
      rest, met orelse same_string(str, head),
      if same_string(head, str) then accumulator else head::accumulator)
  in
    helper(lst, false, [])
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
