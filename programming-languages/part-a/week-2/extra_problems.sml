(*
Write a function alternate : int list -> int\verb|alternate : int list -> int|alternate:intlist->int
that takes a list of numbers and adds them with alternating sign.
For example alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2\verb|alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2|alternate[1,2,3,4]=1-2+3-4=-2.
*)

fun alternate (numbers: int list) =
  let
    fun inner(alternator, numbers) =
      if null numbers then 0
      else alternator * hd numbers + inner(~alternator, tl numbers)
  in
    inner(1, numbers)
  end

val alternate_test = alternate([1,2,3,4]) = ~2

(* 2. Write a function min_max : int list -> int * int\verb|min_max : int list -> int * int|min_max:intlist->int*int
 that takes a non-empty list of numbers, and returns a pair (min, max)\verb|(min, max)|(min,max)
 of the minimum and maximum of the numbers in the list.
*)
fun min_max (numbers: int list) =
  if null (tl numbers) then (hd numbers, hd numbers)
  else let
    val (tailMin, tailMax) = min_max(tl numbers)
  in
    (Int.min(tailMin, hd numbers), Int.max(tailMax, hd numbers))
  end

val min_max_test = min_max([1,2,3,5,5,6,3,2,0]) = (0,6)

(*
3. Write a function cumsum : int list -> int list\verb|cumsum : int list -> int list|cumsum:intlist->intlist
that takes a list of numbers and returns a list of the partial sums of those numbers.
For example cumsum [1,4,20] = [1,5,25]\verb|cumsum [1,4,20] = [1,5,25]|cumsum[1,4,20]=[1,5,25].
*)
fun cumsum (numbers: int list) =
  let
    fun inner(sum, numbers) =
      let val local_sum = sum + hd numbers in
        if null (tl numbers)
        then [local_sum]
        else local_sum :: inner(local_sum, tl numbers)
      end
  in
    inner(0, numbers)
  end

val cumsum_test = cumsum([1,4,20]) = [1,5,25]

(*
4. Write a function greeting : string option -> string\verb|greeting : string option -> string|greeting:stringoption->string
that given a string option SOME\verb|SOME|SOME name returns the string "Hello there, ...!"\verb|"Hello there, ...!"|"Hellothere,...!"
where the dots would be replaced by name. Note that the name is given as an option, so if it is NONE\verb|NONE|NONE then replace the dots with "you"\verb|"you"|"you".
*)
fun valOfDefault(opt: string option, default: string) =
  if isSome opt then valOf opt
  else default

fun greeting (name: string option) =
  "Hello there, " ^ valOfDefault(name, "you") ^ "!"

val greeting_test_1 = greeting(SOME("Andy"))
val greeting_test_2 = greeting(NONE)

(*
5. Write a function repeat : int list * int list -> int list\verb|repeat : int list * int list -> int list|repeat:intlist*intlist->intlist
that given a list of integers and another list of nonnegative integers, repeats the integers in the first
list according to the numbers indicated by the second list.
For example: repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]\verb|repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]|repeat([1,2,3],[4,0,3])=[1,1,1,1,3,3,3].
*)
fun repeat (numbers: int list, times: int list) =
  if null numbers then []
  else let
    fun clone(number: int, times) =
      if times <= 0 then []
      else number :: clone(number, times - 1)
    val cloned = clone(hd numbers, hd times)
  in
    cloned @ repeat(tl numbers, tl times)
  end

val repeat_test = repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]

fun addOpt(a: int option, b: int option) =
  if not(isSome(a) andalso isSome(b)) then NONE
  else SOME(valOf(a) + valOf(b))

val addOpt_test_1 = addOpt(NONE, SOME(1)) = NONE
val addOpt_test_1_1 = addOpt(SOME(1), NONE) = NONE
val addOpt_test_2 = addOpt(SOME(2), SOME(1)) = SOME(3)