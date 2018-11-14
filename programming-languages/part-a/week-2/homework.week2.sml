(* define Date is a triple integers *)
type Date = int * int * int

(* helper functions *)
fun date_in_month(date: Date, month) =
  #2 date = month

fun count_date(date: Date, month) =
  if date_in_month(date, month) then 1 else 0

(* 11 homework functions *)
(* 1. is first date earlier than second date? *)
fun is_older(first: Date, second: Date) =
  #1 first < #1 second orelse #1 first = #1 second andalso
    #2 first < #2 second orelse #2 first = #2 second andalso
      #3 first < #3 second

(* 2. how many dates are in given month *)
fun number_in_month(dates: Date list, month) =
  if null dates then 0
  else count_date(hd dates, month) + number_in_month(tl dates, month)

(* 3. how many dates are in given months *)
fun number_in_months(dates: Date list, months: int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. which dates are in given month *)
fun dates_in_month(dates: Date list, month) =
  if null dates then []
  else
    if date_in_month(hd dates, month) then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* 5. which dates are in given months *)
fun dates_in_months(dates: Date list, months: int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6. get Nth element of the list *)
fun get_nth(items, number) =
  if number <= 1 then hd items
  else get_nth(tl items, number - 1)

(* 7. Convert date to string *)
fun month_name(nMonth) =
  let
    val monthNames = ["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"]
  in
    get_nth(monthNames, nMonth)
  end

fun date_to_string(date: Date) =
  month_name(#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* 8. number of list elements just before we hit the sum *)
fun number_before_reaching_sum(sum, numbers) =
  let val head = hd numbers in
    if head >= sum then 0
    else 1 + number_before_reaching_sum(sum-head, tl numbers)
  end

(* 9. guess month number by the number of day in the year *)

fun what_month(dayNumber) =
  let
    val monthDurations = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(dayNumber, 0 :: monthDurations)
  end

(* 10. list of number of month for each day in a range *)
fun month_range(startDay, endDay) =
 if startDay > endDay then []
 else what_month(startDay) :: month_range(startDay + 1, endDay)

(* 11. get the oldest date. Evaluates to NONE for empty list or SOME date, returing the oldes date *)
fun oldest (dates: Date list) =
  if null dates then NONE
  else let
    fun oldest_non_empty (dates: Date list) =
      if null (tl dates) then hd dates
      else let val oldest_tail = oldest_non_empty(tl dates)
      in
        if is_older(hd dates, oldest_tail)
        then hd dates
        else oldest_tail
      end
  in
    SOME (oldest_non_empty(dates))
  end