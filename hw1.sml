fun is_older(x : int*int*int, y : int*int*int) =
    if (#1 x < #1 y)
    then true
    else if (#1 x > #1 y) 
    then false
    else if (#2 x < #2 y)
    then true
    else if (#2 x > #2 y)
    then false
    else #3 x < #3 y

fun number_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else 
        let val x = #2 (hd dates)  = month
            val y = number_in_month(tl(dates), month)
        in (if x then y+1 else y+0)
        end
fun number_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months)) 

(* generic function *)
fun get_nth(x , n : int) =
    if n = 1
    then hd(x)
    else get_nth(tl(x), n-1)

val months_of_year = ["January", "February", "March", "April", "May", "June", "July", "August",
                      "September", "October", "November", "December"]

fun date_to_string(date : int*int*int) = 
    get_nth(months_of_year, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 

fun number_before_reaching_sum(sum : int, list : int list) =
    if sum <= (hd list)
    then 0
    else 1 + number_before_reaching_sum(sum - (hd list), (tl list))

val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month(day : int) =
    number_before_reaching_sum(day, days_in_month) + 1

fun month_range(d1 : int, d2 : int) = 
    if d2 < d1
    then []
    else if d1 = d2
    then [what_month d1]
    else (what_month d1)::month_range(d1+1, d2)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else let 
             fun oldest_nonempty(dates : (int*int*int) list) = 
                 if null (tl dates)
                 then hd dates
                 else let val d = oldest_nonempty(tl dates)
                      in 
                          if is_older(hd dates, d)   
                          then hd dates
                          else d
                      end
         in 
             SOME (oldest_nonempty dates)
         end
(*generic function*)
fun is_member_of(item, list) =
    if null list
    then false
    else if item = hd list
    then true
    else is_member_of(item, tl list)

(* generic function*)
fun remove_duplicates(list) = 
    if null list
    then list
    else if is_member_of(hd list, tl list)
    then remove_duplicates(tl list)
    else (hd list)::remove_duplicates(tl list)

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) = 
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list ) =
    dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date : (int*int*int)) = 
    if (#1 date) < 1
    then false
    (*check to see month <=12 *)
    else if (#2 date > 12)
    then false
    (*check for leap year*)
    else if (#2 date = 2) andalso (#3 date > 28)
    then
        let fun was_leap_year(year : int) = 
            if (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
            then true
            else false 
        in 
            was_leap_year(#1 date)
        end
    (*check to see if day matches possibilites*)
    else
        let  
            val month = (#2 date) 
        in 
            (#3 date) <= get_nth(days_in_month, month) 
        end
