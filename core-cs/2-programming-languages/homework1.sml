type date = int * int * int

fun print_date (d : date) =
  print(Int.toString(#1 d) ^ ", " ^ Int.toString(#2 d) ^ ", " ^ Int.toString(#3 d));

fun is_older (d1 : date, d2 : date) = 
  #1 d1 < #1 d2 orelse (#1 d1 = #1 d2 andalso 
    (#2 d1 < #2 d2 orelse (#2 d1 = #2 d2 andalso 
      #3 d1 < #3 d2)
    )
  );

fun number_in_month (l : date list, m : int) =
  if null l
  then 0
  else (if #2 (hd l) = m then 1 else 0) + number_in_month(tl l, m);

fun number_in_months (l : date list, m : int list) =
  if null l orelse null m
  then 0
  else (number_in_month(l, hd m)) + number_in_months(l, tl m);

fun dates_in_month (l : date list, m : int) =
  if null l
  then []
  else
    let 
      val dates_rest = dates_in_month(tl l, m)
    in
      if #2 (hd l) = m then (hd l) :: dates_rest else dates_rest
    end;

fun dates_in_months (l : date list, ml : int list) =
  if null l
  then []
  else
    let 
      fun date_has_month (d : date, m : int) = #2 d = m;
      fun date_has_one_of_months (d : date, ml : int list) =
        if null ml
        then false
        else date_has_month(d, hd ml) orelse date_has_one_of_months(d, tl ml);
      val dates_rest = dates_in_months(tl l, ml)
    in
      if date_has_one_of_months(hd l, ml) then (hd l) :: dates_rest else
        dates_rest
    end;

fun get_nth (l : string list, i : int) =
  if null l orelse i <= 1
  then hd l
  else get_nth(tl l, i - 1);

fun date_to_string (d : date) =
  let 
    val monthes = ["January", "February", "March", "April", "May", 
      "June", "July", "August", "September", "October", "November", "December"];
    fun get_month (v : int) =
      get_nth(monthes, v);
    val get_day = Int.toString;
    val get_year = Int.toString;
  in
    get_month(#2 d) ^ " " ^ get_day(#3 d) ^ ", " ^ get_year(#1 d)
  end;

fun number_before_reaching_sum (sum : int, l : int list) =
  let 
    fun reduce (sum_curr : int, l_curr : int list, i : int) =
      let 
        val sum_new = sum_curr + hd l_curr
      in
        if sum_new >= sum
        then i
        else reduce(sum_new, tl l_curr, i + 1)
      end;
  in
    reduce(0, l, 0)
  end;

fun what_month (d : int) =
  let 
    val day_in_monthes = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
    number_before_reaching_sum(d, day_in_monthes) + 1
  end;

fun month_range (d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2);

fun oldest (l : date list) =
  if null l
  then NONE
  else 
    let
      fun reduce (l : date list, d : date) =
        if null l
        then SOME d
        else 
          reduce(tl l, if is_older(hd l, d) then hd l else d);
    in
      reduce(tl l, hd l)
    end;
