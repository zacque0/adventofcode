(*
Problem:
A fish, f1,  gives birth to a new fish, f2, after 7 days. Then the new fish f2 will give birth to another new fish, f4, after 9 days. During that period, the old fish f1 will give birth to another fish, f3, again after 7 days. The previously young fish f2 becomes mature after giving birth to one fish and its birth cycle now becomes 7.

So, given a fish, fN, and the number of days, bdN, until the fish to give birth, calculate the total number of fish after M days.
*)

(*
Thoughts:
Suppose there is only one fish f_1 and let bd_1 = 0:
Day 0 : 0
Day 1 : 6 8
Day 2 : 5 7
Day 3 : 4 6
...
Day 8 : 6 1 8
Day 9 : 5 0 7
Day 10: 4 6 6 8
...
Day 15: 6 1 1 3 8
Day 16: 5 0 0 2 7
Day 17: 4 6 6 1 6 8 8
Day 18: 3 5 5 0 5 7 7
Day 19: 2 4 4 6 4 6 6 8
Day 20: 1 3 3 5 3 5 5 7
Day 21: 0 2 2 4 2 4 4 6
Day 22: 6 1 1 3 1 3 3 5 8

Notice Day 1, 8 and 15: a new born fish lags two days behind. Also, note that it is assume that every fish is independent and won't affect the birth cycle of another fish.

Let dd_N be the number of direct descendant of f_N. Suppose M = 22,
dd_1 = 4 (f_2, f_3, f_5, f_9 on Day 1, Day 8, Day 15, Day 22 respectively)
dd_2 = 2 (f_4, f_6 on Day 10, Day 17 respectively)
dd_3 = 1 (f_7 on Day 17)
dd_4 = 1 (f_8, Day 19)

Birth cycle:
f_1 : (bn_1+1),7,7,7,7
f_N : 9,7,7,7,7
*)

fun expandChild (x,b) totalDays curDay =
    let val firstB = curDay+x
	(* Results *)
	fun rest accDay res = if accDay>totalDays then List.rev res
			      else rest (accDay+b) (accDay::res)
    in if firstB>totalDays
       then []
       else firstB::(rest (firstB+b) [])
    end

val expandDChild = expandChild (9,7)

fun fishSum totalDays initLag =
    let fun sum [] = 0
	  | sum (ls as x::xs) = List.length ls + (List.foldl op+ 0 (List.map (sum o (expandDChild totalDays)) ls))
    in 1 + (sum (expandChild (initLag+1,7) totalDays 0))
    end

fun initFish totalDays ls =
    List.foldl op+ 0 (List.map (fishSum totalDays) ls)
