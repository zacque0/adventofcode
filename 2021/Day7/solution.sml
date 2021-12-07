(*
Problem:
Given a list of points, compute the point that minimises the sum of Manhattan distances (for 1D) to all other points. From a simple Web search, this point appears to be the geometric median.

Read:
1. https://en.m.wikipedia.org/wiki/Geometric_median
2. https://stackoverflow.com/questions/12934213/how-to-find-out-geometric-median
*)

(*
Solution idea:
1. Brute force (iterate through all points, compute the sumManDis, then select the minimum sum.)
2. Weiszfeld's algorithm (approximate + iterate approach)
*)
fun meanPos posls =
    let val sum = List.foldl op+ 0 posls
	val count = List.length posls
    in sum div count
    end

(* Sum of manhattan distances. *)
fun sumManDis posls mdpt =
    let val diff = List.map (fn x => abs (x-mdpt)) posls
    in List.foldl op+ 0 diff
    end

(* bruteForce assumes that the geometric mean is found in posls \cup meanPos (which is of course not true!). *)
fun bruteForce posls =
    let val mean = meanPos posls
	val minPos = List.foldl (fn (x,y) =>
				       let val sumx = (sumManDis posls x)
					   val sumy = (sumManDis posls y)
				       in if sumx > sumy then y else x
				       end) mean posls
    in sumManDis posls minPos
    end

(* Part 2 *)
fun fuelCost steps = (steps * (steps+1)) div 2

fun newCost posls mdpt =
    let val diff = List.map (fn x => abs (x-mdpt)) posls
	val costs = List.map (fn x => fuelCost x) diff
    in List.foldl op+ 0 costs
    end

(* Try to search around iteratively for minManDis. This is a hack. *)
fun localMin posls mdpt =
    let val minPos' = List.foldl (fn (x,y) => if (newCost posls x)>(newCost posls y) then y else x) mdpt (List.tabulate (5, (fn x => x+mdpt-3)))
    in if minPos' = mdpt
       then mdpt
       else localMin posls minPos'
    end

fun bruteForce2 posls =
    let val mean = meanPos posls
	val minPos = List.foldl (fn (x,y) =>
				       let val sumx = (newCost posls x)
					   val sumy = (newCost posls y)
				       in if sumx > sumy then y else x
				       end) mean posls
	val localMinPos = localMin posls minPos
    in (localMinPos, newCost posls localMinPos)
    end
