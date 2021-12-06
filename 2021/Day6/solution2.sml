(*
A different approach:
Use counter bin: (0,1,2,3,4,5,6,7,8,9)
For every day, shift to the left by one. For 0, add to 8.

For actual implementation, use an array of size 10. With an extra one at the end. It is for convenience dealing with 0 by moving to index 9; then shift the entire array.
*)


val counter = Array.array (10,0)

fun initialiseList [] arr = arr
  | initialiseList (x::xs) arr =
    (Array.update (arr, x, (Array.sub (arr,x) + 1));
     initialiseList xs arr)

(* All values from index N to N-1. Index 0 to Index 9. *)
fun shiftLeft arr =
    let (* Add index 0 to 7 *)
	val _ = Array.update (arr, 7, (Array.sub (arr,7) + Array.sub (arr,0)))

	val _ = Array.update (arr, 9, (Array.sub (arr,0)))

	(* Shift from 1 to 9. *)
	fun circularLS 10 arr = arr
	  | circularLS n arr = (Array.update (arr, n-1, Array.sub (arr,n));
			       circularLS (n+1) arr)

	(* Reset index 9 for summation later. *)
	fun resetIdx9 arr = Array.update (arr, 9, 0)

    in (circularLS 1 arr;
	resetIdx9 arr;
       arr)
    end

(* numDays days arr *)
fun numDays 0 arr = arr
  | numDays days arr = (shiftLeft arr;
			numDays (days-1) arr)

fun totalFish arr = Array.foldli (fn (_,a,b) => a+b) 0 arr

fun main days ls =
    let val counter = Array.array (10,0)
	val _ = initialiseList ls counter
	val _ = numDays days counter
	val result = totalFish counter
    in result
    end
