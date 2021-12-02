(*
Suppose you have a file containing a list of positive integers separated by newlines.

Let's index the integers by (1,2,3,4,5,..., N). Now, define the three-measurement sliding window (tmsum_i) as the sum of integer indexed (i, i+1, i+2), for 1 <= i <= N-2.

Count the number of *consecutive* pairs (tmsum_i, tmsum_j), such that tmsum_i < tmsum_j. Let's call such a pair by slsumpair (second-larger tmsum pair).

Assume that there are at least three positive integers in the file.
*)
fun tmsum i j k = i+j+k

fun countSlsumpair filename =
    let val ifstrm = TextIO.openIn filename
	val scanInt = TextIO.scanStream (Int.scan StringCvt.DEC)

	val firstInt = valOf (scanInt ifstrm)
	val secondInt = valOf (scanInt ifstrm)
	val thirdInt = valOf (scanInt ifstrm)
	val firstTmsum = tmsum firstInt secondInt thirdInt

	(* Count slpairs to the end of stream.
	   countToEOS : int -> stream -> int -> int
	   countToEOS firstitem stream count.
	 *)
	fun countToEOS firstItem secondItem preTmsum stream count =
	    let val nextInt = scanInt stream
	    in if isSome nextInt then
		   let val nval = valOf nextInt
		       val nTmsum = tmsum firstItem secondItem nval
		   in if preTmsum < nTmsum
		      then (print (Int.toString preTmsum ^ "," ^ Int.toString nTmsum ^ "\n");
			    countToEOS secondItem nval nTmsum stream (count+1))
		      else countToEOS secondItem nval nTmsum stream count
		   end
	       else count
	    end
    in countToEOS secondInt thirdInt firstTmsum ifstrm 0
	(* Note that ifstrm is not closed! *)
    end
