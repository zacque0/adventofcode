(*
Suppose you have a file containing a list of positive integers separated by newlines, count the number of *consecutive* pairs (i,j) such that i<j. Let's call such a pair by slpair (second-larger pair).
*)
fun countSlpair filename =
    let val ifstrm = TextIO.openIn filename
	val scanInt = TextIO.scanStream (Int.scan StringCvt.DEC)
	val firstInt = scanInt ifstrm

	(* Count slpairs to the end of stream.
	   countToEOS : int -> stream -> int -> int
	   countToEOS firstitem stream count.
	 *)
	fun countToEOS firstItem stream count =
	    let val nextInt = scanInt stream
	    in if isSome nextInt then
		   let val nval = valOf nextInt
		   in if firstItem < nval
		      then countToEOS nval stream (count+1)
		      else countToEOS nval stream count
		   end
	       else count
	    end
    in if isSome firstInt then countToEOS (valOf firstInt) ifstrm 0
       else 0
	(* Note that ifstrm is not closed! *)
    end
