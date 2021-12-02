(*
A variant of my solution to read all ints into a list. Then process the list to count # of slsumpair.

Assume that the ints from the file is finite and can all be hold in the memory.
*)

(* Read all ints into a list *)
fun streamToInts fstrm =
    let val scanInt = TextIO.scanStream (Int.scan StringCvt.DEC)

	(* acc : TextIO.istream -> int option -> int list *)
	(* acc fstrm nextInt ls *)
	fun acc fstrm NONE ls = ls
	  | acc fstrm (SOME x) ls = acc fstrm (scanInt fstrm) (x::ls)
    in List.rev (acc fstrm (scanInt fstrm) [])
    end

fun tmsum i j k = i+j+k

fun countSlsumpair ls =
    let (* preSum to hold previous sum for comparison. Make it an int option for initialisation when preSum = NONE. *)
	fun acc (a::b::c::xs) NONE count =
		let val nTmsum = tmsum a b c
		in acc (b::c::xs) (SOME nTmsum) count
		end
	  | acc (a::b::c::xs) (SOME ps) count =
		let val nTmsum = tmsum a b c
		in if ps < nTmsum then acc (b::c::xs) (SOME nTmsum) (count+1)
		else acc (b::c::xs) (SOME nTmsum) count
		end
	  | acc _ preSum count = count
    in acc ls NONE 0
    end

(* Code driver *)
fun main filename =
    let val ifstrm = TextIO.openIn filename
	val intList = streamToInts ifstrm
	val result = countSlsumpair intList
    in (TextIO.closeIn ifstrm;
       result)
    end
