(*
Problem:
Suppose you have a file containing a list of positive integers separated by newlines, count the number of *consecutive* pairs (i,j) such that i<j. Let's call such a pair by slpair (second-larger pair).

Solution:
A variant of my solution to read all ints into a list. Then process the list to count # of slpair.

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

fun countSlpair ls =
    let fun acc [] count = count
	  | acc [a] count = count
	  | acc (a::b::xs) count = if a<b then acc (b::xs) (count+1)
				   else acc (b::xs) count
    in acc ls 0
    end

(* Code driver *)
fun main filename =
    let val ifstrm = TextIO.openIn filename
	val intList = streamToInts ifstrm
	val result = countSlpair intList
    in (TextIO.closeIn ifstrm;
       result)
    end
