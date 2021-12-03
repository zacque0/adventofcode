(*
Suppose that you have a file with a list of *binary* numbers delimited by newlines, compute the multiple of gamma rate and epsilon rate from the numbers.

Each bit of gamma rate is the most common bit in the position. Each bit of epsilon rate is the least common bit in the position. So gamma rate is the complement of epsilon rate. A bit is most common if its number > half of the number of binary numbers. (What if there are even numbers and the number of bit = half of it?)

Assume that the binaries have fixed size/number of bits. And you know the number of bits before hand.
*)

fun scanIntoList stream =
    let val scanItem = TextIO.scanStream (Word.scan (StringCvt.BIN))
	(* acc : TextIO.istream -> int option -> int list *)
	(* acc fstrm nextInt ls *)
	fun acc fstrm NONE ls = ls
	    | acc fstrm (SOME x) ls = acc fstrm (scanItem fstrm) (x::ls)
    in List.rev (acc stream (scanItem stream) [])
    end

(*
How to compute the most common bit? Find the number of 1 at each position.

Binary:   10001
	      ^
Position: 43210
*)
(* numberOfOnePos : Word.word -> Word.word list -> int *)
fun numberOfOnePos posVal inls =
    let fun iterEachItem count [] = count
	  | iterEachItem count (x::xs) =
	    let val bitVal = Word.andb (posVal,x)
	    in if bitVal = posVal (* eq for Word *)
	       then iterEachItem (count+1) xs (* skip if bitVal=1. *)
	       else iterEachItem count xs
	    end
    in iterEachItem 0 inls
    end

(* numberOfOneAllPos inputList outputList
   numberOfOneAllPos : Word list -> int list -> int list
   Note that the result list is from least significant bit to most significant bit.
*)
fun numberOfOneAllPos maxPos inls =
    let (* Loop from maxPos to position 0.  *)
	fun iterEachPos (~1 : int) ls = ls
	  | iterEachPos pos ls =
	    let val posVal = Word.fromInt (Real.toInt (IEEEReal.TO_ZERO) (Math.pow (2.0, Real.fromInt pos)))
		val countOne = numberOfOnePos posVal inls
	    in iterEachPos (pos-1) (countOne::ls)
	    end
    in iterEachPos maxPos []
    end

fun mostCommonBit totalCount oneCount =
    if oneCount > (totalCount div 2)
    then 1
    else 0

fun flipBit 1 = 0
  | flipBit 0 = 1

(* Turn a list of bits into an int. The list is ordered from least to most significant bit. *)
fun bitList ls =
    let fun innerbitList (pos : real) (num : real) [] = num
	  | innerbitList pos num (x::xs) =
	    let val currentVal = (Real.fromInt x) * Math.pow (2.0, pos)
	    in innerbitList (pos+1.0) (num+currentVal) xs
	    end
    in Real.toInt (IEEEReal.TO_ZERO) (innerbitList 0.0 0.0 ls)
    end

fun result maxPos filename =
    let val fstrm = TextIO.openIn filename
	val binaries = scanIntoList fstrm
	val oneAllPos = numberOfOneAllPos maxPos binaries
	val mostCommonBits = List.map (mostCommonBit (List.length binaries)) oneAllPos
	val gammaRate = bitList mostCommonBits
	val epsilonRate = bitList (List.map flipBit mostCommonBits)
	val result = gammaRate * epsilonRate
    in (TextIO.closeIn fstrm;
	result)
    end
