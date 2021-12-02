(* Assume that Forward always take positive integers only. *)
datatype command = Up of int | Down of int | Forward of int

fun scanIntoList stream =
    let val scanItem = TextIO.inputLine
	(* acc : TextIO.istream -> int option -> int list *)
	(* acc fstrm nextInt ls *)
	fun acc fstrm NONE ls = ls
	    | acc fstrm (SOME x) ls = acc fstrm (scanItem fstrm) (x::ls)
    in List.rev (acc stream (scanItem stream) [])
    end

fun processCommandStr str =
    let (* Tokenise into the form ["up", "5"]. *)
	val tokenised = String.tokens (Char.isSpace) str

	(* Convert the list into the command datatype. *)
	fun strToCmd [a,b] =
	    let val inputInt = valOf (Int.fromString b)
		fun matchString str =
		    if str="up" then Up inputInt
		    else if str="down" then Down inputInt
		    else (* str="forward" *) Forward inputInt
	    in matchString a
	    end
    in strToCmd tokenised
    end

fun commandToPair (Up x) = (0, x)
  | commandToPair (Down x) = (0, ~x)
  | commandToPair (Forward x) = (x, 0)

fun sumPair ((a,b),(c,d)) = (a+c, b+d)

fun pairResult (a,b) = a * (~b)

fun result stream =
    let val lines = scanIntoList stream
	val commands = List.map processCommandStr lines
	val pairs = List.map commandToPair commands
	val resultPair = List.foldl sumPair (0,0) pairs
    in pairResult resultPair
    end
