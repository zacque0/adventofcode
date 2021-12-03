fun isBinary #"0" = true
  | isBinary #"1" = true
  | isBinary _ = false

fun scanWord predicate getc stream : (string * 'a) option =
    let (* Skip leading white space. *)
	val strm = StringCvt.skipWS getc stream

	(* A helper function to read in a list of matching characters. *)
	fun matchChar getc strm ls =
	    case getc strm of
		NONE => NONE (* end of stream *)
	      | SOME (ch', strm') => if predicate ch'
				     then matchChar getc strm' (ch'::ls)
				     else (* Return results *)
					 if null ls
					 then NONE
					 else SOME (List.rev ls, strm)
    in (* Convert list of chars into string. *)
	case matchChar getc strm [] of
	    NONE => NONE
	  | SOME (ls, strm') => SOME (String.implode ls, strm')
    end

fun scanIntoList stream =
    let val scanItem = TextIO.scanStream (scanWord isBinary)
	(* acc : TextIO.istream -> int option -> int list *)
	(* acc fstrm nextInt ls *)
	fun acc fstrm NONE ls = ls
	    | acc fstrm (SOME x) ls = acc fstrm (scanItem fstrm) (x::ls)
    in List.rev (acc stream (scanItem stream) [])
    end

fun secr f y x = f(x,y)

(* Number of one at the position.
   Note: Pos starts from 0.
*)
fun numOnePos pos ls =
    if (List.nth ls pos) = #"1"
    then 1
    else 0

exception InvalidCase

fun plusOne #"1" num = (num+1)
  | plusOne #"0" num = num

fun countOneLs (pos : int) ((ls,count) : char list * int) =
    let val posChar = List.nth (ls,pos)
    in plusOne posChar count
    end

(* Match char with char1 at position *)
fun matchPosChar pos char ls = (List.nth (ls,pos))=char

fun filterPos pred pos ls =
    let val oneCount = List.foldl (countOneLs pos) 0 ls
    in case pred (oneCount, List.length ls) of
	   LESS =>  List.filter (matchPosChar pos #"0") ls
	 | EQUAL => ls
	 | GREATER => List.filter (matchPosChar pos #"1") ls
    end

fun oxyGenRate charStrings =
    let	fun checkFilter pos [] = raise InvalidCase
	  | checkFilter pos [x] = x
	  | checkFilter pos ls =
	    let val ones = List.foldl (countOneLs pos) 0 ls
		val zeroes = (List.length ls)-ones
		val filtered =
		    if ones>=zeroes then List.filter (matchPosChar pos #"1") ls
		    else List.filter (matchPosChar pos #"0") ls
	    in checkFilter (pos+1) filtered
	    end
    in checkFilter 0 charStrings
    end

fun co2ScrubRate charStrings =
    let fun checkFilter pos [] = raise InvalidCase
	  | checkFilter pos [x] = x
	  | checkFilter pos ls =
	    let val ones = List.foldl (countOneLs pos) 0 ls
		val zeroes = (List.length ls)-ones
		val filtered =
		    if ones>zeroes
		    then List.filter (matchPosChar pos #"0") ls
		    else if ones<zeroes
		    then List.filter (matchPosChar pos #"1") ls
		    else (* ones=zeroes *) List.filter (matchPosChar pos #"0") ls
	    in checkFilter (pos+1) filtered
	    end
    in checkFilter 0 charStrings
    end

fun binaryStrToInt str =
    let val strStrm = TextIO.openString str
	val result = valOf (TextIO.scanStream (Int.scan StringCvt.BIN) strStrm)
    in (TextIO.closeIn strStrm;
	result)
    end

fun result filename =
    let val fstrm = TextIO.openIn filename
	val binaries = scanIntoList fstrm
	val charStrings = List.map String.explode binaries
	val oxyStr = String.implode (oxyGenRate charStrings)
	val co2Str = String.implode (co2ScrubRate charStrings)
	val result = (binaryStrToInt oxyStr) * (binaryStrToInt co2Str)
    in (TextIO.closeIn fstrm;
	result)
    end
