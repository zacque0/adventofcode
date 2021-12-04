(*
Play bingo.

Given a list of bingo boards and a list of numbers called, calculate the final score of the first winning board. A bingo board is a 5x5 matrix (containing nonnegative integers). For every number called, the corresponding number (if exists) on the board will be marked. A bingo board wins if there is >=1 rows or columns of marked numbers. Then the final score of a winning board is calculated by (sum of all unmarked numbers * last number called that helps the board to win).

*)

(* Idea:
Let's use
bingob to denote a bingo board,
entryn to denote a entry number in the bingob,
unmarkn to denote an unmarked number,
markn to denote a marked number, and
calln to denote a calling number.

Design Decision 1
-----------------
I can represent a matrix using a list of lists. But a bingob has states for each entry. I can model the state (called entrystate) as Marked and Unmarked. Then model an entry as a pair (int * entrystate). Then the bingob as (entry list) list.

Design Decision 2
-----------------
Problem: How to check whether a board has won? In other word, how to check that the current state of a board satisfies the winning condition?

I can loop through the entire bingob row by row and column by column after every calln. But it gets tedious/slower as the bingob grows larger or after many calln.

To trade space for time, there is a counter for the number of unmarkn for each row and column. So a row/column is fully marked when the counter goes to zero.

Design Decision 3
-----------------
Assume that each entryn in the bingob is unique, such that every calln either mark a entry or none.

Design Decision 4
-----------------
Give every entryn a unique coordinate, (rowIndex, colIndex). Label top-left as (0,0) and bottom-right as (4,4).
	0,1,2,3,4
0   : [[1,2,3,4,5],
1   :  [6,7,8,9,10],
... :  [...],
       ...]
*)
datatype entrystate = Marked | Unmarked
type entry = int * entrystate
type board = {body : (entry list) list,
	      rowUnmarkedC : (int ref) list,
	      colUnmarkedC : (int ref) list}

(*
Initialise a bingob given a list of int lists.

Assume that intBoard is a matrix, such that every row has equal length.
*)
fun initBingob intBoard =
    let (* Initialise a row. *)
	(* initRow inputRow outputRow *)
	fun initRow row =
	    let fun aux [] outR = List.rev outR
		  | aux (x::xs) outR = aux xs ((x,Unmarked)::outR)
	    in aux row []
	    end

	(* iterRow inputBoard outputBoard *)
	fun iterRow [] outB = List.rev outB
	  | iterRow (x::xs) outB = iterRow xs ((initRow x)::outB)

	val rowCount = List.length intBoard

	val colCount = List.length (hd intBoard)
    in {body = iterRow intBoard [],
	rowUnmarkedC = List.tabulate (rowCount, fn _ => ref colCount),
	colUnmarkedC = List.tabulate (colCount, fn _ => ref rowCount)}
    end

(*
Given a bingob and a list of calln, find out the number of calln needed to win a board (if the board can be won using the given calln's).
*)
fun numCalln callns bingob =
    let (* Is it important to know whether a calln marks an entry? Yes. If a calln marks an entry, you can return early the updated state. *)
	(* Return NONE if calln does not match any entry of the bingob; SOME (updatedBoard, updated coordinate) for successful match. *)
	fun markBoardBody calln bingob =
	    let (* markRow calln inputRow resultRow/resultList *)
		(* Returns SOME (row, columnIndex) for successful update. *)
		fun markRow calln inputRow =
		    let fun aux calln [] rls colIndex = NONE
			  | aux calln ((i,s)::xs) rls colIndex =
			    if calln=i
			    then let val result = (List.rev ((i,Marked)::rls)) @ xs
				 in SOME (result, colIndex) (* return early *)
				 end
			    else aux calln xs ((i,s)::rls) (colIndex+1)
		    in aux calln inputRow [] 0
		    end

		(* iterRow calln inputBoard *)
		(* Returns SOME (updatedBord, updated coordinate) for successful update; otherwise NONE *)
		fun iterRow calln bingob =
		    let (* aux calln inputBoard resultboard rowindex *)
			fun aux calln [] rb rowIndex = NONE
			  | aux calln (x::xs) rb rowIndex =
			    let val updateRow = markRow calln x
			    in if isSome updateRow
			       then (* return early *)
				   let val (row, colIndex) = valOf updateRow
				       val head = List.rev (row::rb)
				   in SOME (head @ xs, (rowIndex, colIndex))
				   end
			       else aux calln xs (x::rb) (rowIndex+1)
			    end
		    in aux calln bingob [] 0
		    end
	    in iterRow calln bingob
	    end

	(* Update rowUnmarkedC given an updated coordinate. *)
	(* Minus count by 1. *)
	fun updateRc rowc (a,_) =
	    let val item = List.nth (rowc,a)
	    in (item := (!item) - 1)
	    end

	fun updateCc colc (_,b) =
	    let val item = List.nth (colc,b)
	    in (item := (!item) - 1)
	    end

	fun checkWin rowc colc =
	    let val hasZero = List.exists (fn x => (!x)=0)
	    in (hasZero rowc) orelse (hasZero colc)
	    end

	(* Responsibilities:
	1. Keep updating the bingob given a list of calln. Count the calln needed until it wins.
	2. For every successful update, update unmarked counter for rows and columns. Then check for winning condition.
	3. Stop only if success. If you runs out of callns before success, returns NONE. It means that the callns cannot let the given bingob wins.
	 *)
	(* Returns NONE if calln cannot let the bingob wins; SOME (number of calln, last calln, winning board state). *)
	(* iterBoard bingob callns *)
	fun iterBoard bingob callns =
	    let (* *)
		fun aux bingob [] countCalln preCalln false = NONE
		  | aux bingob _ countCalln preCalln true = SOME (countCalln, preCalln, bingob)
		  | aux (bingob as {body=bbody, rowUnmarkedC=rowc, colUnmarkedC=colc})
			(x::xs) countCalln preCalln false =
		    (case markBoardBody x bbody of
			NONE => aux bingob xs (countCalln+1) x false
		      | SOME (updatedBB, updCoor) =>
			(updateRc rowc updCoor;
			 updateCc colc updCoor;
			 aux {body=updatedBB,
			      rowUnmarkedC=rowc,
			      colUnmarkedC=colc}
			     xs (countCalln+1) x (checkWin rowc colc)))
	    in aux bingob callns 0 ~100 false
	    end
    in iterBoard bingob callns
    end

(* Driver *)
(* Convert a string of comma delimited integers into a list of ints. *)
fun strToCallns str =
    let val delimitedInt = String.tokens (fn x => x = #",") str
    in List.map (valOf o Int.fromString) delimitedInt
    end

fun scanBoard fstrm =
    let (* Scan into list by calling func N times. *)
	(* func returns 'a option *)
	fun scan count func fstrm =
	    let fun aux func 0 ls = SOME (List.rev ls)
		  | aux func n ls = case func fstrm of
					NONE => NONE
				      | SOME x => aux func (n-1) (x::ls)
	    in aux func count []
	    end

	val scan5ints = scan 5 (TextIO.scanStream (Int.scan StringCvt.DEC))
    in scan 5 scan5ints fstrm
    end

fun scanStrm fstrm =
    let val firstline = valOf (TextIO.inputLine fstrm)
	val callns = strToCallns firstline

	(* Keep scanning stream for board until NONE. *)
	fun iterScanB fstrm NONE ls = List.rev ls
	  | iterScanB fstrm (SOME x) ls = iterScanB fstrm (scanBoard fstrm) (x::ls)

	val boards = iterScanB fstrm (scanBoard fstrm) []
    in (callns, boards)
    end

fun lessCallnCount (a as (x,_,_), b as (y,_,_)) = if x>y then b
						  else a

fun unmarkedSum body =
    let fun sumRow row =
	    let val unmarkedns = List.filter (fn (_,b) => b = Unmarked) row
		val sums = List.foldl (fn ((a,_),b) => a+b) 0 unmarkedns
	    in sums
	    end

	val sums = List.map sumRow body
    in List.foldl op+ 0 sums
    end

fun finalScore (_, lastCalln, (board : board)) = lastCalln * (unmarkedSum (#body board))

fun result filename =
    let val fstrm = TextIO.openIn filename
	val (callns, intmats) = scanStrm fstrm
	val bingobs = List.map initBingob intmats
	val steps = List.mapPartial (numCalln callns) bingobs
	val firstBingob = List.foldl lessCallnCount (hd steps) (tl steps)
	val finalres = finalScore firstBingob
    in (TextIO.closeIn fstrm;
	finalres)
    end

(* Part 2*)
fun moreCallnCount (a as (x,_,_), b as (y,_,_)) = if x>y then a
						  else b

fun result2 filename =
    let val fstrm = TextIO.openIn filename
	val (callns, intmats) = scanStrm fstrm
	val bingobs = List.map initBingob intmats
	val steps = List.mapPartial (numCalln callns) bingobs
	val firstBingob = List.foldl moreCallnCount (hd steps) (tl steps)
	val finalres = finalScore firstBingob
    in (TextIO.closeIn fstrm;
	finalres)
    end
