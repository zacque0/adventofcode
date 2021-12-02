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

fun secr f y x = f(x,y)

(* Function of depth operates on aim. *)
fun commandToFuns (Up x) =
	{horiposFun=NONE, depthFun=NONE, aimFun=SOME (secr op- x)}
  | commandToFuns (Down x) =
	{horiposFun=NONE, depthFun=NONE, aimFun=SOME (secr op+ x)}
  | commandToFuns (Forward x) =
	{horiposFun=SOME (secr op+ x), depthFun=SOME (secr op* x), aimFun=NONE}

fun operatePos ({horiposFun=hf, depthFun=df, aimFun=af},
		{horipos=hd, depth=dd, aim=ad}) =
    let fun maybeOperate data NONE = data
	  | maybeOperate data (SOME func) = func data

	fun maybeOperateDepth data NONE = 0
	  | maybeOperateDepth data (SOME func) = func data

    in {horipos=(maybeOperate hd hf),
	depth=dd + (maybeOperateDepth ad df),
	aim=(maybeOperate ad af)}
    end

fun computeResult {horipos=hd, depth=dd, aim=ad} = hd * dd

fun result stream =
    let val lines = scanIntoList stream
	val commands = List.map processCommandStr lines
	val funs = List.map commandToFuns commands
	val resultState = List.foldl operatePos {horipos=0, depth=0, aim=0} funs
    in computeResult resultState
    end
