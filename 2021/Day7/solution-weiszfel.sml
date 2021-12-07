(*
An approach to compute the geometric median using weiszfeld's algorithm.

Problem: How to apply it to Part 2?
*)
fun realpos posls = List.map Real.fromInt posls

fun meanPos posls =
    let val sum = List.foldl op+ 0.0 posls
	val count = List.length posls
    in sum / Real.fromInt (count)
    end

(* Assume accuracy to 0.1. *)
fun weiszfeld posls mdpt =
    let fun iter posls mdpt accuracy =
	    let val numerator = List.foldl op+ 0.0 (List.map (fn x => x / abs (x-mdpt)) posls)
		val denominator = List.foldl op+ 0.0 (List.map (fn x => 1.0 / abs (x-mdpt)) posls)
		val mdpt' = numerator / denominator
	    in if abs (mdpt - mdpt') < accuracy
	       then mdpt'
	       else iter posls mdpt' accuracy
	    end
    in iter posls mdpt 0.1
    end

(* Sum of manhattan distances. *)
fun sumManDis posls mdpt =
    let val diff = List.map (fn x => abs (x-mdpt)) posls
    in List.foldl op+ 0 diff
    end

fun solve1 posls =
    let val reals = realpos posls
	val mdpt = meanPos reals
	val gm = weiszfeld reals mdpt
	val gmint = Real.toInt (IEEEReal.TO_NEAREST) gm
    in sumManDis posls gmint
    end
