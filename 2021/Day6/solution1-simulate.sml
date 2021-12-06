(* Simulation *)
fun nextDay ls =
    let val zeroes = List.length (List.filter (fn x => x=0) ls)
	val ls' = List.map (fn x => if x=0 then 6 else (x-1)) ls (* cannot use mod 7 because there are 8 and 7. *)
    in ls' @ (List.tabulate (zeroes, fn _ => 8))
    end

fun nextNDays ls N =
    let fun aux ls 0 = ls
	  | aux ls n = aux (nextDay ls) (n-1)
    in aux ls N
    end
