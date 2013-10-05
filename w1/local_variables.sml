(* let expressions *)
(* used to define a scope *)

fun silly(t : int) =
    let 
	val x = if t > 0 then t else 10
	val y = x + t + 9
    in
	if x > y then x * 2 else y * y
    end

fun silly2() =
    let
	val x = 1
    in 
	(let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
    
    end
