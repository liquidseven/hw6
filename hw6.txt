datatype 'a node = nil | node of 'a node * 'a * 'a node;

fun mergesort _ [] = []
|	mergesort [x] = [x]
|	mergesort (op <) L =
	let
		fun split [] = ([],[])
		|	split (x::y::l) = 
				let val (x1,y1) = split l
			in
				(x :: x1, y :: y1)
			end
		|	split [x] = ([x], [])
		
		fun merge _ ([],list2) = lis2
		|	merge _ (lis1,[]) = lis1
		|	merge (op <) (x::xr, y::yr) = if (op <) x y then x::(merge (op <) (xr,y::yr) else y::(merge (op <) (yr,x::xr)
	val (x1, y1) = split L
	in
		merge(mergesort (op <) x1, mergesort (op <) y1)
	end;