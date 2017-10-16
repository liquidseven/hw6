(*use "C:\\Users\\Matt\\Desktop\\hw6.sml";*)

datatype 'a node = nil | node of 'a node * 'a * 'a node;

fun split [] = ([],[])
	|	split (x::y::l) = 
		let val (x1,y1) = split l
		in
			(x :: x1, y :: y1)
		end
	|	split [x] = ([x], []);

fun merge  ([],[]) = []
|	merge ([],lis2) = lis2
|	merge (lis1,[]) = lis1
|	merge (x::xr, y::yr) = if x < y then x::(merge (xr,y::yr)) else y::(merge (yr,x::xr));

fun mergesort [] = []
|	mergesort [x] = [x]
|	mergesort L =
	let
		val (x1, y1) = split L
	in
		merge(mergesort x1, mergesort y1)
	end;
	
fun makeBST [] = []
| makeBST [x] = node(nil, x, nil)
| L = fn L => if (length L mod 2) = 0 then 0 else 1 = mergesort L;