(*a del*)

type filter_tree =
	| Vozlisce of filter_tree * int * filter_tree
	| List of int list
	
let primer = Vozlisce(Vozlisce(List([1]), 5, List([])) , 10, Vozlisce(List([]), 15, List([19; 20])))

(*b del*)

let rec vstavi x = function
	| List(sez) -> List(x :: sez)
	| Vozlisce(l, y, d) -> if x <= y then Vozlisce(vstavi x l, y, d) else Vozlisce(l, y, vstavi x d)
	
(*c del*)

let rec vstavi_seznam sez drevo =
	match sez with
	| [] -> drevo
	| hd :: tl -> vstavi_seznam tl (vstavi hd drevo)
	
(*d del*)	

let preveri_skatle drevo = 
	let koren = ref 100000 in
		let rec preveri_levo_aux drevo koren =
			match drevo with
			| List(sez) -> if List.exists ((<)!koren) sez then false else true
			| Vozlisce(l, y, d) -> if (y < !koren) then (koren := y; (preveri_levo_aux l koren) && (preveri_desno_aux d koren)) else false
		and preveri_desno_aux drevo koren =
			match drevo with
			| List(sez) -> if List.exists ((>=)!koren) sez then false else true
			| Vozlisce(l, y, d) -> if (y > !koren) then (koren := y; (preveri_levo_aux l koren) && (preveri_desno_aux d koren)) else false
		in preveri_levo_aux drevo koren