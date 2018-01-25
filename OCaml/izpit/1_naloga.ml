(*a del*)

let rec izpisi_vsa_stevila = function
	| [] -> print_newline ()
	| hd :: tl -> print_int hd ; izpisi_vsa_stevila tl
	
(*b del*)

let map2_opt f sez1 sez2 =
	let rec map2_opt_aux f sez1 sez2 acc =
		match (sez1, sez2) with
		| ([], []) -> Some acc
		| (_, []) | ([], _) -> None
		| (hd1 :: tl1, hd2 :: tl2) -> map2_opt_aux f tl1 tl2 ((f hd1 hd2) :: acc)
	in map2_opt_aux f sez1 sez2 []