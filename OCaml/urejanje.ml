(* Konstruiraj seznam dolžine "len" z naključnimi vrednostmi med 0 in "max". 
   Primer:
   utop[1]> let l = randlist 10 10 ;;
   val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
   S pomočjo te funkcije lahko kasneje testiraš algoritme za urejanje.
   let l = (randlist 100 100) in selection_imperative_list l = List.sort compare l;;
 *)
 
(* TA FUNKCIJA MAX NE DOSEŽE, VRAČA ŠTEVILA DO MAX-1*)
let rec randlist len max = 
	if len <= 0 then []
	else Random.int max :: randlist (len - 1) max

(* Funkcija "insert y xs" vstavi "y" v že urejen seznam "xs" in vrne posodobljen
   urejen seznam.
   
   Primer:
   utop[75]> insert 9 [0; 2];;
   - : int list = [0; 2; 9]
   utop[76]> insert 1 [0; 2];;
   - : int list = [0; 1; 2]
   utop[79]> insert 1 [];;
   - : int list = [1]
 *)
let rec insert y xs =
	match xs with
	| [] -> y :: []
	| hd :: tl -> if hd > y then y :: hd :: tl
					else hd :: (insert y tl)
	

(* Prazen seznam je urejen. Seznam lahko uredimo tako, da pravilno vstavljamo
   vse elemente seznama v prazen seznam. S pomočjo te ideje napiši funkcijo
   "insertion_sort" z uporabo "List.fold_left" in "insert". *)
   
(* let rec ins_sort l = 
	let rec ins_sort_aux l acc =
	match l with
	| [] -> List.rev acc
	| hd :: tl -> ins_sort_aux tl (insert hd acc)
	in ins_sort_aux l [] *)   (* MOJ NAČIN, UPORABITI MORAM List.fold_left*)

let rec ins_sort l = List.fold_left (fun acc x -> insert x acc) [] l


	
(* Napiši rekurzivno funkcijo, ki sprejme seznam "l" in v primeru, ko "l"
   ni prazen seznam, vrne par "Some (z, l_without_z)" pri čemer je "z" 
   najmanjši element seznama "l" in "l_without_z" ne vsebuje prve ponovitve
   elementa "z". *)
   
let min_and_rest = function
	| [] -> None
	| (x :: xs) as l -> 
		let rec delete_min x = function
			| [] -> []
			| (hd :: tl) as t -> if hd = x then tl else hd :: delete_min x tl
		in
		let glava = List.nth (ins_sort (l)) 0 in
		Some (glava, delete_min glava l)

(* Urejanje z izbiranjem poteka tako, da hranimo seznam "l" ločen v 
   dveh podseznamih. Prvi seznam vsebuje že urejen del seznama, drugi 
   pa vse elemente, ki jih je še potrebno urediti.
   Nato zaporedno izmed še neurejenih elementov izbiramo najmanjšega in
   ga dodamo urejenemu podseznamu. *)

(* Z uporabo "min_and_rest" napiši rekurzivno funkcijo, ki ureja z 
   izbiranjem. *)
   
let selection_sort l = 
	let rec selection_sort_aux l acc =
		match min_and_rest l with
		| None -> List.rev acc
		| Some (urejen, neurejeni) -> selection_sort_aux neurejeni (urejen :: acc)
	in selection_sort_aux l []


(* Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem
   naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda.
   Kot prej tabelo ločujemo na že urejen del in še neurejen del, le da 
   tokrat vse elemente hranimo v vhodni tabeli, mejo med deloma pa hranimo
   v spremenljivki (npr. "boundary_sorted").
   Na vsakem koraku poiščemo indeks najmanjšega elementa neurejenega dela
   tabele in ga zamenjamo z elementom na meji med deloma (dodamo na konec
   urejenega dela). Postopek končamo, ko meja doseže konec tabele. *)

(* Napiši funkcijo "swap a i j", ki zamenja "a.(i)" in "a.(j)". *)
let swap a i j =
	let boundary_sorted = a.(i) in
	a.(i) <- a.(j) ; a.(j) <- boundary_sorted

(* Napiši funkcijo "index_min a lower upper", ki izračuna index najmanjšega
   elementa v "a" med indeksoma "lower" in "upper".
   
   Primer:
   
   index_min [|0; 2; 9; 3; 6|] 2 4 = 4 
*)
let index_min a lower upper =
	let haha = ref lower in
	for i = lower to upper do
		if a.(i) < a.(!haha) then
			haha := i
	done;
	!haha

(* Konstruiraj urejanje z izbiro na mestu. *)
let selection_imperative a = 
	let upper = (Array.length a) - 1 in
		for i = 0 to upper do
			swap a i (index_min a i upper)
	done
(* Za testiranje urejanja tabel lahko funkcijo pretvoriš v funkcijo, ki 
   ureja sezname z uporabo "Array.of_list" in "Array.to_list". *)
let selection_imperative_list l = Array.of_list(ins_sort(Array.to_list l))