(* ===== Vaja 1: Uvod v OCaml  ===== *)


(* Funkcija "predzadnji_element l" vrne predzadnji element seznama l. 
 V primeru prekratkega seznama vrne napako.
 ----------
 # predzadnji_element [1; 2; 3; 4];;
 - : int = 3
 ---------- *)

let rec predzadnji_element = function
	| [] -> failwith "Prekratek seznam."
	| x::[] -> failwith "Prekratek seznam"
	| x::_::[] -> x
	| hd::tl -> predzadnji_element tl

(* Funkcija "poisci k l" poišče k-ti element v seznamu l.
 Številčenje elementov seznama (kot ponavadi) pričnemo z 0.
 Privzamemo, da je k nenegativno število.
 V primeru prekratkega seznama funkcija vrne napako.
 ----------
 # poisci 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
 ---------- *)

let rec poisci k l =
	match l with
	| [] -> failwith "Prekratek seznam."
	| hd::tl -> if k = 0 then hd else poisci (k-1) tl

let rec poisci2 k l =
	match (k,l) with
	| (_, []) -> failwith "Prekratek seznam."
	| (0, hd::tl) -> hd
	| (a, hd::tl) -> if a = 0 then hd else poisci2 (a-1) tl
	
(* Funkcija "podvoji l" podvoji pojavitve elementov v seznamu l.
 ----------
 # podvoji [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
 ---------- *)

let rec podvoji = function
	| [] -> []
	| k :: [] -> k :: k ::[]
	| hd :: tl -> hd :: hd :: podvoji tl

(* Funkcija "razdeli k l" seznam l razdeli na dva seznama. Prvi vsebuje prvih k elementov
 seznama l, v drugem pa vsi ostali. Funkcija vrne par teh dveh seznamov.
 V primeru, ko je k izven mej seznama, je bo primeren od seznamov enak [])
 ----------
 # razdeli 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # razdeli 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
 ---------- *)
 
let rec razdeli k l = 
	match (k,l) with
	| (0, x) -> ([],x)
	| (_,[]) -> ([],[])
	| (k, hd :: tl) -> 
		let (l1, l2) = razdeli (k-1) tl in
		(hd::l1, l2)

(* Funkcija "zbrisi k l" iz seznama l pobriše k-ti element.
 V primeru prekratkega seznama funkcija vrne napako.
 ----------
 # zbrisi 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
 ---------- *)
 
let zbrisi k l = 
	let (l1, l2) = razdeli k l in
	match (l1, l2) with
	| (_, []) -> failwith "Prekratek seznam."
	| (l1, x :: []) -> l1
	| (l1, hd::tl) -> l1@tl
	
	

(* Funkcija "rezina i k l" sestavi novi seznam, ki vsebuje elemente seznama l od vključno
 i-tega do k-tega (brez k-tega).
 Predpostavimo, da sta i in k primerna.
 ----------
 # rezina 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
 ---------- *)
 
let rec rezina i k l = 
	 let (l1, l2) = razdeli i l in
	 let (s1, s2) = razdeli (k-i) l2 in
	 s1

	
(* Funkcija "vstavi x k l" na k-to mesto seznama l vrine element x.
 Če je k izven mej seznama, ga doda na začetek oz. konec.
 ----------
 # vstavi 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # vstavi 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
 ---------- *)

(*let vstavi x k l = 
	let (l1, l2) = razdeli k l in
	let l3 = x::l2 in
	l1@l3*)
	
let vstavi x k l = 
	let (l1, l2) = razdeli k l in
	match  (l1, l2) with
	|(_,[]) -> let l3 = x::l1 in
				l3@l2
	|(a, b) -> let l3 = x::l2 in
				l1@l3

(* Funkcija "zavrti n l" seznam l zavrti za n mest v levo.
 Predpostavimo, da je n v mejah seznama.
 ----------
 # zavrti 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
 ---------- *)

let zavrti n l = 
	let (l1, l2) = razdeli n l in
	l2 @ l1
 
(* Funkcija "pobrisi x l" iz seznam l izbriše vse pojavitve elementa x.
 ----------
 # pobrisi 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
 ---------- *)

let rec pobrisi x l = 
	let (hd, tl) = razdeli 1 l in
		match (hd, tl) with
		| ([], []) -> []
		| ([], tl) -> pobrisi x tl
		| (a :: [], tl) -> if a = x then pobrisi x tl else a :: (pobrisi x tl)

	

(* Funkcija "je_palindrom l" ugotovi ali seznam l predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama. 
 ----------
 # je_palindrom [1; 2; 3; 2; 1];;
 - : bool = true
 # je_palindrom [0; 0; 1; 0];;
 - : bool = false
 ---------- *)
 
let rec je_palindrom = function
	| [] -> true
	| a :: [] -> true
	| hd :: tl -> let a = (List.length tl) - 1 in
					if hd = List.nth tl a then je_palindrom (rezina 0 (a-1) tl) else false
	
  
(* Funkcija "max_po_komponentah l1 l2" vrne seznam, ki ima za elemente
 večjega od elementov na ustreznih mestih v seznamih l1 in l2.
 Skupni seznam ima dolžino krajšega od seznamov l1 in l2. 
 ----------
 # max_po_komponentah [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
 ---------- *)
let rec max_po_komponentah l1 l2 = 
	match (l1, l2) with
	| ([], _) -> []
	| (_, []) -> []
	| (hd1 :: [], hd2 :: []) -> [max hd1 hd2]
	| (hd1 :: tl1, hd2 :: tl2) -> [max hd1 hd2] @ max_po_komponentah tl1 tl2
  
(* Funkcija "drugi_najvecji l" vrne drugo največjo vrednost v seznamu l.
 Ponovitve elementa se štejejo kot ena vrednost.
 Predpostavimo, da ima seznam vsaj dva različna elementa.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu. 
 ----------
 # drugi_najvecji [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
 ---------- *)
let rec max_number_list l =
    match l with 
    |[] -> 0 (* najbrž ne najbolj pravilno*)
    |x::_ -> x
    |x::xs -> max x (max_number_list xs)
	
let drugi_najvecji l = 
	let m = max_number_list l in
	let l2 = pobrisi m l in
	max_number_list l2