(* ===== Vaja 2: Funkcijsko Programiranje  ===== *)
(*Namig: Napiši si pomožno funkcijo za obračanje seznamov. *)
let rec reverse l =
	let rec reverse_aux l acc =
		match l with
		| [] -> acc
		| hd :: tl -> reverse_aux tl (hd :: acc)
	in reverse_aux l [] 

(* Funkcija "repeat x n" vrne seznam n ponovitev x. 
 Za neprimerne n funkcija vrne prazen seznam.
 ----------
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
 ---------- *)

let rec repeat x n =
	let rec repeat_aux x n acc = 
		match n with
		| 0 -> acc
		| a -> repeat_aux x (n - 1) (x :: acc)
	in repeat_aux x n []
	

(* Funkcija "range n" sprejme številio n in vrne seznam vseh celih števil od 0
 do vključno n. Za neprimerne n funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 ----------
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
 ---------- *)

let range n = 
	let rec range_aux n acc = 
		match n with
		| 0 -> 0 :: acc
		| a -> range_aux (n - 1) (a :: acc)
	in range_aux n []

(* Funkcija "map f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 in vrne seznam [f(l0); f(l1); f(l2); ...].
 ----------
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let rec map f l = 
	match l with
	| [] -> []
	| hd::tl -> (f hd) :: (map f tl)

(* Funkcija "map_tlrec" je tail-recursive verzija funkcije map.
 ----------
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
 ---------- *)

let map_tlrec f l = 
	let rec map_aux f l acc =
		match l with
		| [] -> reverse acc
		| hd :: tl -> map_aux f tl ((f hd) :: acc)
	in map_aux f l []

(* Funkcija "mapi f l" sprejme seznam l = [l0; l1; l2; ...] in funkcijo f
 ter vrne seznam [f 0 l0; f 1 l1; f 2 l2; ...].
 ----------
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
 ---------- *)

let rec mapi f l = 
	let rec mapi_aux f l n acc =
		match l with
		| [] -> reverse acc
		| hd :: tl -> mapi_aux f tl (n+1) ((f (n) hd) :: acc) 
		in mapi_aux f l 0 []
		

(* Funkcija "zip l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] in
 l2 = [l2_0; l2_1; l2_2; ...] in vrne seznam [(l1_0,l2_0); (l1_1,l2_1); ...].
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Seznama razlicnih dolzin.".
 ---------- *)

let zip l1 l2 = 
	if List.length l1 = List.length l2 then List.combine l1 l2 else failwith "Seznama razlicnih dolzin."

(* Funkcija "zip_enum_tlrec l1 l2" sprejme seznama l1 = [l1_0; l1_1; l1_2; ...] 
 in l2 = [l2_0; l2_1; l2_2; ...] in vrne [(0, l1_0, l2_0); (1, l1_1, l2_1); ...].
 Funkcija je repno rekurzivna.
 Če seznama nista enake dolžine vrne napako.
 ----------
 # zip_enum_tlrec ["a"; "b"; "c"; "d"] [7; 3; 4; 2];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4); (3, "d", 2)]
 ---------- *)

(*let zip_enum_tlrec l1 l2 = 
	let rec zip_enum_tlrec_aux l1 l2 n acc =
		match (l1, l2) with
		| ([], []) -> reverse acc
		| ([],_) | (_,[]) -> failwith "Seznama razlicnih dolcin"
		| (hd1 :: tl1, hd2 :: tl2) -> zip_enum_tlrec_aux l1 l2 (n+1) ((n, hd1, hd2) :: acc) 
		in zip_enum_tlrec_aux l1 l2 0 [] *)  (*TA KODA SAMO NE DELA, NE POZNAM VZROKA*)

let zip_enum_tlrec l1 l2 =
  let rec zip_enum_aux l1 l2 index acc =
    match (l1, l2) with
    | ([],[]) -> reverse acc
    | (_,[]) | ([],_) -> failwith("Seznama razlicnih dolzin.")
    | (hd1::tl1,hd2::tl2) ->
      let new_head = (index,hd1,hd2) in
      zip_enum_aux tl1 tl2 (index+1) (new_head::acc)
  in
zip_enum_aux l1 l2 0 [] 		
		
(* Funkcija "unzip l" sprejme seznam l = [(a0, b0); (a1, b2); ...]
 in vrne dvojico seznamov ([a0; a1; ...], [b0; b1; ...]).
 ----------
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip l = 
	List.split l

(* Funkcija "unzip_tlrec l" je tail-recursive verzija funkcije unzip.
 ----------
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
 ---------- *)

let unzip_tlrec l =
	let rec unzip_tlrec_aux l acc1 acc2 =
	match l with
	| [] -> (reverse acc1, reverse acc2)
	| hd :: tl -> let (a1, a2) = hd in
					unzip_tlrec_aux tl (a1 :: acc1) (a2 :: acc2)
	in unzip_tlrec_aux l [] [] 

(* Funkcija "fold_left_no_acc f l" sprejme seznam l = [l0; l1; l2; ...; ln] in funkcijo f,
 vrne pa f(... (f (f (f l0 l1) l2) l3) ... ln).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 ----------
 # fold_left_no_acc (^) ["F"; "I"; "K"; "U"; "S"];;
 - : string = "FIKUS"
 ---------- *)

let fold_left_no_acc1 f l = List.fold_left f "" l

let rec fold_left_no_acc f l =
	match l with
	| []|[_] -> failwith "Seznam z manj kot dvema elementoma."
	| a :: b :: [] -> f a b
	| hd1 :: hd2 :: tl -> fold_left_no_acc f ((f hd1 hd2)::tl)

(* Funkcija "apply_sequence f x n" vrne seznam zaporednih uporab funkcije f na x,
 [x; f x; f (f x); ...; f uporabljena n-krat na x].
 Funkcija je repno rekurzivna.
 ----------
 # apply_sequence (fun x -> x*x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x*x) 2 (-5);;
 - : int list = []
 ---------- *)

let apply_sequence f x n = 
	let rec apply_sequence_aux f x n acc =
		match n with
		| 0 -> reverse acc
		| n -> apply_sequence_aux f (f x) (n-1) ((f x) :: acc)
	in apply_sequence_aux f x n []

(* Funkcija "filter f l" sprejme seznam l in vrne seznam elementov l,
 za katere funkcija f vrne true.
 ----------
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
 ---------- *)

let filter f l = 
	let rec filter_aux f l acc =
		match l with
		| [] -> reverse acc
		| hd :: tl -> if f hd = true then filter_aux f tl (hd::acc)
						else filter_aux f tl acc
	in filter_aux f l []

(* Funkcija "exists f l" sprejme seznam l in vrne true če obstaja 
element
 seznama l, za katerega fukcija f vrne true in false sicer.
 Funkcija je repno rekurzivna.
 ----------
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
 ---------- *)

let rec exists f l = 
	match l with
	| [] -> false
	| hd :: tl -> if f hd = true then true else exists f tl

(* Funkcija "first f none_value l" sprejme seznam l in vrne prvi element seznama,
 za katerega funkcija f vrne true, če takšnega elementa ni, pa vrne none_value.
 Funkcija je repno rekurzivna.
 ----------
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
 ---------- *)

let rec first f none_value = function
	| [] -> none_value
	| hd :: tl -> if f hd = true then hd else first f none_value tl
  
(* Severnjaki napadajo Medbrezje. Kot vrhovni čarodej poznaš zaporedje urokov s katerimi
 lahko Medbrezje zaščitiš pred napadom, zaporedje urokov pa je predstavljeno v seznamu oblike
 [("ime1", vrednost1); ("ime2", vrednost2); ...].
 Na razpolago imaš skupino čarodejov, ki so prav tako predstavljeni v seznamu oblike
 [("ime1", spretnost1); ("ime2", spretnost2); ...].
 Čarodej lahko izvede zaporedje urokov, če je njegova spretnost večja ali enaka skupni
 vrednosti vseh urokov v zaporedju.
 Funkcija "able_protectors spells wizards" vrne seznam imen vseh čarodejov, ki lahko
 samostojno zaščitijo Medbrezje.
 Funkcija "fails_on spells wizards" vrne seznam parov (čarodej, neuspešni urok), kjer
 je neuspešni urok prvi urok v zaporedju, za katerega čarodej nima več dovolj spretnosti.
 Če lahko čarodej zaporedje izvede v celoti, to predstavlja prazen niz.
 Namig: Dober čarodej uporablja svoje znanje in izkušnje, ki jih pridobi tekom učenja.
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Atijam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   able_protectors spells wizards;;
 - : string list = ["Merlin"; "Atijam"]
 ----------
 # let spells = [("Protect",51); ("Renounce", 17); ("Blaze", 420); ("Banish",103)] in
   let wizards = [("Merlin", 1832); ("Frodo", 53); ("Atijam", 1337);
  ("Mr Duck", 7); ("Kylo Ren", 589); ("Snoop Dogg", 123)] in
   fails_on spells wizards;;
 - : (string * string) list = [("Merlin", ""); ("Frodo", "Renounce"); ("Atijam", "");
  ("Mr Duck", "Protect"); ("Kylo Ren", "Banish"); ("Snoop Dogg", "Blaze")]
 ----------*)
 
(* MOJA REŠITEV, delujoča *)
let sum_spells spells =
	let rec sum_spells_aux spells acc = 
		match spells with
		| [] -> acc
		| hd :: tl -> let (_, a2) = hd in
						sum_spells_aux tl (a2 + acc)
	in sum_spells_aux spells 0
 
let able_protectors1 spells wizards = 
	let rec able_protectors_aux spells wizards acc = 
		match wizards with
		| [] -> reverse acc
		| hd :: tl -> let (wizard, ability) = hd in
						let total_power = sum_spells spells in 
							if ability >= total_power then able_protectors_aux spells tl (wizard :: acc)
								else able_protectors_aux spells tl acc
		in able_protectors_aux spells wizards []

		
(* URADNA REŠITEV *)
let able_protectors spells wizards = 
	let (_, spell_values) = unzip spells in
	let sequence_cost = fold_left_no_acc (+) (0::0::spell_values) in (* Odspredaj dodamo 0::0::, saj si ne želimo failwith*)
	let can_cast (_, wizard_ability) = (wizard_ability >= sequence_cost) in
	let mighty_wizards = filter can_cast wizards in
	let (mighty_wizard_names, _) = unzip_tlrec mighty_wizards in
	mighty_wizard_names

(* URADNA REŠITEV *)
let fails_on spells wizards = 
	let rec gets_stuck spells wizard_ability =
		match spells with
		| [] -> ""
		| (spell_name, spell_value)::tl ->
		  if wizard_ability >= spell_value
		  then gets_stuck tl (wizard_ability - spell_value)
		  else spell_name
	in
	let (wizard_names, wizard_abilities) = unzip wizards in
	let stuck_spells = map (gets_stuck spells) wizard_abilities in
	zip wizard_names stuck_spells