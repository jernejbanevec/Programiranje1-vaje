(* Prioritetna vrsta je podatkovna struktura, ki hrani elemente glede na 
   njihovo prioriteto. Elementi z višjo prioriteto so na voljo pred 
   elementi z nižjo prioriteto. *)

type comparison = LT | EQ | GT

(* Prioriteto definiramo s pomočjo modula primerjav.
   Opomba: Želimo, da je implementacija tipa t vidna, saj potrebujemo tip
   za uporabo funktorjev. Ker pa ne vemo katere tipe bomo primerjali, jo
   za ta trenutek pustimo abstraktno. *)
   
module type Comparable = sig
    type t
    val compare : t -> t -> comparison
  end

(* Napiši modul za primerjavo celih števil *)

module Cmp_Int = struct
  type t = int
  let compare x y = 
		if x > y then GT
		else if x < y then LT
		else EQ
end 


(* Primer: *)
let p0 = Cmp_Int.compare 9000 42;;

(* Ko modulu predpišemo signaturo, je potrebno paziti katere tipe naredimo
   abstraktne. Če je tip abstrakten v signaturi, ga lahko kljub temu
   'razkrijemo' s pomočjo "with type t = int", ko to potrebujemo. 
   Primer: *)
   
module Cmp_Int_prescribed = (Cmp_Int : Comparable with type t = Cmp_Int.t) 

(* Primer uporabe: *)
let p1 = Cmp_Int_prescribed.compare (-9000) 42;;

(* Sedaj napiši modul, ki primerja nize. Pri pisanju primerjalne funkcije
   si pomagaj s funkcijo "compare" iz vgrajenega modula "Pervasives".
   Funkcija Pervasives.compare s t vrne -1 če je s<t, 0 če s=t in 1 za s>t. *)
   
module Cmp_String = struct
	type t = string
	let compare s t =
		match Pervasives.compare s t with
		| -1 -> LT
		| 0 -> EQ
		| 1 -> GT
end


(* Funktor je preslikava iz modula v modul. Sedaj definiraj funktor,
   ki sprejme modul, ki ustreza Comparable signaturi in vrne nov Comparable
   modul na istem osnovnem tipu, vendar z obrnjeno funkcijo primerjanja. *)

(* Spodnja definicija uporabla oznake za tipe. Brez oznak bi bila zgolj
   "module Cmp_inv (Cmp)" vendar z oznako tipov povemo, da se tip končnega 
   modula ujema s tipom modula, ki ga podamo kot argument. *)

module Cmp_Inv (Cmp : Comparable) : Comparable with type t = Cmp.t = struct
	type t = Cmp.t
	let compare x y = match Cmp.compare x y with
		| LT -> GT
		| EQ -> EQ
		| GT -> LT
end
   

(* Funktor uporabljamo podobno kot funkcije, le da v tem primeru potrebujemo
   oklepaje okrog argumentov. *)


module Cmp_Int_inv = Cmp_Inv (Cmp_Int)
let _ = Cmp_Int.compare (-9000) 42;;
let _ = Cmp_Int_inv.compare (-9000) 42;;


(* Sedaj napišemo signaturo kopice [heap] (oz. prioritetne vrste). Imamo tip kopice,
   ki ga označimo s "h", tip elementov "el", prazno kopico in pa operacijo "push", ki na
   kopico doda element in operacijo "pop", ki iz kopice odvzame prvi element po prioriteti. 
   Ker je kopica lahko prazna, nam "pop" vrne opcijski tip. *)


module type Heap = sig
    type h
    type el
    val empty : h
    val pop : h -> (h * el) option
    val push : h -> el -> h
  end



(* Kopico bomo implementirali kot urejen seznam. Napiši funktor, ki sprejme Comparable
   modul in s pomočjo primerjave v tem modulu naredi kopico preko urejenega seznama.
   Seznam vsebuje elementa tipa Cmp.t *)


module Sorted_List_Heap (Cmp : Comparable) : Heap with type el = Cmp.t = struct

	type h = Cmp.t list
	type el = Cmp.t
	
	let empty = []
	
	let pop = function
		| [] -> None
		| x :: xs -> Some (xs, x)
	
	let rec insert x xs =
		match xs with 
		| [] -> [x]
		| hd :: tl -> 
			(match Cmp.compare x hd with
			| LT -> x :: hd :: tl
			| EQ -> xs
			| GT -> hd :: (insert x tl))
		
	let push xs x = insert x xs
end

module IntH = Sorted_List_Heap (Cmp_Int)
module StringH = Sorted_List_Heap (Cmp_String)

(* Za lažjo uporabo, napiši funktor "To_List", ki sprejme implementacijo kopice in
   vrne modul z operacijo "to_list" s katero elemente v kopici shrani v seznam. *)

module To_List (H : Heap) = struct
	let rec to_list h =
		match H.pop h with
		| None -> []
		| Some (h, x) -> x :: (to_list h)
end


(* Sedaj lahko lažje testiramo implementacijo modulov.
   Module lahko uporabljamo lokalno, le da pri tem v "let ... in" dodatno uporabimo 
   še besedo "module". Imena globalnih modulov so praviloma daljša in bolj opisna,
   ko pa uporabljamo module lokalno si lahko privoščimo krajša imena. *)
   

let _ =
  let h = List.fold_left IntH.push IntH.empty [1; 0; 9; 2] in
  let module TL = To_List(IntH) in
  TL.to_list h
let _ =
  let module H = Sorted_List_Heap (Cmp_Inv(Cmp_Int)) in
  let module L = To_List(H) in
  let h = List.fold_left H.push H.empty [1; 0; 9; 2] in
  L.to_list h
 