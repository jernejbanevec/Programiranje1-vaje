(* ===== Vaja: Dinamično programiranje  ===== *)

(* Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
   samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
   desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
   različne (pozitivne) mase. Miška bi se rada kar se da nažrla, zato jo zanima,
   katero pot naj ubere.
   Napišite funkcijo "max_cheese cheese_matrix", ki dobi matriko z
   masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
   optimalni poti.
   ----------
   # max_cheese test_matrix;;
   - : int = 13
   ----------*)

let test_matrix = [| [| 1 ; 2 ; 0 |];
                     [| 2 ; 4 ; 5 |];
                     [| 7 ; 0 ; 1 |]  |]

let rec max_cheese cheese_matrix =
	let max_v = Array.length cheese_matrix - 1 in
	let max_s = Array.length cheese_matrix.(0) - 1 in
	
	let rec pozeruh v s =
		if (v = max_v) then
			if (s = max_s) then
			cheese_matrix.(v).(s)
			else
				cheese_matrix.(v).(s) + pozeruh v (s+1)
		else
			if (s = max_s) then
				cheese_matrix.(v).(s) + pozeruh (v+1) s
			else 
				let desno = pozeruh v (s+1) in
				let dol = pozeruh (v+1) s in
				cheese_matrix.(v).(s) + max desno dol
	in
	pozeruh 0 0

let rec max_cheese2 cheese_matrix =
	let max_v = Array.length cheese_matrix - 1 in
	let max_s = Array.length cheese_matrix.(0) - 1 in
	
	let rec pozeruh_odviti pozeruh v s =
		if (v = max_v) then
			if (s = max_s) then
			cheese_matrix.(v).(s)
			else
				cheese_matrix.(v).(s) + pozeruh v (s+1)
		else
			if (s = max_s) then
				cheese_matrix.(v).(s) + pozeruh (v+1) s
			else 
				let desno = pozeruh v (s+1) in
				let dol = pozeruh (v+1) s in
				cheese_matrix.(v).(s) + max desno dol
	in
	pozeruh_odviti pozeruh 0 0	
	
(* Rešujemo problem stolpov, ko smo ga spoznali na predavanjih.
   Imamo štiri različne tipe gradnikov, dva modra in dva rdeča.
   Modri gradniki so višin 2 in 3, rdeči pa višin 1 in 2.
   Napiši funkcijo "alternating_towers height", ki za podano višino "height"
   izpiše število različnih stolpov podane višine, kjer se barva gradnikov
   izmenjuje (rdeč na modrem, moder na rdečem itd.).
   Namig: Uporabi dve pomožni funkciji. Za medsebojno rekurzijo uporabi
          ukaz "and".
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)
   

let alternating_towers height = 
	if (height = 1) then
		1
	else
		let rec max_stevilo_rdecih visina = 
			match visina with
			| 0 -> 0
			| 1|2 -> 1
			| visina -> max_stevilo_modrih (visina - 1) + max_stevilo_modrih (visina - 2)
		and max_stevilo_modrih visina =
			match visina with
			| 0|1 -> 0
			| 2 -> 1
			| 3 -> 2
			| visina -> max_stevilo_rdecih (visina - 2) + max_stevilo_rdecih (visina - 3) in
		max_stevilo_rdecih height + max_stevilo_modrih height


(* #use "dinamicno_programiranje.ml";; *)