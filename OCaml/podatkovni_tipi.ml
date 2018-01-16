(* Vojne čarodejov se nadaljujejo. *)

(*let rec strongest_wizard (wizards : wizard list) : wizard option = 
	match wizards with
	| [] -> None
	| [x] -> Some x
	| x :: y :: tl -> 
		let rec max_wizard (a, b) =
			if a.ability > b.ability then a else b in
		let new_list = max_wizard (x,y) :: tl in
		strongest_wizard new_list*)

(*let narejena_skoda spell (target : wizard) : int =
	match effectiveness (school_of_spell spell) target.race with
	| x -> int_of_float ((coef x) *. float_of_int (mana_of_spell spell)) *)
	

(* Čarodeji; ki se borijo v vojnah so pripadniki teh treh ras.  *)
type race = Orc | Hobbit | Human


(* Uroki [spells] prihajajo iz treh šol [school] magije: firewall in blaze sta ognjena uroka [Fire];
   resurrect in cripple sta nekromantska [Necrotic]; in renounce ter
   banish sta angelska [Angelic].
   Definiraj tipa; ki predstavljata različne uroke in šole magije.
*)


type school = Fire | Necrotic | Angelic


type spell = Firewall | Blaze | Resurrect | Cripple | Renounce | Banish

(* Veščine [skills]; ki jih je čarodej osvojil; so seznam vseh urokov;
   ki jih lahko hitro izvede. Definiraj tip `skills'. *)

type skills = spell list

(* Čarodeja opišemo z imenom; številom življenskih točk [hp]; sposobnost [ability]
   ki jo predstavimo s številom točk mane; raso [race] in veščino [skills].
   To shranimo kot zapisni tip (record). *)

type mana = int
type health = int


type wizard = {name : string; hp : health; ability : mana; race: race ; skills: skills}


(* Napiši funkcijo ki vsakemu uroku priredi primerno šolo magije. *)
let school_of_spell = function
  | Firewall -> Fire
  | Blaze -> Fire
  | Resurrect -> Necrotic
  | Cripple -> Necrotic
  | Renounce -> Angelic
  | Banish -> Angelic

(* Glede na tabelo napiši funkcijo; ki uroku priredi količino mane;
   ki jo čarodej potrebuje za izvršitev uroka:
  blaze : 420
  firewall : 35
  renounce : 17
  banish : 103
  resurrect : 178
  cripple : 250
   Namig: Lahko si pomagaš z regex-replace v Notepad++
 *)
let mana_of_spell = function
  | Firewall -> 35
  | Blaze -> 420
  | Resurrect -> 178
  | Cripple -> 250
  | Renounce -> 17
  | Banish -> 103

(* Ustvari nekaj primerov čarodejov; tako kot je prikazano na primeru Merlina.
   Ponovno si lahko pomagaš s regex-replace.*)
(*
name : "Frodo";      ability : 53;   hp : 1000;  skills : [Renounce];                      race : Hobbit
name : "Ajitam";     ability : 1337; hp : 7331;  skills : [Firewall; Resurrect; Firewall]; race : Hobbit
name : "Mr Duck";    ability : 7;    hp : 90000; skills : [Cripple];                       race : Orc
name : "Kylo Ren";   ability : 589;  hp : 90;    skills : [Resurrect];                     race : Human
name : "Snoop Dogg"; ability : 420;  hp : 4000;  skills : [Blaze];                         race : Orc
*)

(* let merlin : {name : "Merlin";   ability : 1832; hp : 9001; skills : [Renounce; Banish];  race : Human} *)
let frodo =  {name = "Frodo";      ability = 53;   hp = 1000;  skills = [Renounce];                      race = Hobbit}
let ajitam = {name = "Ajitam";     ability = 1337; hp = 7331;  skills = [Firewall; Resurrect; Firewall]; race = Hobbit}
let mrDuck = {name = "Mr Duck";    ability = 7;    hp = 90000; skills = [Cripple];                       race = Orc}
let kYloReN = {name = "Kylo Ren";   ability = 589;  hp = 90;    skills = [Resurrect];                     race = Human}
let snoop_dogg = {name = "Snoop Dogg"; ability = 420;  hp = 4000;  skills = [Blaze];                         race = Orc}


(* Napiši funkcijo; ki iz seznama čarodejev vrne čarodeja z največ mane. *)

		
let rec strongest_wizard (wizards : wizard list) : wizard option =
	match wizards with
	| [] -> None
	| [a] -> Some a
	| hd1 :: hd2 :: tl -> if hd1.ability >= hd2.ability then strongest_wizard (hd1 :: tl)
							else strongest_wizard (hd2 :: tl)
							
							
	(* PRIMER BREZ TEGA KAM SLIKA JE SLABŠI, SAJ NAM SICER NE VRNE NAPAKE KO KODO FUNKCIJO NAROBE NAPIŠEM*)						
(*let rec strongest_wizard wizards =
	match wizards with
	| [] -> None
	| [a] -> Some a
	| hd1 :: hd2 :: tl -> if hd1.ability >= hd2.ability then strongest_wizard (hd1 :: tl)
							else strongest_wizard (hd2 :: tl)*)
							
							
(* Posploši funkcijo strongest_wizard na funkcijo max_list; ki sprejme seznam
   in dodatno funkcijo dveh elementov max : 'a -> 'a -> 'a in vrne maksimalni element seznama
   glede na funkcijo max.
*)

let rec max_list (max_f : 'a -> 'a -> 'a)(l : 'a list): 'a option =
	match l with
	| [] -> None
	| x :: [] -> Some x
	| x::y::xs -> 
		let new_list = (max_f x y)::xs in
		max_list max_f new_list (* TKO SI TI SPISOU *)

		
	(* URADNA REŠITEV - ENA OD NJIJU JE*)
(*let rec max_list (max_f: 'a -> 'a -> 'a) (l: a' list) : 'a option =
	match l with
	| [] -> None
	| x::l -> 
	  begin match max_list max_f l with
        | None -> Some x
        | Some y ->
          Some (max x y)
      end *)
	  
(*let rec max_list (xs : 'a list) (max : 'a -> 'a -> 'a) : 'a option =
  match xs with
  | [] -> None
  | x :: xs ->
    begin match max_list xs max with
      | None -> Some x
      | Some y ->
        Some (max x y)
    end *)
		
		
let max_list3 max_f l = 
	match l with
	| [] -> None
	| hd :: tl -> Some (List.fold_left max_f hd tl)
	
(* Rase imajo različno občutljivost [vulnerability] na določene šole magije.
   Napiši tip s katerim lahko izraziš kdaj ima rasa visoko [High]; navadno [Normal]
   ali pa nizko [Low] občutljivost na urok. *)

type vulnerability = High | Normal | Low


   
(* Napiši funkcijo; ki glede na šolo uroka in raso izračuna občutljivost.
   Low za:     orcs:necrotic; hobbits:fire; humans:angelic;
   High za:    hobbit:necrotic; human:fire; orc:angelic
   Sicer vrne Normal
*)

(* let set_vulnerability wizard spell=
	match (wizard.race, school_of_spell spell) with
	| (Orc,Necrotic) -> Low
	| (Hobbit,Fire) -> Low
	| (Human,Angelic) -> Low
	| (Hobbit,Necrotic) -> High
	| (Human,Fire) -> High
	| (Orc,Angelic) -> High
	| (_,_) -> Normal *) (* SAMO PREKOPIRAL NA SPODNJO *)

let effectiveness (school : school) (race : race) : vulnerability =
	match (race, school) with
	| (Orc,Necrotic) -> Low
	| (Hobbit,Fire) -> Low
	| (Human,Angelic) -> Low
	| (Hobbit,Necrotic) -> High
	| (Human,Fire) -> High
	| (Orc,Angelic) -> High
	| (_,_) -> Normal


(* Zapiši funkcijo; ki za čarodeja izračuna njegovo občutljivost na podani urok. *)

let vulnerable spell wizard= effectiveness (school_of_spell spell) wizard.race 
	


(* Občutljivost se v boju izrazi kot koeficient škode; ki jo utrpi čarodej; če ga urok zadane.
   Zapiši funkcijo; ki glede na občutljivost vrne primeren koeficient; tako da čarodej z nizko
   občutljivostjo utrpi le pol škode; čarodej z visoko občutljivostjo pa dvakratnik.*)

let coef = function
	| Low -> 0.5
	| High -> 2.
	| Normal -> 1.
	
   
(* Vsak urok naredi toliko škode; kot je potrebnih točk mane za izvršitev uroka.
   Napiši funkcijo; ki glede na urok in čarodeja izračuna koliko škode utrpi;
   če ga urok zadane.
   Namig: za pretvarjanje med int in float se uporabljata funkciji float_of_int in
   int_of_float.
*)

let narejena_skoda (spell : spell) (target : wizard) : int =
	int_of_float((coef (effectiveness (school_of_spell spell) target.race)) *. float_of_int((mana_of_spell spell)))


(* Zapiši funkcijo; ki vrne novo stanje čarodeja (z znižanimi življenskimi točkami [hp]);
   po tem; ko ga je zadel izbrani urok.
   (Novo stanje čarodeja je prav tako tipa wizard)
*)

(*let Wizard_after wizard spell = {wizard with hp = wizard.hp - narejena_skoda spell wizard}*)

let attack wizard spell = {wizard with hp = wizard.hp - (narejena_skoda spell wizard)}

(* Napiši funkcijo; ki za danega čarovnika izvršuje uroke; dokler ne izvede vseh urokov
   na seznamu; ali pa mu zmanjka točk mane. *)
   
let cast_spells (caster : wizard) : wizard * spell list = 
	let moc = caster.ability in
		let sez = caster.skills in
			let rec izmuci_casterja_aux moc sez acc =
				match (moc, sez) with
				| (0, _) -> ({caster with ability = 0}, List.rev acc)
				| (a, []) -> ({caster with ability = a},List.rev acc)
				| (m, hd :: tl) -> if m > (mana_of_spell hd) then izmuci_casterja_aux (moc - (mana_of_spell hd)) tl (hd :: acc) else ({caster with ability = m},List.rev acc)
			in izmuci_casterja_aux moc sez []



(* Napiši funkcijo; ki simulira spopad dveh čarodejev. V primeru; ko napadalec ne more izvršiti
   nobenega uroka; napadalec izgubi. V nasprotnem primeru uporabi vse uroke; ki jih lahko.
   Če branilcu zmanjka življenskih točk; izgubi; sicer pa se vlogi napadalca in branilca zamenjata.
*)
let rec duel (attacker : wizard) (defender : wizard) : wizard =
	let defender =
		if attacker.hp > 0 then
			let (attacker, napadi) = cast_spells defender in
				match napadi with
				| [] -> defender
				| _ -> 
					let defender = List.fold_left attack defender napadi in
					duel defender attacker
		else defender
	in defender
	
let _ = duel frodo snoop_dogg