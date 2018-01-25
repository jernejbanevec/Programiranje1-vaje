
let primer = [| [| 2 ; 4 ; 1 ; 1 |];
                [| 3 ; 2 ; 0 ; 5 |];
				[| 8 ; 0 ; 7 ; 2 |] |]
				
let rec max_apple apple_matrix max_jablan =
	let max_v = Array.length apple_matrix - 1 in
	let max_s = Array.length apple_matrix.(0) - 1 in
	
	let rec pozeruh v s n =
		if n > 0 then
			if (v = max_v) then
				if (s = max_s) then
					apple_matrix.(v).(s) 
				else
					apple_matrix.(v).(s) + pozeruh v (s+1) (n-1)
			else	
				if (s = max_s) then
					apple_matrix.(v).(s)
				else
					apple_matrix.(v).(s) + max (pozeruh v (s+1) (n-1)) (pozeruh (v+1) 0 (n-1))
		else apple_matrix.(v).(s)
	in
	pozeruh 0 0 max_jablan
	
	
	
(* Časovna zahtevnost je O(max_v * max_s), 
 saj se v najslabšem primeru algoritem izvede na (število elementov matrike - max_v) mestih,
 kar spada v prej navedeno časovno zahtevnost*)