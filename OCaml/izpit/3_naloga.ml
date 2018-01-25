type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
	type t
	val id : t
	val uporabi : t -> vektor -> vektor
	val iz_matrike : matrika -> t
	val iz_funkcije : (vektor -> vektor) -> t
	val kompozitum : t -> t -> t
end

(* a del *)

module Matrika : Linearna = struct
	type t = matrika
	let id = (1, 0, 0, 1)
	let uporabi (a, b, c, d) (x, y) = (a*x + b*y, c*x + d*y)
	let iz_matrike (a, b, c, d) = (a, b, c, d)
	let iz_funkcije (fun (x0, y0) = (a * x0 + b * y0, c * x0 + d * y0)) = (a, b, c, d)
	let kompozitum (a, b, c, d) (e, f, g, h) = (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)
end

(* b del *)

module Funkcija : Linearna = struct
	type t = (vektor -> vektor)
	val id = (fun (x, y) = (x, y))
	val uporabi (fun (x0, y0) = (a * x0 + b * y0, c * x0 + d * yo))(x, y) = (a * x + b * y, c * x + d * y)
	let iz_matrike (a, b, c, d) = (fun (x0, y0) = (a * x0 + b * y0, c * x0 + d * y0))
	let iz_funkcije (fun (x, y) = (x0, y0)) = (fun (x, y) = (x0, y0))
	let kompozitum (fun (x0, y0) = (a0 * x0 + b0 * y0, c0 * x0 + d0 * yo)) (fun (x1, y1) = (a1 * x1 + b1 * y1, c1 * x1 + d1 * y1)) = 
				(fun (x1, y1) = (a0 * (a1 * x1 + b1 * y1) + b0 * (c1 * x1 + d1 * y1), c0 * (a1 * x1 + b1 * y1) + d0 * (c1 * x1 + d1 * y1)))
end