

class virtual reaction (start:Molecules.molecule list) (result:Molecules.molecule list) =
	object (self)
		val _start = start
		val _result = start
		(* method virtual get_start: (Molecules.molecule * int) list *)
		(* method virtual get_result: (Molecules.molecule * int) list *)
		(* method virtual balance: reaction *)
		method virtual is_balanced: bool
	end

let rec print_pairs lst =
	match lst with
	| [] -> print_endline "    ----"
	| (atm, nb)::tl -> print_endline (atm#to_string ^ " * " ^ (string_of_int nb)); print_pairs tl

class alkane_combustion (start:Molecules.molecule list) (result:Molecules.molecule list) =
	object (self)
		val _start = start
		val _result = result

		(* method get_start: (Molecules.molecule * int) list *)
		(* method get_result: (Molecules.molecule * int) list *)
		(* method balance: reaction *)
		method is_balanced =
			let getAtmFromLst (lstMol:Molecules.molecule list) =
				let rec loopAtm ls ret =
					match ls with
					| [] -> ret
					| hd::tl -> loopAtm tl (ret @ hd#getAtoms)
				in
				let formula (atomLst:Atoms.atom list) =
					let rec notIn atm lst =
						match lst with
						| [] -> true
						| (sb, _)::tl when sb#symbol = atm#symbol -> false
						| hd::tl -> notIn atm tl
					in
					let rec countIn (atm:Atoms.atom) (lst:Atoms.atom list) ret =
						match lst with
						| [] -> ret
						| hd::tl when hd#symbol = atm#symbol -> countIn atm tl (ret + 1)
						| hd::tl -> countIn atm tl ret
					in
					let rec formPair (lst:Atoms.atom list) ret =
						match lst with
						| [] -> ret
| hd::tl when (notIn hd ret) -> formPair tl (ret @ [(hd, (countIn hd atomLst 0))])
						| hd::tl -> formPair tl ret
					in
 					List.sort (fun (sb1, _) (sb2, _) -> if sb1#symbol > sb2#symbol then 1 else if sb1#symbol < sb2#symbol then -1 else 0) (formPair atomLst [])
				in
				formula (loopAtm lstMol [])
			in
			let (atmLstStart:(Atoms.atom * int) list) = getAtmFromLst _start in
			let (atmLstResult:(Atoms.atom * int) list) = getAtmFromLst _result in
			let rec checkBalance ls1 ls2 =
				let rec isIn (atm, nb) lst2 =
					match lst2 with
					| [] -> false
					| (atm2, nb2)::tl when atm = atm2 && nb = nb2 -> true
					| hd::tl -> isIn (atm, nb) tl
				in
				match ls1 with
				| [] -> true
				| hd::tl when isIn hd ls2 -> checkBalance tl ls2
				| hd::tl -> false
			in
			checkBalance atmLstStart atmLstResult

	end
