

class virtual molecule (nam:string) (atomLst:Atoms.atom list) =
	object (self)
		val _name = nam
		val _atoms = atomLst

		method name = _name
		method formula =
			let rec notIn atm lst =
				match lst with
				| [] -> true
				| (sb, _)::tl when sb = atm#symbol -> false
				| hd::tl -> notIn atm tl
			in
			let rec countIn atm lst ret =
				match lst with
				| [] -> ret
				| hd::tl when hd#symbol = atm#symbol -> countIn atm tl (ret + 1)
				| hd::tl -> countIn atm tl ret
			in
			let rec formPair lst ret =
				match lst with
				| [] -> ret
				| hd::tl when (notIn hd ret) -> formPair tl (ret @ [(hd#symbol, (countIn hd _atoms 0))])
				| hd::tl -> formPair tl ret
			in
			let rec toFormula pairs ret =
				match pairs with
				| [] -> ret
				| (sb, nb)::tl -> toFormula tl (ret ^ sb ^ (string_of_int nb))
			in
			toFormula (List.sort (fun (sb1, _) (sb2, _) -> if sb1 > sb2 then 1 else if sb1 < sb2 then -1 else 0) (formPair _atoms [])) ""
		method to_string = (_name ^ " : " ^ self#formula)
		method equals (that:molecule) = (self#name = that#name)
		method getAtoms = _atoms
	end

let rec genMolLst lst ret =
	let rec loop atm nb ret =
		if nb <= 0 then ret
		else
			let cpy = Oo.copy atm in
			loop atm (nb - 1) (ret @ [cpy])
	in
	match lst with
	| [] -> ret
	| (atm, nb)::tl -> genMolLst tl (ret @ (loop atm nb []))

class water =
	object (self)
		inherit molecule "Water" (genMolLst [(new Atoms.hydrogen, 1); (new Atoms.oxygen, 2)] [])
	end

class oxygen =
	object (self)
		inherit molecule "Oxygen" (genMolLst [(new Atoms.oxygen, 2)] [])
	end

class carbonDioxyde =
	object (self)
		inherit molecule "Carbon dioxyde" (genMolLst [(new Atoms.carbon, 1); (new Atoms.oxygen, 2)] [])
	end


class trinitrotoluene =
	object (self)
		inherit molecule "Trinitrotoluene" (genMolLst [(new Atoms.nitrogen, 3); (new Atoms.hydrogen, 5); (new Atoms.carbon, 7); (new Atoms.oxygen, 6)] [])
	end

class chlorophylleA =
	object (self)
		inherit molecule "Chlorophylle a" (genMolLst [(new Atoms.nitrogen, 4); (new Atoms.hydrogen, 72); (new Atoms.carbon, 55); (new Atoms.oxygen, 5); (new Atoms.magnesium, 1)] [])
	end

class methane =
	object (self)
		inherit molecule "Methane" (genMolLst [(new Atoms.hydrogen, 4); (new Atoms.carbon, 1)] [])
	end

class ethylene =
	object (self)
		inherit molecule "Ethylene" (genMolLst [(new Atoms.hydrogen, 4); (new Atoms.carbon, 2)] [])
	end
