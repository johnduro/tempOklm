

let getName  nn =
	match nn with
	| 1 -> "Methane"
	| 2 -> "Ethane"
	| 3 -> "Propane"
	| 4 -> "Butane"
	| 5 -> "Pentane"
	| 6 -> "Hexane"
	| 7 -> "Heptane"
	| 8 -> "Octane"
	| 9 -> "Nonane"
	| 10 -> "Decane"
	| 11 -> "Undecane"
	| 12 -> "Dodecane"
	| 16 -> "Hexadecane"
	| 20 -> "Icosane"
	| 30 -> "Triacontane"
	| 40 -> "Tetracontane"
	| 50-> "Pentacontane"
	| 60 -> "Hexacontane"
	| _ -> "wut ?"

class virtual alkanes n =
	object (self)
		val _n = n

		inherit Molecules.molecule (getName n) (Molecules.genMolLst [(new Atoms.hydrogen, ((2 * n) + 2)); (new Atoms.carbon, n)] [])
	end

class methane =
	object (self)
		inherit alkanes 1
	end

class ethane =
	object (self)
		inherit alkanes 2
	end

class octane =
	object (self)
		inherit alkanes 8
	end

class decane =
	object (self)
		inherit alkanes 10
	end


class dodecane =
	object (self)
		inherit alkanes 20
	end

class hexacontane =
	object (self)
		inherit alkanes 60
	end
