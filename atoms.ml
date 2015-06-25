

class virtual atom nm symb atnb =
	object (self)
		val _name = nm
		val _symbol = symb
		val _atomic_number = atnb

		method name = _name
		method symbol = _symbol
		method atomic_number = _atomic_number
		method to_string = "[ " ^ (string_of_int _atomic_number) ^ " ] " ^ _name ^ " - " ^  _symbol
		method equals (that:atom) = self#name = that#name
	end


class hydrogen =
	object (self)
		inherit atom "Hydrogen" "H" 1
	end

class carbon =
	object (self)
		inherit atom "Carbon" "C" 6
	end

class oxygen =
	object (self)
		inherit atom "Oxygen" "O" 8
	end

class polonium =
	object (self)
		inherit atom "Polonium" "Po" 84
	end

class gold =
	object (self)
		inherit atom "Gold" "Au" 79
	end

class titanium =
	object (self)
		inherit atom "Titanium" "Ti" 22
	end

class francium =
	object (self)
		inherit atom "Francium" "Fr" 87
	end

class nickel =
	object (self)
		inherit atom "Nickel" "Ni" 28
	end

class nitrogen =
	object (self)
		inherit atom "Nitrogen" "N" 7
	end

class magnesium =
	object (self)
		inherit atom "Magnesium" "Mg" 12
	end
