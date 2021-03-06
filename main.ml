(* Molecules are cool, but today we’re focusing on a particular type of molecules, which *)
(* are alkanes. Alkanes are a family of simple molecules composed of just carbon and *)
(* hydrogen, which means we can create alkanes easily! The formula of an acyclic alkane is *)
(* CnH2n+2. The name of the alkane simply depends on the value you give to n. *)
(* As such, your alkane’s constructor only needs one parameter, which is n. It must be *)
(* able to guess the name and the formula from this only n parameter. *)
(* Note that an alkane is still a molecule, which means you still have to provide the *)
(* following methods: *)

(* • name *)
(* • formula *)
(* • to_string *)
(* • equals *)

(* To go along with your alkane class, you will write...some real alkanes! As usual. Of *)
(* course, they will inherit your alkane class and you will write at least the following ones: *)

(* • methane *)
(* • ethane *)
(* • octane *)

let main () =
	let reac = new Reactions.alkane_combustion [new Alkanes.methane; new Molecules.oxygen] [new Molecules.carbonDioxyde;new Molecules.water] in
	print_endline (string_of_bool reac#is_balanced)
	(* let meth = new Alkanes.methane in *)
	(* let eth = new Alkanes.ethane in *)
	(* let oct = new Alkanes.octane in *)
	(* let dec = new Alkanes.decane in *)
	(* let dod = new Alkanes.dodecane in *)
	(* let hex = new Alkanes.hexacontane in *)
	(* print_endline meth#to_string; *)
	(* print_endline eth#to_string; *)
	(* print_endline oct#to_string; *)
	(* print_endline dec#to_string; *)
	(* print_endline dod#to_string; *)
	(* print_endline hex#to_string; *)
	(* print_endline "hexacontane : "; *)
	(* print_endline hex#name; *)
	(* print_endline hex#formula; *)
	(* print_endline ("hexacontane ?= hexacontane -> " ^ (string_of_bool (hex#equals hex))); *)
	(* print_endline ("hexacontane ?= octane -> " ^ (string_of_bool (hex#equals oct))) *)

let () = main ()
