type status = Alive | Dead

class cell (a : int) (st : status) c = 
object

	(* Starting age, 0 *)
	val mutable age = a

	(* Starting status, dead *)
	val mutable status = st

	(* Starting color, white - should check that this will actually work *)
	val mutable color = c

	(* Updates the color to the value c *)
	method update_color c = color <- c 

	(* Kills or resurrects the cell *)
	method change_state s = status <- s

	(* Ages the cell by a specific amount *)
	method age i = age <- age + i
end

class dead_cell = 
object

	(* Starting age, 0 *)
	val mutable age = 0

	(* Starting status, dead *)
	val mutable status = Dead

	(* Starting color, white - should check that this will actually work *)
	val mutable color = Graphics.white

	(* Updates the color to the value c *)
	method update_color c = color <- c 

	(* Kills or resurrects the cell *)
	method change_state s = status <- s

	(* Ages the cell by a specific amount *)
	method age i = age <- age + i
end