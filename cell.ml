type status = Alive | Dead

class cell = 
object

	val mutable age = 0

	val mutable status = Dead

	val mutable color = Graphics.white

	method update_color c = color <- c 

	method change_state s = status <- s

	method age i = age <- age + i
end