(* Conway's Game of Life *)

(* ocamlopt -o life graphics.cmxa lifegame.ml *)

type state = Dead | Alive
type cell = {state : state; age : int}
type grid = cell array array

module View : 
sig
  val size_x : int
  val size_y : int
  val draw : grid -> unit
  val close : unit -> unit
end = 
struct

  open Graphics

  let () =
    open_graph (Sys.getenv "DISPLAY" ^ " 512x512");
    display_mode false;
    set_window_title "Game of Life"

  let size_x = size_x ()
  let size_y = size_y ()

  let draw w =
    let n = Array.length w in
    let m = Array.length w.(0) in
    let x = size_x / n in
    let y = size_y / m in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
	set_color (match w.(i).(j) with Dead -> white | Alive -> black);
	let a = i * x in
	let b = j * y in
	fill_rect a b x y;
      done
    done;
    synchronize ()

  let close = close_graph

end

module Model : 
sig
  val next_state : unit -> unit
  val default_size : int
  val get_x : unit -> int
  val get_y : unit -> int
  val set_x : int -> unit
  val set_y : int -> unit
  val rand : unit -> unit
end = 
struct

  let default_size = 64
  let x = ref default_size
  let y = ref default_size

  let w = ref [||] (* <-- Empty Array *)

  let get_x () = Array.length !w
  let get_y () = Array.length !w.(0)

  let set_x n = 
    assert (n > 0); 
    x := n

  let set_y n =
    assert (n > 0);
    y := n

  let rand =
    Random.self_init ();
    fun () -> 
      w := Array.init !x (fun _ -> Array.init !y 
	(fun _ -> if Random.bool () then Dead else Alive));
      View.draw !w

  let () = rand ()
  (* Random initialization of [w] *)

  let neighbours w x y =
    (* The world is considered as a tore *)
    let h = Array.length w - 1 in
    let v = Array.length w.(0) - 1 in
    let wi = w.(x) in
    let wp = w.(if x = 0 then h else x - 1) in
    let wn = w.(if x = h then 0 else x + 1) in
    let yp = if y = 0 then v else y - 1 in
    let yn = if y = v then 0 else y + 1 in
    [ wp.(y); wp.(yp); wi.(yp); wn.(yp); wn.(y); wn.(yn); wi.(yn); wp.(yn) ]

  let next_one_state x y = 
    let n = 
      List.fold_left 
	(fun a -> function Alive -> a + 1 | Dead -> a) 0 (neighbours !w x y)
    in
    match !w.(x).(y) with
    | Dead  -> if n = 3 then Alive else Dead
    | Alive -> if n < 2 || n > 3 then Dead else Alive

  let next_state () =
    let x = get_x () in
    let y = get_y () in
    w := Array.init x (fun i -> Array.init y (next_one_state i));
    View.draw !w
end

let () = 
  try
    while true do Model.next_state () done
  with Exit ->
    View.close ()
