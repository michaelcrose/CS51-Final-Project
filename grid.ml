open Core
open cell

module type Grid =
sig
  type 'a array array
  val width : int
  val height : int
  val make_random : int -> int -> 'a -> 'a array
  val make_acorn : int -> int -> 'a -> 'a array
  val make_square : int -> int -> 'a -> 'a array
end

let argc = Array.length Sys.argv in
let width = if argc > 2 then int_of_string Sys.argv.(2) else 500 in
let height = if argc > 3 then int_of_string Sys.argv.(3) else 500 in


let init =
     let default () = (print_endline "Using random start"; make_random) in
     if argc < 2 then default () else
     match Sys.argv.(1) with
        | "acorn" -> make_acorn
        | “square -> make_square
        | "random" -> make_random
        | "-h" -> Printf.printf
           "Usage: %s [acorn|square|random] width height\n" Sys.argv.(0);
           exit 0
        | _ -> default () in
