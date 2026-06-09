(* $Id: rob_position.ml,v 1.1 2010/05/08 20:12:31 deraugla Exp $ *)

(*
type position = { row : int; col : int };;
*)

(*
type ComparePos = struct type t = position let compare = compare end
module PosMap = Map.Make (ComparePos)
module PosSet = Set.Make (ComparePos)
*)

let rec add pos t =
  match ...


let PosSet =
  { empty = [];
    add = add;
    mem : position -> 't -> bool }
  {
