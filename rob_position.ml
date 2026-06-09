(* $Id: rob_position.ml,v 1.1 2010/05/08 20:12:31 deraugla Exp $ *)

(*
type position = { row : int; col : int };;
*)

(*
type ComparePos = struct type t = position let compare = compare end
module PosMap = Map.Make (ComparePos)
module PosSet = Set.Make (ComparePos)
*)

type PosSet_t == position list;;

let rec PosSet_add pos t =
  match t with
  | [] -> [pos]
  | p :: t' ->
      if pos.row < p.row then pos :: p :: t'
      else if pos.row > p.row then p :: PosSet_add pos t'
      else if pos.col < p.col then pos :: p :: t'
      else if pos.col > p.col then p :: PosSet_add pos t'
      else p :: t'
;;

let rec PosSet_mem pos t =
  match t with
  | [] -> false
  | p :: t' ->
      if pos.row < p.row then false
      else if pos.row > p.row then PosSet_mem pos t'
      else if pos.col < p.col then false
      else if pos.col > p.col then PosSet_mem pos t'
      else true
;;

let PosSet =
  { empty = [];
    add = PosSet_add;
    mem = PosSet_mem }
;;
