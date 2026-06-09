(* $Id: rob_position.ml,v 1.1 2010/05/08 20:12:31 deraugla Exp $ *)

(*
type ComparePos = struct type t = position let compare = compare end
module PosMap = Map.Make (ComparePos)
module PosSet = Set.Make (ComparePos)
*)

type 'a PosMap_t == (position * 'a) list;;

let PosMap_empty = [];;

let rec PosMap_add pos a t =
  match t with
  | [] -> [(pos, a)]
  | (p, b) :: t' ->
      if pos.row < p.row then (pos, a) :: (p, b) :: t'
      else if pos.row > p.row then (p, b) :: PosMap_add pos a t'
      else if pos.col < p.col then (pos, a) :: (p, b) :: t'
      else if pos.col > p.col then (p, b) :: PosMap_add pos a t'
      else (p, a) :: t'
;;

let rec PosMap_mem pos t =
  match t with
  | [] -> false
  | (p, a) :: t' ->
      if pos.row < p.row then false
      else if pos.row > p.row then PosMap_mem pos t'
      else if pos.col < p.col then false
      else if pos.col > p.col then PosMap_mem pos t'
      else true
;;

let rec PosMap_find pos t =
  match t with
  | [] -> raise Not_found
  | (p, a) :: t' ->
      if pos.row < p.row then raise Not_found
      else if pos.row > p.row then PosMap_find pos t'
      else if pos.col < p.col then raise Not_found
      else if pos.col > p.col then PosMap_find pos t'
      else a
;;

type PosSet_t == position list;;

let PosSet_empty = [];;

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
