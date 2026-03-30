(* $Id: rob_position.mli,v 1.2 2010/05/09 05:01:15 deraugla Exp $ *)

type position = { row : int; col : int }

module PosMap :
  sig
    type 'a t
    val empty : 'a t
    val add : position -> 'a -> 'a t -> 'a t
    val mem : position -> 'a t -> bool
    val find : position -> 'a t -> 'a
  end

module PosSet :
  sig
    type t
    val empty : t
    val add : position -> t -> t
    val mem : position -> t -> bool
  end
