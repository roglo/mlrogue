(* $Id: rob_position.mli,v 1.2 2010/05/09 05:01:15 deraugla Exp $ *)

type position = { row : int; col : int };;

type ('a, 't) PosMap =
  { empty : 't;
    add : position -> 'a -> 't -> 't;
    mem : position -> 't -> bool;
    find : position -> 't -> 'a }
;;

type PosSet_t;;

type PosSet =
  { empty : PosSet_t;
    add : position -> PosSet_t -> PosSet_t;
    mem : position -> PosSet_t -> bool }
;;
