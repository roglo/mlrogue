(* $Id: rob_position.mli,v 1.2 2010/05/09 05:01:15 deraugla Exp $ *)

type position = { row : int; col : int };;

type ('a, 't) PosMap =
  { PosMap_empty : 't;
    PosMap_add : position -> 'a -> 't -> 't;
    PosMap_mem : position -> 't -> bool;
    PosMap_find : position -> 't -> 'a }
;;

type PosSet_t;;

value PosSet_empty : PosSet_t;;
value PosSet_add : position -> PosSet_t -> PosSet_t;;
value PosSet_mem : position -> PosSet_t -> bool;;
