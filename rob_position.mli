(* $Id: rob_position.mli,v 1.2 2010/05/09 05:01:15 deraugla Exp $ *)

type position = { row : int; col : int };;

type 'a PosMap_t;;

value PosMap_empty : 'a PosMap_t;;
value PosMap_add : position -> 'a -> 'a PosMap_t -> 'a PosMap_t;;
value PosMap_mem : position -> 'a PosMap_t -> bool;;
value PosMap_find : position -> 'a PosMap_t -> 'a;;

type PosSet_t;;

value PosSet_empty : PosSet_t;;
value PosSet_add : position -> PosSet_t -> PosSet_t;;
value PosSet_mem : position -> PosSet_t -> bool;;
