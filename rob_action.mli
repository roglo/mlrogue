(* $Id: rob_action.mli,v 1.5 2010/05/25 16:48:27 deraugla Exp $ *)

open Rob_def
open Rob_misc
open Rob_position

type result = command * next_action * move option

val run_away_if_possible :
  game -> bool -> move list -> move -> (move * move list) option

val stop_paradise : t -> _
val start_drop_scare : position -> char -> drop_scare

val start_search : game -> t -> graph -> result

val go_to_stairs :
  game -> t -> graph -> position -> position -> bool -> result
val random_move : game -> position -> next_action -> result

val slow_down : game -> t -> unit

val apply : game -> t -> string -> result
