(* $Id: rob_path.mli,v 1.5 2010/07/01 13:35:02 deraugla Exp $ *)

open Rob_def
open Rob_position

val direct_path_excl :
  game -> position list -> position -> (position list -> position -> bool) ->
    (position list * position) option
val gen_path_in_room_to :
  game -> room -> (position -> bool) -> position -> position ->
    (position * position list) option

val path_in_room_to2 :
  game -> room -> position list -> position -> position ->
    position list option
val path_in_room_to :
  game -> room -> position list -> position -> position -> move list option

val one_step_to : game -> position -> move * move list
val one_step_to2 : game -> position -> position * position list

val path_excl_from_to :
  game -> position list -> position -> position -> global_path option
val old_path_excl_from_to :
  game -> position list -> position -> position -> int -> global_path

val path_to : game -> position -> position -> global_path option
val old_path_to : game -> position -> position -> global_path
val path_in_room_excl_mon :
  game -> room -> position -> position -> global_path

val paths_in_corridors_from :
  game -> position -> position -> (position list * position) list
val path_to_closest2 :
  game -> position -> (position list -> position -> bool) ->
    global_path option

val monster_path : game -> position -> position -> position list option

val make_graph : game -> bool -> graph
val reinit_graph_search : game -> graph -> unit
val nothing_to_search : graph -> bool
val path_to_closest :
  game -> graph -> position -> (position list * position * around) option

val path_to_closest_gold : game -> t -> position -> global_path option
val path_to_closest_static_monster :
  game -> t -> position -> global_path option

val find_random_around : game -> char -> position option
