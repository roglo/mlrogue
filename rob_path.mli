(* $Id: rob_path.mli,v 1.5 2010/07/01 13:35:02 deraugla Exp $ *)

open Rob_def;
open Rob_position;

value direct_path_excl :
  game -> list position -> position -> (list position -> position -> bool) ->
    option (list position * position);
value gen_path_in_room_to :
  game -> room -> (position -> bool) -> position -> position ->
    option (position * list position);

value path_in_room_to2 :
  game -> room -> list position -> position -> position ->
    option (list position);
value path_in_room_to :
  game -> room -> list position -> position -> position -> option (list move);

value one_step_to : game -> position -> (move * list move);
value one_step_to2 : game -> position -> (position * list position);

value path_excl_from_to :
  game -> list position -> position -> position -> option global_path;
value old_path_excl_from_to :
  game -> list position -> position -> position -> int -> global_path;

value path_to : game -> position -> position -> option global_path;
value old_path_to : game -> position -> position -> global_path;
value path_in_room_excl_mon :
  game -> room -> position -> position -> global_path;

value paths_in_corridors_from :
  game -> position -> position -> list (list position * position);
value path_to_closest2 :
  game -> position -> (list position -> position -> bool) ->
    option global_path;

value monster_path : game -> position -> position -> option (list position);

value make_graph : game -> bool -> graph;
value reinit_graph_search : game -> graph -> unit;
value nothing_to_search : graph -> bool;
value path_to_closest :
  game -> graph -> position -> option (list position * position * around);

value path_to_closest_gold : game -> t -> position -> option global_path;
value path_to_closest_static_monster :
  game -> t -> position -> option global_path;

value find_random_around : game -> char -> option position;
