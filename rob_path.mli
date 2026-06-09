(* $Id: rob_path.mli,v 1.5 2010/07/01 13:35:02 deraugla Exp $ *)

#open "rob_def";;
#open "rob_position";;

value direct_path_excl :
  game -> position list -> position -> (position list -> position -> bool) ->
    (position list * position) option;;
value gen_path_in_room_to :
  game -> room -> (position -> bool) -> position -> position ->
    (position * position list) option;;

value path_in_room_to2 :
  game -> room -> position list -> position -> position ->
    position list option;;
value path_in_room_to :
  game -> room -> position list -> position -> position -> move list option;;

value one_step_to : game -> position -> move * move list;;
value one_step_to2 : game -> position -> position * position list;;

value path_excl_from_to :
  game -> position list -> position -> position -> global_path option;;
value old_path_excl_from_to :
  game -> position list -> position -> position -> int -> global_path;;

value path_to : game -> position -> position -> global_path option;;
value old_path_to : game -> position -> position -> global_path;;
value path_in_room_excl_mon :
  game -> room -> position -> position -> global_path;;

value paths_in_corridors_from :
  game -> position -> position -> (position list * position) list;;
value path_to_closest2 :
  game -> position -> (position list -> position -> bool) ->
    global_path option;;

value monster_path : game -> position -> position -> position list option;;

value make_graph : game -> bool -> graph;;
value reinit_graph_search : game -> graph -> unit;;
value nothing_to_search : graph -> bool;;
value path_to_closest :
  game -> graph -> position -> (position list * position * around) option;;

value path_to_closest_gold : game -> t -> position -> global_path option;;
value path_to_closest_static_monster :
  game -> t -> position -> global_path option;;

value find_random_around : game -> char -> position option;;
