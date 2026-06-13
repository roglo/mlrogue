(* $Id: rob_monster.mli,v 1.4 2010/05/10 17:47:51 deraugla Exp $ *)

#open "rob_def";;
#open "rob_misc";;
#open "rob_position";;

type monster_power == (int * (int * int) list) list;;

value is_flaming_monster : game -> char -> bool;;
value is_fliting_monster : game -> char -> bool;;
value is_freezing_monster : game -> char -> bool;;
value is_holding_monster : game -> char -> bool;;
value is_mean_monster : game -> char -> bool;;
value is_not_attackable_monster : game -> char -> bool;;
value is_attackable_monster : game -> char -> bool;;
value is_monster_attackable_at_distance : game -> char -> bool;;

value write_monster_power_list_fname : monster_power vect -> string -> unit;;
(*
value get_monster_power_list : t -> monster_power vect;;
*)
value set_monster_power : game -> t -> char -> int -> unit;;
value basic_monster_power :
  game -> t -> char -> (monster_power -> (int * int) list) -> int;;
(*
value monster_power_at_level : game -> t -> char -> int;;
*)

value monsters_and_moves_around : game -> move list * move list;;
value monsters_around : game -> position -> move list;;
value monster_around : (char -> bool) -> game -> position -> move option;;
value holding_monster_around : game -> position -> move option;;
value flaming_monster_around : game -> position -> move option;;
value aquator_around : game -> bool;;
value flaming_monster_dir : game -> position -> (move * int * position) option;;
value monster_moving_to : game -> t -> position -> bool;;
