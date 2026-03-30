(* $Id: rob_monster.mli,v 1.4 2010/05/10 17:47:51 deraugla Exp $ *)

open Rob_def
open Rob_misc
open Rob_position

type monster_power = (int * (int * int) list) list

val is_flaming_monster : game -> char -> bool
val is_fliting_monster : game -> char -> bool
val is_freezing_monster : game -> char -> bool
val is_holding_monster : game -> char -> bool
val is_mean_monster : game -> char -> bool
val is_not_attackable_monster : game -> char -> bool
val is_attackable_monster : game -> char -> bool
val is_monster_attackable_at_distance : game -> char -> bool

val write_monster_power_list_fname : monster_power array -> string -> unit
val get_monster_power_list : t -> monster_power array
val set_monster_power : game -> t -> char -> int -> unit
val basic_monster_power :
  game -> t -> char -> (monster_power -> (int * int) list) -> int
val monster_power_at_level : game -> t -> char -> int

val monsters_and_moves_around : game -> move list * move list
val monsters_around : game -> position -> move list
val monster_around : (char -> bool) -> game -> position -> move option
val holding_monster_around : game -> position -> move option
val flaming_monster_around : game -> position -> move option
val aquator_around : game -> bool
val flaming_monster_dir : game -> position -> (move * int * position) option
val monster_moving_to : game -> t -> position -> bool
