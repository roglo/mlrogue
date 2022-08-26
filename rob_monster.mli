(* $Id: rob_monster.mli,v 1.4 2010/05/10 17:47:51 deraugla Exp $ *)

open Rob_def;
open Rob_misc;
open Rob_position;

type monster_power = list (int * list (int * int));

value is_flaming_monster : game -> char -> bool;
value is_fliting_monster : game -> char -> bool;
value is_freezing_monster : game -> char -> bool;
value is_holding_monster : game -> char -> bool;
value is_mean_monster : game -> char -> bool;
value is_not_attackable_monster : game -> char -> bool;
value is_attackable_monster : game -> char -> bool;
value is_monster_attackable_at_distance : game -> char -> bool;

value write_monster_power_list_fname : array monster_power -> string -> unit;
value get_monster_power_list : t -> array monster_power;
value set_monster_power : game -> t -> char -> int -> unit;
value basic_monster_power :
  game -> t -> char -> (monster_power -> list (int * int)) -> int;
value monster_power_at_level : game -> t -> char -> int;

value monsters_and_moves_around : game -> (list move * list move);
value monsters_around : game -> position -> list move;
value monster_around : (char -> bool) -> game -> position -> option move;
value holding_monster_around : game -> position -> option move;
value flaming_monster_around : game -> position -> option move;
value aquator_around : game -> bool;
value flaming_monster_dir :
  game -> position -> option (move * int * position);
value monster_moving_to : game -> t -> position -> bool;
