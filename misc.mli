(* $Id: misc.mli,v 1.34 2010/03/23 12:15:35 deraugla Exp $ *)

open Rogue;

value gmc : game -> monster -> char;
value tgmc : game -> char -> char;
value itgmc : game -> char -> char;

value level_points : array int;
value add_exp : game -> int -> (game -> int) -> unit;

value is_direction : char -> bool;
value is_passable : game -> int -> int -> bool;
value can_move : game -> int -> int -> int -> int -> bool;
value fast : game -> bool;

value gold_at : game -> int -> int -> bool;
value monster_at : game -> int -> int -> monster;
value object_at : game -> int -> int -> objet;
value trap_at : game -> int -> int -> option trap_type;

value imitating : game -> int -> int -> bool;

value take_from_pack : game -> char -> unit;
value take_from_monsters : game -> monster -> unit;

value get_mask_char : objet -> char;
value get_dungeon_char : game -> int -> int -> char;
value get_dir_rc : char -> int -> int -> bool -> (int * int);
value vanish : game -> char -> objet -> unit;

value unwield : game -> unit;
value unwear : game -> unit;
value un_put_on : game -> ring -> unit;
value ring_stats : game -> unit;

value get_letter_object : game -> char -> bool -> option objet;

value relight : game -> unit;
value light_passage : game -> int -> int -> unit;
value light_up_room : game -> int -> unit;

value save_into_file : game -> string -> unit;
value restore : string -> game;

value show_rogue : game -> unit;
value show_monster : game -> int -> int -> monster -> char -> unit;
value show_trap : int -> int -> trap_type -> unit;
value display_dungeon : game -> array (array char) -> unit;

value show_monsters : game -> unit;
