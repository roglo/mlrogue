(* $Id: misc.mli,v 1.34 2010/03/23 12:15:35 deraugla Exp $ *)

open Rogue

val gmc : game -> monster -> char
val tgmc : game -> char -> char
val itgmc : game -> char -> char

val level_points : int array
val add_exp : game -> int -> (game -> int) -> unit

val is_direction : char -> bool
val is_passable : game -> int -> int -> bool
val can_move : game -> int -> int -> int -> int -> bool
val fast : game -> bool

val gold_at : game -> int -> int -> bool
val monster_at : game -> int -> int -> monster
val object_at : game -> int -> int -> objet
val trap_at : game -> int -> int -> trap_type option

val imitating : game -> int -> int -> bool

val take_from_pack : game -> char -> unit
val take_from_monsters : game -> monster -> unit

val get_mask_char : objet -> char
val get_dungeon_char : game -> int -> int -> char
val get_dir_rc : char -> int -> int -> bool -> int * int
val vanish : game -> char -> objet -> unit

val unwield : game -> unit
val unwear : game -> unit
val un_put_on : game -> ring -> unit
val ring_stats : game -> unit

val get_letter_object : game -> char -> bool -> objet option

val relight : game -> unit
val light_passage : game -> int -> int -> unit
val light_up_room : game -> int -> unit

val save_into_file : game -> string -> unit
val restore : string -> game

val show_rogue : game -> unit
val show_monster : game -> int -> int -> monster -> char -> unit
val show_trap : int -> int -> trap_type -> unit
val display_dungeon : game -> char array array -> unit

val show_monsters : game -> unit
