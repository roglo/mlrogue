(* $Id: imisc.mli,v 1.18 2010/04/27 09:52:33 deraugla Exp $ *)

open Rogue

val get_rand : int -> int -> int
val rand_percent : int -> bool
val coin_toss : unit -> bool
val rand_around : game -> int -> int -> int -> int * int
val gr_row_col : game -> int -> int -> int * int * int
val gr_obj_char : unit -> char

val has_amulet : game -> bool
val rogue_can_see : game -> int -> int -> bool
val rogue_is_around : game -> int -> int -> bool
val hp_raise : game -> int

val get_room_number : game -> int -> int -> int option
val get_damage : game -> damage -> bool -> int
val get_armor_class : (char * armor) option -> int

val pack_count : game -> objet option -> int
val add_to_pack : game -> objet -> char * objet

val do_wear : game -> char -> armor -> unit
val do_wield : game -> char -> weapon -> unit

val put_m_at : game -> int -> int -> monster -> unit
val wake_up : monster -> unit

val nth_field : string -> int -> string
val string_eq : string -> int -> string -> int -> int -> bool
