(* $Id: imisc.mli,v 1.18 2010/04/27 09:52:33 deraugla Exp $ *)

open Rogue;

value get_rand : int -> int -> int;
value rand_percent : int -> bool;
value coin_toss : unit -> bool;
value rand_around : game -> int -> int -> int -> (int * int);
value gr_row_col : game -> int -> int -> (int * int * int);
value gr_obj_char : unit -> char;

value has_amulet : game -> bool;
value rogue_can_see : game -> int -> int -> bool;
value rogue_is_around : game -> int -> int -> bool;
value hp_raise : game -> int;

value get_room_number : game -> int -> int -> option int;
value get_damage : game -> damage -> bool -> int;
value get_armor_class : option (char * armor) -> int;

value pack_count : game -> option objet -> int;
value add_to_pack : game -> objet -> (char * objet);

value do_wear : game -> char -> armor -> unit;
value do_wield : game -> char -> weapon -> unit;

value put_m_at : game -> int -> int -> monster -> unit;
value wake_up : monster -> unit;

value nth_field : string -> int -> string;
value string_eq : string -> int -> string -> int -> int -> bool;
