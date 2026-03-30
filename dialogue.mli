(* $Id: dialogue.mli,v 1.5 2010/02/02 15:57:47 deraugla Exp $ *)

open Rogue

val name_of : game -> objet -> string

val armor_desc : game -> armor -> string
val ring_desc : game -> ring -> bool -> string
val get_desc : game -> objet -> bool -> string

val rgetchar : game -> char
val sound_bell : unit -> unit

val message : game -> string -> bool -> unit
val remessage : game -> unit
val check_message : game -> unit

val print_stats : game -> int -> unit

val inv_sel :
  game -> (char * objet) list -> (object_kind -> bool) -> string -> string ->
    char option
val pack_letter : game -> string -> (object_kind -> bool) -> char

val inventory : game -> (char * objet) list -> (object_kind -> bool) -> unit

val new_object_for_wizard : game -> unit
