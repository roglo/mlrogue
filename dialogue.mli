(* $Id: dialogue.mli,v 1.5 2010/02/02 15:57:47 deraugla Exp $ *)

open Rogue;

value name_of : game -> objet -> string;

value armor_desc : game -> armor -> string;
value ring_desc : game -> ring -> bool -> string;
value get_desc : game -> objet -> bool -> string;

value rgetchar : game -> char;
value sound_bell : unit -> unit;

value message : game -> string -> bool -> unit;
value remessage : game -> unit;
value check_message : game -> unit;

value print_stats : game -> int -> unit;

value inv_sel :
  game -> list (char * objet) -> (object_kind -> bool) -> string -> string ->
    option char;
value pack_letter : game -> string -> (object_kind -> bool) -> char;

value inventory :
  game -> list (char * objet) -> (object_kind -> bool) -> unit;

value new_object_for_wizard : game -> unit;
