(* $Id: level.mli,v 1.6 2005/11/06 02:34:10 ddr Exp $ *)

open Rogue

val place_at : game -> objet -> int -> int -> unit
val put_player : game -> int option -> unit
val create : game -> unit

val trap_mess : trap_type -> string
val trap_string : trap_type -> string
