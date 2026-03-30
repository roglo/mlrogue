(* $Id: move.mli,v 1.12 2005/10/17 23:20:24 ddr Exp $ *)

open Rogue

val one_move_rogue : game -> char -> bool -> unit
val multiple_move_rogue : game -> char -> unit
val move_onto : game -> unit
val kick_into_pack : game -> unit
val search : game -> int -> bool -> unit
val id_trap : game -> unit

val reg_move : game -> unit

val unhallucinate : game -> unit
val unblind : game -> unit
val unconfuse : game -> unit

val tele : game -> unit
val take_a_nap : game -> unit

val fight : game -> bool -> unit

(**)

val wake_room : game -> int -> bool -> int -> int -> unit
