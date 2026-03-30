(* $Id: use.mli,v 1.5 2005/10/17 23:20:24 ddr Exp $ *)

open Rogue

val show_objects : game -> unit

val wield : game -> unit
val take_off : game -> unit
val put_on_ring : game -> unit

val wear : game -> unit
val remove_ring : game -> unit

val inv_rings : game -> unit

val quaff : game -> unit

val draw_magic_map : game -> bool -> unit

val read_scroll : game -> unit
val eat : game -> unit
