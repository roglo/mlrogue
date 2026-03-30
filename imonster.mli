(* $Id: imonster.mli,v 1.3 2010/03/23 12:15:35 deraugla Exp $ *)

open Rogue

val gr_monster : game -> int option -> monster
val visible_mon_name : game -> int -> string
val mon_init_hp : game -> int -> int
