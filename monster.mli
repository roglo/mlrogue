(* $Id: monster.mli,v 1.18 2005/10/18 14:13:12 roglo Exp $ *)

open Rogue

val mon_name : game -> monster -> string

val mv_mons : game -> unit
val mv_aquators : game -> unit

val confuse : game -> unit
val rust : game -> monster option -> unit
