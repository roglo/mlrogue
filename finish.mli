(* $Id: finish.mli,v 1.3 2005/10/20 11:55:47 roglo Exp $ *)

open Rogue

val clean_up : string -> _
val put_scores : string -> bool -> (game * ending) option -> unit
val win : game -> _
val killed_by : game -> ending -> _
