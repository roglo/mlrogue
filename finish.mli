(* $Id: finish.mli,v 1.3 2005/10/20 11:55:47 roglo Exp $ *)

open Rogue;

value clean_up : string -> _;
value put_scores : string -> bool -> option (game * ending) -> unit;
value win : game -> _;
value killed_by : game -> ending -> _;
