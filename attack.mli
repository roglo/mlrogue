(* $Id: attack.mli,v 1.3 2007/02/03 14:31:12 deraugla Exp $ *)

open Rogue;

value rogue_hit : game -> monster -> bool -> unit;
value one_throw : game -> char -> (char * objet) -> bool;
value zap : game -> bool;
