(* $Id: init.mli,v 1.14 2010/04/27 10:15:30 deraugla Exp $ *)

open Rogue;
open Rfield;

type init = [ NewGame of game | RestoreGame of string | ScoreOnly ];

value f :
  array string ->
    (string * init * option (player_species * bool) * option (string * int) *
     bool * bool * bool)
;
