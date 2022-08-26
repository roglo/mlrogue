(* $Id: move.mli,v 1.12 2005/10/17 23:20:24 ddr Exp $ *)

open Rogue;

value one_move_rogue : game -> char -> bool -> unit;
value multiple_move_rogue : game -> char -> unit;
value move_onto : game -> unit;
value kick_into_pack : game -> unit;
value search : game -> int -> bool -> unit;
value id_trap : game -> unit;

value reg_move : game -> unit;

value unhallucinate : game -> unit;
value unblind : game -> unit;
value unconfuse : game -> unit;

value tele : game -> unit;
value take_a_nap : game -> unit;

value fight : game -> bool -> unit;

(**)

value wake_room : game -> int -> bool -> int -> int -> unit;
