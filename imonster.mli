(* $Id: imonster.mli,v 1.3 2010/03/23 12:15:35 deraugla Exp $ *)

open Rogue;

value gr_monster : game -> option int -> monster;
value visible_mon_name : game -> int -> string;
value mon_init_hp : game -> int -> int;
