(* $Id: monster.mli,v 1.18 2005/10/18 14:13:12 roglo Exp $ *)

open Rogue;

value mon_name : game -> monster -> string;

value mv_mons : game -> unit;
value mv_aquators : game -> unit;

value confuse : game -> unit;
value rust : game -> option monster -> unit;
