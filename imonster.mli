(* $Id: imonster.mli,v 1.3 2010/03/23 12:15:35 deraugla Exp $ *)

#open "rogue";;

value gr_monster : game -> int option -> monster;;
value visible_mon_name : game -> int -> string;;
value mon_init_hp : game -> int -> int;;
