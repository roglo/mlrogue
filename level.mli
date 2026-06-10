(* $Id: level.mli,v 1.6 2005/11/06 02:34:10 ddr Exp $ *)

#open "rogue";;

value place_at : game -> objet -> int -> int -> unit;;
value put_player : game -> int option -> unit;;
value create : game -> unit;;

value trap_mess : trap_type -> string;;
value trap_string : trap_type -> string;;
