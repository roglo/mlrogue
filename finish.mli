(* $Id: finish.mli,v 1.3 2005/10/20 11:55:47 roglo Exp $ *)

#open "rogue";;

value clean_up : string -> 'a;;
value put_scores : string -> bool -> (game * ending) option -> unit;;
value win : game -> 'a;;
value killed_by : game -> ending -> 'a;;
