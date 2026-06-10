(* $Id: rob_action.mli,v 1.5 2010/05/25 16:48:27 deraugla Exp $ *)

#open "rob_def";;
#open "rob_misc";;
#open "rob_position";;

type result == command * next_action * move option;;

value run_away_if_possible :
  game -> bool -> move list -> move -> (move * move list) option;;

(*
value stop_paradise : t -> 'a;;
*)
value start_drop_scare : position -> char -> drop_scare;;

value start_search : game -> t -> graph -> result;;

value go_to_stairs :
  game -> t -> graph -> position -> position -> bool -> result;;
value random_move : game -> position -> next_action -> result;;

value slow_down : game -> t -> unit;;

(*
value apply : game -> t -> string -> result;;
*)
