(* $Id: rob_action.mli,v 1.5 2010/05/25 16:48:27 deraugla Exp $ *)

open Rob_def;
open Rob_misc;
open Rob_position;

type result = (command * next_action * option move);

value run_away_if_possible :
  game -> bool -> list move -> move -> option (move * list move);

value stop_paradise : t -> _;
value start_drop_scare : position -> char -> drop_scare;

value start_search : game -> t -> graph -> result;

value go_to_stairs :
  game -> t -> graph -> position -> position -> bool -> result;
value random_move : game -> position -> next_action -> result;

value slow_down : game -> t -> unit;

value apply : game -> t -> string -> result;
