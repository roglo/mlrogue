(* $Id: rob_misc.mli,v 1.11 2010/07/01 09:06:50 deraugla Exp $ *)

open Rob_def;
open Rob_position;

value contains : string -> string -> bool;
value transl : game -> lang;

value min_time_in_level : int;
value max_time_in_level : int;

value not_impl : string -> 'a -> string;
value trace : t -> string -> unit;

value is_monster : char -> bool;

value tempo : game -> float -> unit;
value rogue_pos : game -> position;
value dung_char : dung -> position -> char;

value add_mov : position -> move -> position;
value opposite_move : move -> move;

value pos_up : position -> position;
value pos_down : position -> position;
value pos_left : position -> position;
value pos_right : position -> position;

value pos_room_and_door : dung -> position -> option (room * option door_dir);
value in_dung : game -> position -> bool;

value current_room : game -> position -> option room;
value current_room_possibly_at_door : game -> position -> option room;
value is_inside_room : game -> position -> bool;
value is_at_door : game -> position -> bool;
value room_of_door : game -> position -> option (room * door_dir);
value common_room_with : game -> position -> option room;
value in_same_rooms : game -> position -> position -> bool;

value old_can_move_to : game -> bool -> position -> position -> bool;
value can_move_to : game -> position -> bool;

value level_of_faster_monsters : int;
value level_of_very_mean_monsters : int;

value health_points : game -> int;
value max_health_points : game -> int;
value health_is_maximum : game -> bool;
value random_int : game -> int -> int;
value mov_of_k : int -> move;
value around_pos : game -> position -> around;

value no_move : move;
value move_between : position -> position -> move;
value basic_command_of_move : move -> char;
value move_command2 :
  game -> position -> position -> next_action ->
    (command * next_action * option move);

value list_mov_ch : list char;
value list_obj_ch : list char;

value shuffle : game -> list 'a -> list 'a;
value armor_value : string -> option int;
value main_sword_value : game -> int;
value wand_value : string -> option int;
value weapon_value : string -> option int;

value move_command :
  game -> position -> move -> next_action -> (command * next_action);
value find_doors :
  game -> room -> list (position * door_dir);
value inside_room : room -> position -> bool;
value inside_room_or_at_door : room -> position -> bool;
value dist_to_closest :
  game -> room -> position -> (char -> move -> bool) -> option move;

value is_big_room : game -> bool;

value gen_room_row : room -> option int;
value room_row : room -> int;
value gen_room_col : room -> option int;
value room_col : room -> int;
value number_of_visited_rooms : game -> int;

value one_step_to_exit_room : door_dir -> move;
value one_step_to_enter_room : door_dir -> move;

value doors_not_explorated :
  game -> room -> list (position * door_dir) -> list (position * door_dir);
value doors_not_explorated_in_current_room :
  game -> list (position * door_dir);
value nothing_interesting_in_current_room : game -> bool;

value is_trap : game -> position -> bool;

value run_around_list : list int;
value is_moving : game -> t -> position -> bool;

value distance : position -> position -> int;
value identified_scroll_kind_of_message : game -> string -> scroll_kind;
value quaffed_potion_of_message : game -> string -> potion_obj;
value read_scroll_kind_of_message : game -> string -> scroll_kind;
value identified_potion_kind_of_message : game -> string -> potion_kind;
value weapon_kind_of_message : game -> string -> weapon_kind;
value stairs_pos : game -> list position;
value list_find : ('a -> bool) -> list 'a -> option 'a;

value monster : game -> char -> char;
value is_gold_seeker_monster : game -> char -> bool;
