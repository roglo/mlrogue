(* $Id: rob_misc.mli,v 1.11 2010/07/01 09:06:50 deraugla Exp $ *)

open Rob_def
open Rob_position

val contains : string -> string -> bool
val transl : game -> lang

val min_time_in_level : int
val max_time_in_level : int

val not_impl : string -> 'a -> string
val trace : t -> string -> unit

val is_monster : char -> bool

val tempo : game -> float -> unit
val rogue_pos : game -> position
val dung_char : dung -> position -> char

val add_mov : position -> move -> position
val opposite_move : move -> move

val pos_up : position -> position
val pos_down : position -> position
val pos_left : position -> position
val pos_right : position -> position

val pos_room_and_door : dung -> position -> (room * door_dir option) option
val in_dung : game -> position -> bool

val current_room : game -> position -> room option
val current_room_possibly_at_door : game -> position -> room option
val is_inside_room : game -> position -> bool
val is_at_door : game -> position -> bool
val room_of_door : game -> position -> (room * door_dir) option
val common_room_with : game -> position -> room option
val in_same_rooms : game -> position -> position -> bool

val old_can_move_to : game -> bool -> position -> position -> bool
val can_move_to : game -> position -> bool

val level_of_faster_monsters : int
val level_of_very_mean_monsters : int

val health_points : game -> int
val max_health_points : game -> int
val health_is_maximum : game -> bool
val random_int : game -> int -> int
val mov_of_k : int -> move
val around_pos : game -> position -> around

val no_move : move
val move_between : position -> position -> move
val basic_command_of_move : move -> char
val move_command2 :
  game -> position -> position -> next_action ->
    command * next_action * move option

val list_mov_ch : char list
val list_obj_ch : char list

val shuffle : game -> 'a list -> 'a list
val armor_value : string -> int option
val main_sword_value : game -> int
val wand_value : string -> int option
val weapon_value : string -> int option

val move_command :
  game -> position -> move -> next_action -> command * next_action
val find_doors : game -> room -> (position * door_dir) list
val inside_room : room -> position -> bool
val inside_room_or_at_door : room -> position -> bool
val dist_to_closest :
  game -> room -> position -> (char -> move -> bool) -> move option

val is_big_room : game -> bool

val gen_room_row : room -> int option
val room_row : room -> int
val gen_room_col : room -> int option
val room_col : room -> int
val number_of_visited_rooms : game -> int

val one_step_to_exit_room : door_dir -> move
val one_step_to_enter_room : door_dir -> move

val doors_not_explorated :
  game -> room -> (position * door_dir) list -> (position * door_dir) list
val doors_not_explorated_in_current_room : game -> (position * door_dir) list
val nothing_interesting_in_current_room : game -> bool

val is_trap : game -> position -> bool

val run_around_list : int list
val is_moving : game -> t -> position -> bool

val distance : position -> position -> int
val identified_scroll_kind_of_message : game -> string -> scroll_kind
val quaffed_potion_of_message : game -> string -> potion_obj
val read_scroll_kind_of_message : game -> string -> scroll_kind
val identified_potion_kind_of_message : game -> string -> potion_kind
val weapon_kind_of_message : game -> string -> weapon_kind
val stairs_pos : game -> position list
val list_find : ('a -> bool) -> 'a list -> 'a option

val monster : game -> char -> char
val is_gold_seeker_monster : game -> char -> bool
