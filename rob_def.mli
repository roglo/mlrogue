(* $Id: rob_def.mli,v 1.29 2010/07/03 14:47:59 deraugla Exp $ *)

open Rob_position;

type dung =
  { tab : array string;
    nrow : int;
    ncol : int }
;

type status_line =
  { sl_level : int;
    sl_gold : int;
    sl_hp : int;
    sl_max_hp : int;
    sl_stren : int;
    sl_max_stren : int;
    sl_exp : int;
    sl_max_exp : int;
    sl_hunger : string }
;

type room = (int * int * int * int);
type door_dir = [ DoorUp | DoorRight | DoorDown | DoorLeft ];

type graph = array (array node)
and node =
  { connection : array bool;
    search : mutable search }
and search =
  [ ToSearch | SearchFailed | NotToSearch ]
;

type game =
  { dung : dung;
    rogtime : int;
    time : int;
    status_line : option status_line;
    is_message_more : bool;
    move_result : move_result;
    random_state : Random.State.t;
    level : int;
    lang : mutable string;
    speed : mutable float;
    time_in_level : mutable int;
    trail : mutable Rob_position.PosMap.t int;
    rogue_pos : mutable option position;
    sure_stairs_pos : mutable option position;
    rogue_room_and_door : option (room * option door_dir);
    on_something_at : mutable option (position * bool);
    pack : mutable list (char * (int * pack_obj));
    pack_full : mutable bool;
    worn_armor : mutable option (char * armor_obj);
    main_sword : mutable char;
    armor_cursed : mutable bool;
    weapon_cursed : mutable bool;
    ring_of_slow_digestion_on_hand : mutable (option char);
    garbage : mutable list position;
    scare_pos : mutable list position;
    graph : mutable option graph;
    visited : array (array bool);
    map_showed_since : mutable int;
    mon_detected : mutable bool;
    confused : mutable bool;
    hallucinated : mutable bool;
    was_hallucinated : mutable bool;
    blind : mutable bool;
    held : mutable bool;
    attacked : mutable int;
    attacked_by_invisible : mutable bool;
    attacked_by_flame : mutable int;
    frozen_monsters : mutable list (position * char);
    regrets : mutable list position;
    blindness_discovered : mutable bool;
    hallucination_discovered : mutable bool;
    teleport_discovered : mutable bool;
    after_first_pack_full : mutable bool;
    nb_of_reinit_search : mutable int;
    traps : Hashtbl.t position (option (option trap_kind));
    paradise : mutable bool;
    hist_dung : mutable list (array string);
    dead : mutable bool }
and pack_obj =
  [ Parmor of armor_obj
  | Ppotion of potion_obj
  | Pscroll of scroll_obj
  | Pweapon of weapon_obj
  | Pring of ring_obj
  | Pwand of wand_obj
  | Pfood
  | Pamulet ]
and armor_obj =
  { ar_value : option int;
    ar_protected : bool }
and potion_obj =
  [ Ipotion of potion_kind
  | Upotion of string ]
and potion_kind =
  [ PKacceler
  | PKblindness
  | PKdetect_mon
  | PKdetect_obj
  | PKextra_heal
  | PKhallucination
  | PKhealing
  | PKincr_stren
  | PKraise_level
  | PKrestore_str
  | PKsee_invis
  | PKother ]
and scroll_obj =
  [ Iscroll of scroll_kind
  | Uscroll of string ]
and scroll_kind =
  [ SKaggr_mon
  | SKench_ar
  | SKench_wea
  | SKhold_mon
  | SKident
  | SKmagic_map
  | SKprotect
  | SKscare_mon
  | SKteleport
  | SKuncurse
  | SKother ]
and weapon_obj =
  { we_kind : weapon_kind;
    we_value : option int }
and weapon_kind =
  [ WKtwo_handed_sword
  | WKlong_sword
  | WKmace
  | WKshort_bow
  | WKarrows
  | WKother ]
and ring_obj =
  [ Iring of ring_kind
  | Uring of string ]
and ring_kind =
  [ RKslow_digestion
  | RKother ]
and wand_obj =
  [ Iwand of wand_kind and option int
  | Uwand of string ]
and wand_kind =
  [ AKcancel
  | AKmagic_miss
  | AKother ]
and trap_kind =
  [ TKtrap_door
  | TKother ]
and move = {di : int; dj : int}
and move_result = [ MRcaught | MRteleported | MRok ];

type command =
  [ Cmov of move
  | Cansw_left
  | Cansw_yes
  | Coth of char ]
;

type next_action =
  [ NAalone_in_room of alone_room
  | NAcheck_no_trap of position and position
  | NAdrop_scare of option char and next_action
  | NAdrop_scare_and_kill of drop_scare
  | NAfight of char and bool and next_action
  | NAfind_another_room_and_return of find_another_room_and_return
  | NAglobal_search1 of global_path and around
  | NAglobal_search2 of graph and around and int
  | NAgo_and_kill of global_path and char
  | NAgo_identify_trap of global_path and string and next_action
  | NAgo_in_corridor_and_hit of corridor_excursion
  | NAgo_to of global_path
  | NAgo_to_door of global_path
  | NAgo_to_stairs of global_path and bool
  | NAgo_to_shelter_and_test_scrolls of global_path
  | NAgo_unblock_monster of unblock_monster
  | NAlet_come of char and move
  | NAmove of move and next_action
  | NAmove_in_corridor of position and global_path and list position
  | NAmove_throw1 of list move and position and char
  | NAmove_throw2 of position and char and string
  | NAnone
  | NAput_ring of char and next_action and int
  | NAread_scroll_for_best_armor of char and next_action and wear_read_state
  | NArestore_health of next_action
  | NAreturn_to_base of global_path and next_action
  | NArun_away of char and list move and next_action
  | NAsearch_and_back of move and around and int
  | NAseek_gold_or_monster of global_path and bool
  | NAseek_object of global_path
  | NAstring of string and bool and next_action
  | NAtest_monster of list (test_monster * position * char) and next_action
  | NAtest_potions of char and int
  | NAthrow_away of char and string
  | NAthrow_in_the_garbage of char and global_path and next_action and string
  | NAuse_object of use_object and next_action
  | NAwear of char and int and next_action
  | NAwear_armor_and_test_scrolls of char and char and wear_read_state
  | NAwield_bow_test_moving of position and char and int
  | NAzap of move and char and next_action and int ]
and find_another_room_and_return =
  { fa_state : string;
    fa_gp : global_path;
    fa_doors : list position;
    fa_base : position }
and corridor_excursion =
  { ce_base : position;
    ce_state : string;
    ce_gp : global_path;
    ce_kont : next_action }
and unblock_monster =
  { um_base : position;
    um_mpos : position;
    um_gp : global_path;
    um_kont : next_action }
and global_path =
  { epos : position;
    tpos : position;
    path : list position }
and wear_read_state =
  [ WStoken_off
  | WSwear_what
  | WSarmor_worn of char
  | WSscroll_read ]
and alone_room =
  { ar_state : alone_room_state;
    ar_room : room;
    ar_doors : list alone_room_door;
    ar_trip_cnt : int }
and alone_room_state =
  [ ARgo_and_put_scare of position
  | ARseek_object of global_path
  | ARgo_and_kill of global_path
  | ARgo_to_door of bool and global_path
  | ARforce_kill of move and alone_room_state
  | ARcommand of string ]
and alone_room_door =
  { ard_pos : position;
    ard_dir : door_dir;
    ard_trip_cnt : int;
    ard_monster_perhaps_blocked : option (list position * int) }
and drop_scare =
  { ds_base : position;
    ds_state : drop_scare_state;
    ds_last_corridor_kill_time : int;
    ds_nb_killed_in_corr : int;
    ds_nb_attempt : int;
    ds_outside_tested : bool;
    ds_monster_perhaps_blocked : option (list position * int) }
and drop_scare_state =
  [ DSdrop of char
  | DSdropped of int
  | DSfree_monster of global_path and move and move and int
  | DSforce_kill of char
  | DScheck_monsters
  | DSgive_them_chance of string and int
  | DSgo_and_hit of global_path and int
  | DSseek_object of global_path
  | DStest_move of int and array (array char)
  | DStest_out_in of move and array (array char) and int ]
and test_monster =
  [ TMstay
  | TMforward
  | TMbackward ]
and use_object =
  [ UOeat_food of char and string
  | UOquaff_potion of char and quaff_state
  | UOread_scroll of char and read_state
  | UOthrow_unuseful_objects of char and int
  | UOwield_sword of char and string ]
and read_state =
  [ RSread_what
  | RSscroll_read
  | RSidentify_what
  | RSidentify of char
  | RScontinue
  | RSput_ring of char ]
and quaff_state =
  [ QSquaff_what
  | QSquaffed ]
and around = { ar : string };

type t =
  { t_prev_pos : option game;
    t_prev_game : option game;
    t_speed : float;
    t_monpow_fname : string;
    t_move_trace : bool;
    t_no_lang_dep : bool;
    t_slow_at_level : option int;
    t_slow_at_time : option int;
    t_stop_at_paradise : bool;
    t_breakpoint : int;
    t_prev_comm : option command;
    t_prev_mov : option move;
    t_on_stairs : bool;
    t_next_action : next_action }
;

type lang =
  { scan_status_line : string -> status_line;
    answer_left_hand : char;
    answer_yes : char;
    flaming_monster : char;
    is_armor : string -> bool;
    is_arrow : string -> bool;
    is_fallen_down : string -> bool;
    is_food : string -> bool;
    is_identified_potion : string -> bool;
    is_identified_ring : string -> bool;
    is_identified_scroll : string -> bool;
    is_identified_wand_kind : string -> bool;
    is_leather_armor : string -> bool;
    is_long_sword : string -> bool;
    is_mace : string -> bool;
    is_message_about_fliting_monster : string -> bool;
    is_message_aggressive_monster : string -> bool;
    is_message_armor_blue : string -> bool;
    is_message_attacked_by_flame : string -> bool;
    is_message_better : string -> bool;
    is_message_boring : string -> bool;
    is_message_cosmic : string -> bool;
    is_message_cursed : string -> bool;
    is_message_darkness : string -> bool;
    is_message_dart : string -> bool;
    is_message_dead : string -> bool;
    is_message_faint : string -> bool;
    is_message_faster : string -> bool;
    is_message_gold_shield : string -> bool;
    is_message_has_been_confused : string -> bool;
    is_message_held : string -> bool;
    is_message_hold_monster : string -> bool;
    is_message_less_confused : string -> bool;
    is_message_maniacal_laughter : string -> bool;
    is_message_move_again : string -> bool;
    is_message_moved_onto : string -> bool;
    is_message_much_better : string -> bool;
    is_message_no_darkness : string -> bool;
    is_message_nothing_appropriate : string -> bool;
    is_message_pack_full : string -> bool;
    is_message_pitched_noise : string -> bool;
    is_message_really_pick : string -> bool;
    is_message_shows_a_map : string -> bool;
    is_message_something_attacked : string -> bool;
    is_message_something_there : string -> bool;
    is_message_stronger : string -> bool;
    is_message_tastes_like : string -> bool;
    is_message_there_is_no : string -> bool;
    is_message_warm_all_over : string -> bool;
    is_message_watching_over : string -> bool;
    is_message_weaken_armor : string -> bool;
    is_message_weapon_blue : string -> bool;
    is_message_welcome_to_level : string -> bool;
    is_potion : string -> bool;
    is_potion_of_blindness : string -> bool;
    is_potion_of_extra_healing : string -> bool;
    is_potion_of_hallucination : string -> bool;
    is_potion_of_haste_self : string -> bool;
    is_potion_of_healing : string -> bool;
    is_potion_of_increase_strength : string -> bool;
    is_potion_of_monster_detection : string -> bool;
    is_potion_of_object_detection : string -> bool;
    is_potion_of_raise_level : string -> bool;
    is_potion_of_restore_strength : string -> bool;
    is_potion_of_see_invisible : string -> bool;
    is_ring : string -> bool;
    is_ring_of_slow_digestion : string -> bool;
    is_scroll : string -> bool;
    is_scroll_of_scare_monsters : string -> bool;
    is_scroll_of_aggravate_monsters : string -> bool;
    is_scroll_of_enchant_armor : string -> bool;
    is_scroll_of_enchant_weapon : string -> bool;
    is_scroll_of_hold_monsters : string -> bool;
    is_scroll_of_identification : string -> bool;
    is_scroll_of_magic_mapping : string -> bool;
    is_scroll_of_protection : string -> bool;
    is_scroll_of_remove_curse : string -> bool;
    is_scroll_of_teleport : string -> bool;
    is_short_bow : string -> bool;
    is_trap_door : string -> bool;
    is_two_handed_sword : string -> bool;
    is_very_hungry : string -> bool;
    is_wand : string -> bool;
    is_wand_of_cancellation : string -> bool;
    is_wand_of_magic_missile : string -> bool;
    is_weapon : string -> bool;
    message_more : string -> string;
    monsters : string }
;

type alt 'a 'b = [ Left of 'a | Right of 'b ];
