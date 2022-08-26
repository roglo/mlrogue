(* $Id: rob_object.mli,v 1.6 2010/06/01 17:42:04 deraugla Exp $ *)

open Rob_def;
open Rob_position;

type pack_item = (char * (int * pack_obj));
type pack = list pack_item;

value remove_from_pack : game -> char -> int -> pack_obj -> unit;
value redefine_in_pack : game -> char -> pack_obj -> unit;

value potion_of_increase_strength_in_pack : pack -> option pack_item;
value potion_of_detect_monsters_in_pack : pack -> option pack_item;
value potion_of_healing_in_pack : pack -> option pack_item;
value several_potions_of_healing_in_pack : pack -> option pack_item;
value potion_of_extra_healing_in_pack : pack -> option pack_item;
value several_potions_of_extra_healing_in_pack : pack -> option pack_item;
value potion_of_see_invisible_in_pack : pack -> option pack_item;

value scroll_of_enchant_armor_in_pack : pack -> option pack_item;
value scroll_of_enchant_weapon_in_pack : pack -> option pack_item;
value scroll_of_hold_monsters_in_pack : pack -> option pack_item;
value scroll_of_identification_in_pack : pack -> option pack_item;
value scroll_of_magic_mapping_in_pack : pack -> option pack_item;
value scroll_of_protect_armor_in_pack : pack -> option pack_item;
value scroll_of_remove_curse_in_pack : pack -> option pack_item;
value scroll_of_scare_monsters_in_pack : pack -> option pack_item;
value scroll_of_teleport_in_pack : pack -> option pack_item;
value number_of_potions_of_detect_monsters_in_pack : pack -> int;
value number_of_scrolls_of_scare_monsters_in_pack : pack -> int;
value number_of_scrolls_of_magic_mapping_in_pack : pack -> int;

value arrows_in_pack : pack -> option pack_item;
value short_bow_in_pack : pack -> option pack_item;
value two_handed_swords_in_pack : game -> list pack_item;

value food_in_pack : pack -> option pack_item;

value wand_of_magic_missile_in_pack : pack -> option pack_item;
value usable_anti_flamer_wand_in_pack : pack -> option pack_item;
value enough_anti_flamer_wand_in_pack : pack -> bool;

value best_armor : game -> option (char * armor_obj);
value good_armor : game -> option (char * armor_obj);
value is_best_armor : game -> char -> bool;
value wearing_best_armor : game -> bool;
value is_good_armor : game -> char -> armor_obj -> bool;
value wearing_good_armor : game -> bool;
value armor_of_ch : game -> char -> option (char * armor_obj);

value unidentified_armor_in_pack : pack -> option pack_item;
value unidentified_scroll_in_pack : pack -> option pack_item;
value unidentified_potion_in_pack : pack -> option pack_item;
value unidentified_ring_in_pack : pack -> option pack_item;
value unidentified_object_not_used_in_pack : game -> option pack_item;

value wielding_a_two_handed_sword : game -> bool;

value worn_armor_value : game -> int;
value worn_armor_protected : game -> bool;

value interesting_objects : game -> list char;
value will_take_not_interesting_object : game -> char -> bool;
value move_command3 :
  game -> position -> position ->
    next_action -> (command * next_action * option move);

value find_unuseful_object_when_full_pack : game -> option pack_item;
value object_to_be_used_in_pack_when_scaring : game -> option pack_item;
value unuseful_object_in_pack_when_scaring : game -> option pack_item;

value use_object :
  game -> t -> string -> next_action -> use_object ->
    option (char * next_action);
value wear_armor_and_read :
  game -> t -> char -> (command * next_action * option move);

value find_object_to_be_used_when_full_pack : game -> option pack_item;
value interesting_objects : game -> list char;

value is_object : char -> bool;
