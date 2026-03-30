(* $Id: rob_object.mli,v 1.6 2010/06/01 17:42:04 deraugla Exp $ *)

open Rob_def
open Rob_position

type pack_item = char * (int * pack_obj)
type pack = pack_item list

val remove_from_pack : game -> char -> int -> pack_obj -> unit
val redefine_in_pack : game -> char -> pack_obj -> unit

val potion_of_increase_strength_in_pack : pack -> pack_item option
val potion_of_detect_monsters_in_pack : pack -> pack_item option
val potion_of_healing_in_pack : pack -> pack_item option
val several_potions_of_healing_in_pack : pack -> pack_item option
val potion_of_extra_healing_in_pack : pack -> pack_item option
val several_potions_of_extra_healing_in_pack : pack -> pack_item option
val potion_of_see_invisible_in_pack : pack -> pack_item option

val scroll_of_enchant_armor_in_pack : pack -> pack_item option
val scroll_of_enchant_weapon_in_pack : pack -> pack_item option
val scroll_of_hold_monsters_in_pack : pack -> pack_item option
val scroll_of_identification_in_pack : pack -> pack_item option
val scroll_of_magic_mapping_in_pack : pack -> pack_item option
val scroll_of_protect_armor_in_pack : pack -> pack_item option
val scroll_of_remove_curse_in_pack : pack -> pack_item option
val scroll_of_scare_monsters_in_pack : pack -> pack_item option
val scroll_of_teleport_in_pack : pack -> pack_item option
val number_of_potions_of_detect_monsters_in_pack : pack -> int
val number_of_scrolls_of_scare_monsters_in_pack : pack -> int
val number_of_scrolls_of_magic_mapping_in_pack : pack -> int

val arrows_in_pack : pack -> pack_item option
val short_bow_in_pack : pack -> pack_item option
val two_handed_swords_in_pack : game -> pack_item list

val food_in_pack : pack -> pack_item option

val wand_of_magic_missile_in_pack : pack -> pack_item option
val usable_anti_flamer_wand_in_pack : pack -> pack_item option
val enough_anti_flamer_wand_in_pack : pack -> bool

val best_armor : game -> (char * armor_obj) option
val good_armor : game -> (char * armor_obj) option
val is_best_armor : game -> char -> bool
val wearing_best_armor : game -> bool
val is_good_armor : game -> char -> armor_obj -> bool
val wearing_good_armor : game -> bool
val armor_of_ch : game -> char -> (char * armor_obj) option

val unidentified_armor_in_pack : pack -> pack_item option
val unidentified_scroll_in_pack : pack -> pack_item option
val unidentified_potion_in_pack : pack -> pack_item option
val unidentified_ring_in_pack : pack -> pack_item option
val unidentified_object_not_used_in_pack : game -> pack_item option

val wielding_a_two_handed_sword : game -> bool

val worn_armor_value : game -> int
val worn_armor_protected : game -> bool

val interesting_objects : game -> char list
val will_take_not_interesting_object : game -> char -> bool
val move_command3 :
  game -> position -> position -> next_action ->
    command * next_action * move option

val find_unuseful_object_when_full_pack : game -> pack_item option
val object_to_be_used_in_pack_when_scaring : game -> pack_item option
val unuseful_object_in_pack_when_scaring : game -> pack_item option

val use_object :
  game -> t -> string -> next_action -> use_object ->
    (char * next_action) option
val wear_armor_and_read :
  game -> t -> char -> command * next_action * move option

val find_object_to_be_used_when_full_pack : game -> pack_item option
val interesting_objects : game -> char list

val is_object : char -> bool
