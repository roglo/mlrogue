(* $Id: object.mli,v 1.14 2006/06/03 06:12:26 deraugla Exp $ *)

open Rogue

type interest = Useful | Neutral | Harmful

type 'a object_desc =
  { o_kind : 'a; o_title : string; o_interest : interest; o_value : int }

val armor_tab : armor_kind object_desc array
val potion_tab : potion_kind object_desc array
val ring_tab : ring_kind object_desc array
val scroll_tab : scroll_kind object_desc array
val wand_tab : wand_kind object_desc array
val weapon_tab : weapon_kind object_desc array

val int_of_armor : armor_kind -> int
val int_of_potion : potion_kind -> int
val int_of_scroll : scroll_kind -> int
val int_of_ring : ring_kind -> int
val int_of_wand : wand_kind -> int
val int_of_weapon : weapon_kind -> int

val gr_armor : armor_kind option -> objet
val gr_potion : potion_kind option -> objet
val gr_ring : ring_kind option -> objet
val gr_scroll : scroll_kind option -> objet
val gr_wand : wand_kind option -> objet
val gr_weapon : weapon_kind option -> objet

val gr_object : game -> objet

val get_amulet : unit option -> objet
val get_food : food option -> objet
val get_gold : int option -> objet

val default_fruit : string
val colours : string array

val create_obj : object_kind -> int -> objet
