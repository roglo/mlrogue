(* $Id: object.mli,v 1.14 2006/06/03 06:12:26 deraugla Exp $ *)

#open "rogue";;

type interest = Useful | Neutral | Harmful;;

type 'a object_desc =
  { o_kind : 'a; o_title : string; o_interest : interest; o_value : int }
;;

value armor_tab : armor_kind object_desc vect;;
value potion_tab : potion_kind object_desc vect;;
value ring_tab : ring_kind object_desc vect;;
value scroll_tab : scroll_kind object_desc vect;;
value wand_tab : wand_kind object_desc vect;;
value weapon_tab : weapon_kind object_desc vect;;

value int_of_armor : armor_kind -> int;;
value int_of_potion : potion_kind -> int;;
value int_of_scroll : scroll_kind -> int;;
value int_of_ring : ring_kind -> int;;
value int_of_wand : wand_kind -> int;;
value int_of_weapon : weapon_kind -> int;;

value gr_armor : armor_kind option -> objet;;
value gr_potion : potion_kind option -> objet;;
value gr_ring : ring_kind option -> objet;;
value gr_scroll : scroll_kind option -> objet;;
value gr_wand : wand_kind option -> objet;;
value gr_weapon : weapon_kind option -> objet;;

value gr_object : game -> objet;;

value get_amulet : unit option -> objet;;
value get_food : food option -> objet;;
value get_gold : int option -> objet;;

value default_fruit : string;;
value colours : string vect;;

value create_obj : object_kind -> int -> objet;;
