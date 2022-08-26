(* $Id: object.mli,v 1.14 2006/06/03 06:12:26 deraugla Exp $ *)

open Rogue;

type interest = [ Useful | Neutral | Harmful ];

type object_desc 'a =
  { o_kind : 'a;
    o_title : string;
    o_interest : interest;
    o_value : int }
;

value armor_tab : array (object_desc armor_kind);
value potion_tab : array (object_desc potion_kind);
value ring_tab : array (object_desc ring_kind);
value scroll_tab : array (object_desc scroll_kind);
value wand_tab : array (object_desc wand_kind);
value weapon_tab : array (object_desc weapon_kind);

value int_of_armor : armor_kind -> int;
value int_of_potion : potion_kind -> int;
value int_of_scroll : scroll_kind -> int;
value int_of_ring : ring_kind -> int;
value int_of_wand : wand_kind -> int;
value int_of_weapon : weapon_kind -> int;

value gr_armor : option armor_kind -> objet;
value gr_potion : option potion_kind -> objet;
value gr_ring : option ring_kind -> objet;
value gr_scroll : option scroll_kind -> objet;
value gr_wand : option wand_kind -> objet;
value gr_weapon : option weapon_kind -> objet;

value gr_object : game -> objet;

value get_amulet : option unit -> objet;
value get_food : option food -> objet;
value get_gold : option int -> objet;

value default_fruit : string;
value colours : array string;

value create_obj : object_kind -> int -> objet;
