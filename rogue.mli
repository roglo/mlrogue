(* $Id: rogue.mli,v 1.64 2010/04/27 10:15:30 deraugla Exp $ *)

type door =
  { oth_room : int;
    oth_dir : int;
    oth_row : int;
    oth_col : int;
    door_row : int;
    door_col : int }

type room =
  { mutable bottom_row : int;
    mutable right_col : int;
    mutable left_col : int;
    mutable top_row : int;
    doors : door option array;
    mutable is_room : int }

type trap_type =
  TrapDoor | BearTrap | TeleTrap | DartTrap | SleepingGasTrap | RustTrap

type trap = { trap_type : trap_type; trap_row : int; trap_col : int }

type armor_kind = Leather | Ringmail | Scale | Chain | Banded | Splint | Plate

type armor =
  { ar_kind : armor_kind;
    ar_class : int;
    mutable ar_is_cursed : bool;
    mutable ar_is_protected : bool;
    mutable ar_enchant : int;
    mutable ar_in_use : bool;
    mutable ar_identified : bool }

type food = Ration | Fruit

type potion_kind =
    IncreaseStrength
  | RestoreStrength
  | Healing
  | ExtraHealing
  | Poison
  | RaiseLevel
  | Blindness
  | Hallucination
  | DetectMonsters
  | DetectObjects
  | Confusion
  | Levitation
  | HasteSelf
  | SeeInvisible

type ring_kind =
    Stealth
  | RTeleport
  | Regeneration
  | SlowDigest
  | AddStrength
  | SustainStrength
  | Dexterity
  | Adornment
  | RSeeInvisible
  | MaintainArmor
  | Searching

type ring_use = LeftHand | RightHand

type ring =
  { rg_kind : ring_kind;
    rg_class : int;
    mutable rg_is_cursed : bool;
    mutable rg_in_use : ring_use option;
    mutable rg_identified : bool }

type scroll_kind =
    ProtectArmor
  | HoldMonster
  | EnchantWeapon
  | EnchantArmor
  | Identify
  | Teleport
  | Sleep
  | ScareMonster
  | RemoveCurse
  | CreateMonster
  | AggravateMonster
  | MagicMapping

type wand_kind =
    TeleportAway
  | SlowMonster
  | ConfuseMonster
  | Invisibility
  | Polymorph
  | HasteMonster
  | PutToSleep
  | MagicMissile
  | Cancellation
  | DoNothing

type wand =
  { wa_kind : wand_kind; mutable wa_hits : int; mutable wa_identified : bool }

type weapon_kind =
  Bow | Dart | Arrow | Dagger | Shuriken | Mace | LongSword | TwoHandedSword

type damage = int * int * (int * int) option

type weapon =
  { we_kind : weapon_kind;
    we_damage : damage;
    we_quiver : int;
    mutable we_is_cursed : bool;
    mutable we_has_been_uncursed : bool;
    mutable we_hit_enchant : int;
    mutable we_d_enchant : int;
    mutable we_in_use : bool;
    mutable we_identified : bool }

type object_kind =
    Amulet
  | Armor of armor
  | Food of food
  | Gold
  | Potion of potion_kind
  | Ring of ring
  | Scroll of scroll_kind
  | Wand of wand
  | Weapon of weapon

type objet =
  { ob_kind : object_kind;
    mutable ob_quantity : int;
    mutable ob_row : int;
    mutable ob_col : int;
    mutable ob_picked_up : bool }

type mon_flags = int

type monster =
  { mn_unique_id : int;
    mn_damage : damage;
    mn_char : char;
    mn_kill_exp : int;
    mn_disguise : char;
    mn_hit_chance : int;
    mutable mn_flags : mon_flags;
    mutable mn_hp_to_kill : int;
    mutable mn_stationary_damage : int;
    mutable mn_drop_percent : int;
    mutable mn_trail_char : char;
    mutable mn_row : int;
    mutable mn_col : int;
    mutable mn_target : (int * int) option;
    mutable mn_slowed_toggle : bool;
    mutable mn_moves_confused : int;
    mutable mn_nap_length : int;
    mutable mn_o_row : int;
    mutable mn_o_col : int;
    mutable mn_o : int }

type fighter =
  { mutable armor : (char * armor) option;
    mutable weapon : (char * weapon) option;
    mutable gold : int;
    mutable hp_current : int;
    mutable hp_max : int;
    mutable extra_hp : int;
    mutable less_hp : int;
    mutable str_current : int;
    mutable str_max : int;
    mutable exp : int;
    mutable exp_points : int;
    mutable pack : (char * objet) list;
    mutable row : int;
    mutable col : int;
    mutable fight_monster : monster option;
    mutable moves_left : int;
    mutable confused : int;
    mutable blind : int;
    mutable halluc : int;
    mutable see_invisible : bool;
    mutable detect_monster : bool;
    mutable levitate : int;
    mutable haste_self : int;
    mutable bear_trap : int;
    mutable being_held : bool;
    mutable stealthy : int;
    mutable left_ring : ring option;
    mutable right_ring : ring option;
    mutable e_rings : int;
    mutable r_rings : int;
    mutable r_teleport : bool;
    mutable r_see_invisible : bool;
    mutable maintain_armor : bool;
    mutable sustain_strength : bool;
    mutable add_strength : int;
    mutable regeneration : int;
    mutable ring_exp : int;
    mutable auto_search : int;
    fchar : char }

type id =
    Unidentified of string
  | Called of string
  | Identified

type game =
  { saved_uid : int;
    true_uid : int;
    mutable cur_level : int;
    mutable max_level : int;
    mutable cur_room : int option;
    mutable lang : string;
    mutable score_only : bool;
    save_file : string;
    nick_name : string;
    login_name : string;
    fruit : string;
    ask_quit : bool;
    show_skull : bool;
    jump : bool;
    mutable party_counter : int;
    mutable party_room : int option;
    mutable foods : int;
    mutable r_de : int option;
    mutable trap_door : bool;
    mutable interrupted : bool;
    mutable can_int : bool;
    mutable reg_search : bool;
    mutable monsters_count : int;
    mutable mon_disappeared : bool;
    mutable level_objects : objet list;
    mutable level_monsters : monster list;
    mutable new_level_message : string;
    mutable hunger_str : string;
    mutable hit_message : string;
    mutable msg_cleared : bool;
    mutable msg_line : string;
    mutable msg_col : int;
    mutable same_msg : int;
    mutable m_moves : int;
    mutable wizard : bool;
    mutable experimented_pick_up_scare_monster : bool;
    rogue : fighter;
    random_rooms : int array;
    id_potions : id array;
    id_rings : id array;
    id_scrolls : id array;
    id_wands : id array;
    is_wood : bool array;
    rooms : room array;
    traps : trap option array;
    dungeon : int array array;
    env : Rfield.env Efield.t }

type ending =
    Monster of string
  | Hypothermia
  | Starvation
  | PoisonDart
  | Quit
  | Win
