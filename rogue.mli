(* $Id: rogue.mli,v 1.64 2010/04/27 10:15:30 deraugla Exp $ *)

type door =
  { oth_room : int;
    oth_dir : int;
    oth_row : int;
    oth_col : int;
    door_row : int;
    door_col : int }
;

type room =
  { bottom_row : mutable int;
    right_col : mutable int;
    left_col : mutable int;
    top_row : mutable int;
    doors : array (option door);
    is_room : mutable int }
;

type trap_type =
  [ TrapDoor | BearTrap | TeleTrap | DartTrap | SleepingGasTrap | RustTrap ]
;

type trap = { trap_type : trap_type; trap_row : int; trap_col : int };

type armor_kind =
  [ Leather | Ringmail | Scale | Chain | Banded | Splint | Plate ]
;

type armor =
  { ar_kind : armor_kind;
    ar_class : int;
    ar_is_cursed : mutable bool;
    ar_is_protected : mutable bool;
    ar_enchant : mutable int;
    ar_in_use : mutable bool;
    ar_identified : mutable bool }
;

type food = [ Ration | Fruit ];

type potion_kind =
  [ IncreaseStrength
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
  | SeeInvisible ]
;

type ring_kind =
  [ Stealth
  | RTeleport
  | Regeneration
  | SlowDigest
  | AddStrength
  | SustainStrength
  | Dexterity
  | Adornment
  | RSeeInvisible
  | MaintainArmor
  | Searching ]
;

type ring_use = [ LeftHand | RightHand ];

type ring =
  { rg_kind : ring_kind;
    rg_class : int;
    rg_is_cursed : mutable bool;
    rg_in_use : mutable option ring_use;
    rg_identified : mutable bool }
;

type scroll_kind =
  [ ProtectArmor
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
  | MagicMapping ]
;

type wand_kind =
  [ TeleportAway
  | SlowMonster
  | ConfuseMonster
  | Invisibility
  | Polymorph
  | HasteMonster
  | PutToSleep
  | MagicMissile
  | Cancellation
  | DoNothing ]
;

type wand =
  { wa_kind : wand_kind;
    wa_hits : mutable int;
    wa_identified : mutable bool }
;

type weapon_kind =
  [ Bow
  | Dart
  | Arrow
  | Dagger
  | Shuriken
  | Mace
  | LongSword
  | TwoHandedSword ]
;

type damage = (int * int * option (int * int));

type weapon =
  { we_kind : weapon_kind;
    we_damage : damage;
    we_quiver : int;
    we_is_cursed : mutable bool;
    we_has_been_uncursed : mutable bool;
    we_hit_enchant : mutable int;
    we_d_enchant : mutable int;
    we_in_use : mutable bool;
    we_identified : mutable bool }
;

type object_kind =
  [ Amulet
  | Armor of armor
  | Food of food
  | Gold
  | Potion of potion_kind
  | Ring of ring
  | Scroll of scroll_kind
  | Wand of wand
  | Weapon of weapon ]
;

type objet =
  { ob_kind : object_kind;
    ob_quantity : mutable int;
    ob_row : mutable int;
    ob_col : mutable int;
    ob_picked_up : mutable bool }
;

type mon_flags = int;

type monster =
  { mn_unique_id : int;
    mn_damage : damage;
    mn_char : char;
    mn_kill_exp : int;
    mn_disguise : char;
    mn_hit_chance : int;
    mn_flags : mutable mon_flags;
    mn_hp_to_kill : mutable int;
    mn_stationary_damage : mutable int;
    mn_drop_percent : mutable int;
    mn_trail_char : mutable char;
    mn_row : mutable int;
    mn_col : mutable int;
    mn_target : mutable option (int * int);
    mn_slowed_toggle : mutable bool;
    mn_moves_confused : mutable int;
    mn_nap_length : mutable int;
    mn_o_row : mutable int;
    mn_o_col : mutable int;
    mn_o : mutable int }
;

type fighter =
  { armor : mutable option (char * armor);
    weapon : mutable option (char * weapon);
    gold : mutable int;
    hp_current : mutable int;
    hp_max : mutable int;
    extra_hp : mutable int;
    less_hp : mutable int;
    str_current : mutable int;
    str_max : mutable int;
    exp : mutable int;
    exp_points : mutable int;
    pack : mutable list (char * objet);
    row : mutable int;
    col : mutable int;
    fight_monster : mutable option monster;
    moves_left : mutable int;
    confused : mutable int;
    blind : mutable int;
    halluc : mutable int;
    see_invisible : mutable bool;
    detect_monster : mutable bool;
    levitate : mutable int;
    haste_self : mutable int;
    bear_trap : mutable int;
    being_held : mutable bool;
    stealthy : mutable int;
    left_ring : mutable option ring;
    right_ring : mutable option ring;
    e_rings : mutable int;
    r_rings : mutable int;
    r_teleport : mutable bool;
    r_see_invisible : mutable bool;
    maintain_armor : mutable bool;
    sustain_strength : mutable bool;
    add_strength : mutable int;
    regeneration : mutable int;
    ring_exp : mutable int;
    auto_search : mutable int;
    fchar : char }
;

type id = [ Unidentified of string | Called of string | Identified ];

type game =
  { saved_uid : int;
    true_uid : int;
    cur_level : mutable int;
    max_level : mutable int;
    cur_room : mutable option int;
    lang : mutable string;
    score_only : mutable bool;
    save_file : string;
    nick_name : string;
    login_name : string;
    fruit : string;
    ask_quit : bool;
    show_skull : bool;
    jump : bool;
    party_counter : mutable int;
    party_room : mutable option int;
    foods : mutable int;
    r_de : mutable option int;
    trap_door : mutable bool;
    interrupted : mutable bool;
    can_int : mutable bool;
    reg_search : mutable bool;
    monsters_count : mutable int;
    mon_disappeared : mutable bool;
    level_objects : mutable list objet;
    level_monsters : mutable list monster;
    new_level_message : mutable string;
    hunger_str : mutable string;
    hit_message : mutable string;
    msg_cleared : mutable bool;
    msg_line : mutable string;
    msg_col : mutable int;
    same_msg : mutable int;
    m_moves : mutable int;
    wizard : mutable bool;
    experimented_pick_up_scare_monster : mutable bool;
    rogue : fighter;
    random_rooms : array int;
    id_potions : array id;
    id_rings : array id;
    id_scrolls : array id;
    id_wands : array id;
    is_wood : array bool;
    rooms : array room;
    traps : array (option trap);
    dungeon : array (array int);
    env : Efield.t Rfield.env }
;

type ending =
  [ Monster of string | Hypothermia | Starvation | PoisonDart | Quit | Win ]
;
