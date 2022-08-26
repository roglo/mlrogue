(* $Id: imonster.ml,v 1.9 2010/03/23 12:15:35 deraugla Exp $ *)

#use "rogue.def";

open Rogue;

value get_rand = Imisc.get_rand;
value gr_obj_char = Imisc.gr_obj_char;

type mon =
  { m_char : char;
    m_name : string;
    m_damage : damage;
    m_flags : int;
    m_first_level : int;
    m_last_level : int;
    m_hp_to_kill : int;
    m_kill_exp : int;
    m_hit_chance : int;
    m_drop_percent : int }
;

value mon_tab =
  [| {m_char = 'A'; m_name = "aquator@(n)"; m_damage = (0, 0, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS lor RUSTS;
      m_first_level = 9; m_last_level = 18; m_hp_to_kill = 25;
      m_kill_exp = 20; m_hit_chance = 100; m_drop_percent = 0};
     {m_char = 'B'; m_name = "bat"; m_damage = (1, 3, None);
      m_flags = ASLEEP lor WANDERS lor FLITS;
      m_first_level = 1; m_last_level = 8; m_hp_to_kill = 10;
      m_kill_exp = 2; m_hit_chance = 60; m_drop_percent = 0};
     {m_char = 'C'; m_name = "centaur"; m_damage = (3, 3, Some (2, 5));
      m_flags = ASLEEP lor WANDERS;
      m_first_level = 7; m_last_level = 16; m_hp_to_kill = 32;
      m_kill_exp = 15; m_hit_chance = 85; m_drop_percent = 10};
     {m_char = 'D'; m_name = "dragon"; m_damage = (4, 6, Some (4, 9));
      m_flags = ASLEEP lor WAKENS lor FLAMES;
      m_first_level = 21; m_last_level = 126; m_hp_to_kill = 145;
      m_kill_exp = 5000; m_hit_chance = 100; m_drop_percent = 90};
     {m_char = 'E'; m_name = "emu@(n)"; m_damage = (1, 3, None);
      m_flags = ASLEEP lor WAKENS;
      m_first_level = 1; m_last_level = 7; m_hp_to_kill = 11;
      m_kill_exp = 2; m_hit_chance = 65; m_drop_percent = 0};
     {m_char = 'F'; m_name = "venus fly-trap"; m_damage = (5, 5, None);
      m_flags = HOLDS lor STATIONARY;
      m_first_level = 12; m_last_level = 126; m_hp_to_kill = 73;
      m_kill_exp = 91; m_hit_chance = 80; m_drop_percent = 0};
     {m_char = 'G'; m_name = "griffin"; m_damage = (5, 5, Some (5, 5));
      m_flags = ASLEEP lor WAKENS lor WANDERS lor FLIES;
      m_first_level = 20; m_last_level = 126; m_hp_to_kill = 115;
      m_kill_exp = 2000; m_hit_chance = 85; m_drop_percent = 10};
     {m_char = 'H'; m_name = "hobgoblin"; m_damage = (1, 3, Some (1, 2));
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 1; m_last_level = 10; m_hp_to_kill = 15;
      m_kill_exp = 3; m_hit_chance = 67; m_drop_percent = 0};
     {m_char = 'I'; m_name = "ice monster@(n)"; m_damage = (0, 0, None);
      m_flags = ASLEEP lor FREEZES;
      m_first_level = 2; m_last_level = 11; m_hp_to_kill = 15;
      m_kill_exp = 5; m_hit_chance = 68; m_drop_percent = 0};
     {m_char = 'J'; m_name = "jabberwock"; m_damage = (3, 10, Some (4, 5));
      m_flags = ASLEEP lor WANDERS;
      m_first_level = 21; m_last_level = 126; m_hp_to_kill = 132;
      m_kill_exp = 3000; m_hit_chance = 100; m_drop_percent = 0};
     {m_char = 'K'; m_name = "kestrel"; m_damage = (1, 4, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS lor FLIES;
      m_first_level = 1; m_last_level = 6; m_hp_to_kill = 10;
      m_kill_exp = 2; m_hit_chance = 60; m_drop_percent = 0};
     {m_char = 'L'; m_name = "leprechaun"; m_damage = (0, 0, None);
      m_flags = ASLEEP lor STEALS_GOLD;
      m_first_level = 6; m_last_level = 16; m_hp_to_kill = 25;
      m_kill_exp = 21; m_hit_chance = 75; m_drop_percent = 0};
     {m_char = 'M'; m_name = "medusa"; m_damage = (4, 4, Some (3, 7));
      m_flags = ASLEEP lor WAKENS lor WANDERS lor CONFUSES;
      m_first_level = 18; m_last_level = 126; m_hp_to_kill = 97;
      m_kill_exp = 250; m_hit_chance = 85; m_drop_percent = 25};
     {m_char = 'N'; m_name = "nymph"; m_damage = (0, 0, None);
      m_flags = ASLEEP lor STEALS_ITEM;
      m_first_level = 10; m_last_level = 19; m_hp_to_kill = 25;
      m_kill_exp = 39; m_hit_chance = 75; m_drop_percent = 100};
     {m_char = 'O'; m_name = "orc@(n)";  m_damage = (1, 6, None);
      m_flags = ASLEEP lor WANDERS lor WAKENS lor SEEKS_GOLD;
      m_first_level = 4; m_last_level = 13; m_hp_to_kill = 25;
      m_kill_exp = 5; m_hit_chance = 70; m_drop_percent = 10};
     {m_char = 'P'; m_name = "phantom"; m_damage = (5, 4, None);
      m_flags = ASLEEP lor INVISIBLE lor WANDERS lor FLITS;
      m_first_level = 15; m_last_level = 24; m_hp_to_kill = 76;
      m_kill_exp = 120; m_hit_chance = 80; m_drop_percent = 50};
     {m_char = 'Q'; m_name = "quagga"; m_damage = (3, 5, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 8; m_last_level = 17; m_hp_to_kill = 30;
      m_kill_exp = 20; m_hit_chance = 78; m_drop_percent = 20};
     {m_char = 'R'; m_name = "rattlesnake"; m_damage = (2, 5, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS lor STINGS;
      m_first_level = 3; m_last_level = 12; m_hp_to_kill = 19;
      m_kill_exp = 10; m_hit_chance = 70; m_drop_percent = 0};
     {m_char = 'S'; m_name = "snake"; m_damage = (1, 3, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 1; m_last_level = 9; m_hp_to_kill = 8;
      m_kill_exp = 2; m_hit_chance = 50; m_drop_percent = 0};
     {m_char = 'T'; m_name = "troll"; m_damage = (4, 6, Some (1, 4));
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 13; m_last_level = 22; m_hp_to_kill = 75;
      m_kill_exp = 125; m_hit_chance = 75; m_drop_percent = 33};
     {m_char = 'U'; m_name = "black unicorn"; m_damage = (4, 10, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 17; m_last_level = 26; m_hp_to_kill = 90;
      m_kill_exp = 200; m_hit_chance = 85; m_drop_percent = 33};
     {m_char = 'V'; m_name = "vampire"; m_damage = (1, 4, Some (1, 4));
      m_flags = ASLEEP lor WAKENS lor WANDERS lor DRAINS_LIFE;
      m_first_level = 19; m_last_level = 126; m_hp_to_kill = 55;
      m_kill_exp = 350; m_hit_chance = 85; m_drop_percent = 18};
     {m_char = 'W'; m_name = "wraith"; m_damage = (2, 8, None);
      m_flags = ASLEEP lor WANDERS lor DROPS_LEVEL;
      m_first_level = 14; m_last_level = 23; m_hp_to_kill = 45;
      m_kill_exp = 55; m_hit_chance = 75; m_drop_percent = 0};
     {m_char = 'X'; m_name = "xeroc"; m_damage = (4, 6, None);
      m_flags = ASLEEP lor IMITATES;
      m_first_level = 16; m_last_level = 25; m_hp_to_kill = 42;
      m_kill_exp = 110; m_hit_chance = 75; m_drop_percent = 0};
     {m_char = 'Y'; m_name = "yeti"; m_damage = (3, 6, None);
      m_flags = ASLEEP lor WANDERS;
      m_first_level = 11; m_last_level = 20; m_hp_to_kill = 35;
      m_kill_exp = 50; m_hit_chance = 80; m_drop_percent = 20};
     {m_char = 'Z'; m_name = "zombie"; m_damage = (1, 7, None);
      m_flags = ASLEEP lor WAKENS lor WANDERS;
      m_first_level = 5; m_last_level = 14; m_hp_to_kill = 21;
      m_kill_exp = 8; m_hit_chance = 69; m_drop_percent = 0} |]
;

value nb_monsters = Array.length mon_tab;

value gr_monster g shlev =
  let mn =
    loop () where rec loop () =
      let mn = get_rand 0 (nb_monsters - 1) in
      match shlev with
      [ Some shlev ->
          if g.cur_level >= mon_tab.(mn).m_first_level - shlev &&
            g.cur_level <= mon_tab.(mn).m_last_level
          then mn
          else loop ()
      | None -> mn ]
  in
  let monster = mon_tab.(mn) in
  let disguise =
    if monster.m_flags land IMITATES <> 0 then gr_obj_char () else 'x'
  in
  let hasted = if g.cur_level > AMULET_LEVEL + 2 then HASTED else 0 in
  let id = do {
    g.monsters_count := g.monsters_count + 1;
    g.monsters_count;
  }
  in
  {mn_unique_id = id; mn_damage = monster.m_damage;
   mn_char = monster.m_char; mn_kill_exp = monster.m_kill_exp;
   mn_disguise = disguise; mn_hit_chance = monster.m_hit_chance;
   mn_flags = monster.m_flags lor hasted;
   mn_hp_to_kill = monster.m_hp_to_kill; mn_stationary_damage = 0;
   mn_drop_percent = monster.m_drop_percent; mn_trail_char = ' ';
   mn_row = 0; mn_col = 0; mn_target = None; mn_slowed_toggle = False;
   mn_moves_confused = 0; mn_nap_length = 0; mn_o_row = 0; mn_o_col = 0;
   mn_o = 0}
;

value visible_mon_name g i = mon_tab.(i).m_name;
value mon_init_hp g i = mon_tab.(i).m_hp_to_kill;
