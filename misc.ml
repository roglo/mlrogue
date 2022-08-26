(* $Id: misc.ml,v 1.107 2018/04/26 09:52:37 deraugla Exp $ *)

#load "pa_more.cmo";

#use "rogue.def";
#use "keyboard.def";

open Rogue;
open Rfield;
open Printf;
open Translate;

value string_create = Bytes.create;
value string_length = Bytes.length;
value string_of_bytes = Bytes.to_string;

value level_points =
  [| 10; 20; 40; 80; 160; 320; 640; 1300; 2600; 5200; 10000; 20000; 40000;
     80000; 160000; 320000; 1000000; 3333333; 6666666; MAX_EXP; 99900000 |]
;

value is_passable g row col =
  if row < MIN_ROW || row > DROWS - 2 || col < 0 || col > DCOLS - 1 then False
  else if g.dungeon.(row).(col) land HIDDEN <> 0 then
    if g.dungeon.(row).(col) land TRAP <> 0 then True else False
  else if
    g.dungeon.(row).(col) land
    (FLOOR lor TUNNEL lor DOOR lor STAIRS lor TRAP) <>
      0
  then
    True
  else False
;

value object_at g row col =
  try
    List.find (fun ob -> ob.ob_row = row && ob.ob_col = col) g.level_objects
  with
  [ Not_found -> invalid_arg "object_at" ]
;

value monster_at g row col =
  try
    List.find (fun mn -> mn.mn_row = row && mn.mn_col = col) g.level_monsters
  with
  [ Not_found -> invalid_arg "monster_at" ]
;

value trap_at g row col =
  loop_i 0 where rec loop_i i =
    if i < MAX_TRAPS then
      match g.traps.(i) with
      [ None -> loop_i (i + 1)
      | Some t ->
          if t.trap_row = row && t.trap_col = col then Some t.trap_type
          else loop_i (i + 1) ]
    else None
;

value gold_at g row col =
  try
    let obj =
      List.find (fun ob -> ob.ob_row = row && ob.ob_col = col) g.level_objects
    in
    obj.ob_kind = Gold
  with
  [ Not_found -> False ]
;

value imitating g row col =
  if g.dungeon.(row).(col) land MONSTER <> 0 then
    let monster = monster_at g row col in
    if monster.mn_flags land IMITATES <> 0 then True else False
  else False
;

value take_from_pack g ch =
  g.rogue.pack := List.filter (fun (c, _) -> ch <> c) g.rogue.pack
;

value take_from_monsters g monster =
  g.level_monsters :=
    List.filter (fun mn -> mn.mn_unique_id <> monster.mn_unique_id)
      g.level_monsters
;

value tgmc g c =
  let t = fast_transl g.lang "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let m = Char.code c - Char.code 'A' in
  if String.length t = 26 && t.[m] >= 'A' && t.[m] <= 'Z' then t.[m] else c
;

value itgmc g ch =
  let t = fast_transl g.lang "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  loop_c 'A' where rec loop_c c =
    if c > 'Z' then ch
    else
      let cc =
        let m = Char.code c - Char.code 'A' in
        if String.length t = 26 && t.[m] >= 'A' && t.[m] <= 'Z' then t.[m]
        else c
      in
      if cc = ch then c else loop_c (Char.chr (Char.code c + 1))
;

value gmc g monster =
  if not
       (g.rogue.detect_monster || g.rogue.see_invisible ||
        g.rogue.r_see_invisible) &&
     monster.mn_flags land INVISIBLE <> 0 ||
     g.rogue.blind > 0
  then
    monster.mn_trail_char
  else if monster.mn_flags land IMITATES <> 0 then monster.mn_disguise
  else tgmc g monster.mn_char
;

value get_mask_char obj =
  match obj.ob_kind with
  [ Amulet -> ','
  | Armor _ -> ']'
  | Food _ -> ':'
  | Gold -> '*'
  | Potion _ -> '!'
  | Ring _ -> '='
  | Scroll _ -> '?'
  | Wand _ -> '/'
  | Weapon _ -> ')' ]
;

value get_dungeon_char g row col =
  let mask = g.dungeon.(row).(col) in
  if mask land MONSTER <> 0 then gmc g (monster_at g row col)
  else if mask land OBJECT <> 0 then get_mask_char (object_at g row col)
  else if mask land STAIRS <> 0 then '%'
  else if mask land TUNNEL <> 0 && mask land HIDDEN = 0 then '#'
  else if mask land HORWALL <> 0 then '-'
  else if mask land VERTWALL <> 0 then '|'
  else if mask land FLOOR <> 0 then
    if mask land TRAP <> 0 && mask land HIDDEN = 0 then '^' else '.'
  else if mask land DOOR <> 0 then
    if mask land HIDDEN <> 0 then
      if col > 0 && g.dungeon.(row).(col-1) land HORWALL <> 0 ||
         col < DCOLS - 1 && g.dungeon.(row).(col+1) land HORWALL <> 0
      then
        '-'
      else '|'
    else '+'
  else ' '
;

value get_exp_level e =
  loop_i 0 where rec loop_i i =
    if i < MAX_EXP_LEVEL - 1 then
      if level_points.(i) > e then i + 1 else loop_i (i + 1)
    else i + 1
;

value show_rogue g = do {
  let rogue = g.rogue in
(*
  if rogue.hp_current <= rogue.hp_max / 2 then Curses.color_set 1 (-1)
  else ();
*)
  if rogue.confused > 0 then Curses.color_set 2 (-1) else ();
(**)
  Curses.mvaddch rogue.row rogue.col rogue.fchar;
(*
  if rogue.hp_current <= rogue.hp_max / 2 then Curses.color_set (-1) (-1)
  else ()
*)
  if rogue.confused > 0 then Curses.color_set (-1) (-1) else ();
(**)
};

value show_monster g row col monster ch =
  if ch >= 'A' && ch <= 'Z' then do {
(*
    Curses.color_set (monster.mn_unique_id mod 8 + 1) (-1);
    Curses.mvaddch row col ch;
    Curses.color_set (-1) (-1)
*)
    let i = Char.code monster.mn_char - Char.code 'A' in
    let init_hp = Imonster.mon_init_hp g i in
    let hp = monster.mn_hp_to_kill in
    if hp < init_hp then Curses.color_set 1 (-1) else ();
    Curses.mvaddch row col ch;
    if hp < init_hp then Curses.color_set (-1) (-1) else ();
(**)
  }
  else Curses.mvaddch row col ch
;

value show_trap i j t = do {
(*
  match t with
  [ TrapDoor -> Curses.color_set 1 (-1)
  | BearTrap -> Curses.color_set 2 (-1)
  | TeleTrap -> Curses.color_set 3 (-1)
  | DartTrap -> Curses.color_set 4 (-1)
  | SleepingGasTrap -> Curses.color_set 5 (-1)
  | RustTrap -> Curses.color_set 6 (-1) ];
*)
  Curses.mvaddch i j '^';
(*
  Curses.color_set (-1) (-1)
*)
};

value add_exp g e promotion = do {
  let rogue = g.rogue in
  rogue.exp_points add_eq e;
  if rogue.exp_points >= level_points.(rogue.exp - 1) then do {
    let new_exp = get_exp_level rogue.exp_points in
    if rogue.exp_points > MAX_EXP then rogue.exp_points := MAX_EXP + 1
    else ();
    for i = rogue.exp + 1 to new_exp do {
      let mbuf =
        sprintf (ftransl g.lang "Welcome to experience level %d!") i
      in
      Dialogue.message g mbuf False;
      let hp = promotion g in
      rogue.hp_current add_eq hp;
      rogue.hp_max add_eq hp;
      rogue.exp := i;
      Dialogue.print_stats g (STAT_HP lor STAT_EXP);
      show_rogue g;
    }
  }
  else Dialogue.print_stats g STAT_EXP
};

type saved = (game * array (array char));
value save_magic = "RGSV0005";

module OLD_GAME =
  struct
    type t =
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
        dungeon : array (array int) }
    ;
  end
;

value g_of_old_g g =
  {saved_uid = g.OLD_GAME.saved_uid;
   true_uid = g.OLD_GAME.true_uid;
   cur_level = g.OLD_GAME.cur_level;
   max_level = g.OLD_GAME.max_level;
   cur_room = g.OLD_GAME.cur_room;
   lang = g.OLD_GAME.lang;
   score_only = g.OLD_GAME.score_only;
   save_file = g.OLD_GAME.save_file;
   nick_name = g.OLD_GAME.nick_name;
   login_name = g.OLD_GAME.login_name;
   fruit = g.OLD_GAME.fruit;
   ask_quit = g.OLD_GAME.ask_quit;
   show_skull = g.OLD_GAME.show_skull;
   jump = g.OLD_GAME.jump;
   party_counter = g.OLD_GAME.party_counter;
   party_room = g.OLD_GAME.party_room;
   foods = g.OLD_GAME.foods;
   r_de = g.OLD_GAME.r_de;
   trap_door = g.OLD_GAME.trap_door;
   interrupted = g.OLD_GAME.interrupted;
   can_int = g.OLD_GAME.can_int;
   reg_search = g.OLD_GAME.reg_search;
   monsters_count = g.OLD_GAME.monsters_count;
   mon_disappeared = g.OLD_GAME.mon_disappeared;
   level_objects = g.OLD_GAME.level_objects;
   level_monsters = g.OLD_GAME.level_monsters;
   new_level_message = g.OLD_GAME.new_level_message;
   hunger_str = g.OLD_GAME.hunger_str;
   hit_message = g.OLD_GAME.hit_message;
   msg_cleared = g.OLD_GAME.msg_cleared;
   msg_line = g.OLD_GAME.msg_line;
   msg_col = g.OLD_GAME.msg_col;
   same_msg = g.OLD_GAME.same_msg;
   m_moves = g.OLD_GAME.m_moves;
   wizard = g.OLD_GAME.wizard;
   experimented_pick_up_scare_monster =
     g.OLD_GAME.experimented_pick_up_scare_monster;
   rogue = g.OLD_GAME.rogue;
   random_rooms = g.OLD_GAME.random_rooms;
   id_potions = g.OLD_GAME.id_potions;
   id_rings = g.OLD_GAME.id_rings;
   id_scrolls = g.OLD_GAME.id_scrolls;
   id_wands = g.OLD_GAME.id_wands;
   is_wood = g.OLD_GAME.is_wood;
   rooms = g.OLD_GAME.rooms;
   traps = g.OLD_GAME.traps;
   dungeon = g.OLD_GAME.dungeon;
   env = Efield.make ()}
;

type old_saved = (OLD_GAME.t * array (array char));
value old_save_magic = "RGSV0004";

value save_into_file g fname = do {
  if g.score_only then
    f_random.Efield.set g.env "random" (Some (Random.get_state ()))
  else ();
  let oc = open_out_bin fname in
  let buf =
    Array.init DROWS (fun i -> Array.init DCOLS (fun j -> Curses.mvinch i j))
  in
  output_string oc save_magic;
  output_value oc ((g, buf) : saved);
  close_out oc
};

value display_dungeon g buf =
  for i = 0 to Array.length buf - 1 do {
    let line = buf.(i) in
    for j = 0 to Array.length line - 1 do {
      if line.(j) >= 'A' && line.(j) <= 'Z' &&
         g.dungeon.(i).(j) land MONSTER <> 0
      then
        let monster = monster_at g i j in
        show_monster g i j monster (gmc g monster)
      else if line.(j) = '^' then
        match trap_at g i j with
        [ Some trap -> show_trap i j trap
        | None -> Curses.mvaddch i j line.(j) ]
      else Curses.mvaddch i j line.(j);
    };
  }
;

value restore fname = do {
  let ic = open_in_bin fname in
  let b = string_create (String.length save_magic) in
  really_input ic b 0 (string_length b);
  let b = string_of_bytes b in
  if b = save_magic then do {
    let (g, buf) = (input_value ic : saved) in
    display_dungeon g buf;
    close_in ic;
    Sys.remove fname;
    g
  }
  else if b = old_save_magic then do {
    let (old_g, buf) = (input_value ic : old_saved) in
    let g = g_of_old_g old_g in
    display_dungeon g buf;
    close_in ic;
    Sys.remove fname;
    g
  }
  else do {
    close_in ic;
    failwith (sprintf "not a mlrogue saved file %s" b)
  }
};

value get_dir_rc dir row col allow_off_screen =
  match dir with
  [ ROGUE_KEY_WEST ->
      (row, if allow_off_screen || col > 0 then col - 1 else col)
  | ROGUE_KEY_SOUTH ->
      (if allow_off_screen || row < DROWS - 2 then row + 1 else row, col)
  | ROGUE_KEY_NORTH ->
      (if allow_off_screen || row > MIN_ROW then row - 1 else row, col)
  | ROGUE_KEY_EAST ->
      (row, if allow_off_screen || col < DCOLS - 1 then col + 1 else col)
  | ROGUE_KEY_NORTHWEST ->
      if allow_off_screen || row > MIN_ROW && col > 0 then (row - 1, col - 1)
      else (row, col)
  | ROGUE_KEY_NORTHEAST ->
      if allow_off_screen || row > MIN_ROW && col < DCOLS - 1 then
        (row - 1, col + 1)
      else (row, col)
  | ROGUE_KEY_SOUTHWEST ->
      if allow_off_screen || row < DROWS - 2 && col > 0 then
        (row + 1, col - 1)
      else (row, col)
  | ROGUE_KEY_SOUTHEAST ->
      if allow_off_screen || row < DROWS - 2 && col < DCOLS - 1 then
        (row + 1, col + 1)
      else (row, col)
  | _ -> invalid_arg "get_dir_rc" ]
;

value is_direction =
  fun
  [ ROGUE_KEY_WEST | ROGUE_KEY_SOUTH | ROGUE_KEY_NORTH | ROGUE_KEY_EAST |
    ROGUE_KEY_SOUTHWEST | ROGUE_KEY_NORTHWEST | ROGUE_KEY_NORTHEAST |
    ROGUE_KEY_SOUTHEAST | ROGUE_KEY_CANCEL ->
      True
  | _ -> False ]
;

value can_move g row1 col1 row2 col2 =
  if not (is_passable g row2 col2) then False
  else if row1 <> row2 && col1 <> col2 then
    if g.dungeon.(row1).(col1) land DOOR <> 0 ||
       g.dungeon.(row2).(col2) land DOOR <> 0 ||
       g.dungeon.(row1).(col2) = 0 || g.dungeon.(row2).(col1) = 0
    then
      False
    else True
  else True
;

value light_passage g row col =
  if g.rogue.blind > 0 then ()
  else
    let i_end = if row < DROWS - 2 then 1 else 0 in
    let j_end = if col < DCOLS - 1 then 1 else 0 in
    for i = if row > MIN_ROW then -1 else 0 to i_end do {
      for j = if col > 0 then -1 else 0 to j_end do {
        if can_move g row col (row + i) (col + j) then
          let i = row + i in
          let j = col + j in
          if g.dungeon.(i).(j) land MONSTER <> 0 then
            let monster = monster_at g i j in
            show_monster g i j monster (gmc g monster)
          else Curses.mvaddch i j (get_dungeon_char g i j)
        else ();
      };
    }
;

value light_up_room g rn =
  if g.rogue.blind = 0 then do {
    let rm = g.rooms.(rn) in
    for i = rm.top_row to rm.bottom_row do {
      for j = rm.left_col to rm.right_col do {
        if g.dungeon.(i).(j) land MONSTER <> 0 then do {
          let monster = monster_at g i j in
          g.dungeon.(i).(j) land_eq lnot MONSTER;
          monster.mn_trail_char := get_dungeon_char g i j;
          g.dungeon.(i).(j) or_eq MONSTER;
          show_monster g i j monster (gmc g monster)
        }
        else
          let ch = get_dungeon_char g i j in
          if ch = '^' then
            match trap_at g i j with
            [ Some t -> show_trap i j t
            | None -> Curses.mvaddch i j ch ]
          else Curses.mvaddch i j ch
      };
    };
    show_rogue g;
  }
  else ()
;

value relight g = do {
  match g.cur_room with
  [ None -> light_passage g g.rogue.row g.rogue.col
  | Some rn -> light_up_room g rn ];
  show_rogue g;
};

value ring_stats g = do {
  g.rogue.stealthy := 0;
  g.rogue.r_rings := 0;
  g.rogue.e_rings := 0;
  g.rogue.r_teleport := False;
  g.rogue.sustain_strength := False;
  g.rogue.add_strength := 0;
  g.rogue.regeneration := 0;
  g.rogue.ring_exp := 0;
  g.rogue.r_see_invisible := False;
  g.rogue.maintain_armor := False;
  g.rogue.auto_search := 0;
  for i = 0 to 1 do {
    let ring = if i = 0 then g.rogue.left_ring else g.rogue.right_ring in
    match ring with
    [ None -> ()
    | Some ring -> do {
        g.rogue.r_rings ++;
        g.rogue.e_rings ++;
        match ring.rg_kind with
        [ Stealth -> g.rogue.stealthy ++
        | RTeleport -> g.rogue.r_teleport := True
        | Regeneration -> g.rogue.regeneration ++
        | SlowDigest -> g.rogue.e_rings sub_eq 2
        | AddStrength -> g.rogue.add_strength add_eq ring.rg_class
        | SustainStrength -> g.rogue.sustain_strength := True
        | Dexterity -> g.rogue.ring_exp add_eq ring.rg_class
        | Adornment -> ()
        | RSeeInvisible -> g.rogue.r_see_invisible := True
        | MaintainArmor -> g.rogue.maintain_armor := True
        | Searching -> g.rogue.auto_search add_eq 2 ]
      } ];
  }
};

value unwield g =
  match g.rogue.weapon with
  [ Some (_, w) -> do { w.we_in_use := False; g.rogue.weapon := None }
  | None -> () ]
;

value unwear g =
  match g.rogue.armor with
  [ Some (_, a) -> do { a.ar_in_use := False; g.rogue.armor := None }
  | None -> () ]
;

value un_put_on g ring = do {
  match ring.rg_in_use with
  [ Some LeftHand -> g.rogue.left_ring := None
  | Some RightHand -> g.rogue.right_ring := None
  | None -> () ];
  ring.rg_in_use := None;
  ring_stats g;
  Dialogue.print_stats g STAT_STRENGTH;
  relight g
};

value vanish g ch obj =
  if obj.ob_quantity > 1 then obj.ob_quantity --
  else do {
    match obj.ob_kind with
    [ Weapon w -> if w.we_in_use then unwield g else ()
    | Armor a -> if a.ar_in_use then unwear g else ()
    | Ring r -> if r.rg_in_use <> None then un_put_on g r else ()
    | _ -> () ];
    take_from_pack g ch
  }
;

value show_monsters g = do {
  g.rogue.detect_monster := True;
  if g.rogue.blind > 0 then ()
  else
    List.iter
      (fun monster -> do {
         show_monster g monster.mn_row monster.mn_col monster
           (tgmc g monster.mn_char);
         if monster.mn_flags land IMITATES <> 0 then do {
           monster.mn_flags land_eq lnot IMITATES;
           monster.mn_flags or_eq WAKENS
         }
         else ()
       })
      g.level_monsters
};

value get_letter_object g ch mess_try_again =
  try Some (List.assoc ch g.rogue.pack) with
  [ Not_found -> do {
      Dialogue.message g
        (if mess_try_again then transl g.lang "No such item. Try again."
         else transl g.lang "No such item.")
        False;
      None
    } ]
;

value fast g = f_bool.Efield.get g.env "fast" False;
