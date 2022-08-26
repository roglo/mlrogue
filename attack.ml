(* $Id: attack.ml,v 1.34 2018/04/26 09:52:36 deraugla Exp $ *)

#load "pa_more.cmo";

#use "rogue.def";
#use "keyboard.def";

open Rogue;
open Dialogue;
open Imisc;
open Misc;
open Printf;
open Translate;

value check_imitator g monster =
  if monster.mn_flags land IMITATES <> 0 then do {
    wake_up monster;
    if g.rogue.blind = 0 then do {
      Curses.mvaddch monster.mn_row monster.mn_col
        (get_dungeon_char g monster.mn_row monster.mn_col);
      check_message g;
      let mess =
        sprintf (ftransl g.lang "Wait, that's a %s!")
          (transl g.lang (Monster.mon_name g monster))
      in
      message g (etransl mess) True
    }
    else ();
    True
  }
  else False
;

value to_hit =
  fun
  [ None -> 1
  | Some (_, w) ->
      let (n, _, _) = w.we_damage in
      n + w.we_hit_enchant ]
;

value get_hit_chance g weapon =
  let hit_chance = 40 in
  let hit_chance = hit_chance + 3 * to_hit weapon in
  hit_chance + 2 * g.rogue.exp + 2 * g.rogue.ring_exp - g.rogue.r_rings
;

value get_w_damage g weapon =
  match weapon with
  [ None -> -1
  | Some (_, w) ->
      let (n, d, _) = w.we_damage in
      let to_hit = n + w.we_hit_enchant in
      let damage = d + w.we_d_enchant in
      let new_damage = (to_hit, damage, None) in
      get_damage g new_damage True ]
;

value damage_for_strength g =
  let strength = g.rogue.str_current + g.rogue.add_strength in
  if strength <= 6 then strength - 5
  else if strength <= 14 then 1
  else if strength <= 17 then 3
  else if strength <= 18 then 4
  else if strength <= 20 then 5
  else if strength <= 21 then 6
  else if strength <= 30 then 7
  else 8
;

value get_weapon_damage g weapon =
  let damage = get_w_damage g weapon in
  let damage = damage + damage_for_strength g in
  damage + (g.rogue.exp + g.rogue.ring_exp - g.rogue.r_rings + 1) / 2
;

value coughable g row col =
  if row < MIN_ROW || row > DROWS - 2 || col < 0 || col > DCOLS - 1 then False
  else if
    g.dungeon.(row).(col) land (OBJECT lor STAIRS lor TRAP lor HIDDEN) = 0 &&
    g.dungeon.(row).(col) land (TUNNEL lor FLOOR lor DOOR) <> 0
  then
    True
  else False
;

value cough_up g monster =
  if g.cur_level < g.max_level then ()
  else
    let obj =
      if monster.mn_flags land STEALS_GOLD <> 0 then
        let q = get_rand (g.cur_level * 15) (g.cur_level * 30) in
        Some (Object.get_gold (Some q))
      else if not (rand_percent monster.mn_drop_percent) then None
      else Some (Object.gr_object g)
    in
    match obj with
    [ Some obj ->
        let row = monster.mn_row in
        let col = monster.mn_col in
        let add_in_set x s = if List.mem x s then s else [x :: s] in
        loop_n 0 where rec loop_n n =
          if n <= 4 then
            let list =
              loop_i [] (-n) where rec loop_i list i =
                if i <= n then
                  let list =
                    if coughable g (row + n) (col + i) then
                      add_in_set (row + n, col + i) list
                    else list
                  in
                  let list =
                    if coughable g (row - n) (col + i) then
                      add_in_set (row - n, col + i) list
                    else list
                  in
                  loop_i list (i + 1)
                else
                  let rec loop_i list i =
                    if i <= n then
                      let list =
                        if coughable g (row + i) (col - n) then
                          add_in_set (row + i, col - n) list
                        else list
                      in
                      let list =
                        if coughable g (row + i) (col + n) then
                          add_in_set (row + i, col + n) list
                        else list
                      in
                      loop_i list (i + 1)
                    else list
                  in
                  loop_i list (-n)
            in
            if list = [] then loop_n (n + 1)
            else do {
              let r = get_rand 0 (List.length list - 1) in
              let (row, col) = List.nth list r in
              Level.place_at g obj row col;
              if row <> g.rogue.row || col <> g.rogue.col then
                if g.dungeon.(row).(col) land MONSTER = 0 then
                  Curses.mvaddch row col (get_dungeon_char g row col)
                else
                  let mon = monster_at g row col in
                  mon.mn_trail_char := get_mask_char obj
              else ()
            }
          else ()
    | None -> () ]
;

value mon_damage g monster damage = do {
  monster.mn_hp_to_kill sub_eq damage;
  if monster.mn_hp_to_kill <= 0 then do {
    let row = monster.mn_row in
    let col = monster.mn_col in
    g.dungeon.(row).(col) land_eq lnot MONSTER;
    Curses.mvaddch row col (get_dungeon_char g row col);
    g.rogue.fight_monster := None;
    cough_up g monster;
    let mn = transl g.lang (Monster.mon_name g monster) in
    let msg = sprintf (ftransl g.lang "Defeated the %s.") mn in
    (*
    let msg = msg ^ sprintf " (%d-%d<=0)." (monster.mn_hp_to_kill + damage) damage in
    *)
    message g (g.hit_message ^ etransl msg) True;
    g.hit_message := "";
    add_exp g monster.mn_kill_exp hp_raise;
    take_from_monsters g monster;
    if monster.mn_flags land HOLDS <> 0 then g.rogue.being_held := False
    else ();
    False
  }
  else True
};

value check_gold_seeker g monster = monster.mn_flags land_eq lnot SEEKS_GOLD;

value rogue_hit g monster force_hit =
  if check_imitator g monster then ()
  else do {
    let hit_chance =
      if force_hit then 100 else get_hit_chance g g.rogue.weapon
    in
    let hit_chance = if g.wizard then hit_chance * 2 else hit_chance in
    if not (rand_percent hit_chance) then
      if g.rogue.fight_monster = None then
        g.hit_message := etransl (transl g.lang "You miss.") ^ " "
      else ()
    else
      let damage = get_weapon_damage g g.rogue.weapon in
      let damage = if g.wizard then damage * 3 else damage in
      if mon_damage g monster damage then
        if g.rogue.fight_monster = None then do {
          g.hit_message := etransl (transl g.lang "You hit.") ^ " ";
          let row = monster.mn_row in
          let col = monster.mn_col in
          show_monster g row col monster (gmc g monster);
        }
        else ()
      else ();
    check_gold_seeker g monster;
    wake_up monster
  }
;

value tele_away g monster = do {
  if monster.mn_flags land HOLDS <> 0 then g.rogue.being_held := False
  else ();
  let (row, col, _) =
    gr_row_col g (FLOOR lor TUNNEL lor STAIRS lor OBJECT) 0
  in
  Curses.mvaddch monster.mn_row monster.mn_col monster.mn_trail_char;
  g.dungeon.(monster.mn_row).(monster.mn_col) land_eq lnot MONSTER;
  monster.mn_row := row;
  monster.mn_col := col;
  g.dungeon.(row).(col) or_eq MONSTER;
  monster.mn_trail_char := Curses.mvinch row col;
  if g.rogue.detect_monster || rogue_can_see g row col then
(*
    Curses.mvaddch row col (gmc g monster)
*)
    show_monster g row col monster (gmc g monster)
(**)
  else ()
};

value zap_monster g monster wand =
  let row = monster.mn_row in
  let col = monster.mn_col in
  match wand with
  [ SlowMonster ->
      if monster.mn_flags land HASTED <> 0 then
        monster.mn_flags land_eq lnot HASTED
      else do {
        monster.mn_slowed_toggle := False;
        monster.mn_flags or_eq SLOWED
      }
  | HasteMonster ->
      if monster.mn_flags land SLOWED <> 0 then
        monster.mn_flags land_eq lnot SLOWED
      else monster.mn_flags or_eq HASTED
  | TeleportAway -> tele_away g monster
  | ConfuseMonster -> do {
      monster.mn_flags or_eq CONFUSED;
      monster.mn_moves_confused add_eq get_rand 12 22
    }
  | Invisibility -> monster.mn_flags or_eq INVISIBLE
  | Polymorph -> do {
      if monster.mn_flags land HOLDS <> 0 then g.rogue.being_held := False
      else ();
      let tc = monster.mn_trail_char in
      take_from_monsters g monster;
      let monster = Imonster.gr_monster g None in
      monster.mn_row := row;
      monster.mn_col := col;
      g.level_monsters := g.level_monsters @ [monster];
      monster.mn_trail_char := tc;
      if monster.mn_flags land IMITATES = 0 then wake_up monster else ();
      if g.rogue.detect_monster || rogue_can_see g row col then
(*
        Curses.mvaddch row col (gmc g monster)
*)
        show_monster g row col monster (gmc g monster)
(**)
      else ()
    }
  | PutToSleep -> do {
      monster.mn_flags or_eq ASLEEP lor NAPPING;
      monster.mn_nap_length := get_rand 3 6
    }
  | MagicMissile -> rogue_hit g monster True
  | Cancellation -> do {
      if monster.mn_flags land HOLDS <> 0 then g.rogue.being_held := False
      else ();
      if monster.mn_flags land STEALS_ITEM <> 0 then
        monster.mn_drop_percent := 0
      else ();
      monster.mn_flags land_eq
        lnot
          (FLIES lor FLITS lor SPECIAL_HIT lor INVISIBLE lor FLAMES lor
           IMITATES lor CONFUSES lor SEEKS_GOLD lor HOLDS)
    }
  | DoNothing -> message g (transl g.lang "Nothing happens" ^ ".") False ]
;

value get_zapped_monster g dir =
  loop_o g.rogue.row g.rogue.col where rec loop_o orow ocol =
    let (row, col) = get_dir_rc dir orow ocol False in
    if row = orow && col = ocol ||
       g.dungeon.(row).(col) land (HORWALL lor VERTWALL) <> 0 ||
       g.dungeon.(row).(col) = NOTHING
    then
      None
    else if g.dungeon.(row).(col) land MONSTER <> 0 then
      if not (imitating g row col) then Some (monster_at g row col, row, col)
      else loop_o row col
    else loop_o row col
;

value zap g = do {
  let dir =
    loop True where rec loop first_miss =
      let ch = rgetchar g in
      if not (is_direction ch) then do {
        sound_bell ();
        if first_miss then message g (transl g.lang "Direction?") False
        else ();
        loop False
      }
      else ch
  in
  check_message g;
  if dir = ROGUE_KEY_CANCEL then False
  else
    let ch =
      pack_letter g (transl g.lang "Zap with what?")
        (fun
         [ Wand _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then False
    else do {
      check_message g;
      match get_letter_object g ch False with
      [ None -> False
      | Some {ob_kind = Wand w} -> do {
          if w.wa_hits = 0 then
            message g (transl g.lang "Nothing happens" ^ ".") False
          else do {
            w.wa_hits --;
            match get_zapped_monster g dir with
            [ Some (monster, row, col) -> do {
                wake_up monster;
                zap_monster g monster w.wa_kind;
                relight g
              }
            | None -> () ]
          };
          True
        }
      | Some _ -> do {
          message g (transl g.lang "You can't zap with that" ^ ".") False;
          False
        } ]
    }
};

value throw_at_monster g monster obj = do {
  let weapon =
    match obj.ob_kind with
    [ Weapon w -> Some (' ', w)
    | _ -> None ]
  in
  let hit_chance = get_hit_chance g weapon in
  let damage = get_weapon_damage g weapon in
  let (damage, hit_chance) =
    match (obj.ob_kind, g.rogue.weapon) with
    [ (Weapon {we_kind = Arrow}, Some (_, {we_kind = Bow})) ->
        let damage = damage + get_weapon_damage g g.rogue.weapon in
        let damage = damage * 2 / 3 in
        let hit_chance = hit_chance + hit_chance / 3 in
        (damage, hit_chance)
    | (Weapon {we_kind = Dagger | Shuriken | Dart; we_in_use = True}, _) ->
        let damage = damage * 3 / 2 in
        let hit_chance = hit_chance + hit_chance / 3 in
        (damage, hit_chance)
    | _ -> (damage, hit_chance) ]
  in
  let t = obj.ob_quantity in
  obj.ob_quantity := 1;
  let mess = sprintf (ftransl g.lang "The %s") (name_of g obj) in
  obj.ob_quantity := t;
  if not (rand_percent hit_chance) then do {
    g.hit_message := etransl (mess ^ " " ^ transl g.lang "misses." ^ " ");
    False
  }
  else do {
    g.hit_message := etransl (mess ^ " " ^ transl g.lang "hit." ^ " ");
    match obj.ob_kind with
    [ Wand {wa_kind = wk} ->
        if rand_percent 75 then zap_monster g monster wk
        else
          let _ : bool = mon_damage g monster damage in
          ()
    | _ ->
        let _ : bool = mon_damage g monster damage in
        () ];
    True
  }
};

value tempo g =
  if fast g then ()
  else
    let (_, _, _) = Unix.select [] [] [] 0.05 in
    ()
;

value get_thrown_at_monster g obj dir orow ocol =
  let rogue = g.rogue in
  let ch = get_mask_char obj in
  loop orow ocol 0 where rec loop orow ocol i =
    if i < 24 then
      let (row, col) = get_dir_rc dir orow ocol False in
      if g.dungeon.(row).(col) = NOTHING ||
         g.dungeon.(row).(col) land (HORWALL lor VERTWALL lor HIDDEN) <> 0 &&
         g.dungeon.(row).(col) land TRAP = 0
      then
        (None, orow, ocol)
      else do {
        if i <> 0 && rogue_can_see g orow ocol then do {
          tempo g;
          Curses.mvaddch orow ocol (get_dungeon_char g orow ocol);
          Curses.move rogue.row rogue.col
        }
        else ();
        if rogue_can_see g row col then do {
          if g.dungeon.(row).(col) land MONSTER = 0 then do {
            if i = 0 then tempo g else ();
            Curses.mvaddch row col ch;
            Curses.move rogue.row rogue.col
          }
          else ();
          Curses.refresh ()
        }
        else ();
        if g.dungeon.(row).(col) land MONSTER <> 0 &&
           not (imitating g row col)
        then do {
          if rogue_can_see g row col then do {
            Curses.mvaddch row col ch;
            Curses.move rogue.row rogue.col;
            Curses.refresh ()
          }
          else ();
          tempo g;
          (Some (monster_at g row col), row, col)
        }
        else
          let i =
            if g.dungeon.(row).(col) land TUNNEL <> 0 then i + 2 else i
          in
          loop row col (i + 1)
      }
    else (None, orow, ocol)
;

value extract_copy_of_weapon obj =
  let copy_object_kind =
    fun
    [ Weapon w -> Weapon {(w) with we_kind = w.we_kind; we_in_use = False}
    | x -> x ]
  in
  {(obj) with ob_kind = copy_object_kind obj.ob_kind; ob_quantity = 1}
;

value flop_weapon g obj row col =
  let (found, i, row, col) =
    loop row col 0 where rec loop row col i =
      if i < 9 && row < DROWS - 1 && row >= MIN_ROW && col < DCOLS &&
         col >= 0 &&
         g.dungeon.(row).(col) land
         lnot (FLOOR lor TUNNEL lor DOOR lor MONSTER) <>
           0
      then
        let (row, col) = rand_around g i row col in
        let i = i + 1 in
        if row > DROWS - 2 || row < MIN_ROW || col > DCOLS - 1 || col < 0 ||
           g.dungeon.(row).(col) = 0 ||
           g.dungeon.(row).(col) land
           lnot (FLOOR lor TUNNEL lor DOOR lor MONSTER) <>
             0
        then
          loop row col i
        else (True, i, row, col)
      else (False, i, row, col)
  in
  if found || i = 0 then do {
    let new_obj = extract_copy_of_weapon obj in
    Level.place_at g new_obj row col;
    if rogue_can_see g row col && (row <> g.rogue.row || col <> g.rogue.col)
    then do {
      let mon = g.dungeon.(row).(col) land MONSTER in
      g.dungeon.(row).(col) land_eq lnot MONSTER;
      let dch = get_dungeon_char g row col in
      if mon <> 0 then do {
        let mch = Curses.mvinch row col in
        let monster = monster_at g row col in
        monster.mn_trail_char := dch;
        if mch < 'A' || mch > 'Z' then Curses.mvaddch row col dch else ()
      }
      else Curses.mvaddch row col dch;
      g.dungeon.(row).(col) or_eq mon
    }
    else ()
  }
  else do {
    let t = obj.ob_quantity in
    obj.ob_quantity := 1;
    let msg =
      sprintf (ftransl g.lang "The %s vanishes as it hits the ground.")
        (name_of g obj)
    in
    obj.ob_quantity := t;
    message g (etransl msg) False
  }
;

value one_throw g dir (ch, obj) = do {
  let rogue = g.rogue in
  let row = rogue.row in
  let col = rogue.col in
  let just_once =
    match obj with
    [ {ob_kind = Weapon {we_in_use = True}; ob_quantity = 1} -> do {
        unwield g;
        True
      }
    | {ob_kind = Armor {ar_in_use = True}} -> do {
        Monster.mv_aquators g;
        unwear g;
        print_stats g STAT_ARMOR;
        True
      }
    | {ob_kind = Ring ({rg_in_use = Some _} as r)} -> do {
        un_put_on g r;
        True
      }
    | _ -> False ]
  in
  let (monster, row, col) = get_thrown_at_monster g obj dir row col in
  show_rogue g;
  Curses.refresh ();
  if rogue_can_see g row col && (row <> rogue.row || col <> rogue.col) then
    Curses.mvaddch row col (get_dungeon_char g row col)
  else ();
  let just_once =
    match monster with
    [ Some monster -> do {
        wake_up monster;
        check_gold_seeker g monster;
        if not (throw_at_monster g monster obj) then flop_weapon g obj row col
        else ();
        True
      }
    | None -> do { flop_weapon g obj row col; just_once } ]
  in
  vanish g ch obj;
  just_once
};
