(* $Id: monster.ml,v 1.84 2013/05/16 19:24:40 deraugla Exp $ *)

#load "pa_more.cmo";
#use "rogue.def";

open Rogue;
open Dialogue;
open Imisc;
open Misc;
open Printf;
open Translate;

value mon_can_go g monster row col =
  let dr = monster.mn_row - row in
  let dc = monster.mn_col - col in
  if dr >= 2 || dr <= -2 then False
  else if dc >= 2 || dc <= -2 then False
  else if
    g.dungeon.(monster.mn_row).(col) = 0 ||
    g.dungeon.(row).(monster.mn_col) = 0
  then
    False
  else if
    not (is_passable g row col) || g.dungeon.(row).(col) land MONSTER <> 0
  then
    False
  else if
    monster.mn_row <> row && monster.mn_col <> col &&
    (g.dungeon.(row).(col) land DOOR <> 0 ||
     g.dungeon.(monster.mn_row).(monster.mn_col) land DOOR <> 0)
  then
    False
  else
    let r =
      if monster.mn_flags land (FLITS lor CONFUSED lor CAN_FLIT) = 0 &&
         monster.mn_target = None
      then
        if monster.mn_row < g.rogue.row && row < monster.mn_row ||
           monster.mn_row > g.rogue.row && row > monster.mn_row ||
           monster.mn_col < g.rogue.col && col < monster.mn_col ||
           monster.mn_col > g.rogue.col && col > monster.mn_col
        then
          False
        else True
      else True
    in
    if not r then False
    else if g.dungeon.(row).(col) land OBJECT <> 0 then
      match object_at g row col with
      [ {ob_kind = Scroll ScareMonster} -> False
      | _ -> True ]
    else True
;

value mon_sees g monster row col =
  match get_room_number g row col with
  [ Some rn ->
      if get_room_number g monster.mn_row monster.mn_col = Some rn &&
         g.rooms.(rn).is_room land R_MAZE = 0
      then
        True
      else
        let rdif = row - monster.mn_row in
        let cdif = col - monster.mn_col in
        rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1
  | None ->
      let rdif = row - monster.mn_row in
      let cdif = col - monster.mn_col in
      rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1 ]
;

value get_oth_room g rn row col =
  let d =
    if row = g.rooms.(rn).top_row then 0
    else if row = g.rooms.(rn).bottom_row then 2
    else if col = g.rooms.(rn).left_col then 3
    else if col = g.rooms.(rn).right_col then 1
    else -1
  in
  if d <> -1 then
    match g.rooms.(rn).doors.(d) with
    [ Some {oth_row = row; oth_col = col} -> Some (row, col)
    | None -> None ]
  else None
;

value dr_course g monster entering =
  let row = monster.mn_row in
  let col = monster.mn_col in
  let rogue = g.rogue in
  if mon_sees g monster rogue.row rogue.col then monster.mn_target := None
  else
    let rn =
      match get_room_number g row col with
      [ Some rn -> rn
      | None -> assert False ]
    in
    if entering then
      (* look for door to some other room *)
      let tried = Array.make MAXROOMS False in
      loop_i 0 where rec loop_i i =
        if i < MAXROOMS then
          let rr = get_rand 0 (MAXROOMS - 1) in
          if tried.(rr) then loop_i i
          else do {
            tried.(rr) := True;
            if g.rooms.(rr).is_room land (R_ROOM lor R_MAZE) = 0 || rr = rn
            then
              loop_i (i + 1)
            else
              let rec loop_k k =
                if k < 4 then
                  match g.rooms.(rr).doors.(k) with
                  [ Some door ->
                      if door.oth_room = rn then do {
                        monster.mn_target :=
                          Some (door.oth_row, door.oth_col);
                        if door.oth_row = row && door.oth_col = col then
                          loop_k (k + 1)
                        else ()
                      }
                      else loop_k (k + 1)
                  | None -> loop_k (k + 1) ]
                else loop_i (i + 1)
              in
              loop_k 0
          }
        else
          (* look for door to dead end *)
          let rec loop_i list i =
            if i <= g.rooms.(rn).bottom_row then
              let rec loop_j list j =
                if j <= g.rooms.(rn).right_col then
                  if (i <> monster.mn_row || j <> monster.mn_col) &&
                     g.dungeon.(i).(j) land DOOR <> 0
                  then
                    loop_j [(i, j) :: list] (j + 1)
                  else loop_j list (j + 1)
                else loop_i list (i + 1)
              in
              loop_j list g.rooms.(rn).left_col
            else if list <> [] then
              let r = get_rand 0 (List.length list - 1) in
              monster.mn_target := Some (List.nth list r)
            else
              (* return monster to room that he came from *)
              let rec loop_i i =
                if i < MAXROOMS then
                  let rec loop_j j =
                    if j < 4 then
                      match g.rooms.(i).doors.(j) with
                      [ Some door when door.oth_room = rn ->
                          loop_k 0 where rec loop_k k =
                            if k < 4 then
                              match g.rooms.(rn).doors.(k) with
                              [ Some door when door.oth_room = i ->
                                  monster.mn_target :=
                                    Some (door.oth_row, door.oth_col)
                              | _ -> loop_k (k + 1) ]
                            else loop_j (j + 1)
                      | _ -> loop_j (j + 1) ]
                    else loop_i (i + 1)
                  in
                  loop_j 0
                else monster.mn_target := None
              in
              loop_i 0
          in
          loop_i [] g.rooms.(rn).top_row
    else
      (* exiting room *)
      monster.mn_target := get_oth_room g rn row col
;

value move_mon_to g monster row col = do {
  let mrow = monster.mn_row in
  let mcol = monster.mn_col in
  g.dungeon.(mrow).(mcol) land_eq lnot MONSTER;
  g.dungeon.(row).(col) or_eq MONSTER;
  let c = Curses.mvinch mrow mcol in
  if c >= 'A' && c <= 'Z' then do {
    if not g.rogue.detect_monster then ()
    else if rogue_can_see g mrow mcol then ()
    else if monster.mn_trail_char = '.' then monster.mn_trail_char := ' '
    else ();
    Curses.mvaddch mrow mcol monster.mn_trail_char
  }
  else ();
  monster.mn_trail_char := Curses.mvinch row col;
  if g.rogue.blind = 0 && (g.rogue.detect_monster || rogue_can_see g row col)
  then
    if monster.mn_flags land INVISIBLE = 0 || g.rogue.detect_monster ||
       g.rogue.see_invisible || g.rogue.r_see_invisible
    then
      show_monster g row col monster (gmc g monster)
    else ()
  else ();
  if g.dungeon.(row).(col) land DOOR <> 0 then
    match get_room_number g row col with
    [ Some rn ->
        if g.cur_room <> Some rn && g.dungeon.(mrow).(mcol) = FLOOR &&
           g.rogue.blind = 0
        then
          Curses.mvaddch mrow mcol ' '
        else ()
    | None -> () ]
  else ();
  monster.mn_row := row;
  monster.mn_col := col;
  if g.dungeon.(row).(col) land DOOR <> 0 then
    dr_course g monster (g.dungeon.(mrow).(mcol) land TUNNEL <> 0)
  else ()
};

value mtry g monster row col =
  if row < MIN_ROW || row >= DROWS || col < 0 || col >= DCOLS then False
  else if mon_can_go g monster row col then do {
    move_mon_to g monster row col;
    True
  }
  else
    False
;

value flit g monster =
  if not (rand_percent FLIT_PERCENT) then False
  else if rand_percent 10 then True
  else
    let row = monster.mn_row in
    let col = monster.mn_col in
    loop_i 0 where rec loop_i i =
      if i < 9 then
        let (row, col) = rand_around g i row col in
        if row = g.rogue.row && col = g.rogue.col then loop_i (i + 1)
        else if mtry g monster row col then True
        else loop_i (i + 1)
      else True
;

value mon_name g monster =
  if g.rogue.blind > 0 ||
     monster.mn_flags land INVISIBLE <> 0 &&
     not
       (g.rogue.detect_monster || g.rogue.see_invisible ||
        g.rogue.r_see_invisible)
  then
    "something"
  else if g.rogue.halluc > 0 then
    let ch = get_rand (Char.code 'A') (Char.code 'Z') - Char.code 'A' in
    Imonster.visible_mon_name g ch
  else
    let ch = Char.code monster.mn_char - Char.code 'A' in
    Imonster.visible_mon_name g ch
;

value confuse g = do {
  g.rogue.confused add_eq get_rand 12 22;
  show_rogue g
};

value m_confuse g monster =
  if not (rogue_can_see g monster.mn_row monster.mn_col) then False
  else if rand_percent 45 then do { monster.mn_flags land_eq lnot CONFUSES; False }
  else if rand_percent 55 then do {
    monster.mn_flags land_eq lnot CONFUSES;
    confuse g;
    let msg =
      sprintf (ftransl g.lang "The gaze of the %s has confused you.")
        (transl g.lang (mon_name g monster))
    in
    message g (etransl msg) True;
    True
  }
  else False
;

value rogue_damage g d monster =
  if d >= g.rogue.hp_current then do {
    g.rogue.hp_current := 0;
    print_stats g STAT_HP;
    let i = Char.code monster.mn_char - Char.code 'A' in
    Finish.killed_by g (Monster (Imonster.visible_mon_name g i))
  }
  else do {
    g.rogue.hp_current sub_eq d;
    print_stats g STAT_HP;
    show_rogue g
  }
;

value sting g monster =
  let rogue = g.rogue in
  if rogue.str_current <= 3 || g.rogue.sustain_strength then ()
  else
    let sting_chance = 35 + 6 * (6 - get_armor_class rogue.armor) in
    let sting_chance =
      if rogue.exp + rogue.ring_exp > 8 then
        sting_chance - 6 * (rogue.exp - rogue.ring_exp - 8)
      else sting_chance
    in
    if rand_percent sting_chance then do {
      let msg =
        sprintf (ftransl g.lang "The %s's bite has weakened you.")
          (transl g.lang (mon_name g monster))
      in
      message g (etransl msg) False;
      rogue.str_current --;
      print_stats g STAT_STRENGTH
    }
    else ()
;

value rust g monster =
  match g.rogue.armor with
  [ None | Some (_, {ar_kind = Leather}) -> ()
  | Some (_, a) ->
      if get_armor_class g.rogue.armor <= 1 then ()
      else if a.ar_is_protected || g.rogue.maintain_armor then
        match monster with
        [ Some monster ->
            if monster.mn_flags land RUST_VANISHED = 0 then do {
              message g (transl g.lang "The rust vanishes instantly.") False;
              monster.mn_flags or_eq RUST_VANISHED
            }
            else ()
        | None -> () ]
      else do {
        a.ar_enchant --;
        message g (transl g.lang "Your armor weakens.") False;
        print_stats g STAT_ARMOR
      } ]
;

value disappear g monster = do {
  let row = monster.mn_row in
  let col = monster.mn_col in
  g.dungeon.(row).(col) land_eq lnot MONSTER;
  if rogue_can_see g row col then
    Curses.mvaddch row col (get_dungeon_char g row col)
  else ();
  take_from_monsters g monster;
  g.mon_disappeared := True
};

value drain_life g =
  let rogue = g.rogue in
  if rand_percent 60 || rogue.hp_max <= 30 || rogue.hp_current < 10 then ()
  else do {
    let n = get_rand 1 3 in
    if n <> 2 || not rogue.sustain_strength then
      message g (transl g.lang "You feel weaker.") False
    else ();
    if n <> 2 then do {
      rogue.hp_max --;
      rogue.hp_current --;
      rogue.less_hp ++;
      show_rogue g;
    }
    else ();
    if n <> 1 then
      if rogue.str_current > 3 && not rogue.sustain_strength then do {
        rogue.str_current --;
        if coin_toss () then rogue.str_max -- else ()
      }
      else ()
    else ();
    print_stats g (STAT_STRENGTH lor STAT_HP)
  }
;

value drop_level g =
  let rogue = g.rogue in
  if rand_percent 80 || rogue.exp <= 5 then ()
  else do {
    rogue.exp_points := level_points.(rogue.exp - 2) - get_rand 9 29;
    rogue.exp sub_eq 2;
    let hp = hp_raise g in
    rogue.hp_current sub_eq hp;
    if rogue.hp_current <= 0 then rogue.hp_current := 1 else ();
    rogue.hp_max sub_eq hp;
    if rogue.hp_max <= 0 then rogue.hp_max := 1 else ();
    add_exp g 1 (fun _ -> 0);
    show_rogue g
  }
;

value steal_gold g monster =
  if g.rogue.gold <= 0 || rand_percent 10 then ()
  else do {
    let amount = get_rand (g.cur_level * 10) (g.cur_level * 30) in
    let amount = min amount g.rogue.gold in
    g.rogue.gold sub_eq amount;
    message g (transl g.lang "Your purse feels lighter.") False;
    print_stats g STAT_GOLD;
    disappear g monster
  }
;

value in_use obj =
  match obj.ob_kind with
  [ Armor a -> a.ar_in_use
  | Ring r -> r.rg_in_use <> None
  | Weapon w -> w.we_in_use
  | _ -> False ]
;

value steal_item g monster =
  if rand_percent 15 then ()
  else do {
    if g.rogue.pack = [] then ()
    else
      let objs =
        List.filter (fun (_, obj) -> not (in_use obj)) g.rogue.pack
      in
      let len = List.length objs in
      if len = 0 then ()
      else do {
        let n = get_rand 0 (len - 1) in
        let (ch, obj) = List.nth objs n in
        let obj1 =
          {(obj) with
           ob_quantity =
             match obj.ob_kind with
             [ Weapon _ -> obj.ob_quantity
             | _ -> 1 ]}
        in
        let msg = transl g.lang "She stole" ^ " " ^ get_desc g obj1 False in
        message g (etransl msg) False;
        if obj1.ob_quantity = obj.ob_quantity then take_from_pack g ch
        else obj.ob_quantity --
      };
    disappear g monster
  }
;

value get_closer row col trow tcol =
  let row =
    if row < trow then row + 1 else if row > trow then row - 1 else row
  in
  let col =
    if col < tcol then col + 1 else if col > tcol then col - 1 else col
  in
  (row, col)
;

value move_confused g monster =
  if monster.mn_flags land ASLEEP = 0 then do {
    monster.mn_moves_confused --;
    if monster.mn_moves_confused <= 0 then monster.mn_flags land_eq lnot CONFUSED
    else ();
    if monster.mn_flags land STATIONARY <> 0 then coin_toss ()
    else if rand_percent 15 then True
    else
      let row = monster.mn_row in
      let col = monster.mn_col in
      loop_i 0 where rec loop_i i =
        if i < 9 then
          let (row, col) = rand_around g i row col in
          if row = g.rogue.row && col = g.rogue.col then False
          else if mtry g monster row col then True
          else loop_i (i + 1)
        else False
  }
  else False
;

value rec mv_mons g =
  if g.rogue.haste_self mod 2 = 1 then ()
  else
    List.iter
      (fun monster ->
         if g.mon_disappeared &&
            not (List.exists
                   (fun mn -> mn.mn_unique_id = monster.mn_unique_id)
                   g.level_monsters)
         then ()
         else
           let init_pos = Some (monster.mn_row, monster.mn_col) in
           let nm =
             if monster.mn_flags land HASTED <> 0 then do {
               g.mon_disappeared := False;
               mv_monster g monster g.rogue.row g.rogue.col None;
               if g.mon_disappeared then 1 else 0
             }
             else 2
           in
           let nm =
             if nm = 2 && monster.mn_flags land SLOWED <> 0 then do {
               monster.mn_slowed_toggle := not monster.mn_slowed_toggle;
               if monster.mn_slowed_toggle then 1 else 0
             }
             else nm
           in
           let nm =
             if nm <> 1 && monster.mn_flags land CONFUSED <> 0 &&
                move_confused g monster
             then
               1
             else nm
           in
           if nm <> 1 then
             let flew =
               if monster.mn_flags land FLIES <> 0 &&
                  monster.mn_flags land NAPPING = 0 &&
                  not (mon_can_go g monster g.rogue.row g.rogue.col)
               then do {
                 mv_monster g monster g.rogue.row g.rogue.col init_pos;
                 True
               }
               else False
             in
             if not (flew && mon_can_go g monster g.rogue.row g.rogue.col)
             then
               mv_monster g monster g.rogue.row g.rogue.col init_pos
             else ()
           else ())
      g.level_monsters
and mv_monster g monster row col init_pos_opt =
  if monster.mn_flags land ASLEEP <> 0 then
    if monster.mn_flags land NAPPING <> 0 then do {
      monster.mn_nap_length --;
      if monster.mn_nap_length <= 0 then
        monster.mn_flags land_eq lnot (NAPPING lor ASLEEP)
      else ()
    }
    else if
      monster.mn_flags land WAKENS <> 0 &&
      rogue_is_around g monster.mn_row monster.mn_col &&
      rand_percent
        (if g.rogue.stealthy > 0 then
           WAKE_PERCENT / (STEALTH_FACTOR + g.rogue.stealthy)
         else WAKE_PERCENT)
    then
      wake_up monster
    else ()
  else if monster.mn_flags land ALREADY_MOVED <> 0 then
    monster.mn_flags land_eq lnot ALREADY_MOVED
  else if monster.mn_flags land FLITS <> 0 && flit g monster then
    ()
  else if
    monster.mn_flags land STATIONARY <> 0 &&
    not (mon_can_go g monster g.rogue.row g.rogue.col)
  then
    ()
  else if monster.mn_flags land FREEZING_ROGUE <> 0 then
    ()
  else if monster.mn_flags land CONFUSES <> 0 && m_confuse g monster then
    ()
  else if mon_can_go g monster g.rogue.row g.rogue.col then
    mon_hit g monster "" False
  else if monster.mn_flags land FLAMES <> 0 && flame_broil g monster then
    ()
  else if monster.mn_flags land SEEKS_GOLD <> 0 && seek_gold g monster then
    ()
  else
    let (row, col) =
      match monster.mn_target with
      [ Some (trow, tcol) ->
          if monster.mn_row = trow && monster.mn_col = tcol then do {
            monster.mn_target := None;
            (row, col)
          }
          else (trow, tcol)
      | None -> (row, col) ]
    in
    let row =
      if monster.mn_row > row then monster.mn_row - 1
      else if monster.mn_row < row then monster.mn_row + 1
      else row
    in
    let col =
      if monster.mn_col > col then monster.mn_col - 1
      else if monster.mn_col < col then monster.mn_col + 1
      else col
    in
    if g.dungeon.(row).(monster.mn_col) land DOOR <> 0 &&
       mtry g monster row monster.mn_col
    then
      ()
    else if
      g.dungeon.(monster.mn_row).(col) land DOOR <> 0 &&
      mtry g monster monster.mn_row col
    then
      ()
    else if mtry g monster row col then
      ()
    else do {
      let tried = Array.make 6 False in
      loop_i 0 where rec loop_i i =
        if i < 6 then
          let n = get_rand 0 5 in
          if not tried.(n) then
            let (row, col) =
              match n with
              [ 0 -> (row, monster.mn_col - 1)
              | 1 -> (row, monster.mn_col)
              | 2 -> (row, monster.mn_col + 1)
              | 3 -> (monster.mn_row - 1, col)
              | 4 -> (monster.mn_row, col)
              | 5 -> (monster.mn_row + 1, col)
              | _ -> assert False ]
            in
            if mtry g monster row col then ()
            else do { tried.(n) := True; loop_i (i + 1) }
          else loop_i i
        else ();
      if monster.mn_row = monster.mn_o_row &&
         monster.mn_col = monster.mn_o_col ||
         monster.mn_flags land HASTED <> 0 &&
         match init_pos_opt with
         [ Some (init_row, init_col) ->
             monster.mn_row = init_row && monster.mn_col = init_col
         | None -> False ]
      then do {
        monster.mn_o ++;
        if monster.mn_o > 4 then
          if monster.mn_target = None &&
             not (mon_sees g monster g.rogue.row g.rogue.col)
          then
            let trow = get_rand 1 (DROWS - 2) in
            let tcol = get_rand 0 (DCOLS - 1) in
            monster.mn_target := Some (trow, tcol)
          else do {
            monster.mn_target := None;
            monster.mn_o := 0
          }
        else ()
      }
      else do {
        monster.mn_o_row := monster.mn_row;
        monster.mn_o_col := monster.mn_col;
        monster.mn_o := 0
      }
    }
and mon_hit g monster other flame = do {
  let rogue = g.rogue in
  match rogue.fight_monster with
  [ Some m -> if m <> monster then rogue.fight_monster := None else ()
  | None -> () ];
  monster.mn_target := None;
  let hit_chance =
    if g.cur_level >= AMULET_LEVEL * 2 then 100
    else
      monster.mn_hit_chance -
      (2 * rogue.exp + 2 * rogue.ring_exp - rogue.r_rings)
  in
  let hit_chance = if g.wizard then hit_chance / 2 else hit_chance in
  if rogue.fight_monster = None then g.interrupted := True else ();
  let mn = transl g.lang (mon_name g monster) in
  let hit_chance =
    if other <> "" then
      hit_chance - (rogue.exp + rogue.ring_exp - rogue.r_rings)
    else hit_chance
  in
  if not (rand_percent hit_chance) then
    if g.rogue.fight_monster = None then do {
      let msg =
        sprintf (ftransl g.lang "The %s misses.")
          (if other <> "" then other else mn)
      in
      message g (g.hit_message ^ etransl msg) True;
      g.hit_message := ""
    }
    else ()
  else do {
    if rogue.fight_monster = None then do {
      let msg =
        sprintf (ftransl g.lang "The %s hit.")
          (if other <> "" then other else mn)
      in
      message g (g.hit_message ^ etransl msg) True;
      show_rogue g;
      g.hit_message := ""
    }
    else ();
    let damage =
      if monster.mn_flags land STATIONARY = 0 then
        let damage = get_damage g monster.mn_damage True in
        let damage =
          if other <> "" && flame then
            max 1 (damage - get_armor_class rogue.armor)
          else damage
        in
        let minus =
          if g.cur_level >= AMULET_LEVEL * 2 then
            AMULET_LEVEL * 2 - g.cur_level
          else get_armor_class rogue.armor * 3 * damage / 100
        in
        damage - minus
      else do {
        let x = monster.mn_stationary_damage in
        monster.mn_stationary_damage ++;
        x
      }
    in
    let damage = if g.wizard then damage / 3 else damage in
    if damage > 0 then rogue_damage g damage monster else ();
    if monster.mn_flags land SPECIAL_HIT <> 0 then special_hit g monster
    else ()
  }
}
and special_hit g monster =
  if monster.mn_flags land CONFUSED <> 0 && rand_percent 66 then ()
  else do {
    if monster.mn_flags land RUSTS <> 0 then rust g (Some monster) else ();
    if monster.mn_flags land HOLDS <> 0 && g.rogue.levitate = 0 then
      g.rogue.being_held := True
    else ();
    if monster.mn_flags land FREEZES <> 0 then freeze g monster else ();
    if monster.mn_flags land STINGS <> 0 then sting g monster else ();
    if monster.mn_flags land DRAINS_LIFE <> 0 then drain_life g else ();
    if monster.mn_flags land DROPS_LEVEL <> 0 then drop_level g else ();
    if monster.mn_flags land STEALS_GOLD <> 0 then steal_gold g monster
    else if monster.mn_flags land STEALS_ITEM <> 0 then steal_item g monster
    else ()
  }
and freeze g monster =
  if rand_percent 12 then ()
  else
    let rogue = g.rogue in
    let freeze_percent = 99 - rogue.str_current + rogue.str_current / 2 in
    let freeze_percent = freeze_percent - (rogue.exp + rogue.ring_exp) / 4 in
    let freeze_percent = freeze_percent - get_armor_class rogue.armor * 5 in
    let freeze_percent = freeze_percent - rogue.hp_max / 3 in
    if freeze_percent > 10 then do {
      monster.mn_flags or_eq FREEZING_ROGUE;
      message g (transl g.lang "You are frozen.") True;
      let n = get_rand 4 8 in
      for i = 0 to n - 1 do { mv_mons g };
      if rand_percent freeze_percent then do {
        for i = 0 to 49 do { mv_mons g };
        Finish.killed_by g Hypothermia
      }
      else do {
        message g (transl g.lang "You can move again.") True;
        monster.mn_flags land_eq lnot FREEZING_ROGUE
      }
    }
    else ()
and seek_gold g monster =
  match get_room_number g monster.mn_row monster.mn_col with
  [ None -> False
  | Some rn ->
      let rm = g.rooms.(rn) in
      loop_i (rm.top_row + 1) where rec loop_i i =
        if i < rm.bottom_row then
          let rec loop_j j =
            if j < rm.right_col then
              if gold_at g i j && g.dungeon.(i).(j) land MONSTER = 0 then do {
                monster.mn_flags or_eq CAN_FLIT;
                let s = mon_can_go g monster i j in
                monster.mn_flags land_eq lnot CAN_FLIT;
                if s then do {
                  move_mon_to g monster i j;
                  monster.mn_flags or_eq ASLEEP;
                  monster.mn_flags land_eq lnot (WAKENS lor SEEKS_GOLD)
                }
                else do {
                  monster.mn_flags land_eq lnot SEEKS_GOLD;
                  monster.mn_flags or_eq CAN_FLIT;
                  mv_monster g monster i j None;
                  monster.mn_flags land_eq lnot CAN_FLIT;
                  monster.mn_flags or_eq SEEKS_GOLD
                };
                True
              }
              else loop_j (j + 1)
            else loop_i (i + 1)
          in
          loop_j (rm.left_col + 1)
        else False ]
and flame_broil g monster =
  if not (mon_sees g monster g.rogue.row g.rogue.col) || coin_toss () then
    False
  else
    let drow = abs (g.rogue.row - monster.mn_row) in
    let dcol = abs (g.rogue.col - monster.mn_col) in
    if drow <> 0 && dcol <> 0 && drow <> dcol || drow > 7 || dcol > 7 then
      False
    else do {
      if g.rogue.blind = 0 &&
         not (rogue_is_around g monster.mn_row monster.mn_col) &&
         not (fast g)
      then do {
        let row = monster.mn_row in
        let col = monster.mn_col in
        let (row, col) = get_closer row col g.rogue.row g.rogue.col in
        let tempo1 () =
          let (_, _, _) = Unix.select [] [] [] 0.08 in
          ()
        in
        let tempo2 () =
          let (_, _, _) = Unix.select [] [] [] 0.02 in
          ()
        in
        loop row col where rec loop row col = do {
          Curses.mvaddch row col '~';
          Curses.move g.rogue.row g.rogue.col;
          Curses.refresh ();
          tempo1 ();
          let (row, col) = get_closer row col g.rogue.row g.rogue.col in
          if row <> g.rogue.row || col <> g.rogue.col then loop row col
          else ()
        };
        let row = monster.mn_row in
        let col = monster.mn_col in
        let (row, col) = get_closer row col g.rogue.row g.rogue.col in
        loop row col where rec loop row col = do {
          Curses.mvaddch row col (get_dungeon_char g row col);
          Curses.move g.rogue.row g.rogue.col;
          Curses.refresh ();
          tempo2 ();
          let (row, col) = get_closer row col g.rogue.row g.rogue.col in
          if row <> g.rogue.row || col <> g.rogue.col then loop row col
          else ()
        }
      }
      else ();
      mon_hit g monster (transl g.lang "flame") True;
      True
    }
;

value mv_aquators g =
  List.iter
    (fun monster ->
       if monster.mn_flags land RUSTS <> 0 &&
          mon_can_go g monster g.rogue.row g.rogue.col
       then do {
         mv_monster g monster g.rogue.row g.rogue.col None;
         monster.mn_flags or_eq ALREADY_MOVED
       }
       else ())
    g.level_monsters
;
