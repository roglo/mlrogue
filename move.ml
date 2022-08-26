(* $Id: move.ml,v 1.85 2013/05/15 20:12:12 deraugla Exp $ *)

#load "pa_more.cmo";

#use "rogue.def";
#use "keyboard.def";

open Rogue;
open Rfield;
open Dialogue;
open Imisc;
open Misc;
open Printf;
open Translate;

type one_move = [ Moved | StoppedOnSomething | MoveFailed ];

value gr_dir () =
  match get_rand 1 8 with
  [ 1 -> ROGUE_KEY_SOUTH
  | 2 -> ROGUE_KEY_NORTH
  | 3 -> ROGUE_KEY_EAST
  | 4 -> ROGUE_KEY_WEST
  | 5 -> ROGUE_KEY_NORTHWEST
  | 6 -> ROGUE_KEY_NORTHEAST
  | 7 -> ROGUE_KEY_SOUTHWEST
  | 8 -> ROGUE_KEY_SOUTHEAST
  | _ -> assert False ]
;

value unhallucinate g = do {
  g.rogue.halluc := 0;
  relight g;
  message g (transl g.lang "Everything looks so boring now.") True
};

value hallucinate g =
  if g.rogue.blind > 0 then ()
  else do {
    List.iter
      (fun obj ->
         let ch = Curses.mvinch obj.ob_row obj.ob_col in
         if (ch < 'A' || ch > 'Z') &&
            (obj.ob_row <> g.rogue.row || obj.ob_col <> g.rogue.col)
         then
           if ch <> ' ' && ch <> '.' && ch <> '#' && ch <> '+' then
             Curses.addch (gr_obj_char ())
           else ()
         else ())
      g.level_objects;
    List.iter
      (fun monster ->
         let ch = Curses.mvinch monster.mn_row monster.mn_col in
         if ch >= 'A' && ch <= 'Z' then
           let ch = Char.chr (get_rand (Char.code 'A') (Char.code 'Z')) in
           Curses.addch (tgmc g ch)
         else ())
      g.level_monsters
  }
;

value unblind g = do {
  g.rogue.blind := 0;
  message g (transl g.lang "The veil of darkness lifts.") True;
  relight g;
  if g.rogue.halluc > 0 then hallucinate g else ();
  if g.rogue.detect_monster then show_monsters g else ()
};

value unconfuse g = do {
  g.rogue.confused := 0;
  show_rogue g;
  message g
    (sprintf (ftransl g.lang "You feel less %s now.")
       (if g.rogue.halluc > 0 then transl g.lang "trippy"
        else transl g.lang "confused"))
    True
};

value darken_room g rn =
  for i = g.rooms.(rn).top_row + 1 to g.rooms.(rn).bottom_row - 1 do {
    for j = g.rooms.(rn).left_col + 1 to g.rooms.(rn).right_col - 1 do {
      if g.rogue.blind > 0 then Curses.mvaddch i j ' '
      else if
        g.dungeon.(i).(j) land (OBJECT lor STAIRS) = 0 &&
        not (g.rogue.detect_monster && g.dungeon.(i).(j) land MONSTER <> 0)
      then do {
        if not (imitating g i j) then Curses.mvaddch i j ' ' else ();
        if g.dungeon.(i).(j) land TRAP <> 0 &&
           g.dungeon.(i).(j) land HIDDEN = 0
        then
          match trap_at g i j with
          [ Some t -> show_trap i j t
          | None -> Curses.mvaddch i j '^' ]
        else ()
      }
      else ();
    };
  }
;

value wake_room g rn entering row col =
  let wake_percent =
    if g.party_room = Some rn then PARTY_WAKE_PERCENT else WAKE_PERCENT
  in
  let wake_percent =
    if g.rogue.stealthy > 0 then
      wake_percent / (STEALTH_FACTOR + g.rogue.stealthy)
    else wake_percent
  in
  List.iter
    (fun monster ->
       let in_room =
         get_room_number g monster.mn_row monster.mn_col = Some rn
       in
       if in_room then do {
         monster.mn_target := if entering then None else Some (row, col);
         if monster.mn_flags land WAKENS <> 0 && rand_percent wake_percent
         then
           wake_up monster
         else ()
       }
       else ())
    g.level_monsters
;

value tele g = do {
  let dc = get_dungeon_char g g.rogue.row g.rogue.col in
  Curses.mvaddch g.rogue.row g.rogue.col dc;
  match g.cur_room with
  [ Some rn -> if rn >= 0 then darken_room g rn else ()
  | None -> () ];
  Level.put_player g (get_room_number g g.rogue.row g.rogue.col);
  relight g;
  match g.cur_room with
  [ Some rn -> wake_room g rn True g.rogue.row g.rogue.col
  | None -> () ];
  g.rogue.being_held := False;
  g.rogue.bear_trap := 0;
  g.rogue.fight_monster := None
};

value take_a_nap g = do {
  if not (fast g) then Unix.sleep 1 else ();
  for i = 1 to get_rand 2 5 do { Monster.mv_mons g };
  if not (fast g) then Unix.sleep 1 else ();
  message g (transl g.lang "You can move again.") False
};

value trap_player g row col =
  match trap_at g row col with
  [ None -> ()
  | Some t -> do {
      let rogue = g.rogue in
      g.dungeon.(row).(col) land_eq lnot HIDDEN;
      if rand_percent (rogue.exp + rogue.ring_exp) then
        message g (transl g.lang "The trap failed.") True
      else
        match t with
        [ TrapDoor -> do {
            g.trap_door := True;
            g.new_level_message := transl g.lang (Level.trap_mess t)
          }
        | BearTrap -> do {
            message g (transl g.lang (Level.trap_mess t)) True;
            g.rogue.bear_trap := get_rand 4 7
          }
        | TeleTrap -> do { Curses.mvaddch rogue.row rogue.col '^'; tele g }
        | DartTrap -> do {
            message g (transl g.lang (Level.trap_mess t)) True;
            rogue.hp_current sub_eq get_damage g (1, 6, None) True;
            rogue.hp_current := max 0 rogue.hp_current;
            if not rogue.sustain_strength && rand_percent 40 &&
               rogue.str_current >= 3
            then
              rogue.str_current --
            else ();
            print_stats g (STAT_HP lor STAT_STRENGTH);
            show_rogue g;
            if rogue.hp_current <= 0 then Finish.killed_by g PoisonDart
            else ()
          }
        | SleepingGasTrap -> do {
            message g (transl g.lang (Level.trap_mess t)) True;
            take_a_nap g
          }
        | RustTrap -> do {
            message g (transl g.lang (Level.trap_mess t)) True;
            Monster.rust g None
          } ]
    } ]
;

(*
value heal =
  let heal_exp = ref (-1) in
  let n = ref 0 in
  let c = ref 0 in
  let alt = ref False in
  fun g ->
    if g.rogue.hp_current = g.rogue.hp_max then c.val := 0
    else do {
      if g.rogue.exp <> heal_exp.val then do {
        heal_exp.val := g.rogue.exp;
        n.val :=
          match heal_exp.val with
          [ 1 -> 20
          | 2 -> 18
          | 3 -> 17
          | 4 -> 14
          | 5 -> 13
          | 6 -> 10
          | 7 -> 9
          | 8 -> 8
          | 9 -> 7
          | 10 -> 4
          | 11 -> 3
          | _ -> 2 ]
      }
      else ();
      incr c;
      if c.val >= n.val then do {
        c.val := 0;
        g.rogue.hp_current ++;
        alt.val := not alt.val;
        if alt.val then g.rogue.hp_current ++ else ();
        g.rogue.hp_current += g.rogue.regeneration;
        g.rogue.hp_current := min g.rogue.hp_max g.rogue.hp_current;
        print_stats g STAT_HP;
        show_rogue g
      }
      else ()
    }
;
*)

value heal g =
  if g.rogue.hp_current = g.rogue.hp_max then do {
    f_int.Efield.set g.env "heal_c" 0
  }
  else do {
    let heal_exp = f_int.Efield.get g.env "heal_exp" (-1) in
    if g.rogue.exp <> heal_exp then do {
      let heal_exp = g.rogue.exp in
      f_int.Efield.set g.env "heal_exp" heal_exp;
      let n =
        match heal_exp with
        [ 1 -> 20
        | 2 -> 18
        | 3 -> 17
        | 4 -> 14
        | 5 -> 13
        | 6 -> 10
        | 7 -> 9
        | 8 -> 8
        | 9 -> 7
        | 10 -> 4
        | 11 -> 3
        | _ -> 2 ]
      in
      f_int.Efield.set g.env "heal_n" n;
    }
    else ();
    let c = f_int.Efield.get g.env "heal_c" 0 in
    let c = c + 1 in
    f_int.Efield.set g.env "heal_c" c;
    let n = f_int.Efield.get g.env "heal_n" 0 in
    if c >= n then do {
      let c = 0 in
      f_int.Efield.set g.env "heal_c" c;
      g.rogue.hp_current ++;
      let alt = f_bool.Efield.get g.env "heal_alt" False in
      let alt = not alt in
      f_bool.Efield.set g.env "heal_alt" alt;
      if alt then g.rogue.hp_current ++ else ();
      g.rogue.hp_current add_eq g.rogue.regeneration;
      g.rogue.hp_current := min g.rogue.hp_max g.rogue.hp_current;
      print_stats g STAT_HP;
      show_rogue g
    }
    else ()
  }
;
(**)

value rec check_hunger g messages_only =
  let rogue = g.rogue in
  let fainted =
    if rogue.moves_left = HUNGRY then do {
      g.hunger_str := "hungry";
      message g (transl g.lang "hungry") False;
      print_stats g STAT_HUNGER;
      False
    }
    else if rogue.moves_left = WEAK then do {
      g.hunger_str := "weak";
      message g (transl g.lang "weak") True;
      print_stats g STAT_HUNGER;
      False
    }
    else if rogue.moves_left <= FAINT then do {
      if rogue.moves_left = FAINT then do {
        g.hunger_str := "faint";
        message g (transl g.lang "faint") True;
        print_stats g STAT_HUNGER
      }
      else ();
      let n = get_rand 0 (FAINT - rogue.moves_left) in
      if n > 0 then do {
        if rand_percent 40 then rogue.moves_left ++ else ();
        message g (transl g.lang "you faint" ^ ".") True;
        for i = 0 to n - 1 do {
          if coin_toss () then Monster.mv_mons g else ();
        };
        message g (transl g.lang "You can move again.") True
      }
      else ();
      True
    }
    else False
  in
  if messages_only then fainted
  else if rogue.moves_left <= STARVE then Finish.killed_by g Starvation
  else do {
    match rogue.e_rings with
    [ (-2) -> ()
    | (-1) -> rogue.moves_left sub_eq rogue.moves_left mod 2
    | 0 -> rogue.moves_left --
    | 1 -> do {
        rogue.moves_left --;
        let _ : bool = check_hunger g True in
        ();
        rogue.moves_left sub_eq rogue.moves_left mod 2
      }
    | 2 -> do {
        rogue.moves_left --;
        let _ : bool = check_hunger g True in
        ();
        rogue.moves_left --
      }
    | _ -> assert False ];
    fainted
  }
;

value wanderer g =
  let found =
    loop_i 0 where rec loop_i i =
      if i < 15 then
        let monster = Imonster.gr_monster g (Some 0) in
        if monster.mn_flags land (WAKENS lor WANDERS) = 0 then loop_i (i + 1)
        else Some monster
      else None
  in
  match found with
  [ Some monster -> do {
      wake_up monster;
      let rec loop_i i =
        if i < 25 then
          match
            try
              Some (gr_row_col g (FLOOR lor TUNNEL lor STAIRS lor OBJECT) 25)
            with
            [ Not_found -> None ]
          with
          [ Some (row, col, _) ->
              if not (rogue_can_see g row col) then do {
                put_m_at g row col monster;
                monster.mn_trail_char := Curses.mvinch row col
              }
              else loop_i (i + 1)
          | None -> () ]
        else ()
      in
      loop_i 0
    }
  | None -> () ]
;

value rec reg_move g =
  let _ : bool = reg_move_and_check_fainted g in
  ()
and reg_move_and_check_fainted g = do {
  let fainted =
    if g.rogue.moves_left <= HUNGRY || g.cur_level >= g.max_level then
      check_hunger g False
    else False
  in
  Monster.mv_mons g;
  g.m_moves ++;
  if g.m_moves >= 120 then do { g.m_moves := 0; wanderer g } else ();
  if g.rogue.halluc > 0 then do {
    g.rogue.halluc --;
    if g.rogue.halluc = 0 then unhallucinate g else hallucinate g
  }
  else ();
  if g.rogue.blind > 0 then do {
    g.rogue.blind --;
    if g.rogue.blind = 0 then unblind g else ()
  }
  else ();
  if g.rogue.confused > 0 then do {
    g.rogue.confused --;
    if g.rogue.confused = 0 then unconfuse g else ()
  }
  else ();
  if g.rogue.bear_trap > 0 then g.rogue.bear_trap -- else ();
  if g.rogue.levitate > 0 then do {
    g.rogue.levitate --;
    if g.rogue.levitate = 0 then do {
      message g (transl g.lang "You float gently to the ground.") True;
      if g.dungeon.(g.rogue.row).(g.rogue.col) land TRAP <> 0 then
        trap_player g g.rogue.row g.rogue.col
      else ()
    }
    else ()
  }
  else ();
  if g.rogue.haste_self > 0 then do {
    g.rogue.haste_self --;
    if g.rogue.haste_self = 0 then
      message g (transl g.lang "You feel yourself slowing down.") False
    else ()
  }
  else ();
  heal g;
  if g.rogue.auto_search > 0 then search g g.rogue.auto_search True else ();
  fainted
}
and search g n is_auto =
  let found =
    loop_i 0 (-1) where rec loop_i found i =
      if i <= 1 then
        let rec loop_j found j =
          if j <= 1 then
            let row = g.rogue.row + i in
            let col = g.rogue.col + j in
            let found =
              if row < MIN_ROW || row >= DROWS - 1 || col < 0 || col >= DCOLS
              then
                found
              else if g.dungeon.(row).(col) land HIDDEN <> 0 then found + 1
              else found
            in
            loop_j found (j + 1)
          else loop_i found (i + 1)
        in
        loop_j found (-1)
      else found
  in
  loop_s 0 0 where rec loop_s shown s =
    if s < n then
      let rec loop_i shown i =
        if i <= 1 then
          let rec loop_j shown j =
            if j <= 1 then
              let row = g.rogue.row + i in
              let col = g.rogue.col + j in
              if row < MIN_ROW || row >= DROWS - 1 || col < 0 || col >= DCOLS
              then
                loop_j shown (j + 1)
              else
                let shown =
                  if g.dungeon.(row).(col) land HIDDEN <> 0 then
                    if rand_percent (17 + g.rogue.exp + g.rogue.ring_exp)
                    then do {
                      g.dungeon.(row).(col) land_eq lnot HIDDEN;
                      if g.rogue.blind = 0 &&
                         (row <> g.rogue.row || col <> g.rogue.col)
                      then
                        Curses.mvaddch row col (get_dungeon_char g row col)
                      else ();
                      if g.dungeon.(row).(col) land TRAP <> 0 then
                        match trap_at g row col with
                        [ Some t ->
                            message g (transl g.lang (Level.trap_string t))
                              True
                        | None -> () ]
                      else ();
                      shown + 1
                    }
                    else shown
                  else shown
                in
                if shown = found && found > 0 || g.interrupted then ()
                else loop_j shown (j + 1)
            else loop_i shown (i + 1)
          in
          loop_j shown (-1)
        else do {
          if not is_auto then do {
            g.reg_search := not g.reg_search;
            if g.reg_search then reg_move g else ()
          }
          else ();
          loop_s shown (s + 1)
        }
      in
      loop_i shown (-1)
    else ()
;

value rec list_remove x =
  fun
  [ [y :: l] -> if x = y then l else [y :: list_remove x l]
  | [] -> [] ]
;

value ask_pick_up_scroll g = do {
  check_message g;
  message g (transl g.lang "Really pick up scroll? (y/n)") True;
  let yes =
    loop () where rec loop () =
      let r = rgetchar g in
      if r = translc g.lang 'y' then True
      else if r = translc g.lang 'n' then False
      else loop ()
  in
  check_message g;
  yes
};

value pick_up g row col =
  let obj = object_at g row col in
  if obj.ob_kind = Scroll ScareMonster && obj.ob_picked_up then do {
    let pick_it_up =
      if g.experimented_pick_up_scare_monster then ask_pick_up_scroll g
      else True
    in
    if pick_it_up then do {
      message g (transl g.lang "The scroll turns to dust as you pick it up.")
        False;
      g.dungeon.(row).(col) land_eq lnot OBJECT;
      g.level_objects := list_remove obj g.level_objects;
      g.id_scrolls.(Object.int_of_scroll ScareMonster) := Identified;
      g.experimented_pick_up_scare_monster := True
    }
    else ();
    (None, False)
  }
  else if obj.ob_kind = Gold then do {
    g.rogue.gold := min MAX_GOLD (g.rogue.gold + obj.ob_quantity);
    g.dungeon.(row).(col) land_eq lnot OBJECT;
    g.level_objects := list_remove obj g.level_objects;
    print_stats g STAT_GOLD;
    (Some (obj, None), True)
  }
  else if pack_count g (Some obj) >= MAX_PACK_COUNT then do {
    message g (transl g.lang "Pack too full.") True;
    (None, True)
  }
  else do {
    g.dungeon.(row).(col) land_eq lnot OBJECT;
    g.level_objects := list_remove obj g.level_objects;
    let (c, obj) = add_to_pack g obj in
    obj.ob_picked_up := True;
    (Some (obj, Some c), True)
  }
;

value one_move g dirch pickup =
  let row = g.rogue.row in
  let col = g.rogue.col in
  let dirch = if g.rogue.confused > 0 then gr_dir () else dirch in
  let (row, col) = get_dir_rc dirch row col True in
  if not (can_move g g.rogue.row g.rogue.col row col) then MoveFailed
  else if
    (g.rogue.being_held || g.rogue.bear_trap > 0) &&
    g.dungeon.(row).(col) land MONSTER = 0
  then do {
    if g.rogue.being_held then
      message g (transl g.lang "You are being held.") True
    else do {
      message g (transl g.lang "You are still stuck in the bear trap.") False;
      reg_move g
    };
    MoveFailed
  }
  else if g.rogue.r_teleport && rand_percent R_TELE_PERCENT then do {
    tele g;
    StoppedOnSomething
  }
  else if g.dungeon.(row).(col) land MONSTER <> 0 then do {
    Attack.rogue_hit g (monster_at g row col) False;
    reg_move g;
    MoveFailed
  }
  else do {
    if g.dungeon.(row).(col) land DOOR <> 0 then
      match g.cur_room with
      [ None ->
          match get_room_number g row col with
          [ Some rn -> do {
              g.cur_room := Some rn;
              light_up_room g rn;
              wake_room g rn True row col
            }
          | None -> assert False ]
      | Some _ -> light_passage g row col ]
    else if
      g.dungeon.(g.rogue.row).(g.rogue.col) land DOOR <> 0 &&
      g.dungeon.(row).(col) land TUNNEL <> 0
    then
      match g.cur_room with
      [ Some rn -> do {
          light_passage g row col;
          wake_room g rn False g.rogue.row g.rogue.col;
          darken_room g rn;
          g.cur_room := None
        }
      | None -> assert False ]
    else if g.dungeon.(row).(col) land TUNNEL <> 0 then
      light_passage g row col
    else ();
    Curses.mvaddch g.rogue.row g.rogue.col
      (get_dungeon_char g g.rogue.row g.rogue.col);
    g.rogue.row := row;
    g.rogue.col := col;
    show_rogue g;
    if not g.jump then Curses.refresh () else ();
    if g.dungeon.(row).(col) land OBJECT <> 0 then do {
      if g.rogue.levitate > 0 && pickup then reg_move g
      else if pickup then do {
        let (obj, status) = pick_up g row col in
        match obj with
        [ Some (obj, Some c) ->
            let desc = etransl (get_desc g obj True) in
            message g (sprintf "%s (%c)" desc c) True
        | Some (obj, None) ->
            let desc = get_desc g obj True in
            message g desc True
        | None ->
            if not status then ()
            else
              let obj = object_at g row col in
              let msg =
                transl g.lang "moved onto" ^ " " ^ get_desc g obj False
              in
              message g (etransl msg) True ];
        reg_move g
      }
      else do {
        let obj = object_at g row col in
        let msg = transl g.lang "moved onto" ^ " " ^ get_desc g obj False in
        message g (etransl msg) True;
        reg_move g
      };
      StoppedOnSomething
    }
    else if
      g.dungeon.(row).(col) land (DOOR lor STAIRS lor TRAP) <> 0
    then do {
      if g.rogue.levitate = 0 && g.dungeon.(row).(col) land TRAP <> 0 then
        trap_player g row col
      else ();
      reg_move g;
      StoppedOnSomething
    }
    else
      let fainted = reg_move_and_check_fainted g in
      if fainted || g.rogue.confused > 0 then StoppedOnSomething else Moved
  }
;

value next_to_something g drow dcol =
  if g.rogue.confused > 0 then True
  else if g.rogue.blind > 0 then False
  else
    let i_end = if g.rogue.row < DROWS - 2 then 1 else 0 in
    let j_end = if g.rogue.col < DCOLS - 1 then 1 else 0 in
    loop_i 0 (if g.rogue.row > MIN_ROW then -1 else 0)
    where rec loop_i pass_count i =
      if i <= i_end then
        let rec loop_j pass_count j =
          if j <= j_end then
            if i = 0 && j = 0 then loop_j pass_count (j + 1)
            else if g.rogue.row + i = drow && g.rogue.col + j = dcol then
              loop_j pass_count (j + 1)
            else
              let row = g.rogue.row + i in
              let col = g.rogue.col + j in
              let s = g.dungeon.(row).(col) in
              if s land HIDDEN <> 0 then loop_j pass_count (j + 1)
              else if
                s land (MONSTER lor OBJECT lor STAIRS lor TRAP) <> 0
              then
                if (row = drow || col = dcol) &&
                   not (row = g.rogue.row || col = g.rogue.col)
                then
                  loop_j pass_count (j + 1)
                else True
              else if (i - j = 1 || i - j = -1) && s land TUNNEL <> 0 then
                if pass_count > 0 then True
                else loop_j (pass_count + 1) (j + 1)
              else if s land DOOR <> 0 && (i = 0 || j = 0) then True
              else loop_j pass_count (j + 1)
          else loop_i pass_count (i + 1)
        in
        loop_j pass_count (if g.rogue.col > 0 then -1 else 0)
      else False
;

value get_room_number_not_maze g =
  if g.rogue.blind = 0 then
    match get_room_number g g.rogue.row g.rogue.col with
    [ Some i ->
        let is_room = g.rooms.(i).is_room land R_ROOM <> 0 in
        let is_maze = g.rooms.(i).is_room land R_MAZE <> 0 in
        if is_room && not is_maze then Some i else None
    | None -> None ]
  else None
;

value multiple_move_rogue g dirch =
  match dirch with
  [ ROGUE_KEY_WEST_CTRL | ROGUE_KEY_SOUTH_CTRL | ROGUE_KEY_NORTH_CTRL |
    ROGUE_KEY_EAST_CTRL | ROGUE_KEY_NORTHWEST_CTRL |
    ROGUE_KEY_NORTHEAST_CTRL | ROGUE_KEY_SOUTHEAST_CTRL |
    ROGUE_KEY_SOUTHWEST_CTRL ->
      let monsters_in_room =
        match get_room_number_not_maze g with
        [ Some rn ->
            List.filter
              (fun monster ->
                 match get_room_number g monster.mn_row monster.mn_col with
                 [ Some rn2 -> rn = rn2
                 | None -> False ])
              g.level_monsters
        | None -> [] ]
      in
      let tmpdirch = Char.chr (Char.code dirch + Char.code 'a' - 1) in
      loop tmpdirch where rec loop tmpdirch =
        let row = g.rogue.row in
        let col = g.rogue.col in
        let m = one_move g tmpdirch True in
        match m with
        [ MoveFailed ->
            if g.dungeon.(row).(col) land TUNNEL <> 0 then
              match tmpdirch with
              [ ROGUE_KEY_WEST | ROGUE_KEY_EAST ->
                  let dir1 = if is_passable g (row - 1) col then 1 else 0 in
                  let dir2 = if is_passable g (row + 1) col then 1 else 0 in
                  if dir1 + dir2 = 1 then
                    loop
                      (if dir1 = 1 then ROGUE_KEY_NORTH else ROGUE_KEY_SOUTH)
                  else ()
              | ROGUE_KEY_NORTH | ROGUE_KEY_SOUTH ->
                  let dir1 = if is_passable g row (col - 1) then 1 else 0 in
                  let dir2 = if is_passable g row (col + 1) then 1 else 0 in
                  if dir1 + dir2 = 1 then
                    loop (if dir1 = 1 then ROGUE_KEY_WEST else ROGUE_KEY_EAST)
                  else ()
              | _ -> () ]
            else ()
        | StoppedOnSomething -> ()
        | Moved ->
            if not g.interrupted && not (next_to_something g row col) then
              match get_room_number_not_maze g with
              [ Some rn ->
                  let new_monster_appeared =
                    List.exists
                      (fun monster ->
                         match
                           get_room_number g monster.mn_row monster.mn_col
                         with
                         [ Some rn2 ->
                             rn = rn2 &&
                             not (List.mem monster monsters_in_room)
                         | None -> False ])
                      g.level_monsters
                  in
                  if new_monster_appeared then () else loop tmpdirch
              | None -> loop tmpdirch ]
            else () ]
  | ROGUE_KEY_WEST_SHIFT | ROGUE_KEY_SOUTH_SHIFT | ROGUE_KEY_NORTH_SHIFT |
    ROGUE_KEY_EAST_SHIFT | ROGUE_KEY_NORTHWEST_SHIFT |
    ROGUE_KEY_NORTHEAST_SHIFT | ROGUE_KEY_SOUTHEAST_SHIFT |
    ROGUE_KEY_SOUTHWEST_SHIFT ->
      let c = Char.chr (Char.code dirch - Char.code 'A' + Char.code 'a') in
      loop g.rogue.hp_current where rec loop hp =
        if one_move g c True = Moved then
          if g.rogue.hp_current < hp then () else loop g.rogue.hp_current
        else ()
  | _ -> invalid_arg "multiple_move_rogue" ]
;

value one_move_rogue g dirch pickup =
  let _ : one_move = one_move g dirch pickup in
  ()
;

value move_onto g =
  loop True (rgetchar g) where rec loop first_miss ch =
    if not (is_direction ch) then do {
      if ch = ROGUE_KEY_MOVE then () else sound_bell ();
      if first_miss then message g (transl g.lang "Direction?") False else ();
      loop False (rgetchar g)
    }
    else do {
      check_message g;
      if ch <> ROGUE_KEY_CANCEL then one_move_rogue g ch False else ()
    }
;

value id_trap g = do {
  message g (transl g.lang "Direction?") True;
  let dir =
    loop () where rec loop () =
      let dir = rgetchar g in
      if not (is_direction dir) then do { sound_bell (); loop () } else dir
  in
  check_message g;
  if dir = ROGUE_KEY_CANCEL then ()
  else
    let row = g.rogue.row in
    let col = g.rogue.col in
    let (row, col) = get_dir_rc dir row col False in
    if g.dungeon.(row).(col) land TRAP <> 0 &&
       g.dungeon.(row).(col) land HIDDEN = 0
    then
      match trap_at g row col with
      [ Some t -> message g (transl g.lang (Level.trap_string t)) False
      | None -> () ]
    else message g (transl g.lang "No trap there.") False
};

value kick_into_pack g =
  if g.dungeon.(g.rogue.row).(g.rogue.col) land OBJECT = 0 then
    message g (transl g.lang "Nothing here.") False
  else do {
    let (obj, status) = pick_up g g.rogue.row g.rogue.col in
    match obj with
    [ Some (obj, Some c) ->
        let desc = get_desc g obj True in
        message g (sprintf "%s (%c)" (etransl desc) c) True
    | Some (obj, None) ->
        let desc = get_desc g obj True in
        message g desc True
    | None -> () ];
    match (obj, status) with
    [ (Some _, _) | (_, False) -> reg_move g
    | _ -> () ]
  }
;

value fight g to_the_death = do {
  let rogue = g.rogue in
  let ch =
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
  if ch = ROGUE_KEY_CANCEL then ()
  else
    let (row, col) = get_dir_rc ch rogue.row rogue.col False in
    let c = Curses.mvinch row col in
    if c < 'A' || c > 'Z' || not (can_move g rogue.row rogue.col row col) then
      message g (transl g.lang "I see no monster there.") False
    else do {
      let monster = monster_at g row col in
      rogue.fight_monster := Some monster;
      let possible_damage =
        if monster.mn_flags land STATIONARY = 0 then
          get_damage g monster.mn_damage False * 2
        else monster.mn_stationary_damage - 1
      in
      loop () where rec loop () = do {
        one_move_rogue g ch False;
        if not to_the_death && rogue.hp_current <= possible_damage ||
           g.interrupted || g.dungeon.(row).(col) land MONSTER = 0 ||
           rogue.confused > 0
        then
          rogue.fight_monster := None
        else if monster_at g row col <> monster then
          rogue.fight_monster := None
        else loop ()
      }
    }
};
