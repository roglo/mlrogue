(* $Id: use.ml,v 1.59 2010/03/23 12:44:56 deraugla Exp $ *)

#load "pa_more.cmo";

#use "rogue.def";
#use "keyboard.def";

open Rogue;
open Dialogue;
open Imisc;
open Misc;
open Object;
open Printf;
open Translate;

value potion_heal g extra = do {
  let rogue = g.rogue in
  rogue.hp_current add_eq rogue.exp;
  let ratio = float rogue.hp_current /. float rogue.hp_max in
  if ratio >= 1.00 then do {
    rogue.hp_max := min MAX_HP (rogue.hp_max + (if extra then 2 else 1));
    rogue.extra_hp add_eq if extra then 2 else 1;
    rogue.hp_current := rogue.hp_max
  }
  else if ratio >= 0.90 then do {
    rogue.hp_max := min MAX_HP (rogue.hp_max + (if extra then 1 else 0));
    rogue.extra_hp add_eq if extra then 1 else 0;
    rogue.hp_current := rogue.hp_max
  }
  else do {
    let ratio = if ratio < 0.33 then 0.33 else ratio in
    let add = ratio *. float (rogue.hp_max - rogue.hp_current) in
    rogue.hp_current add_eq int_of_float add;
    rogue.hp_current := min rogue.hp_current rogue.hp_max
  };
  if rogue.blind > 0 then Move.unblind g else ();
  if rogue.confused > 0 && extra then Move.unconfuse g
  else if rogue.confused > 0 then rogue.confused := rogue.confused / 2 + 1
  else ();
  if rogue.halluc > 0 && extra then Move.unhallucinate g
  else if rogue.halluc > 0 then rogue.halluc := rogue.halluc / 2 + 1
  else ();
  show_rogue g
};

value go_blind g = do {
  if g.rogue.blind = 0 then
    message g (transl g.lang "A cloak of darkness falls around you.") False
  else ();
  g.rogue.blind add_eq get_rand 500 800;
  if g.rogue.detect_monster then
    List.iter
      (fun monster ->
         show_monster g monster.mn_row monster.mn_col monster
           monster.mn_trail_char)
      g.level_monsters
  else ();
  match g.cur_room with
  [ Some rn ->
      if rn >= 0 then
        let rm = g.rooms.(rn) in
        for i = rm.top_row + 1 to rm.bottom_row - 1 do {
          for j = rm.left_col + 1 to rm.right_col - 1 do {
            Curses.mvaddch i j ' ';
          };
        }
      else ()
  | None -> () ];
  show_rogue g;
};

value show_objects g =
  if g.rogue.blind > 0 then ()
  else do {
    List.iter
      (fun obj -> do {
         let row = obj.ob_row in
         let col = obj.ob_col in
         let rc = get_mask_char obj in
         if g.dungeon.(row).(col) land MONSTER <> 0 then
           let monster = monster_at g row col in
           monster.mn_trail_char := rc
         else ();
         let mc = Curses.mvinch row col in
         if (mc < 'A' || mc > 'Z') &&
            (row <> g.rogue.row || col <> g.rogue.col)
         then
           Curses.mvaddch row col rc
         else ()
       })
      g.level_objects;
    List.iter
      (fun monster ->
         if monster.mn_flags land IMITATES <> 0 then
           Curses.mvaddch monster.mn_row monster.mn_col monster.mn_disguise
         else ())
      g.level_monsters
  }
;

value apply_potion g =
  fun
  [ IncreaseStrength -> do {
      message g (transl g.lang "You feel stronger now, what bulging muscles!")
        False;
      g.rogue.str_current ++;
      if g.rogue.str_current > g.rogue.str_max then
        g.rogue.str_max := g.rogue.str_current
      else ();
      if g.rogue.str_max > MAX_STRENGTH then do {
        g.rogue.str_current sub_eq g.rogue.str_max - MAX_STRENGTH;
        g.rogue.str_max := MAX_STRENGTH
      }
      else ()
    }
  | RestoreStrength -> do {
      g.rogue.str_current := g.rogue.str_max;
      message g (transl g.lang "This tastes great, you feel warm all over.")
        False
    }
  | Healing -> do {
      message g (transl g.lang "You begin to feel better.") False;
      potion_heal g False
    }
  | ExtraHealing -> do {
      message g (transl g.lang "You begin to feel much better.") False;
      potion_heal g True
    }
  | Poison -> do {
      if not g.rogue.sustain_strength then
        g.rogue.str_current := max 1 (g.rogue.str_current - get_rand 1 3)
      else ();
      message g (transl g.lang "You feel very sick now.") False;
      if g.rogue.halluc > 0 then Move.unhallucinate g else ()
    }
  | Blindness -> go_blind g
  | RaiseLevel -> do {
      if g.rogue.exp_points < MAX_EXP then
        g.rogue.exp_points := level_points.(g.rogue.exp - 1)
      else ();
      add_exp g 1 hp_raise
    }
  | Hallucination -> do {
      message g (transl g.lang "Oh wow, everything seems so cosmic.") False;
      g.rogue.halluc add_eq get_rand 500 800
    }
  | DetectMonsters -> do {
      show_monsters g;
      if g.level_monsters = [] then
        message g
          (transl g.lang
             "You have a strange feeling for a moment, then it passes.")
          False
      else ()
    }
  | DetectObjects -> do {
      show_objects g;
      if g.level_objects = [] then
        message g
          (transl g.lang
             "You have a strange feeling for a moment, then it passes.")
          False
      else ()
    }
  | Confusion -> do {
      if g.rogue.halluc > 0 then
        message g (transl g.lang "What a trippy feeling.") False
      else message g (transl g.lang "You feel confused.") False;
      Monster.confuse g
    }
  | Levitation -> do {
      message g (transl g.lang "You start to float in the air.") False;
      g.rogue.levitate add_eq get_rand 15 30;
      g.rogue.being_held := False;
      g.rogue.bear_trap := 0
    }
  | HasteSelf -> do {
      message g (transl g.lang "You feel yourself moving much faster.") False;
      g.rogue.haste_self add_eq get_rand 11 21;
      if g.rogue.haste_self mod 2 = 0 then g.rogue.haste_self ++ else ()
    }
  | SeeInvisible -> do {
      let buf =
        sprintf (ftransl g.lang "Hmm ... this potion tastes like %sjuice.")
          (if g.fruit <> default_fruit then g.fruit ^ " "
           else transl g.lang g.fruit ^ " ")
      in
      message g (etransl buf) False;
      if g.rogue.blind > 0 then Move.unblind g else ();
      g.rogue.see_invisible := True;
      relight g
    } ]
;

value quaff g =
  let ch =
    pack_letter g (transl g.lang "Quaff what?")
      (fun
       [ Potion _ -> True
       | _ -> False ])
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch False with
    [ None -> ()
    | Some obj ->
        match obj.ob_kind with
        [ Potion p -> do {
            apply_potion g p;
            print_stats g (STAT_STRENGTH lor STAT_HP);
            g.id_potions.(int_of_potion p) := Identified;
            vanish g ch obj;
            Move.reg_move g
          }
        | _ -> message g (transl g.lang "You can't drink that!") False ] ]
;

value get_ench_color g =
  if g.rogue.halluc > 0 then
    transl g.lang colours.(get_rand 0 (Array.length colours - 1))
  else transl g.lang "blue"
;

value idntfy g =
  loop () where rec loop () =
    let ch =
      pack_letter g (transl g.lang "What would you like to identify?")
        (fun _ -> True)
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch True with
      [ None -> do { message g "" False; check_message g; loop () }
      | Some obj -> do {
          match obj.ob_kind with
          [ Scroll s -> g.id_scrolls.(int_of_scroll s) := Identified
          | Potion p -> g.id_potions.(int_of_potion p) := Identified
          | Weapon w -> w.we_identified := True
          | Armor a -> a.ar_identified := True
          | Wand x -> do {
              g.id_wands.(int_of_wand x.wa_kind) := Identified;
              x.wa_identified := True
            }
          | Ring r -> do {
              g.id_rings.(int_of_ring r.rg_kind) := Identified;
              r.rg_identified := True
            }
          | _ -> () ];
          message g (etransl (get_desc g obj True)) False
        } ]
;

value uncurse_all g =
  List.iter
    (fun (_, obj) ->
       match obj.ob_kind with
       [ Armor a -> a.ar_is_cursed := False
       | Weapon w -> do {
           w.we_is_cursed := False;
           w.we_has_been_uncursed := True
         }
       | Ring r -> r.rg_is_cursed := False
       | _ -> () ])
    g.rogue.pack
;

value draw_magic_map g wizard_key = do {
  let mask =
    HORWALL lor VERTWALL lor DOOR lor TUNNEL lor TRAP lor STAIRS lor MONSTER
  in
  for i = 0 to DROWS - 1 do {
    for j = 0 to DCOLS - 1 do {
      let s = g.dungeon.(i).(j) in
      if s land mask <> 0 then
        let ch = Curses.mvinch i j in
        if ch = ' ' || ch >= 'A' && ch <= 'Z' || s land (TRAP lor HIDDEN) <> 0
        then do {
          let och = ch in
          if not wizard_key || s land TRAP <> 0 then
            g.dungeon.(i).(j) land_eq lnot HIDDEN
          else ();
          let ch =
            if g.dungeon.(i).(j) land HIDDEN <> 0 then get_dungeon_char g i j
            else if s land HORWALL <> 0 then '-'
            else if s land VERTWALL <> 0 then '|'
            else if s land DOOR <> 0 then '+'
            else if s land TRAP <> 0 then '^'
            else if s land STAIRS <> 0 then '%'
            else if s land TUNNEL <> 0 then '#'
            else ' '
          in
          if ch = ' ' then ()
          else do {
            if s land MONSTER = 0 || och = ' ' then Curses.addch ch else ();
            if s land MONSTER <> 0 then
              let monster = monster_at g i j in
              monster.mn_trail_char := ch
            else ()
          }
        }
        else ()
      else ();
    };
  };
  if wizard_key then
    for i = 0 to MAXROOMS - 1 do {
      let rm = g.rooms.(i) in
      if rm.is_room land (R_ROOM lor R_MAZE) = 0 then do {
        for col = rm.left_col to rm.right_col do {
          if Curses.mvinch rm.top_row col = ' ' then
            Curses.mvaddch rm.top_row col '''
          else ();
          if Curses.mvinch rm.bottom_row col = ' ' then
            Curses.mvaddch rm.bottom_row col '''
          else ();
        };
        for row = rm.top_row to rm.bottom_row do {
          if Curses.mvinch row rm.left_col = ' ' then
            Curses.mvaddch row rm.left_col '''
          else ();
          if Curses.mvinch row rm.right_col = ' ' then
            Curses.mvaddch row rm.right_col '''
          else ();
        }
      }
      else ();
    }
  else ()
};

value create_monster g =
  let row = g.rogue.row in
  let col = g.rogue.col in
  let found =
    loop 0 where rec loop i =
      if i = 9 then None
      else
        let (row, col) = rand_around g i row col in
        if row = g.rogue.row && col = g.rogue.col || row < MIN_ROW ||
           row > DROWS - 2 || col < 0 || col > DCOLS - 1
        then
          loop (i + 1)
        else
          let ch = g.dungeon.(row).(col) in
          if ch land MONSTER = 0 && ch land HIDDEN = 0 &&
             ch land (FLOOR lor TUNNEL lor STAIRS lor DOOR) <> 0
          then
            Some (row, col)
          else loop (i + 1)
  in
  match found with
  [ Some (row, col) -> do {
      let monster = Imonster.gr_monster g (Some 0) in
      put_m_at g row col monster;
      monster.mn_trail_char := Curses.mvinch row col;
      show_monster g row col monster (gmc g monster);
      if monster.mn_flags land (WANDERS lor WAKENS) <> 0 then wake_up monster
      else ()
    }
  | None ->
      message g
        (transl g.lang "You hear a faint cry of anguish in the distance.")
        False ]
;

value aggravate_monster g = do {
  message g (transl g.lang "You hear a high pitched humming noise.") False;
  List.iter
    (fun monster -> do {
       wake_up monster;
       monster.mn_flags land_eq lnot IMITATES;
       if rogue_can_see g monster.mn_row monster.mn_col then
         Curses.mvaddch monster.mn_row monster.mn_col (tgmc g monster.mn_char)
       else ()
     })
    g.level_monsters
};

value hold_monster g =
  let mcount =
    loop_i 0 (-2) where rec loop_i mcount i =
      if i <= 2 then
        let rec loop_j mcount j =
          if j <= 2 then
            let row = g.rogue.row + i in
            let col = g.rogue.col + j in
            if row < MIN_ROW || row > DROWS - 1 || col < 0 || col > DCOLS - 1
            then
              loop_j mcount (j + 1)
            else if g.dungeon.(row).(col) land MONSTER <> 0 then do {
              let monster = monster_at g row col in
              monster.mn_flags or_eq ASLEEP;
              monster.mn_flags land_eq lnot WAKENS;
              loop_j (mcount + 1) (j + 1)
            }
            else loop_j mcount (j + 1)
          else loop_i mcount (i + 1)
        in
        loop_j mcount (-2)
      else mcount
  in
  match mcount with
  [ 0 -> message g (transl g.lang "You feel a strange sense of loss.") False
  | 1 -> message g (transl g.lang "The monster freezes.") False
  | _ -> message g (transl g.lang "The monsters around you freeze.") False ]
;

value apply_scroll g =
  fun
  [ ScareMonster ->
      message g
        (transl g.lang "You hear a maniacal laughter in the distance.") False
  | HoldMonster -> hold_monster g
  | EnchantWeapon ->
      match g.rogue.weapon with
      [ Some (ch, w) -> do {
          let n = (List.assoc ch g.rogue.pack).ob_quantity in
          let i = int_of_weapon w.we_kind in
          let t = weapon_tab.(i).o_title in
          let t = transl g.lang (nth_field t (if n <= 1 then 0 else 1)) in
          let msg =
            sprintf (ftransl g.lang "Your %s glow%s %s for a moment.") t
              (if n <= 1 then "s" else "") (get_ench_color g)
          in
          message g (etransl msg) False;
          if coin_toss () then w.we_hit_enchant ++ else w.we_d_enchant ++;
          w.we_is_cursed := False
        }
      | None -> message g (transl g.lang "Your hands tingle.") False ]
  | ProtectArmor ->
      match g.rogue.armor with
      [ Some (_, a) -> do {
          message g
            (transl g.lang
               "Your armor is covered by a shimmering gold shield.")
            False;
          a.ar_is_protected := True;
          a.ar_is_cursed := False
        }
      | None ->
          message g (transl g.lang "Your acne seems to have disappeared.")
            False ]
  | EnchantArmor ->
      match g.rogue.armor with
      [ Some (_, a) -> do {
          let msg =
            sprintf (ftransl g.lang "Your armor glows %sfor a moment.")
              (get_ench_color g ^ " ")
          in
          message g (etransl msg) False;
          a.ar_enchant ++;
          a.ar_enchant := min a.ar_enchant (MAX_ARMOR - a.ar_class);
          a.ar_is_cursed := False;
          print_stats g STAT_ARMOR
        }
      | None -> message g (transl g.lang "Your skin crawls.") False ]
  | Identify -> do {
      message g (transl g.lang "This is a scroll of identify.") False;
      g.id_scrolls.(int_of_scroll Identify) := Identified;
      idntfy g
    }
  | Teleport -> Move.tele g
  | Sleep -> do {
      message g (transl g.lang "You fall asleep.") False;
      Move.take_a_nap g
    }
  | RemoveCurse -> do {
      if g.rogue.halluc > 0 then
        message g
          (transl g.lang "You feel in touch with the universal oneness.")
          False
      else
        message g
          (transl g.lang "You feel as though someone is watching over you.")
          False;
      uncurse_all g
    }
  | CreateMonster -> create_monster g
  | AggravateMonster -> aggravate_monster g
  | MagicMapping -> do {
      message g (transl g.lang "This scroll seems to have a map on it.")
        False;
      draw_magic_map g False
    } ]
;

value read_scroll g =
  if g.rogue.blind > 0 then
    message g (transl g.lang "You can't see to read the scroll.") False
  else
    let ch =
      pack_letter g (transl g.lang "Read what?")
        (fun
         [ Scroll _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch False with
      [ None -> ()
      | Some obj ->
          match obj.ob_kind with
          [ Scroll s -> do {
              apply_scroll g s;
              g.id_scrolls.(int_of_scroll s) := Identified;
              vanish g ch obj;
              if s <> Sleep then Move.reg_move g else ()
            }
          | _ -> message g (transl g.lang "You can't read that!") False ] ]
;

value eat g =
  let ch =
    pack_letter g (transl g.lang "Eat what?")
      (fun
       [ Food _ -> True
       | _ -> False ])
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch False with
    [ None -> ()
    | Some obj ->
        match obj.ob_kind with
        [ Food fk -> do {
            let moves =
              if fk = Fruit || rand_percent 60 then do {
                if fk = Ration then
                  message g (transl g.lang "Yum, that tasted good.") False
                else
                  let buf =
                    sprintf (ftransl g.lang "My, that was a yummy %s.")
                      (if g.fruit <> default_fruit then g.fruit
                       else transl g.lang g.fruit)
                  in
                  message g (etransl buf) False;
                get_rand 900 1100
              }
              else do {
                message g (transl g.lang "Yuk, that food tasted awful.")
                  False;
                add_exp g 2 hp_raise;
                get_rand 700 900
              }
            in
            g.rogue.moves_left div_eq 3;
            g.rogue.moves_left add_eq moves;
            g.hunger_str := "";
            print_stats g STAT_HUNGER;
            vanish g ch obj;
            Move.reg_move g
          }
        | _ -> message g (transl g.lang "You can't eat that!") False ] ]
;

value wear g =
  match g.rogue.armor with
  [ Some _ -> message g (transl g.lang "You're already wearing some.") False
  | None ->
      let mask =
        fun
        [ Armor _ -> True
        | _ -> False ]
      in
      let ch = pack_letter g (transl g.lang "Wear what?") mask in
      if ch = ROGUE_KEY_CANCEL then ()
      else
        match get_letter_object g ch False with
        [ None -> ()
        | Some obj ->
            match obj.ob_kind with
            [ Armor a -> do {
                a.ar_identified := True;
                let d = get_desc g obj False in
                do_wear g ch a;
                let msg = transl g.lang "Wearing" ^ " " ^ d in
                message g (etransl msg ^ ".") False;
                print_stats g STAT_ARMOR;
                Move.reg_move g
              }
            | _ ->
                message g (transl g.lang "You can't wear that" ^ ".")
                  False ] ] ]
;

value wield g =
  let cursed =
    match g.rogue.weapon with
    [ Some (_, w) -> w.we_is_cursed
    | None -> False ]
  in
  if cursed then
    message g (transl g.lang "You can't, it appears to be cursed.") False
  else
    let ch =
      pack_letter g (transl g.lang "Wield what?")
        (fun
         [ Weapon _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch False with
      [ None -> ()
      | Some obj ->
          match obj.ob_kind with
          [ Weapon w ->
              if w.we_in_use then
                message g (transl g.lang "In use" ^ ".") False
              else do {
                unwield g;
                let msg =
                  transl g.lang "Wielding" ^ " " ^ get_desc g obj False
                in
                message g (etransl msg) False;
                do_wield g ch w;
                Move.reg_move g
              }
          | _ ->
              let msg =
                sprintf (ftransl g.lang "You can't wield %s") (name_of g obj)
              in
              message g (etransl msg) False ] ]
;

value take_off g =
  match g.rogue.armor with
  [ Some (_, a) ->
      if a.ar_is_cursed then
        message g (transl g.lang "You can't, it appears to be cursed.") False
      else do {
        Monster.mv_aquators g;
        unwear g;
        let msg = transl g.lang "Was wearing" ^ " " ^ armor_desc g a in
        message g (etransl msg ^ ".") False;
        print_stats g STAT_ARMOR;
        Move.reg_move g
      }
  | None -> message g (transl g.lang "Not wearing any" ^ ".") False ]
;

value do_put_on g ring on_left =
  if on_left then do {
    ring.rg_in_use := Some LeftHand;
    g.rogue.left_ring := Some ring
  }
  else do {
    ring.rg_in_use := Some RightHand;
    g.rogue.right_ring := Some ring
  }
;

value put_on_ring g =
  if g.rogue.r_rings = 2 then
    message g (transl g.lang "Wearing two rings already.") False
  else
    let ch =
      pack_letter g (transl g.lang "Put on what?")
        (fun
         [ Ring _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch False with
      [ None -> ()
      | Some obj ->
          match obj.ob_kind with
          [ Ring ring ->
              if ring.rg_in_use <> None then
                message g (transl g.lang "That ring is already being worn.")
                  False
              else
                let ch =
                  if g.rogue.r_rings = 1 then
                    if g.rogue.left_ring <> None then translc g.lang 'r'
                    else translc g.lang 'l'
                  else do {
                    message g (transl g.lang "Left or right hand?") False;
                    let rec loop () =
                      let ch = rgetchar g in
                      if ch <> ROGUE_KEY_CANCEL && ch <> translc g.lang 'r' &&
                         ch <> translc g.lang 'l'
                      then
                        loop ()
                      else ch
                    in
                    loop ()
                  }
                in
                if ch = ROGUE_KEY_CANCEL then check_message g
                else do {
                  do_put_on g ring (ch = translc g.lang 'l');
                  check_message g;
                  ring_stats g;
                  print_stats g STAT_STRENGTH;
                  relight g;
                  let desc = get_desc g obj True in
                  message g (etransl desc) False;
                  Move.reg_move g
                }
          | _ -> message g (transl g.lang "That's not a ring!") False ] ]
;

value inv_rings g = do {
  if g.rogue.r_rings = 0 then
    message g (transl g.lang "Not wearing any rings.") False
  else do {
    match g.rogue.left_ring with
    [ Some ring -> message g (etransl (ring_desc g ring True)) False
    | None -> () ];
    match g.rogue.right_ring with
    [ Some ring -> message g (etransl (ring_desc g ring True)) False
    | None -> () ]
  };
  if g.wizard then
    let r = g.rogue in
    let buf =
      sprintf "ste %d, r_r %d, e_r %d, r_t %d, s_s %d, a_s %d, reg %d, \
         r_e %d, s_i %d, m_a %d, aus %d"
        r.stealthy r.r_rings r.e_rings (if r.r_teleport then 1 else 0)
        (if r.sustain_strength then 1 else 0) r.add_strength r.regeneration
        r.ring_exp (if r.r_see_invisible then 1 else 0)
        (if r.maintain_armor then 1 else 0) r.auto_search
    in
    message g buf False
  else ()
};

value remove_ring g =
  let (left, right) =
    if g.rogue.r_rings = 0 then do { inv_rings g; (False, False) }
    else if g.rogue.left_ring <> None && g.rogue.right_ring = None then
      (True, False)
    else if g.rogue.left_ring = None && g.rogue.right_ring <> None then
      (False, True)
    else do {
      message g (transl g.lang "Left or right hand?") False;
      let rec loop () =
        let ch = rgetchar g in
        if ch <> ROGUE_KEY_CANCEL && ch <> translc g.lang 'r' && ch <> '\r' &&
           ch <> translc g.lang 'l'
        then
          loop ()
        else do {
          check_message g;
          (ch = translc g.lang 'l', ch = translc g.lang 'r')
        }
      in
      loop ()
    }
  in
  if left || right then
    let ring =
      if left then
        match g.rogue.left_ring with
        [ Some ring -> ring
        | None -> assert False ]
      else
        match g.rogue.right_ring with
        [ Some ring -> ring
        | None -> assert False ]
    in
    if ring.rg_is_cursed then
      message g (transl g.lang "You can't, it appears to be cursed.") False
    else do {
      un_put_on g ring;
      let msg = transl g.lang "Removed" ^ " " ^ ring_desc g ring False in
      message g (etransl msg) False;
      Move.reg_move g
    }
  else ()
;
