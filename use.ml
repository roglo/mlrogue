(* $Id: use.ml,v 1.59 2010/03/23 12:44:56 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

open Rogue_def
open Keyboard_def

open Rogue
open Dialogue
open Imisc
open Misc
open Object
open Printf
open Translate

let potion_heal g extra =
  let rogue = g.rogue in
  rogue.hp_current <- rogue.hp_current + rogue.exp;
  let ratio = float rogue.hp_current /. float rogue.hp_max in
  if ratio >= 1.00 then
    begin
      rogue.hp_max <- min _MAX_HP (rogue.hp_max + (if extra then 2 else 1));
      rogue.extra_hp <- rogue.extra_hp + (if extra then 2 else 1);
      rogue.hp_current <- rogue.hp_max
    end
  else if ratio >= 0.90 then
    begin
      rogue.hp_max <- min _MAX_HP (rogue.hp_max + (if extra then 1 else 0));
      rogue.extra_hp <- rogue.extra_hp + (if extra then 1 else 0);
      rogue.hp_current <- rogue.hp_max
    end
  else
    begin let ratio = if ratio < 0.33 then 0.33 else ratio in
      let add = ratio *. float (rogue.hp_max - rogue.hp_current) in
      rogue.hp_current <- rogue.hp_current + int_of_float add;
      rogue.hp_current <- min rogue.hp_current rogue.hp_max
    end;
  if rogue.blind > 0 then Move.unblind g;
  if rogue.confused > 0 && extra then Move.unconfuse g
  else if rogue.confused > 0 then rogue.confused <- rogue.confused / 2 + 1;
  if rogue.halluc > 0 && extra then Move.unhallucinate g
  else if rogue.halluc > 0 then rogue.halluc <- rogue.halluc / 2 + 1;
  show_rogue g

let go_blind g =
  if g.rogue.blind = 0 then
    message g (transl g.lang "A cloak of darkness falls around you.") false;
  g.rogue.blind <- g.rogue.blind + get_rand 500 800;
  if g.rogue.detect_monster then
    List.iter
      (fun monster ->
         show_monster g monster.mn_row monster.mn_col monster
           monster.mn_trail_char)
      g.level_monsters;
  begin match g.cur_room with
    Some rn ->
      if rn >= 0 then
        let rm = g.rooms.(rn) in
        for i = rm.top_row + 1 to rm.bottom_row - 1 do
          for j = rm.left_col + 1 to rm.right_col - 1 do
            Curses.mvaddch i j ' '
          done
        done
  | None -> ()
  end;
  show_rogue g

let show_objects g =
  if g.rogue.blind > 0 then ()
  else
    begin
      List.iter
        (fun obj ->
           let row = obj.ob_row in
           let col = obj.ob_col in
           let rc = get_mask_char obj in
           if g.dungeon.(row).(col) land _MONSTER <> 0 then
             begin let monster = monster_at g row col in
               monster.mn_trail_char <- rc
             end;
           let mc = Curses.mvinch row col in
           if (mc < 'A' || mc > 'Z') &&
              (row <> g.rogue.row || col <> g.rogue.col)
           then
             Curses.mvaddch row col rc)
        g.level_objects;
      List.iter
        (fun monster ->
           if monster.mn_flags land _IMITATES <> 0 then
             Curses.mvaddch monster.mn_row monster.mn_col monster.mn_disguise)
        g.level_monsters
    end

let apply_potion g =
  function
    IncreaseStrength ->
      message g (transl g.lang "You feel stronger now, what bulging muscles!")
        false;
      g.rogue.str_current <- g.rogue.str_current + 1;
      if g.rogue.str_current > g.rogue.str_max then
        g.rogue.str_max <- g.rogue.str_current;
      if g.rogue.str_max > _MAX_STRENGTH then
        begin
          g.rogue.str_current <-
            g.rogue.str_current - (g.rogue.str_max - _MAX_STRENGTH);
          g.rogue.str_max <- _MAX_STRENGTH
        end
  | RestoreStrength ->
      g.rogue.str_current <- g.rogue.str_max;
      message g (transl g.lang "This tastes great, you feel warm all over.")
        false
  | Healing ->
      message g (transl g.lang "You begin to feel better.") false;
      potion_heal g false
  | ExtraHealing ->
      message g (transl g.lang "You begin to feel much better.") false;
      potion_heal g true
  | Poison ->
      if not g.rogue.sustain_strength then
        g.rogue.str_current <- max 1 (g.rogue.str_current - get_rand 1 3);
      message g (transl g.lang "You feel very sick now.") false;
      if g.rogue.halluc > 0 then Move.unhallucinate g
  | Blindness -> go_blind g
  | RaiseLevel ->
      if g.rogue.exp_points < _MAX_EXP then
        g.rogue.exp_points <- level_points.(g.rogue.exp - 1);
      add_exp g 1 hp_raise
  | Hallucination ->
      message g (transl g.lang "Oh wow, everything seems so cosmic.") false;
      g.rogue.halluc <- g.rogue.halluc + get_rand 500 800
  | DetectMonsters ->
      show_monsters g;
      if g.level_monsters = [] then
        message g
          (transl g.lang
             "You have a strange feeling for a moment, then it passes.")
          false
  | DetectObjects ->
      show_objects g;
      if g.level_objects = [] then
        message g
          (transl g.lang
             "You have a strange feeling for a moment, then it passes.")
          false
  | Confusion ->
      if g.rogue.halluc > 0 then
        message g (transl g.lang "What a trippy feeling.") false
      else message g (transl g.lang "You feel confused.") false;
      Monster.confuse g
  | Levitation ->
      message g (transl g.lang "You start to float in the air.") false;
      g.rogue.levitate <- g.rogue.levitate + get_rand 15 30;
      g.rogue.being_held <- false;
      g.rogue.bear_trap <- 0
  | HasteSelf ->
      message g (transl g.lang "You feel yourself moving much faster.") false;
      g.rogue.haste_self <- g.rogue.haste_self + get_rand 11 21;
      if g.rogue.haste_self mod 2 = 0 then
        g.rogue.haste_self <- g.rogue.haste_self + 1
  | SeeInvisible ->
      let buf =
        sprintf (ftransl g.lang "Hmm ... this potion tastes like %sjuice.")
          (if g.fruit <> default_fruit then g.fruit ^ " "
           else transl g.lang g.fruit ^ " ")
      in
      message g (etransl buf) false;
      if g.rogue.blind > 0 then Move.unblind g;
      g.rogue.see_invisible <- true;
      relight g

let quaff g =
  let ch =
    pack_letter g (transl g.lang "Quaff what?")
      (function
         Potion _ -> true
       | _ -> false)
  in
  if ch = _ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch false with
      None -> ()
    | Some obj ->
        match obj.ob_kind with
          Potion p ->
            apply_potion g p;
            print_stats g (_STAT_STRENGTH lor _STAT_HP);
            g.id_potions.(int_of_potion p) <- Identified;
            vanish g ch obj;
            Move.reg_move g
        | _ -> message g (transl g.lang "You can't drink that!") false

let get_ench_color g =
  if g.rogue.halluc > 0 then
    transl g.lang colours.(get_rand 0 (Array.length colours - 1))
  else transl g.lang "blue"

let idntfy g =
  let rec loop () =
    let ch =
      pack_letter g (transl g.lang "What would you like to identify?")
        (fun _ -> true)
    in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch true with
        None -> message g "" false; check_message g; loop ()
      | Some obj ->
          begin match obj.ob_kind with
            Scroll s -> g.id_scrolls.(int_of_scroll s) <- Identified
          | Potion p -> g.id_potions.(int_of_potion p) <- Identified
          | Weapon w -> w.we_identified <- true
          | Armor a -> a.ar_identified <- true
          | Wand x ->
              g.id_wands.(int_of_wand x.wa_kind) <- Identified;
              x.wa_identified <- true
          | Ring r ->
              g.id_rings.(int_of_ring r.rg_kind) <- Identified;
              r.rg_identified <- true
          | _ -> ()
          end;
          message g (etransl (get_desc g obj true)) false
  in
  loop ()

let uncurse_all g =
  List.iter
    (fun (_, obj) ->
       match obj.ob_kind with
         Armor a -> a.ar_is_cursed <- false
       | Weapon w -> w.we_is_cursed <- false; w.we_has_been_uncursed <- true
       | Ring r -> r.rg_is_cursed <- false
       | _ -> ())
    g.rogue.pack

let draw_magic_map g wizard_key =
  let mask =
    _HORWALL lor _VERTWALL lor _DOOR lor _TUNNEL lor _TRAP lor _STAIRS lor
    _MONSTER
  in
  for i = 0 to _DROWS - 1 do
    for j = 0 to _DCOLS - 1 do
      let s = g.dungeon.(i).(j) in
      if s land mask <> 0 then
        let ch = Curses.mvinch i j in
        if ch = ' ' || ch >= 'A' && ch <= 'Z' ||
           s land (_TRAP lor _HIDDEN) <> 0
        then
          let och = ch in
          if not wizard_key || s land _TRAP <> 0 then
            g.dungeon.(i).(j) <- g.dungeon.(i).(j) land lnot _HIDDEN;
          let ch =
            if g.dungeon.(i).(j) land _HIDDEN <> 0 then get_dungeon_char g i j
            else if s land _HORWALL <> 0 then '-'
            else if s land _VERTWALL <> 0 then '|'
            else if s land _DOOR <> 0 then '+'
            else if s land _TRAP <> 0 then '^'
            else if s land _STAIRS <> 0 then '%'
            else if s land _TUNNEL <> 0 then '#'
            else ' '
          in
          if ch = ' ' then ()
          else
            begin
              if s land _MONSTER = 0 || och = ' ' then Curses.addch ch;
              if s land _MONSTER <> 0 then
                let monster = monster_at g i j in monster.mn_trail_char <- ch
            end
    done
  done;
  if wizard_key then
    for i = 0 to _MAXROOMS - 1 do
      let rm = g.rooms.(i) in
      if rm.is_room land (_R_ROOM lor _R_MAZE) = 0 then
        begin
          for col = rm.left_col to rm.right_col do
            if Curses.mvinch rm.top_row col = ' ' then
              Curses.mvaddch rm.top_row col '\'';
            if Curses.mvinch rm.bottom_row col = ' ' then
              Curses.mvaddch rm.bottom_row col '\''
          done;
          for row = rm.top_row to rm.bottom_row do
            if Curses.mvinch row rm.left_col = ' ' then
              Curses.mvaddch row rm.left_col '\'';
            if Curses.mvinch row rm.right_col = ' ' then
              Curses.mvaddch row rm.right_col '\''
          done
        end
    done

let create_monster g =
  let row = g.rogue.row in
  let col = g.rogue.col in
  let found =
    let rec loop i =
      if i = 9 then None
      else
        let (row, col) = rand_around g i row col in
        if row = g.rogue.row && col = g.rogue.col || row < _MIN_ROW ||
           row > _DROWS - 2 || col < 0 || col > _DCOLS - 1
        then
          loop (i + 1)
        else
          let ch = g.dungeon.(row).(col) in
          if ch land _MONSTER = 0 && ch land _HIDDEN = 0 &&
             ch land (_FLOOR lor _TUNNEL lor _STAIRS lor _DOOR) <> 0
          then
            Some (row, col)
          else loop (i + 1)
    in
    loop 0
  in
  match found with
    Some (row, col) ->
      let monster = Imonster.gr_monster g (Some 0) in
      put_m_at g row col monster;
      monster.mn_trail_char <- Curses.mvinch row col;
      show_monster g row col monster (gmc g monster);
      if monster.mn_flags land (_WANDERS lor _WAKENS) <> 0 then
        wake_up monster
  | None ->
      message g
        (transl g.lang "You hear a faint cry of anguish in the distance.")
        false

let aggravate_monster g =
  message g (transl g.lang "You hear a high pitched humming noise.") false;
  List.iter
    (fun monster ->
       wake_up monster;
       monster.mn_flags <- monster.mn_flags land lnot _IMITATES;
       if rogue_can_see g monster.mn_row monster.mn_col then
         Curses.mvaddch monster.mn_row monster.mn_col
           (tgmc g monster.mn_char))
    g.level_monsters

let hold_monster g =
  let mcount =
    let rec loop_i mcount i =
      if i <= 2 then
        let rec loop_j mcount j =
          if j <= 2 then
            let row = g.rogue.row + i in
            let col = g.rogue.col + j in
            if row < _MIN_ROW || row > _DROWS - 1 || col < 0 ||
               col > _DCOLS - 1
            then
              loop_j mcount (j + 1)
            else if g.dungeon.(row).(col) land _MONSTER <> 0 then
              let monster = monster_at g row col in
              monster.mn_flags <- monster.mn_flags lor _ASLEEP;
              monster.mn_flags <- monster.mn_flags land lnot _WAKENS;
              loop_j (mcount + 1) (j + 1)
            else loop_j mcount (j + 1)
          else loop_i mcount (i + 1)
        in
        loop_j mcount (-2)
      else mcount
    in
    loop_i 0 (-2)
  in
  match mcount with
    0 -> message g (transl g.lang "You feel a strange sense of loss.") false
  | 1 -> message g (transl g.lang "The monster freezes.") false
  | _ -> message g (transl g.lang "The monsters around you freeze.") false

let apply_scroll g =
  function
    ScareMonster ->
      message g
        (transl g.lang "You hear a maniacal laughter in the distance.") false
  | HoldMonster -> hold_monster g
  | EnchantWeapon ->
      begin match g.rogue.weapon with
        Some (ch, w) ->
          let n = (List.assoc ch g.rogue.pack).ob_quantity in
          let i = int_of_weapon w.we_kind in
          let t = weapon_tab.(i).o_title in
          let t = transl g.lang (nth_field t (if n <= 1 then 0 else 1)) in
          let msg =
            sprintf (ftransl g.lang "Your %s glow%s %s for a moment.") t
              (if n <= 1 then "s" else "") (get_ench_color g)
          in
          message g (etransl msg) false;
          if coin_toss () then w.we_hit_enchant <- w.we_hit_enchant + 1
          else w.we_d_enchant <- w.we_d_enchant + 1;
          w.we_is_cursed <- false
      | None -> message g (transl g.lang "Your hands tingle.") false
      end
  | ProtectArmor ->
      begin match g.rogue.armor with
        Some (_, a) ->
          message g
            (transl g.lang
               "Your armor is covered by a shimmering gold shield.")
            false;
          a.ar_is_protected <- true;
          a.ar_is_cursed <- false
      | None ->
          message g (transl g.lang "Your acne seems to have disappeared.")
            false
      end
  | EnchantArmor ->
      begin match g.rogue.armor with
        Some (_, a) ->
          let msg =
            sprintf (ftransl g.lang "Your armor glows %sfor a moment.")
              (get_ench_color g ^ " ")
          in
          message g (etransl msg) false;
          a.ar_enchant <- a.ar_enchant + 1;
          a.ar_enchant <- min a.ar_enchant (_MAX_ARMOR - a.ar_class);
          a.ar_is_cursed <- false;
          print_stats g _STAT_ARMOR
      | None -> message g (transl g.lang "Your skin crawls.") false
      end
  | Identify ->
      message g (transl g.lang "This is a scroll of identify.") false;
      g.id_scrolls.(int_of_scroll Identify) <- Identified;
      idntfy g
  | Teleport -> Move.tele g
  | Sleep ->
      message g (transl g.lang "You fall asleep.") false; Move.take_a_nap g
  | RemoveCurse ->
      if g.rogue.halluc > 0 then
        message g
          (transl g.lang "You feel in touch with the universal oneness.")
          false
      else
        message g
          (transl g.lang "You feel as though someone is watching over you.")
          false;
      uncurse_all g
  | CreateMonster -> create_monster g
  | AggravateMonster -> aggravate_monster g
  | MagicMapping ->
      message g (transl g.lang "This scroll seems to have a map on it.")
        false;
      draw_magic_map g false

let read_scroll g =
  if g.rogue.blind > 0 then
    message g (transl g.lang "You can't see to read the scroll.") false
  else
    let ch =
      pack_letter g (transl g.lang "Read what?")
        (function
           Scroll _ -> true
         | _ -> false)
    in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch false with
        None -> ()
      | Some obj ->
          match obj.ob_kind with
            Scroll s ->
              apply_scroll g s;
              g.id_scrolls.(int_of_scroll s) <- Identified;
              vanish g ch obj;
              if s <> Sleep then Move.reg_move g
          | _ -> message g (transl g.lang "You can't read that!") false

let eat g =
  let ch =
    pack_letter g (transl g.lang "Eat what?")
      (function
         Food _ -> true
       | _ -> false)
  in
  if ch = _ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch false with
      None -> ()
    | Some obj ->
        match obj.ob_kind with
          Food fk ->
            let moves =
              if fk = Fruit || rand_percent 60 then
                begin
                  if fk = Ration then
                    message g (transl g.lang "Yum, that tasted good.") false
                  else
                    begin let buf =
                      sprintf (ftransl g.lang "My, that was a yummy %s.")
                        (if g.fruit <> default_fruit then g.fruit
                         else transl g.lang g.fruit)
                    in
                      message g (etransl buf) false
                    end;
                  get_rand 900 1100
                end
              else
                begin
                  message g (transl g.lang "Yuk, that food tasted awful.")
                    false;
                  add_exp g 2 hp_raise;
                  get_rand 700 900
                end
            in
            g.rogue.moves_left <- g.rogue.moves_left / 3;
            g.rogue.moves_left <- g.rogue.moves_left + moves;
            g.hunger_str <- "";
            print_stats g _STAT_HUNGER;
            vanish g ch obj;
            Move.reg_move g
        | _ -> message g (transl g.lang "You can't eat that!") false

let wear g =
  match g.rogue.armor with
    Some _ -> message g (transl g.lang "You're already wearing some.") false
  | None ->
      let mask =
        function
          Armor _ -> true
        | _ -> false
      in
      let ch = pack_letter g (transl g.lang "Wear what?") mask in
      if ch = _ROGUE_KEY_CANCEL then ()
      else
        match get_letter_object g ch false with
          None -> ()
        | Some obj ->
            match obj.ob_kind with
              Armor a ->
                a.ar_identified <- true;
                let d = get_desc g obj false in
                do_wear g ch a;
                let msg = transl g.lang "Wearing" ^ " " ^ d in
                message g (etransl msg ^ ".") false;
                print_stats g _STAT_ARMOR;
                Move.reg_move g
            | _ -> message g (transl g.lang "You can't wear that" ^ ".") false

let wield g =
  let cursed =
    match g.rogue.weapon with
      Some (_, w) -> w.we_is_cursed
    | None -> false
  in
  if cursed then
    message g (transl g.lang "You can't, it appears to be cursed.") false
  else
    let ch =
      pack_letter g (transl g.lang "Wield what?")
        (function
           Weapon _ -> true
         | _ -> false)
    in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch false with
        None -> ()
      | Some obj ->
          match obj.ob_kind with
            Weapon w ->
              if w.we_in_use then
                message g (transl g.lang "In use" ^ ".") false
              else
                begin
                  unwield g;
                  let msg =
                    transl g.lang "Wielding" ^ " " ^ get_desc g obj false
                  in
                  message g (etransl msg) false;
                  do_wield g ch w;
                  Move.reg_move g
                end
          | _ ->
              let msg =
                sprintf (ftransl g.lang "You can't wield %s") (name_of g obj)
              in
              message g (etransl msg) false

let take_off g =
  match g.rogue.armor with
    Some (_, a) ->
      if a.ar_is_cursed then
        message g (transl g.lang "You can't, it appears to be cursed.") false
      else
        begin
          Monster.mv_aquators g;
          unwear g;
          let msg = transl g.lang "Was wearing" ^ " " ^ armor_desc g a in
          message g (etransl msg ^ ".") false;
          print_stats g _STAT_ARMOR;
          Move.reg_move g
        end
  | None -> message g (transl g.lang "Not wearing any" ^ ".") false

let do_put_on g ring on_left =
  if on_left then
    begin ring.rg_in_use <- Some LeftHand; g.rogue.left_ring <- Some ring end
  else
    begin
      ring.rg_in_use <- Some RightHand;
      g.rogue.right_ring <- Some ring
    end

let put_on_ring g =
  if g.rogue.r_rings = 2 then
    message g (transl g.lang "Wearing two rings already.") false
  else
    let ch =
      pack_letter g (transl g.lang "Put on what?")
        (function
           Ring _ -> true
         | _ -> false)
    in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch false with
        None -> ()
      | Some obj ->
          match obj.ob_kind with
            Ring ring ->
              if ring.rg_in_use <> None then
                message g (transl g.lang "That ring is already being worn.")
                  false
              else
                let ch =
                  if g.rogue.r_rings = 1 then
                    if g.rogue.left_ring <> None then translc g.lang 'r'
                    else translc g.lang 'l'
                  else
                    begin
                      message g (transl g.lang "Left or right hand?") false;
                      let rec loop () =
                        let ch = rgetchar g in
                        if ch <> _ROGUE_KEY_CANCEL &&
                           ch <> translc g.lang 'r' &&
                           ch <> translc g.lang 'l'
                        then
                          loop ()
                        else ch
                      in
                      loop ()
                    end
                in
                if ch = _ROGUE_KEY_CANCEL then check_message g
                else
                  begin
                    do_put_on g ring (ch = translc g.lang 'l');
                    check_message g;
                    ring_stats g;
                    print_stats g _STAT_STRENGTH;
                    relight g;
                    let desc = get_desc g obj true in
                    message g (etransl desc) false; Move.reg_move g
                  end
          | _ -> message g (transl g.lang "That's not a ring!") false

let inv_rings g =
  if g.rogue.r_rings = 0 then
    message g (transl g.lang "Not wearing any rings.") false
  else
    begin
      begin match g.rogue.left_ring with
        Some ring -> message g (etransl (ring_desc g ring true)) false
      | None -> ()
      end;
      match g.rogue.right_ring with
        Some ring -> message g (etransl (ring_desc g ring true)) false
      | None -> ()
    end;
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
    message g buf false

let remove_ring g =
  let (left, right) =
    if g.rogue.r_rings = 0 then begin inv_rings g; false, false end
    else if g.rogue.left_ring <> None && g.rogue.right_ring = None then
      true, false
    else if g.rogue.left_ring = None && g.rogue.right_ring <> None then
      false, true
    else
      begin
        message g (transl g.lang "Left or right hand?") false;
        let rec loop () =
          let ch = rgetchar g in
          if ch <> _ROGUE_KEY_CANCEL && ch <> translc g.lang 'r' &&
             ch <> '\r' && ch <> translc g.lang 'l'
          then
            loop ()
          else
            begin
              check_message g;
              ch = translc g.lang 'l', ch = translc g.lang 'r'
            end
        in
        loop ()
      end
  in
  if left || right then
    let ring =
      if left then
        match g.rogue.left_ring with
          Some ring -> ring
        | None -> assert false
      else
        match g.rogue.right_ring with
          Some ring -> ring
        | None -> assert false
    in
    if ring.rg_is_cursed then
      message g (transl g.lang "You can't, it appears to be cursed.") false
    else
      begin
        un_put_on g ring;
        let msg = transl g.lang "Removed" ^ " " ^ ring_desc g ring false in
        message g (etransl msg) false; Move.reg_move g
      end
