(* $Id: move.ml,v 1.85 2013/05/15 20:12:12 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

open Rogue_def
open Keyboard_def

open Rogue
open Rfield
open Dialogue
open Imisc
open Misc
open Printf
open Translate

type one_move = Moved | StoppedOnSomething | MoveFailed

let gr_dir () =
  match get_rand 1 8 with
    1 -> _ROGUE_KEY_SOUTH
  | 2 -> _ROGUE_KEY_NORTH
  | 3 -> _ROGUE_KEY_EAST
  | 4 -> _ROGUE_KEY_WEST
  | 5 -> _ROGUE_KEY_NORTHWEST
  | 6 -> _ROGUE_KEY_NORTHEAST
  | 7 -> _ROGUE_KEY_SOUTHWEST
  | 8 -> _ROGUE_KEY_SOUTHEAST
  | _ -> assert false

let unhallucinate g =
  g.rogue.halluc <- 0;
  relight g;
  message g (transl g.lang "Everything looks so boring now.") true

let hallucinate g =
  if g.rogue.blind > 0 then ()
  else
    begin
      List.iter
        (fun obj ->
           let ch = Curses.mvinch obj.ob_row obj.ob_col in
           if (ch < 'A' || ch > 'Z') &&
              (obj.ob_row <> g.rogue.row || obj.ob_col <> g.rogue.col)
           then
             if ch <> ' ' && ch <> '.' && ch <> '#' && ch <> '+' then
               Curses.addch (gr_obj_char ()))
        g.level_objects;
      List.iter
        (fun monster ->
           let ch = Curses.mvinch monster.mn_row monster.mn_col in
           if ch >= 'A' && ch <= 'Z' then
             let ch = Char.chr (get_rand (Char.code 'A') (Char.code 'Z')) in
             Curses.addch (tgmc g ch))
        g.level_monsters
    end

let unblind g =
  g.rogue.blind <- 0;
  message g (transl g.lang "The veil of darkness lifts.") true;
  relight g;
  if g.rogue.halluc > 0 then hallucinate g;
  if g.rogue.detect_monster then show_monsters g

let unconfuse g =
  g.rogue.confused <- 0;
  show_rogue g;
  message g
    (sprintf (ftransl g.lang "You feel less %s now.")
       (if g.rogue.halluc > 0 then transl g.lang "trippy"
        else transl g.lang "confused"))
    true

let darken_room g rn =
  for i = g.rooms.(rn).top_row + 1 to g.rooms.(rn).bottom_row - 1 do
    for j = g.rooms.(rn).left_col + 1 to g.rooms.(rn).right_col - 1 do
      if g.rogue.blind > 0 then Curses.mvaddch i j ' '
      else if
        g.dungeon.(i).(j) land (_OBJECT lor _STAIRS) = 0 &&
        not (g.rogue.detect_monster && g.dungeon.(i).(j) land _MONSTER <> 0)
      then
        begin
          if not (imitating g i j) then Curses.mvaddch i j ' ';
          if g.dungeon.(i).(j) land _TRAP <> 0 &&
             g.dungeon.(i).(j) land _HIDDEN = 0
          then
            match trap_at g i j with
              Some t -> show_trap i j t
            | None -> Curses.mvaddch i j '^'
        end
    done
  done

let wake_room g rn entering row col =
  let wake_percent =
    if g.party_room = Some rn then _PARTY_WAKE_PERCENT else _WAKE_PERCENT
  in
  let wake_percent =
    if g.rogue.stealthy > 0 then
      wake_percent / (_STEALTH_FACTOR + g.rogue.stealthy)
    else wake_percent
  in
  List.iter
    (fun monster ->
       let in_room =
         get_room_number g monster.mn_row monster.mn_col = Some rn
       in
       if in_room then
         begin
           monster.mn_target <- if entering then None else Some (row, col);
           if monster.mn_flags land _WAKENS <> 0 && rand_percent wake_percent
           then
             wake_up monster
         end)
    g.level_monsters

let tele g =
  let dc = get_dungeon_char g g.rogue.row g.rogue.col in
  Curses.mvaddch g.rogue.row g.rogue.col dc;
  begin match g.cur_room with
    Some rn -> if rn >= 0 then darken_room g rn
  | None -> ()
  end;
  Level.put_player g (get_room_number g g.rogue.row g.rogue.col);
  relight g;
  begin match g.cur_room with
    Some rn -> wake_room g rn true g.rogue.row g.rogue.col
  | None -> ()
  end;
  g.rogue.being_held <- false;
  g.rogue.bear_trap <- 0;
  g.rogue.fight_monster <- None

let take_a_nap g =
  if not (fast g) then Unix.sleep 1;
  for i = 1 to get_rand 2 5 do Monster.mv_mons g done;
  if not (fast g) then Unix.sleep 1;
  message g (transl g.lang "You can move again.") false

let trap_player g row col =
  match trap_at g row col with
    None -> ()
  | Some t ->
      let rogue = g.rogue in
      g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot _HIDDEN;
      if rand_percent (rogue.exp + rogue.ring_exp) then
        message g (transl g.lang "The trap failed.") true
      else
        match t with
          TrapDoor ->
            g.trap_door <- true;
            g.new_level_message <- transl g.lang (Level.trap_mess t)
        | BearTrap ->
            message g (transl g.lang (Level.trap_mess t)) true;
            g.rogue.bear_trap <- get_rand 4 7
        | TeleTrap -> Curses.mvaddch rogue.row rogue.col '^'; tele g
        | DartTrap ->
            message g (transl g.lang (Level.trap_mess t)) true;
            rogue.hp_current <-
              rogue.hp_current - get_damage g (1, 6, None) true;
            rogue.hp_current <- max 0 rogue.hp_current;
            if not rogue.sustain_strength && rand_percent 40 &&
               rogue.str_current >= 3
            then
              rogue.str_current <- rogue.str_current - 1;
            print_stats g (_STAT_HP lor _STAT_STRENGTH);
            show_rogue g;
            if rogue.hp_current <= 0 then Finish.killed_by g PoisonDart
        | SleepingGasTrap ->
            message g (transl g.lang (Level.trap_mess t)) true; take_a_nap g
        | RustTrap ->
            message g (transl g.lang (Level.trap_mess t)) true;
            Monster.rust g None

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
        print_stats g _STAT_HP;
        show_rogue g
      }
      else ()
    }
;
*)

let heal g =
  if g.rogue.hp_current = g.rogue.hp_max then
    f_int.Efield.set g.env "heal_c" 0
  else
    let heal_exp = f_int.Efield.get g.env "heal_exp" (-1) in
    if g.rogue.exp <> heal_exp then
      begin let heal_exp = g.rogue.exp in
        f_int.Efield.set g.env "heal_exp" heal_exp;
        let n =
          match heal_exp with
            1 -> 20
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
          | _ -> 2
        in
        f_int.Efield.set g.env "heal_n" n
      end;
    let c = f_int.Efield.get g.env "heal_c" 0 in
    let c = c + 1 in
    f_int.Efield.set g.env "heal_c" c;
    let n = f_int.Efield.get g.env "heal_n" 0 in
    if c >= n then
      let c = 0 in
      f_int.Efield.set g.env "heal_c" c;
      g.rogue.hp_current <- g.rogue.hp_current + 1;
      let alt = f_bool.Efield.get g.env "heal_alt" false in
      let alt = not alt in
      f_bool.Efield.set g.env "heal_alt" alt;
      if alt then g.rogue.hp_current <- g.rogue.hp_current + 1;
      g.rogue.hp_current <- g.rogue.hp_current + g.rogue.regeneration;
      g.rogue.hp_current <- min g.rogue.hp_max g.rogue.hp_current;
      print_stats g _STAT_HP;
      show_rogue g
(**)

let rec check_hunger g messages_only =
  let rogue = g.rogue in
  let fainted =
    if rogue.moves_left = _HUNGRY then
      begin
        g.hunger_str <- "hungry";
        message g (transl g.lang "hungry") false;
        print_stats g _STAT_HUNGER;
        false
      end
    else if rogue.moves_left = _WEAK then
      begin
        g.hunger_str <- "weak";
        message g (transl g.lang "weak") true;
        print_stats g _STAT_HUNGER;
        false
      end
    else if rogue.moves_left <= _FAINT then
      begin
        if rogue.moves_left = _FAINT then
          begin
            g.hunger_str <- "faint";
            message g (transl g.lang "faint") true;
            print_stats g _STAT_HUNGER
          end;
        let n = get_rand 0 (_FAINT - rogue.moves_left) in
        if n > 0 then
          begin
            if rand_percent 40 then rogue.moves_left <- rogue.moves_left + 1;
            message g (transl g.lang "you faint" ^ ".") true;
            for i = 0 to n - 1 do if coin_toss () then Monster.mv_mons g done;
            message g (transl g.lang "You can move again.") true
          end;
        true
      end
    else false
  in
  if messages_only then fainted
  else if rogue.moves_left <= _STARVE then Finish.killed_by g Starvation
  else
    begin
      begin match rogue.e_rings with
        (-2) -> ()
      | (-1) -> rogue.moves_left <- rogue.moves_left - rogue.moves_left mod 2
      | 0 -> rogue.moves_left <- rogue.moves_left - 1
      | 1 ->
          rogue.moves_left <- rogue.moves_left - 1;
          let _ = (check_hunger g true : bool) in
          (); rogue.moves_left <- rogue.moves_left - rogue.moves_left mod 2
      | 2 ->
          rogue.moves_left <- rogue.moves_left - 1;
          let _ = (check_hunger g true : bool) in
          (); rogue.moves_left <- rogue.moves_left - 1
      | _ -> assert false
      end;
      fainted
    end

let wanderer g =
  let found =
    let rec loop_i i =
      if i < 15 then
        let monster = Imonster.gr_monster g (Some 0) in
        if monster.mn_flags land (_WAKENS lor _WANDERS) = 0 then loop_i (i + 1)
        else Some monster
      else None
    in
    loop_i 0
  in
  match found with
    Some monster ->
      wake_up monster;
      let rec loop_i i =
        if i < 25 then
          match
            try
              Some (gr_row_col g (_FLOOR lor _TUNNEL lor _STAIRS lor _OBJECT) 25)
            with Not_found -> None
          with
            Some (row, col, _) ->
              if not (rogue_can_see g row col) then
                begin
                  put_m_at g row col monster;
                  monster.mn_trail_char <- Curses.mvinch row col
                end
              else loop_i (i + 1)
          | None -> ()
      in
      loop_i 0
  | None -> ()

let rec reg_move g = let _ = (reg_move_and_check_fainted g : bool) in ()
and reg_move_and_check_fainted g =
  let fainted =
    if g.rogue.moves_left <= _HUNGRY || g.cur_level >= g.max_level then
      check_hunger g false
    else false
  in
  Monster.mv_mons g;
  g.m_moves <- g.m_moves + 1;
  if g.m_moves >= 120 then begin g.m_moves <- 0; wanderer g end;
  if g.rogue.halluc > 0 then
    begin
      g.rogue.halluc <- g.rogue.halluc - 1;
      if g.rogue.halluc = 0 then unhallucinate g else hallucinate g
    end;
  if g.rogue.blind > 0 then
    begin
      g.rogue.blind <- g.rogue.blind - 1;
      if g.rogue.blind = 0 then unblind g
    end;
  if g.rogue.confused > 0 then
    begin
      g.rogue.confused <- g.rogue.confused - 1;
      if g.rogue.confused = 0 then unconfuse g
    end;
  if g.rogue.bear_trap > 0 then g.rogue.bear_trap <- g.rogue.bear_trap - 1;
  if g.rogue.levitate > 0 then
    begin
      g.rogue.levitate <- g.rogue.levitate - 1;
      if g.rogue.levitate = 0 then
        begin
          message g (transl g.lang "You float gently to the ground.") true;
          if g.dungeon.(g.rogue.row).(g.rogue.col) land _TRAP <> 0 then
            trap_player g g.rogue.row g.rogue.col
        end
    end;
  if g.rogue.haste_self > 0 then
    begin
      g.rogue.haste_self <- g.rogue.haste_self - 1;
      if g.rogue.haste_self = 0 then
        message g (transl g.lang "You feel yourself slowing down.") false
    end;
  heal g;
  if g.rogue.auto_search > 0 then search g g.rogue.auto_search true;
  fainted
and search g n is_auto =
  let found =
    let rec loop_i found i =
      if i <= 1 then
        let rec loop_j found j =
          if j <= 1 then
            let row = g.rogue.row + i in
            let col = g.rogue.col + j in
            let found =
              if row < _MIN_ROW || row >= _DROWS - 1 || col < 0 || col >= _DCOLS
              then
                found
              else if g.dungeon.(row).(col) land _HIDDEN <> 0 then found + 1
              else found
            in
            loop_j found (j + 1)
          else loop_i found (i + 1)
        in
        loop_j found (-1)
      else found
    in
    loop_i 0 (-1)
  in
  let rec loop_s shown s =
    if s < n then
      let rec loop_i shown i =
        if i <= 1 then
          let rec loop_j shown j =
            if j <= 1 then
              let row = g.rogue.row + i in
              let col = g.rogue.col + j in
              if row < _MIN_ROW || row >= _DROWS - 1 || col < 0 || col >= _DCOLS
              then
                loop_j shown (j + 1)
              else
                let shown =
                  if g.dungeon.(row).(col) land _HIDDEN <> 0 then
                    if rand_percent (17 + g.rogue.exp + g.rogue.ring_exp) then
                      begin
                        g.dungeon.(row).(col) <-
                          g.dungeon.(row).(col) land lnot _HIDDEN;
                        if g.rogue.blind = 0 &&
                           (row <> g.rogue.row || col <> g.rogue.col)
                        then
                          Curses.mvaddch row col (get_dungeon_char g row col);
                        if g.dungeon.(row).(col) land _TRAP <> 0 then
                          begin match trap_at g row col with
                            Some t ->
                              message g (transl g.lang (Level.trap_string t))
                                true
                          | None -> ()
                          end;
                        shown + 1
                      end
                    else shown
                  else shown
                in
                if shown = found && found > 0 || g.interrupted then ()
                else loop_j shown (j + 1)
            else loop_i shown (i + 1)
          in
          loop_j shown (-1)
        else
          begin
            if not is_auto then
              begin
                g.reg_search <- not g.reg_search;
                if g.reg_search then reg_move g
              end;
            loop_s shown (s + 1)
          end
      in
      loop_i shown (-1)
  in
  loop_s 0 0

let rec list_remove x =
  function
    y :: l -> if x = y then l else y :: list_remove x l
  | [] -> []

let ask_pick_up_scroll g =
  check_message g;
  message g (transl g.lang "Really pick up scroll? (y/n)") true;
  let yes =
    let rec loop () =
      let r = rgetchar g in
      if r = translc g.lang 'y' then true
      else if r = translc g.lang 'n' then false
      else loop ()
    in
    loop ()
  in
  check_message g; yes

let pick_up g row col =
  let obj = object_at g row col in
  if obj.ob_kind = Scroll ScareMonster && obj.ob_picked_up then
    let pick_it_up =
      if g.experimented_pick_up_scare_monster then ask_pick_up_scroll g
      else true
    in
    if pick_it_up then
      begin
        message g
          (transl g.lang "The scroll turns to dust as you pick it up.") false;
        g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot _OBJECT;
        g.level_objects <- list_remove obj g.level_objects;
        g.id_scrolls.(Object.int_of_scroll ScareMonster) <- Identified;
        g.experimented_pick_up_scare_monster <- true
      end;
    None, false
  else if obj.ob_kind = Gold then
    begin
      g.rogue.gold <- min _MAX_GOLD (g.rogue.gold + obj.ob_quantity);
      g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot _OBJECT;
      g.level_objects <- list_remove obj g.level_objects;
      print_stats g _STAT_GOLD;
      Some (obj, None), true
    end
  else if pack_count g (Some obj) >= _MAX_PACK_COUNT then
    begin message g (transl g.lang "Pack too full.") true; None, true end
  else
    begin
      g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot _OBJECT;
      g.level_objects <- list_remove obj g.level_objects;
      let (c, obj) = add_to_pack g obj in
      obj.ob_picked_up <- true; Some (obj, Some c), true
    end

let one_move g dirch pickup =
  let row = g.rogue.row in
  let col = g.rogue.col in
  let dirch = if g.rogue.confused > 0 then gr_dir () else dirch in
  let (row, col) = get_dir_rc dirch row col true in
  if not (can_move g g.rogue.row g.rogue.col row col) then MoveFailed
  else if
    (g.rogue.being_held || g.rogue.bear_trap > 0) &&
    g.dungeon.(row).(col) land _MONSTER = 0
  then
    begin
      if g.rogue.being_held then
        message g (transl g.lang "You are being held.") true
      else
        begin
          message g (transl g.lang "You are still stuck in the bear trap.")
            false;
          reg_move g
        end;
      MoveFailed
    end
  else if g.rogue.r_teleport && rand_percent _R_TELE_PERCENT then
    begin tele g; StoppedOnSomething end
  else if g.dungeon.(row).(col) land _MONSTER <> 0 then
    begin
      Attack.rogue_hit g (monster_at g row col) false;
      reg_move g;
      MoveFailed
    end
  else
    begin
      if g.dungeon.(row).(col) land _DOOR <> 0 then
        match g.cur_room with
          None ->
            begin match get_room_number g row col with
              Some rn ->
                g.cur_room <- Some rn;
                light_up_room g rn;
                wake_room g rn true row col
            | None -> assert false
            end
        | Some _ -> light_passage g row col
      else if
        g.dungeon.(g.rogue.row).(g.rogue.col) land _DOOR <> 0 &&
        g.dungeon.(row).(col) land _TUNNEL <> 0
      then
        match g.cur_room with
          Some rn ->
            light_passage g row col;
            wake_room g rn false g.rogue.row g.rogue.col;
            darken_room g rn;
            g.cur_room <- None
        | None -> assert false
      else if g.dungeon.(row).(col) land _TUNNEL <> 0 then
        light_passage g row col;
      Curses.mvaddch g.rogue.row g.rogue.col
        (get_dungeon_char g g.rogue.row g.rogue.col);
      g.rogue.row <- row;
      g.rogue.col <- col;
      show_rogue g;
      if not g.jump then Curses.refresh ();
      if g.dungeon.(row).(col) land _OBJECT <> 0 then
        begin
          if g.rogue.levitate > 0 && pickup then reg_move g
          else if pickup then
            let (obj, status) = pick_up g row col in
            begin match obj with
              Some (obj, Some c) ->
                let desc = etransl (get_desc g obj true) in
                message g (sprintf "%s (%c)" desc c) true
            | Some (obj, None) ->
                let desc = get_desc g obj true in message g desc true
            | None ->
                if not status then ()
                else
                  let obj = object_at g row col in
                  let msg =
                    transl g.lang "moved onto" ^ " " ^ get_desc g obj false
                  in
                  message g (etransl msg) true
            end;
            reg_move g
          else
            begin let obj = object_at g row col in
              let msg =
                transl g.lang "moved onto" ^ " " ^ get_desc g obj false
              in
              message g (etransl msg) true; reg_move g
            end;
          StoppedOnSomething
        end
      else if g.dungeon.(row).(col) land (_DOOR lor _STAIRS lor _TRAP) <> 0 then
        begin
          if g.rogue.levitate = 0 && g.dungeon.(row).(col) land _TRAP <> 0 then
            trap_player g row col;
          reg_move g;
          StoppedOnSomething
        end
      else
        let fainted = reg_move_and_check_fainted g in
        if fainted || g.rogue.confused > 0 then StoppedOnSomething else Moved
    end

let next_to_something g drow dcol =
  if g.rogue.confused > 0 then true
  else if g.rogue.blind > 0 then false
  else
    let i_end = if g.rogue.row < _DROWS - 2 then 1 else 0 in
    let j_end = if g.rogue.col < _DCOLS - 1 then 1 else 0 in
    let rec loop_i pass_count i =
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
              if s land _HIDDEN <> 0 then loop_j pass_count (j + 1)
              else if
                s land (_MONSTER lor _OBJECT lor _STAIRS lor _TRAP) <> 0
              then
                if (row = drow || col = dcol) &&
                   not (row = g.rogue.row || col = g.rogue.col)
                then
                  loop_j pass_count (j + 1)
                else true
              else if (i - j = 1 || i - j = -1) && s land _TUNNEL <> 0 then
                if pass_count > 0 then true
                else loop_j (pass_count + 1) (j + 1)
              else if s land _DOOR <> 0 && (i = 0 || j = 0) then true
              else loop_j pass_count (j + 1)
          else loop_i pass_count (i + 1)
        in
        loop_j pass_count (if g.rogue.col > 0 then -1 else 0)
      else false
    in
    loop_i 0 (if g.rogue.row > _MIN_ROW then -1 else 0)

let get_room_number_not_maze g =
  if g.rogue.blind = 0 then
    match get_room_number g g.rogue.row g.rogue.col with
      Some i ->
        let is_room = g.rooms.(i).is_room land _R_ROOM <> 0 in
        let is_maze = g.rooms.(i).is_room land _R_MAZE <> 0 in
        if is_room && not is_maze then Some i else None
    | None -> None
  else None

let multiple_move_rogue g dirch =
  if is_direction_ctrl dirch then
    let monsters_in_room =
      match get_room_number_not_maze g with
        Some rn ->
          List.filter
            (fun monster ->
               match get_room_number g monster.mn_row monster.mn_col with
                 Some rn2 -> rn = rn2
               | None -> false)
            g.level_monsters
      | None -> []
    in
    let tmpdirch = Char.chr (Char.code dirch + Char.code 'a' - 1) in
    let rec loop tmpdirch =
      let row = g.rogue.row in
      let col = g.rogue.col in
      let m = one_move g tmpdirch true in
      match m with
        MoveFailed ->
          if g.dungeon.(row).(col) land _TUNNEL <> 0 then
            if List.mem tmpdirch [_ROGUE_KEY_WEST; _ROGUE_KEY_EAST] then
              let dir1 = if is_passable g (row - 1) col then 1 else 0 in
              let dir2 = if is_passable g (row + 1) col then 1 else 0 in
              if dir1 + dir2 = 1 then
                loop
                  (if dir1 = 1 then _ROGUE_KEY_NORTH else _ROGUE_KEY_SOUTH)
              else ()
	    else if List.mem tmpdirch [_ROGUE_KEY_NORTH; _ROGUE_KEY_SOUTH]
	    then
              let dir1 = if is_passable g row (col - 1) then 1 else 0 in
              let dir2 = if is_passable g row (col + 1) then 1 else 0 in
              if dir1 + dir2 = 1 then
                loop (if dir1 = 1 then _ROGUE_KEY_WEST else _ROGUE_KEY_EAST)
              else ()
            else ()
          else ()
      | StoppedOnSomething -> ()
      | Moved ->
          if not g.interrupted && not (next_to_something g row col) then
            match get_room_number_not_maze g with
              Some rn ->
                let new_monster_appeared =
                  List.exists
                    (fun monster ->
                       match
                         get_room_number g monster.mn_row monster.mn_col
                       with
                         Some rn2 ->
                           rn = rn2 &&
                           not (List.mem monster monsters_in_room)
                       | None -> false)
                    g.level_monsters
                in
                if new_monster_appeared then () else loop tmpdirch
            | None -> loop tmpdirch
    in
    loop tmpdirch
  else if is_direction_shift dirch then
    let c = Char.chr (Char.code dirch - Char.code 'A' + Char.code 'a') in
    let rec loop hp =
      if one_move g c true = Moved then
        if g.rogue.hp_current < hp then () else loop g.rogue.hp_current
    in
    loop g.rogue.hp_current
  else
    invalid_arg "multiple_move_rogue"

let one_move_rogue g dirch pickup =
  let _ = (one_move g dirch pickup : one_move) in ()

let move_onto g =
  let rec loop first_miss ch =
    if not (is_direction ch) then
      begin
        if ch = _ROGUE_KEY_MOVE then () else sound_bell ();
        if first_miss then message g (transl g.lang "Direction?") false;
        loop false (rgetchar g)
      end
    else
      begin
        check_message g;
        if ch <> _ROGUE_KEY_CANCEL then one_move_rogue g ch false
      end
  in
  loop true (rgetchar g)

let id_trap g =
  message g (transl g.lang "Direction?") true;
  let dir =
    let rec loop () =
      let dir = rgetchar g in
      if not (is_direction dir) then begin sound_bell (); loop () end else dir
    in
    loop ()
  in
  check_message g;
  if dir = _ROGUE_KEY_CANCEL then ()
  else
    let row = g.rogue.row in
    let col = g.rogue.col in
    let (row, col) = get_dir_rc dir row col false in
    if g.dungeon.(row).(col) land _TRAP <> 0 &&
       g.dungeon.(row).(col) land _HIDDEN = 0
    then
      match trap_at g row col with
        Some t -> message g (transl g.lang (Level.trap_string t)) false
      | None -> ()
    else message g (transl g.lang "No trap there.") false

let kick_into_pack g =
  if g.dungeon.(g.rogue.row).(g.rogue.col) land _OBJECT = 0 then
    message g (transl g.lang "Nothing here.") false
  else
    let (obj, status) = pick_up g g.rogue.row g.rogue.col in
    begin match obj with
      Some (obj, Some c) ->
        let desc = get_desc g obj true in
        message g (sprintf "%s (%c)" (etransl desc) c) true
    | Some (obj, None) ->
        let desc = get_desc g obj true in message g desc true
    | None -> ()
    end;
    match obj, status with
      Some _, _ | _, false -> reg_move g
    | _ -> ()

let fight g to_the_death =
  let rogue = g.rogue in
  let ch =
    let rec loop first_miss =
      let ch = rgetchar g in
      if not (is_direction ch) then
        begin
          sound_bell ();
          if first_miss then message g (transl g.lang "Direction?") false;
          loop false
        end
      else ch
    in
    loop true
  in
  check_message g;
  if ch = _ROGUE_KEY_CANCEL then ()
  else
    let (row, col) = get_dir_rc ch rogue.row rogue.col false in
    let c = Curses.mvinch row col in
    if c < 'A' || c > 'Z' || not (can_move g rogue.row rogue.col row col) then
      message g (transl g.lang "I see no monster there.") false
    else
      let monster = monster_at g row col in
      rogue.fight_monster <- Some monster;
      let possible_damage =
        if monster.mn_flags land _STATIONARY = 0 then
          get_damage g monster.mn_damage false * 2
        else monster.mn_stationary_damage - 1
      in
      let rec loop () =
        one_move_rogue g ch false;
        if not to_the_death && rogue.hp_current <= possible_damage ||
           g.interrupted || g.dungeon.(row).(col) land _MONSTER = 0 ||
           rogue.confused > 0
        then
          rogue.fight_monster <- None
        else if monster_at g row col <> monster then
          rogue.fight_monster <- None
        else loop ()
      in
      loop ()
