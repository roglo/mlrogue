(* $Id: attack.ml,v 1.34 2018/04/26 09:52:36 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

(* #use "rogue.def" *)


(* #use "keyboard.def" *)


open Rogue
open Dialogue
open Imisc
open Misc
open Printf
open Translate

let check_imitator g monster =
  if monster.mn_flags land 0o20000000 <> 0 then
    begin
      wake_up monster;
      if g.rogue.blind = 0 then
        begin
          Curses.mvaddch monster.mn_row monster.mn_col
            (get_dungeon_char g monster.mn_row monster.mn_col);
          check_message g;
          let mess =
            sprintf (ftransl g.lang "Wait, that's a %s!")
              (transl g.lang (Monster.mon_name g monster))
          in
          message g (etransl mess) true
        end;
      true
    end
  else false

let to_hit =
  function
    None -> 1
  | Some (_, w) -> let (n, _, _) = w.we_damage in n + w.we_hit_enchant

let get_hit_chance g weapon =
  let hit_chance = 40 in
  let hit_chance = hit_chance + 3 * to_hit weapon in
  hit_chance + 2 * g.rogue.exp + 2 * g.rogue.ring_exp - g.rogue.r_rings

let get_w_damage g weapon =
  match weapon with
    None -> -1
  | Some (_, w) ->
      let (n, d, _) = w.we_damage in
      let to_hit = n + w.we_hit_enchant in
      let damage = d + w.we_d_enchant in
      let new_damage = to_hit, damage, None in get_damage g new_damage true

let damage_for_strength g =
  let strength = g.rogue.str_current + g.rogue.add_strength in
  if strength <= 6 then strength - 5
  else if strength <= 14 then 1
  else if strength <= 17 then 3
  else if strength <= 18 then 4
  else if strength <= 20 then 5
  else if strength <= 21 then 6
  else if strength <= 30 then 7
  else 8

let get_weapon_damage g weapon =
  let damage = get_w_damage g weapon in
  let damage = damage + damage_for_strength g in
  damage + (g.rogue.exp + g.rogue.ring_exp - g.rogue.r_rings + 1) / 2

let coughable g row col =
  if row < 1 || row > 24 - 2 || col < 0 || col > 80 - 1 then false
  else if
    g.dungeon.(row).(col) land (0o1 lor 0o4 lor 0o400 lor 0o1000) = 0 &&
    g.dungeon.(row).(col) land (0o200 lor 0o100 lor 0o40) <> 0
  then
    true
  else false

let cough_up g monster =
  if g.cur_level < g.max_level then ()
  else
    let obj =
      if monster.mn_flags land 0o20000 <> 0 then
        let q = get_rand (g.cur_level * 15) (g.cur_level * 30) in
        Some (Object.get_gold (Some q))
      else if not (rand_percent monster.mn_drop_percent) then None
      else Some (Object.gr_object g)
    in
    match obj with
      Some obj ->
        let row = monster.mn_row in
        let col = monster.mn_col in
        let add_in_set x s = if List.mem x s then s else x :: s in
        let rec loop_n n =
          if n <= 4 then
            let list =
              let rec loop_i list i =
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
              loop_i [] (-n)
            in
            if list = [] then loop_n (n + 1)
            else
              let r = get_rand 0 (List.length list - 1) in
              let (row, col) = List.nth list r in
              Level.place_at g obj row col;
              if row <> g.rogue.row || col <> g.rogue.col then
                if g.dungeon.(row).(col) land 0o2 = 0 then
                  Curses.mvaddch row col (get_dungeon_char g row col)
                else
                  let mon = monster_at g row col in
                  mon.mn_trail_char <- get_mask_char obj
        in
        loop_n 0
    | None -> ()

let mon_damage g monster damage =
  monster.mn_hp_to_kill <- monster.mn_hp_to_kill - damage;
  if monster.mn_hp_to_kill <= 0 then
    let row = monster.mn_row in
    let col = monster.mn_col in
    g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot 0o2;
    Curses.mvaddch row col (get_dungeon_char g row col);
    g.rogue.fight_monster <- None;
    cough_up g monster;
    let mn = transl g.lang (Monster.mon_name g monster) in
    let msg = sprintf (ftransl g.lang "Defeated the %s.") mn in
    (*
    let msg = msg ^ sprintf " (%d-%d<=0)." (monster.mn_hp_to_kill + damage) damage in
    *)
    message g (g.hit_message ^ etransl msg) true;
    g.hit_message <- "";
    add_exp g monster.mn_kill_exp hp_raise;
    take_from_monsters g monster;
    if monster.mn_flags land 0o4000 <> 0 then g.rogue.being_held <- false;
    false
  else true

let check_gold_seeker g monster =
  monster.mn_flags <- monster.mn_flags land lnot 0o1000000

let rogue_hit g monster force_hit =
  if check_imitator g monster then ()
  else
    let hit_chance =
      if force_hit then 100 else get_hit_chance g g.rogue.weapon
    in
    let hit_chance = if g.wizard then hit_chance * 2 else hit_chance in
    if not (rand_percent hit_chance) then
      (if g.rogue.fight_monster = None then
         g.hit_message <- etransl (transl g.lang "You miss.") ^ " ")
    else
      begin let damage = get_weapon_damage g g.rogue.weapon in
        let damage = if g.wizard then damage * 3 else damage in
        if mon_damage g monster damage then
          if g.rogue.fight_monster = None then
            begin
              g.hit_message <- etransl (transl g.lang "You hit.") ^ " ";
              let row = monster.mn_row in
              let col = monster.mn_col in
              show_monster g row col monster (gmc g monster)
            end
      end;
    check_gold_seeker g monster;
    wake_up monster

let tele_away g monster =
  if monster.mn_flags land 0o4000 <> 0 then g.rogue.being_held <- false;
  let (row, col, _) = gr_row_col g (0o100 lor 0o200 lor 0o4 lor 0o1) 0 in
  Curses.mvaddch monster.mn_row monster.mn_col monster.mn_trail_char;
  g.dungeon.(monster.mn_row).(monster.mn_col) <-
    g.dungeon.(monster.mn_row).(monster.mn_col) land lnot 0o2;
  monster.mn_row <- row;
  monster.mn_col <- col;
  g.dungeon.(row).(col) <- g.dungeon.(row).(col) lor 0o2;
  monster.mn_trail_char <- Curses.mvinch row col;
  if g.rogue.detect_monster || rogue_can_see g row col then
    show_monster g row col monster (gmc g monster)

let zap_monster g monster wand =
  let row = monster.mn_row in
  let col = monster.mn_col in
  match wand with
    SlowMonster ->
      if monster.mn_flags land 0o1 <> 0 then
        monster.mn_flags <- monster.mn_flags land lnot 0o1
      else
        begin
          monster.mn_slowed_toggle <- false;
          monster.mn_flags <- monster.mn_flags lor 0o2
        end
  | HasteMonster ->
      if monster.mn_flags land 0o2 <> 0 then
        monster.mn_flags <- monster.mn_flags land lnot 0o2
      else monster.mn_flags <- monster.mn_flags lor 0o1
  | TeleportAway -> tele_away g monster
  | ConfuseMonster ->
      monster.mn_flags <- monster.mn_flags lor 0o1000;
      monster.mn_moves_confused <- monster.mn_moves_confused + get_rand 12 22
  | Invisibility -> monster.mn_flags <- monster.mn_flags lor 0o4
  | Polymorph ->
      if monster.mn_flags land 0o4000 <> 0 then g.rogue.being_held <- false;
      let tc = monster.mn_trail_char in
      take_from_monsters g monster;
      let monster = Imonster.gr_monster g None in
      monster.mn_row <- row;
      monster.mn_col <- col;
      g.level_monsters <- g.level_monsters @ [monster];
      monster.mn_trail_char <- tc;
      if monster.mn_flags land 0o20000000 = 0 then wake_up monster;
      if g.rogue.detect_monster || rogue_can_see g row col then
        show_monster g row col monster (gmc g monster)
  | PutToSleep ->
      monster.mn_flags <- monster.mn_flags lor (0o10 lor 0o200000000);
      monster.mn_nap_length <- get_rand 3 6
  | MagicMissile -> rogue_hit g monster true
  | Cancellation ->
      if monster.mn_flags land 0o4000 <> 0 then g.rogue.being_held <- false;
      if monster.mn_flags land 0o40000 <> 0 then monster.mn_drop_percent <- 0;
      monster.mn_flags <-
        monster.mn_flags land
        lnot
          (0o100 lor 0o200 lor 0o776000 lor 0o4 lor 0o40000000 lor
           0o20000000 lor 0o10000000 lor 0o1000000 lor 0o4000)
  | DoNothing -> message g (transl g.lang "Nothing happens" ^ ".") false

let get_zapped_monster g dir =
  let rec loop_o orow ocol =
    let (row, col) = get_dir_rc dir orow ocol false in
    if row = orow && col = ocol ||
       g.dungeon.(row).(col) land (0o10 lor 0o20) <> 0 ||
       g.dungeon.(row).(col) = 0o0
    then
      None
    else if g.dungeon.(row).(col) land 0o2 <> 0 then
      if not (imitating g row col) then Some (monster_at g row col, row, col)
      else loop_o row col
    else loop_o row col
  in
  loop_o g.rogue.row g.rogue.col

let zap g =
  let dir =
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
  if dir = '\027' then false
  else
    let ch =
      pack_letter g (transl g.lang "Zap with what?")
        (function
           Wand _ -> true
         | _ -> false)
    in
    if ch = '\027' then false
    else
      begin
        check_message g;
        match get_letter_object g ch false with
          None -> false
        | Some {ob_kind = Wand w} ->
            if w.wa_hits = 0 then
              message g (transl g.lang "Nothing happens" ^ ".") false
            else
              begin
                w.wa_hits <- w.wa_hits - 1;
                match get_zapped_monster g dir with
                  Some (monster, row, col) ->
                    wake_up monster;
                    zap_monster g monster w.wa_kind;
                    relight g
                | None -> ()
              end;
            true
        | Some _ ->
            message g (transl g.lang "You can't zap with that" ^ ".") false;
            false
      end

let throw_at_monster g monster obj =
  let weapon =
    match obj.ob_kind with
      Weapon w -> Some (' ', w)
    | _ -> None
  in
  let hit_chance = get_hit_chance g weapon in
  let damage = get_weapon_damage g weapon in
  let (damage, hit_chance) =
    match obj.ob_kind, g.rogue.weapon with
      Weapon {we_kind = Arrow}, Some (_, {we_kind = Bow}) ->
        let damage = damage + get_weapon_damage g g.rogue.weapon in
        let damage = damage * 2 / 3 in
        let hit_chance = hit_chance + hit_chance / 3 in damage, hit_chance
    | Weapon {we_kind = Dagger | Shuriken | Dart; we_in_use = true}, _ ->
        let damage = damage * 3 / 2 in
        let hit_chance = hit_chance + hit_chance / 3 in damage, hit_chance
    | _ -> damage, hit_chance
  in
  let t = obj.ob_quantity in
  obj.ob_quantity <- 1;
  let mess = sprintf (ftransl g.lang "The %s") (name_of g obj) in
  obj.ob_quantity <- t;
  if not (rand_percent hit_chance) then
    begin
      g.hit_message <- etransl (mess ^ " " ^ transl g.lang "misses." ^ " ");
      false
    end
  else
    begin
      g.hit_message <- etransl (mess ^ " " ^ transl g.lang "hit." ^ " ");
      begin match obj.ob_kind with
        Wand {wa_kind = wk} ->
          if rand_percent 75 then zap_monster g monster wk
          else let _ = (mon_damage g monster damage : bool) in ()
      | _ -> let _ = (mon_damage g monster damage : bool) in ()
      end;
      true
    end

let tempo g =
  if fast g then () else let (_, _, _) = Unix.select [] [] [] 0.05 in ()

let get_thrown_at_monster g obj dir orow ocol =
  let rogue = g.rogue in
  let ch = get_mask_char obj in
  let rec loop orow ocol i =
    if i < 24 then
      let (row, col) = get_dir_rc dir orow ocol false in
      if g.dungeon.(row).(col) = 0o0 ||
         g.dungeon.(row).(col) land (0o10 lor 0o20 lor 0o1000) <> 0 &&
         g.dungeon.(row).(col) land 0o400 = 0
      then
        None, orow, ocol
      else
        begin
          if i <> 0 && rogue_can_see g orow ocol then
            begin
              tempo g;
              Curses.mvaddch orow ocol (get_dungeon_char g orow ocol);
              Curses.move rogue.row rogue.col
            end;
          if rogue_can_see g row col then
            begin
              if g.dungeon.(row).(col) land 0o2 = 0 then
                begin
                  if i = 0 then tempo g;
                  Curses.mvaddch row col ch;
                  Curses.move rogue.row rogue.col
                end;
              Curses.refresh ()
            end;
          if g.dungeon.(row).(col) land 0o2 <> 0 && not (imitating g row col)
          then
            begin
              if rogue_can_see g row col then
                begin
                  Curses.mvaddch row col ch;
                  Curses.move rogue.row rogue.col;
                  Curses.refresh ()
                end;
              tempo g;
              Some (monster_at g row col), row, col
            end
          else
            let i =
              if g.dungeon.(row).(col) land 0o200 <> 0 then i + 2 else i
            in
            loop row col (i + 1)
        end
    else None, orow, ocol
  in
  loop orow ocol 0

let extract_copy_of_weapon obj =
  let copy_object_kind =
    function
      Weapon w -> Weapon {w with we_kind = w.we_kind; we_in_use = false}
    | x -> x
  in
  {obj with ob_kind = copy_object_kind obj.ob_kind; ob_quantity = 1}

let flop_weapon g obj row col =
  let (found, i, row, col) =
    let rec loop row col i =
      if i < 9 && row < 24 - 1 && row >= 1 && col < 80 && col >= 0 &&
         g.dungeon.(row).(col) land lnot (0o100 lor 0o200 lor 0o40 lor 0o2) <>
           0
      then
        let (row, col) = rand_around g i row col in
        let i = i + 1 in
        if row > 24 - 2 || row < 1 || col > 80 - 1 || col < 0 ||
           g.dungeon.(row).(col) = 0 ||
           g.dungeon.(row).(col) land
           lnot (0o100 lor 0o200 lor 0o40 lor 0o2) <>
             0
        then
          loop row col i
        else true, i, row, col
      else false, i, row, col
    in
    loop row col 0
  in
  if found || i = 0 then
    let new_obj = extract_copy_of_weapon obj in
    Level.place_at g new_obj row col;
    (if rogue_can_see g row col && (row <> g.rogue.row || col <> g.rogue.col)
     then
       let mon = g.dungeon.(row).(col) land 0o2 in
       g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot 0o2;
       let dch = get_dungeon_char g row col in
       if mon <> 0 then
         let mch = Curses.mvinch row col in
         let monster = monster_at g row col in
         monster.mn_trail_char <- dch;
         (if mch < 'A' || mch > 'Z' then Curses.mvaddch row col dch)
       else Curses.mvaddch row col dch;
       g.dungeon.(row).(col) <- g.dungeon.(row).(col) lor mon)
  else
    let t = obj.ob_quantity in
    obj.ob_quantity <- 1;
    let msg =
      sprintf (ftransl g.lang "The %s vanishes as it hits the ground.")
        (name_of g obj)
    in
    obj.ob_quantity <- t; message g (etransl msg) false

let one_throw g dir (ch, obj) =
  let rogue = g.rogue in
  let row = rogue.row in
  let col = rogue.col in
  let just_once =
    match obj with
      {ob_kind = Weapon {we_in_use = true}; ob_quantity = 1} ->
        unwield g; true
    | {ob_kind = Armor {ar_in_use = true}} ->
        Monster.mv_aquators g; unwear g; print_stats g 0o20; true
    | {ob_kind = Ring ({rg_in_use = Some _} as r)} -> un_put_on g r; true
    | _ -> false
  in
  let (monster, row, col) = get_thrown_at_monster g obj dir row col in
  show_rogue g;
  Curses.refresh ();
  if rogue_can_see g row col && (row <> rogue.row || col <> rogue.col) then
    Curses.mvaddch row col (get_dungeon_char g row col);
  let just_once =
    match monster with
      Some monster ->
        wake_up monster;
        check_gold_seeker g monster;
        if not (throw_at_monster g monster obj) then
          flop_weapon g obj row col;
        true
    | None -> flop_weapon g obj row col; just_once
  in
  vanish g ch obj; just_once
