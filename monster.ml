(* $Id: monster.ml,v 1.84 2013/05/16 19:24:40 deraugla Exp $ *)

(* #load "pa_more.cmo" *)
(* #use "rogue.def" *)


open Rogue
open Dialogue
open Imisc
open Misc
open Printf
open Translate

let mon_can_go g monster row col =
  let dr = monster.mn_row - row in
  let dc = monster.mn_col - col in
  if dr >= 2 || dr <= -2 then false
  else if dc >= 2 || dc <= -2 then false
  else if
    g.dungeon.(monster.mn_row).(col) = 0 ||
    g.dungeon.(row).(monster.mn_col) = 0
  then
    false
  else if
    not (is_passable g row col) || g.dungeon.(row).(col) land 0o2 <> 0
  then
    false
  else if
    monster.mn_row <> row && monster.mn_col <> col &&
    (g.dungeon.(row).(col) land 0o40 <> 0 ||
     g.dungeon.(monster.mn_row).(monster.mn_col) land 0o40 <> 0)
  then
    false
  else
    let r =
      if monster.mn_flags land (0o200 lor 0o1000 lor 0o400) = 0 &&
         monster.mn_target = None
      then
        if monster.mn_row < g.rogue.row && row < monster.mn_row ||
           monster.mn_row > g.rogue.row && row > monster.mn_row ||
           monster.mn_col < g.rogue.col && col < monster.mn_col ||
           monster.mn_col > g.rogue.col && col > monster.mn_col
        then
          false
        else true
      else true
    in
    if not r then false
    else if g.dungeon.(row).(col) land 0o1 <> 0 then
      match object_at g row col with
        {ob_kind = Scroll ScareMonster} -> false
      | _ -> true
    else true

let mon_sees g monster row col =
  match get_room_number g row col with
    Some rn ->
      if get_room_number g monster.mn_row monster.mn_col = Some rn &&
         g.rooms.(rn).is_room land 0o4 = 0
      then
        true
      else
        let rdif = row - monster.mn_row in
        let cdif = col - monster.mn_col in
        rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1
  | None ->
      let rdif = row - monster.mn_row in
      let cdif = col - monster.mn_col in
      rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1

let get_oth_room g rn row col =
  let d =
    if row = g.rooms.(rn).top_row then 0
    else if row = g.rooms.(rn).bottom_row then 2
    else if col = g.rooms.(rn).left_col then 3
    else if col = g.rooms.(rn).right_col then 1
    else -1
  in
  if d <> -1 then
    match g.rooms.(rn).doors.(d) with
      Some {oth_row = row; oth_col = col} -> Some (row, col)
    | None -> None
  else None

let dr_course g monster entering =
  let row = monster.mn_row in
  let col = monster.mn_col in
  let rogue = g.rogue in
  if mon_sees g monster rogue.row rogue.col then monster.mn_target <- None
  else
    let rn =
      match get_room_number g row col with
        Some rn -> rn
      | None -> assert false
    in
    if entering then
      let tried = Array.make 9 false in
      let rec loop_i i =
        if i < 9 then
          let rr = get_rand 0 (9 - 1) in
          if tried.(rr) then loop_i i
          else
            begin
              tried.(rr) <- true;
              if g.rooms.(rr).is_room land (0o2 lor 0o4) = 0 || rr = rn then
                loop_i (i + 1)
              else
                let rec loop_k k =
                  if k < 4 then
                    match g.rooms.(rr).doors.(k) with
                      Some door ->
                        if door.oth_room = rn then
                          begin
                            monster.mn_target <-
                              Some (door.oth_row, door.oth_col);
                            if door.oth_row = row && door.oth_col = col then
                              loop_k (k + 1)
                          end
                        else loop_k (k + 1)
                    | None -> loop_k (k + 1)
                  else loop_i (i + 1)
                in
                loop_k 0
            end
        else
          let rec loop_i list i =
            if i <= g.rooms.(rn).bottom_row then
              let rec loop_j list j =
                if j <= g.rooms.(rn).right_col then
                  if (i <> monster.mn_row || j <> monster.mn_col) &&
                     g.dungeon.(i).(j) land 0o40 <> 0
                  then
                    loop_j ((i, j) :: list) (j + 1)
                  else loop_j list (j + 1)
                else loop_i list (i + 1)
              in
              loop_j list g.rooms.(rn).left_col
            else if list <> [] then
              let r = get_rand 0 (List.length list - 1) in
              monster.mn_target <- Some (List.nth list r)
            else
              let rec loop_i i =
                if i < 9 then
                  let rec loop_j j =
                    if j < 4 then
                      match g.rooms.(i).doors.(j) with
                        Some door when door.oth_room = rn ->
                          let rec loop_k k =
                            if k < 4 then
                              match g.rooms.(rn).doors.(k) with
                                Some door when door.oth_room = i ->
                                  monster.mn_target <-
                                    Some (door.oth_row, door.oth_col)
                              | _ -> loop_k (k + 1)
                            else loop_j (j + 1)
                          in
                          loop_k 0
                      | _ -> loop_j (j + 1)
                    else loop_i (i + 1)
                  in
                  loop_j 0
                else monster.mn_target <- None
              in
              loop_i 0
          in
          loop_i [] g.rooms.(rn).top_row
      in
      loop_i 0
    else monster.mn_target <- get_oth_room g rn row col

let move_mon_to g monster row col =
  let mrow = monster.mn_row in
  let mcol = monster.mn_col in
  g.dungeon.(mrow).(mcol) <- g.dungeon.(mrow).(mcol) land lnot 0o2;
  g.dungeon.(row).(col) <- g.dungeon.(row).(col) lor 0o2;
  let c = Curses.mvinch mrow mcol in
  if c >= 'A' && c <= 'Z' then
    begin
      if not g.rogue.detect_monster then ()
      else if rogue_can_see g mrow mcol then ()
      else if monster.mn_trail_char = '.' then monster.mn_trail_char <- ' ';
      Curses.mvaddch mrow mcol monster.mn_trail_char
    end;
  monster.mn_trail_char <- Curses.mvinch row col;
  if g.rogue.blind = 0 && (g.rogue.detect_monster || rogue_can_see g row col)
  then
    if monster.mn_flags land 0o4 = 0 || g.rogue.detect_monster ||
       g.rogue.see_invisible || g.rogue.r_see_invisible
    then
      show_monster g row col monster (gmc g monster);
  if g.dungeon.(row).(col) land 0o40 <> 0 then
    begin match get_room_number g row col with
      Some rn ->
        if g.cur_room <> Some rn && g.dungeon.(mrow).(mcol) = 0o100 &&
           g.rogue.blind = 0
        then
          Curses.mvaddch mrow mcol ' '
    | None -> ()
    end;
  monster.mn_row <- row;
  monster.mn_col <- col;
  if g.dungeon.(row).(col) land 0o40 <> 0 then
    dr_course g monster (g.dungeon.(mrow).(mcol) land 0o200 <> 0)

let mtry g monster row col =
  if row < 1 || row >= 24 || col < 0 || col >= 80 then false
  else if mon_can_go g monster row col then
    begin move_mon_to g monster row col; true end
  else false

let flit g monster =
  if not (rand_percent 33) then false
  else if rand_percent 10 then true
  else
    let row = monster.mn_row in
    let col = monster.mn_col in
    let rec loop_i i =
      if i < 9 then
        let (row, col) = rand_around g i row col in
        if row = g.rogue.row && col = g.rogue.col then loop_i (i + 1)
        else if mtry g monster row col then true
        else loop_i (i + 1)
      else true
    in
    loop_i 0

let mon_name g monster =
  if g.rogue.blind > 0 ||
     monster.mn_flags land 0o4 <> 0 &&
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

let confuse g =
  g.rogue.confused <- g.rogue.confused + get_rand 12 22; show_rogue g

let m_confuse g monster =
  if not (rogue_can_see g monster.mn_row monster.mn_col) then false
  else if rand_percent 45 then
    begin monster.mn_flags <- monster.mn_flags land lnot 0o10000000; false end
  else if rand_percent 55 then
    begin
      monster.mn_flags <- monster.mn_flags land lnot 0o10000000;
      confuse g;
      let msg =
        sprintf (ftransl g.lang "The gaze of the %s has confused you.")
          (transl g.lang (mon_name g monster))
      in
      message g (etransl msg) true; true
    end
  else false

let rogue_damage g d monster =
  if d >= g.rogue.hp_current then
    begin
      g.rogue.hp_current <- 0;
      print_stats g 0o4;
      let i = Char.code monster.mn_char - Char.code 'A' in
      Finish.killed_by g (Monster (Imonster.visible_mon_name g i))
    end
  else
    begin
      g.rogue.hp_current <- g.rogue.hp_current - d;
      print_stats g 0o4;
      show_rogue g
    end

let sting g monster =
  let rogue = g.rogue in
  if rogue.str_current <= 3 || g.rogue.sustain_strength then ()
  else
    let sting_chance = 35 + 6 * (6 - get_armor_class rogue.armor) in
    let sting_chance =
      if rogue.exp + rogue.ring_exp > 8 then
        sting_chance - 6 * (rogue.exp - rogue.ring_exp - 8)
      else sting_chance
    in
    if rand_percent sting_chance then
      let msg =
        sprintf (ftransl g.lang "The %s's bite has weakened you.")
          (transl g.lang (mon_name g monster))
      in
      message g (etransl msg) false;
      rogue.str_current <- rogue.str_current - 1;
      print_stats g 0o10

let rust g monster =
  match g.rogue.armor with
    None | Some (_, {ar_kind = Leather}) -> ()
  | Some (_, a) ->
      if get_armor_class g.rogue.armor <= 1 then ()
      else if a.ar_is_protected || g.rogue.maintain_armor then
        match monster with
          Some monster ->
            if monster.mn_flags land 0o4000000 = 0 then
              begin
                message g (transl g.lang "The rust vanishes instantly.")
                  false;
                monster.mn_flags <- monster.mn_flags lor 0o4000000
              end
        | None -> ()
      else
        begin
          a.ar_enchant <- a.ar_enchant - 1;
          message g (transl g.lang "Your armor weakens.") false;
          print_stats g 0o20
        end

let disappear g monster =
  let row = monster.mn_row in
  let col = monster.mn_col in
  g.dungeon.(row).(col) <- g.dungeon.(row).(col) land lnot 0o2;
  if rogue_can_see g row col then
    Curses.mvaddch row col (get_dungeon_char g row col);
  take_from_monsters g monster;
  g.mon_disappeared <- true

let drain_life g =
  let rogue = g.rogue in
  if rand_percent 60 || rogue.hp_max <= 30 || rogue.hp_current < 10 then ()
  else
    let n = get_rand 1 3 in
    if n <> 2 || not rogue.sustain_strength then
      message g (transl g.lang "You feel weaker.") false;
    if n <> 2 then
      begin
        rogue.hp_max <- rogue.hp_max - 1;
        rogue.hp_current <- rogue.hp_current - 1;
        rogue.less_hp <- rogue.less_hp + 1;
        show_rogue g
      end;
    if n <> 1 then
      if rogue.str_current > 3 && not rogue.sustain_strength then
        begin
          rogue.str_current <- rogue.str_current - 1;
          if coin_toss () then rogue.str_max <- rogue.str_max - 1
        end;
    print_stats g (0o10 lor 0o4)

let drop_level g =
  let rogue = g.rogue in
  if rand_percent 80 || rogue.exp <= 5 then ()
  else
    begin
      rogue.exp_points <- level_points.(rogue.exp - 2) - get_rand 9 29;
      rogue.exp <- rogue.exp - 2;
      let hp = hp_raise g in
      rogue.hp_current <- rogue.hp_current - hp;
      if rogue.hp_current <= 0 then rogue.hp_current <- 1;
      rogue.hp_max <- rogue.hp_max - hp;
      if rogue.hp_max <= 0 then rogue.hp_max <- 1;
      add_exp g 1 (fun _ -> 0);
      show_rogue g
    end

let steal_gold g monster =
  if g.rogue.gold <= 0 || rand_percent 10 then ()
  else
    let amount = get_rand (g.cur_level * 10) (g.cur_level * 30) in
    let amount = min amount g.rogue.gold in
    g.rogue.gold <- g.rogue.gold - amount;
    message g (transl g.lang "Your purse feels lighter.") false;
    print_stats g 0o2;
    disappear g monster

let in_use obj =
  match obj.ob_kind with
    Armor a -> a.ar_in_use
  | Ring r -> r.rg_in_use <> None
  | Weapon w -> w.we_in_use
  | _ -> false

let steal_item g monster =
  if rand_percent 15 then ()
  else
    begin
      if g.rogue.pack = [] then ()
      else
        begin let objs =
          List.filter (fun (_, obj) -> not (in_use obj)) g.rogue.pack
        in
          let len = List.length objs in
          if len = 0 then ()
          else
            let n = get_rand 0 (len - 1) in
            let (ch, obj) = List.nth objs n in
            let obj1 =
              {obj with ob_quantity =
                match obj.ob_kind with
                  Weapon _ -> obj.ob_quantity
                | _ -> 1}
            in
            let msg =
              transl g.lang "She stole" ^ " " ^ get_desc g obj1 false
            in
            message g (etransl msg) false;
            if obj1.ob_quantity = obj.ob_quantity then take_from_pack g ch
            else obj.ob_quantity <- obj.ob_quantity - 1
        end;
      disappear g monster
    end

let get_closer row col trow tcol =
  let row =
    if row < trow then row + 1 else if row > trow then row - 1 else row
  in
  let col =
    if col < tcol then col + 1 else if col > tcol then col - 1 else col
  in
  row, col

let move_confused g monster =
  if monster.mn_flags land 0o10 = 0 then
    begin
      monster.mn_moves_confused <- monster.mn_moves_confused - 1;
      if monster.mn_moves_confused <= 0 then
        monster.mn_flags <- monster.mn_flags land lnot 0o1000;
      if monster.mn_flags land 0o100000000 <> 0 then coin_toss ()
      else if rand_percent 15 then true
      else
        let row = monster.mn_row in
        let col = monster.mn_col in
        let rec loop_i i =
          if i < 9 then
            let (row, col) = rand_around g i row col in
            if row = g.rogue.row && col = g.rogue.col then false
            else if mtry g monster row col then true
            else loop_i (i + 1)
          else false
        in
        loop_i 0
    end
  else false

let rec mv_mons g =
  if g.rogue.haste_self mod 2 = 1 then ()
  else
    List.iter
      (fun monster ->
         if g.mon_disappeared &&
            not
              (List.exists (fun mn -> mn.mn_unique_id = monster.mn_unique_id)
                 g.level_monsters)
         then
           ()
         else
           let init_pos = Some (monster.mn_row, monster.mn_col) in
           let nm =
             if monster.mn_flags land 0o1 <> 0 then
               begin
                 g.mon_disappeared <- false;
                 mv_monster g monster g.rogue.row g.rogue.col None;
                 if g.mon_disappeared then 1 else 0
               end
             else 2
           in
           let nm =
             if nm = 2 && monster.mn_flags land 0o2 <> 0 then
               begin
                 monster.mn_slowed_toggle <- not monster.mn_slowed_toggle;
                 if monster.mn_slowed_toggle then 1 else 0
               end
             else nm
           in
           let nm =
             if nm <> 1 && monster.mn_flags land 0o1000 <> 0 &&
                move_confused g monster
             then
               1
             else nm
           in
           if nm <> 1 then
             let flew =
               if monster.mn_flags land 0o100 <> 0 &&
                  monster.mn_flags land 0o200000000 = 0 &&
                  not (mon_can_go g monster g.rogue.row g.rogue.col)
               then
                 begin
                   mv_monster g monster g.rogue.row g.rogue.col init_pos;
                   true
                 end
               else false
             in
             if not (flew && mon_can_go g monster g.rogue.row g.rogue.col)
             then
               mv_monster g monster g.rogue.row g.rogue.col init_pos)
      g.level_monsters
and mv_monster g monster row col init_pos_opt =
  if monster.mn_flags land 0o10 <> 0 then
    (if monster.mn_flags land 0o200000000 <> 0 then
       begin
         monster.mn_nap_length <- monster.mn_nap_length - 1;
         if monster.mn_nap_length <= 0 then
           monster.mn_flags <-
             monster.mn_flags land lnot (0o200000000 lor 0o10)
       end
     else if
       monster.mn_flags land 0o20 <> 0 &&
       rogue_is_around g monster.mn_row monster.mn_col &&
       rand_percent
         (if g.rogue.stealthy > 0 then 45 / (3 + g.rogue.stealthy) else 45)
     then
       wake_up monster)
  else if monster.mn_flags land 0o400000000 <> 0 then
    monster.mn_flags <- monster.mn_flags land lnot 0o400000000
  else if monster.mn_flags land 0o200 <> 0 && flit g monster then ()
  else if
    monster.mn_flags land 0o100000000 <> 0 &&
    not (mon_can_go g monster g.rogue.row g.rogue.col)
  then
    ()
  else if monster.mn_flags land 0o2000000 <> 0 then ()
  else if monster.mn_flags land 0o10000000 <> 0 && m_confuse g monster then ()
  else if mon_can_go g monster g.rogue.row g.rogue.col then
    mon_hit g monster "" false
  else if monster.mn_flags land 0o40000000 <> 0 && flame_broil g monster then
    ()
  else if monster.mn_flags land 0o1000000 <> 0 && seek_gold g monster then ()
  else
    let (row, col) =
      match monster.mn_target with
        Some (trow, tcol) ->
          if monster.mn_row = trow && monster.mn_col = tcol then
            begin monster.mn_target <- None; row, col end
          else trow, tcol
      | None -> row, col
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
    if g.dungeon.(row).(monster.mn_col) land 0o40 <> 0 &&
       mtry g monster row monster.mn_col
    then
      ()
    else if
      g.dungeon.(monster.mn_row).(col) land 0o40 <> 0 &&
      mtry g monster monster.mn_row col
    then
      ()
    else if mtry g monster row col then ()
    else
      let tried = Array.make 6 false in
      begin let rec loop_i i =
        if i < 6 then
          let n = get_rand 0 5 in
          if not tried.(n) then
            let (row, col) =
              match n with
                0 -> row, monster.mn_col - 1
              | 1 -> row, monster.mn_col
              | 2 -> row, monster.mn_col + 1
              | 3 -> monster.mn_row - 1, col
              | 4 -> monster.mn_row, col
              | 5 -> monster.mn_row + 1, col
              | _ -> assert false
            in
            if mtry g monster row col then ()
            else begin tried.(n) <- true; loop_i (i + 1) end
          else loop_i i
      in
        loop_i 0
      end;
      if monster.mn_row = monster.mn_o_row &&
         monster.mn_col = monster.mn_o_col ||
         monster.mn_flags land 0o1 <> 0 &&
         (match init_pos_opt with
            Some (init_row, init_col) ->
              monster.mn_row = init_row && monster.mn_col = init_col
          | None -> false)
      then
        begin
          monster.mn_o <- monster.mn_o + 1;
          if monster.mn_o > 4 then
            if monster.mn_target = None &&
               not (mon_sees g monster g.rogue.row g.rogue.col)
            then
              let trow = get_rand 1 (24 - 2) in
              let tcol = get_rand 0 (80 - 1) in
              monster.mn_target <- Some (trow, tcol)
            else begin monster.mn_target <- None; monster.mn_o <- 0 end
        end
      else
        begin
          monster.mn_o_row <- monster.mn_row;
          monster.mn_o_col <- monster.mn_col;
          monster.mn_o <- 0
        end
and mon_hit g monster other flame =
  let rogue = g.rogue in
  begin match rogue.fight_monster with
    Some m -> if m <> monster then rogue.fight_monster <- None
  | None -> ()
  end;
  monster.mn_target <- None;
  let hit_chance =
    if g.cur_level >= 26 * 2 then 100
    else
      monster.mn_hit_chance -
      (2 * rogue.exp + 2 * rogue.ring_exp - rogue.r_rings)
  in
  let hit_chance = if g.wizard then hit_chance / 2 else hit_chance in
  if rogue.fight_monster = None then g.interrupted <- true;
  let mn = transl g.lang (mon_name g monster) in
  let hit_chance =
    if other <> "" then
      hit_chance - (rogue.exp + rogue.ring_exp - rogue.r_rings)
    else hit_chance
  in
  if not (rand_percent hit_chance) then
    (if g.rogue.fight_monster = None then
       let msg =
         sprintf (ftransl g.lang "The %s misses.")
           (if other <> "" then other else mn)
       in
       message g (g.hit_message ^ etransl msg) true; g.hit_message <- "")
  else
    begin
      if rogue.fight_monster = None then
        begin let msg =
          sprintf (ftransl g.lang "The %s hit.")
            (if other <> "" then other else mn)
        in
          message g (g.hit_message ^ etransl msg) true;
          show_rogue g;
          g.hit_message <- ""
        end;
      let damage =
        if monster.mn_flags land 0o100000000 = 0 then
          let damage = get_damage g monster.mn_damage true in
          let damage =
            if other <> "" && flame then
              max 1 (damage - get_armor_class rogue.armor)
            else damage
          in
          let minus =
            if g.cur_level >= 26 * 2 then 26 * 2 - g.cur_level
            else get_armor_class rogue.armor * 3 * damage / 100
          in
          damage - minus
        else
          let x = monster.mn_stationary_damage in
          monster.mn_stationary_damage <- monster.mn_stationary_damage + 1; x
      in
      let damage = if g.wizard then damage / 3 else damage in
      if damage > 0 then rogue_damage g damage monster;
      if monster.mn_flags land 0o776000 <> 0 then special_hit g monster
    end
and special_hit g monster =
  if monster.mn_flags land 0o1000 <> 0 && rand_percent 66 then ()
  else
    begin
      if monster.mn_flags land 0o2000 <> 0 then rust g (Some monster);
      if monster.mn_flags land 0o4000 <> 0 && g.rogue.levitate = 0 then
        g.rogue.being_held <- true;
      if monster.mn_flags land 0o10000 <> 0 then freeze g monster;
      if monster.mn_flags land 0o100000 <> 0 then sting g monster;
      if monster.mn_flags land 0o200000 <> 0 then drain_life g;
      if monster.mn_flags land 0o400000 <> 0 then drop_level g;
      if monster.mn_flags land 0o20000 <> 0 then steal_gold g monster
      else if monster.mn_flags land 0o40000 <> 0 then steal_item g monster
    end
and freeze g monster =
  if rand_percent 12 then ()
  else
    let rogue = g.rogue in
    let freeze_percent = 99 - rogue.str_current + rogue.str_current / 2 in
    let freeze_percent = freeze_percent - (rogue.exp + rogue.ring_exp) / 4 in
    let freeze_percent = freeze_percent - get_armor_class rogue.armor * 5 in
    let freeze_percent = freeze_percent - rogue.hp_max / 3 in
    if freeze_percent > 10 then
      begin
        monster.mn_flags <- monster.mn_flags lor 0o2000000;
        message g (transl g.lang "You are frozen.") true;
        let n = get_rand 4 8 in
        for i = 0 to n - 1 do mv_mons g done;
        if rand_percent freeze_percent then
          begin
            for i = 0 to 49 do mv_mons g done;
            Finish.killed_by g Hypothermia
          end
        else
          begin
            message g (transl g.lang "You can move again.") true;
            monster.mn_flags <- monster.mn_flags land lnot 0o2000000
          end
      end
and seek_gold g monster =
  match get_room_number g monster.mn_row monster.mn_col with
    None -> false
  | Some rn ->
      let rm = g.rooms.(rn) in
      let rec loop_i i =
        if i < rm.bottom_row then
          let rec loop_j j =
            if j < rm.right_col then
              if gold_at g i j && g.dungeon.(i).(j) land 0o2 = 0 then
                begin
                  monster.mn_flags <- monster.mn_flags lor 0o400;
                  let s = mon_can_go g monster i j in
                  monster.mn_flags <- monster.mn_flags land lnot 0o400;
                  if s then
                    begin
                      move_mon_to g monster i j;
                      monster.mn_flags <- monster.mn_flags lor 0o10;
                      monster.mn_flags <-
                        monster.mn_flags land lnot (0o20 lor 0o1000000)
                    end
                  else
                    begin
                      monster.mn_flags <-
                        monster.mn_flags land lnot 0o1000000;
                      monster.mn_flags <- monster.mn_flags lor 0o400;
                      mv_monster g monster i j None;
                      monster.mn_flags <- monster.mn_flags land lnot 0o400;
                      monster.mn_flags <- monster.mn_flags lor 0o1000000
                    end;
                  true
                end
              else loop_j (j + 1)
            else loop_i (i + 1)
          in
          loop_j (rm.left_col + 1)
        else false
      in
      loop_i (rm.top_row + 1)
and flame_broil g monster =
  if not (mon_sees g monster g.rogue.row g.rogue.col) || coin_toss () then
    false
  else
    let drow = abs (g.rogue.row - monster.mn_row) in
    let dcol = abs (g.rogue.col - monster.mn_col) in
    if drow <> 0 && dcol <> 0 && drow <> dcol || drow > 7 || dcol > 7 then
      false
    else
      begin
        if g.rogue.blind = 0 &&
           not (rogue_is_around g monster.mn_row monster.mn_col) &&
           not (fast g)
        then
          begin let row = monster.mn_row in
            let col = monster.mn_col in
            let (row, col) = get_closer row col g.rogue.row g.rogue.col in
            let tempo1 () = let (_, _, _) = Unix.select [] [] [] 0.08 in () in
            let tempo2 () = let (_, _, _) = Unix.select [] [] [] 0.02 in () in
            begin let rec loop row col =
              Curses.mvaddch row col '~';
              Curses.move g.rogue.row g.rogue.col;
              Curses.refresh ();
              tempo1 ();
              let (row, col) = get_closer row col g.rogue.row g.rogue.col in
              if row <> g.rogue.row || col <> g.rogue.col then loop row col
            in
              loop row col
            end;
            let row = monster.mn_row in
            let col = monster.mn_col in
            let (row, col) = get_closer row col g.rogue.row g.rogue.col in
            let rec loop row col =
              Curses.mvaddch row col (get_dungeon_char g row col);
              Curses.move g.rogue.row g.rogue.col;
              Curses.refresh ();
              tempo2 ();
              let (row, col) = get_closer row col g.rogue.row g.rogue.col in
              if row <> g.rogue.row || col <> g.rogue.col then loop row col
            in
            loop row col
          end;
        mon_hit g monster (transl g.lang "flame") true;
        true
      end

let mv_aquators g =
  List.iter
    (fun monster ->
       if monster.mn_flags land 0o2000 <> 0 &&
          mon_can_go g monster g.rogue.row g.rogue.col
       then
         begin
           mv_monster g monster g.rogue.row g.rogue.col None;
           monster.mn_flags <- monster.mn_flags lor 0o400000000
         end)
    g.level_monsters
