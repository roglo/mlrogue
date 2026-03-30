(* $Id: rob_action.ml,v 1.195 2010/07/05 21:12:25 deraugla Exp $ *)

(* #load "pa_if_match.cmo" *)

open Printf
open Rob_def
open Rob_misc
open Rob_monster
open Rob_object
open Rob_path
open Rob_position

type result = command * next_action * move option

let test_jeopardized g t monl =
  let pos = rogue_pos g in
  let hp = health_points g in
  match monl with
    [mov] ->
      let mch = dung_char g.dung (add_mov pos mov) in
      hp <= monster_power_at_level g t mch || is_freezing_monster g mch
  | monl ->
      let nb_hp =
        List.fold_left
          (fun nb mov ->
             let mch = dung_char g.dung (add_mov pos mov) in
             nb + monster_power_at_level g t mch)
          0 monl
      in
      g.attacked > 0 && hp <= nb_hp

let flame_risk g t =
  let transl = transl g in
  match g.rogue_room_and_door with
    Some (room, _) ->
      let pos = rogue_pos g in
      let rec loop nb_flames dist =
        function
          k :: kl ->
            let dir = mov_of_k k in
            let dmov = {di = dir.di * dist; dj = dir.dj * dist} in
            let pos1 = add_mov pos dmov in
            let nb_flames =
              if inside_room_or_at_door room pos1 &&
                 is_flaming_monster g (dung_char g.dung pos1)
              then
                nb_flames + 1
              else nb_flames
            in
            loop nb_flames dist kl
        | [] ->
            if dist > 10 then
              let mpow = monster_power_at_level g t transl.flaming_monster in
              mpow * nb_flames
            else loop nb_flames (dist + 1) run_around_list
      in
      loop 0 1 run_around_list
  | None -> 0

let test_monsters_risk g t monl =
  let pos = rogue_pos g in
  let jeopardized = test_jeopardized g t monl in
  match monl with
    [mov] ->
      let mch = dung_char g.dung (add_mov pos mov) in mov, mch, jeopardized
  | monl ->
      let monl =
        let monl2 =
          List.filter
            (fun mov ->
               let mch = dung_char g.dung (add_mov pos mov) in
               not (is_not_attackable_monster g mch))
            monl
        in
        if monl2 <> [] then monl2 else monl
      in
      let mov = List.nth monl (random_int g (List.length monl)) in
      let mch = dung_char g.dung (add_mov pos mov) in mov, mch, jeopardized

let close_to_a_free_door g =
  match g.rogue_room_and_door with
    Some (room, None) ->
      let pos = rogue_pos g in
      let rec loop =
        function
          k :: kl ->
            let mov = mov_of_k k in
            let pos1 = add_mov pos mov in
            if is_at_door g pos1 && not (is_monster (dung_char g.dung pos1))
            then
              Some mov
            else loop kl
        | [] -> None
      in
      loop [1; 3; 4; 6]
  | Some (_, Some _) | None -> None

let run_away_if_possible g in_room movl mmov =
  let pos = rogue_pos g in
  let movl =
    List.fold_left
      (fun movl mov ->
         let pos1 = add_mov pos mov in
         let monl = monsters_around g pos1 in
         let rec loop =
           function
             mmov :: rest ->
               if old_can_move_to g in_room (add_mov pos1 mmov) pos1 then movl
               else loop rest
           | [] -> mov :: movl
         in
         loop monl)
      [] movl
  in
  match movl with
    [] -> tempo g 0.1; None
  | [mov] ->
      tempo g 0.1;
      let monsters_in_room =
        is_at_door g pos && is_inside_room g (add_mov pos mmov)
      in
      if monsters_in_room then Some (mov, [])
      else
        let at_door_of_room_without_exit =
          match g.rogue_room_and_door with
            Some (room, Some _) ->
              let dl = find_doors g room in
              if List.length dl > 1 then false
              else
                begin match
                  dist_to_closest g room pos (fun ch _ -> ch = '%')
                with
                  Some _ -> false
                | None -> true
                end
          | Some (_, None) | None -> false
        in
        if at_door_of_room_without_exit then None else Some (mov, [])
  | movl ->
      let movl =
        List.filter (fun mov -> dung_char g.dung (add_mov pos mov) <> '^')
          movl
      in
      match g.rogue_room_and_door with
        Some (room, None) ->
          let dl = List.map (fun (p, d) -> false, p) (find_doors g room) in
          let dl =
            match dist_to_closest g room pos (fun ch _ -> ch = '%') with
              Some mov -> (true, add_mov pos mov) :: dl
            | None -> dl
          in
          let movl2 =
            List.fold_left
              (fun movl2 (to_stairs, tpos) ->
                 match path_in_room_to g room [] pos tpos with
                   Some (mov :: path) ->
                     if List.mem mov movl then
                       (to_stairs, (mov, (path, tpos))) :: movl2
                     else movl2
                 | Some [] | None -> movl2)
              [] dl
          in
          if movl2 = [] then
            begin
              tempo g 0.1;
              let mov = List.nth movl (random_int g (List.length movl)) in
              Some (mov, [])
            end
          else
            begin
              tempo g 0.1;
              match list_find (fun (to_stairs, _) -> to_stairs) movl2 with
                Some (_, (mov, _)) -> Some (mov, [])
              | None ->
                  let movl3 =
                    List.filter
                      (fun (_, (mov, (path, _))) ->
                         let rec loop pos =
                           function
                             mov :: path ->
                               let pos = add_mov pos mov in
                               if dung_char g.dung pos = '^' then false
                               else loop pos path
                           | [] -> true
                         in
                         loop pos (mov :: path))
                      movl2
                  in
                  let movl2 = if movl3 = [] then movl2 else movl3 in
                  let len = List.length movl2 in
                  let (_, (mov, (path, _))) =
                    List.nth movl2 (random_int g len)
                  in
                  Some (mov, path)
            end
      | Some (_, Some _) | None ->
          tempo g 0.1;
          let paths = paths_in_corridors_from g pos pos in
          let paths =
            List.filter
              (fun (path, _) ->
                 match path with
                   [] -> false
                 | pos1 :: _ -> pos1 <> add_mov pos mmov)
              paths
          in
          let paths =
            List.sort
              (fun (p1, _) (p2, _) ->
                 compare (List.length p2) (List.length p1))
              paths
          in
          let max_len = List.length (fst (List.hd paths)) in
          let paths =
            List.filter (fun (p, _) -> List.length p = max_len) paths
          in
          let len = List.length paths in
          let (path, tpos) = List.nth paths (random_int g len) in
          match path with
            pos1 :: _ -> Some (move_between pos pos1, [])
          | [] -> assert false

let set_regrets g =
  let pos = rogue_pos g in
  let regrets =
    match g.rogue_room_and_door with
      Some (room, _) ->
        dist_to_closest g room pos
          (fun ch mov ->
             List.mem ch list_obj_ch &&
             not (List.mem (add_mov pos mov) g.regrets) &&
             not (List.mem (add_mov pos mov) g.garbage) &&
             not (will_take_not_interesting_object g ch))
    | None -> None
  in
  match regrets with
    Some mov -> g.regrets <- add_mov pos mov :: g.regrets
  | None -> ()

let stop_paradise t =
  let mpt = get_monster_power_list t in
  let fname =
    let rec loop n =
      let fname = sprintf "monpow.%d" n in
      if Sys.file_exists fname then loop (n + 1) else fname
    in
    loop 1
  in
  write_monster_power_list_fname mpt fname; failwith "paradise on earth"

let drop_scare_with base state =
  {ds_base = base; ds_state = state; ds_last_corridor_kill_time = 0;
   ds_nb_killed_in_corr = 0; ds_nb_attempt = 0; ds_outside_tested = false;
   ds_monster_perhaps_blocked = None}

let start_drop_scare base ch = drop_scare_with base (DSdrop ch)

let glup g t s =
  let pos = rogue_pos g in
  let on_scare = List.mem pos g.scare_pos in
  let monl = monsters_around g pos in
  let jeopardized = if monl <> [] then test_jeopardized g t monl else false in
  trace t "*** glup:\n";
  trace t (sprintf "*** confused %b\n" g.confused);
  trace t (sprintf "*** blind %b\n" g.blind);
  trace t (sprintf "*** on scare %b\n" on_scare);
  trace t (sprintf "*** jeopardized %b\n" jeopardized);
  trace t
    (sprintf "*** lev > mean_level %b\n"
       (g.level >= level_of_very_mean_monsters));
  trace t (sprintf "*** attacked %d\n" g.attacked);
  trace t (sprintf "*** attacked_by_flame %d\n" g.attacked_by_flame);
  trace t (sprintf "*** attacked_by_invisible %b\n" g.attacked_by_invisible);
  trace t
    (sprintf "*** usable anti-flamer %b\n"
       (usable_anti_flamer_wand_in_pack g.pack <> None));
  trace t
    (sprintf "*** scroll_of_hold_monster_in_pack %b\n"
       (scroll_of_hold_monsters_in_pack g.pack <> None));
  failwith s

let random_move g pos na =
  let len = List.length run_around_list in
  let rec loop () =
    let k = List.nth run_around_list (random_int g len) in
    let mov = mov_of_k k in
    let pos1 = add_mov pos mov in
    if in_dung g pos1 then
      let (comm, na, _) = move_command3 g pos pos1 na in comm, na, None
    else loop ()
  in
  loop ()

let monster_before_flamer g mdir dist =
  let pos = rogue_pos g in
  let rec loop pos dist =
    if dist = 0 then false
    else if is_monster (dung_char g.dung pos) then true
    else loop (add_mov pos mdir) (dist - 1)
  in
  loop (add_mov pos mdir) (dist - 1)

let drop_scare_or_drop_scare_and_kill g ch na =
  match na with
    NAalone_in_room _ | NAdrop_scare_and_kill _ -> NAdrop_scare (Some ch, na)
  | _ ->
      let ds = start_drop_scare (rogue_pos g) ch in NAdrop_scare_and_kill ds

let treat_critical_situation g t na =
  (*
  if True then None else
  *)
  let pos = rogue_pos g in
  let on_scare = List.mem pos g.scare_pos in
  let monl = monsters_around g pos in
  (*
    let jeopardized_by_flame = g.attacked_by_flame && flame_risk g pos >= hp in
  let _ = trace (sprintf "*** jeo by fla %b\n" jeopardized_by_flame) in
  *)
  let jeopardized = if monl <> [] then test_jeopardized g t monl else false in
  if on_scare && g.attacked_by_flame = 0 then
    if g.confused then
      let na = t.t_next_action in
      let na = NAstring ("3.", false, na) in
      let r = Coth '3', na, None in Some r
    else None
  else if g.confused && on_scare && g.level < level_of_very_mean_monsters then
    let na = t.t_next_action in Some (Coth '.', na, None)
  else
    match
      if (g.confused || monl = [] && not on_scare) &&
         g.attacked_by_flame > 0 && g.level >= level_of_very_mean_monsters
      then
        match flaming_monster_dir g pos with
          Some (mdir, dist, _) ->
            if monster_before_flamer g mdir dist then None
            else
              begin match usable_anti_flamer_wand_in_pack g.pack with
                Some y -> Some (mdir, dist, y)
              | None -> None
              end
        | None -> None
      else None
    with
      Some (mdir, dist, (ch, _)) ->
        let na = t.t_next_action in
        let na = NAzap (mdir, ch, na, 1) in Some (Coth 'z', na, None)
    | None ->
        if g.confused && on_scare && g.attacked_by_flame > 0 &&
           g.level >= level_of_very_mean_monsters
        then
          if monl = [] then
            let na = t.t_next_action in Some (Coth '.', na, None)
          else let na = t.t_next_action in Some (random_move g pos na)
        else if g.confused && not on_scare && g.attacked = 0 then
          let na = t.t_next_action in Some (Coth '.', na, None)
        else if
          not g.confused && not g.blind && not on_scare &&
          g.level < level_of_very_mean_monsters && g.attacked_by_flame = 0 &&
          g.attacked <= 3
        then
          None
        else if
          g.confused && g.attacked > 0 &&
          (monl = [] ||
           List.exists
             (fun mov ->
                not (List.mem_assoc (add_mov pos mov) g.frozen_monsters))
             monl)
        then
          match scroll_of_hold_monsters_in_pack g.pack with
            Some (ch, _) ->
              let na = t.t_next_action in
              let uo = UOread_scroll (ch, RSread_what) in
              let na = NAuse_object (uo, na) in Some (Coth 'r', na, None)
          | None ->
              if g.level < level_of_very_mean_monsters then
                let na = t.t_next_action in Some (random_move g pos na)
              else
                match monl with
                  [mov] ->
                    begin match wand_of_magic_missile_in_pack g.pack with
                      Some (ch, _) ->
                        let mdir = mov in
                        let na = t.t_next_action in
                        let na = NAzap (mdir, ch, na, 1) in
                        Some (Coth 'z', na, None)
                    | None ->
                        match scroll_of_scare_monsters_in_pack g.pack with
                          Some (ch, _) ->
                            let na =
                              drop_scare_or_drop_scare_and_kill g ch na
                            in
                            Some (Coth 'd', na, None)
                        | None -> Some (random_move g pos na)
                    end
                | _ :: _ ->
                    begin match scroll_of_scare_monsters_in_pack g.pack with
                      Some (ch, _) ->
                        let ds = start_drop_scare pos ch in
                        let na = NAdrop_scare_and_kill ds in
                        Some (Coth 'd', na, None)
                    | None -> None
                    end
                | [] -> None
        else if g.level < level_of_very_mean_monsters then None
        else if true then None
        else
          begin
            trace t "*** treat_critical_situation:\n";
            trace t (sprintf "*** confused %b\n" g.confused);
            trace t (sprintf "*** blind %b\n" g.blind);
            trace t (sprintf "*** on scare %b\n" on_scare);
            trace t (sprintf "*** jeopardized %b\n" jeopardized);
            trace t
              (sprintf "*** lev > mean_level %b\n"
                 (g.level >= level_of_very_mean_monsters));
            trace t (sprintf "*** attacked %d\n" g.attacked);
            trace t
              (sprintf "*** attacked_by_flame %d\n" g.attacked_by_flame);
            trace t
              (sprintf "*** attacked_by_invisible %b\n"
                 g.attacked_by_invisible);
            trace t
              (sprintf "*** usable anti-flamer %b\n"
                 (usable_anti_flamer_wand_in_pack g.pack <> None));
            trace t
              (sprintf "*** scroll_of_hold_monster_in_pack %b\n"
                 (scroll_of_hold_monsters_in_pack g.pack <> None));
            failwith "blibop"
          end

let on_something g =
  let pos = rogue_pos g in
  match g.on_something_at with
    Some (pos1, _) -> pos = pos1
  | None -> false

let on_scare_monster g =
  let pos = rogue_pos g in
  match g.on_something_at with
    Some (pos1, on_scare) -> pos = pos1 && on_scare
  | None -> false

let attacked_by_flamer g pos monl =
  List.exists
    (fun mov -> is_flaming_monster g (dung_char g.dung (add_mov pos mov)))
    monl

let attack_monsters g t movl monl prev_a =
  let pos = rogue_pos g in
  let in_room = g.rogue_room_and_door <> None in
  let can_run_away =
    not g.held &&
    List.for_all
      (fun mov ->
         not (is_holding_monster g (dung_char g.dung (add_mov pos mov))))
      monl
  in
  let bad_idea_to_run_away =
    List.exists
      (fun mov ->
         g.level >= level_of_faster_monsters ||
         is_flaming_monster g (dung_char g.dung (add_mov pos mov)))
      monl
  in
  let (mmov, mch, jeopardized) = test_monsters_risk g t monl in
  match
    if in_room && List.length monl >= 2 && g.attacked > 0 then
      close_to_a_free_door g
    else None
  with
    Some mov -> move_command3 g pos (add_mov pos mov) prev_a
  | None ->
      if can_run_away && not bad_idea_to_run_away && jeopardized then
        match run_away_if_possible g in_room movl mmov with
          Some (mov, path) ->
            set_regrets g;
            let na = NArun_away (mch, path, NAnone) in
            let (comm, na) = move_command g pos mov na in comm, na, Some mov
        | None ->
            if g.sure_stairs_pos = Some pos then
              begin tempo g 1.0; Coth '>', NAnone, None end
            else if List.mem pos g.scare_pos then
              let na = NAfight (mch, false, NAnone) in
              move_command2 g pos (add_mov pos mmov) na
            else
              match
                if not (List.mem pos g.garbage) && not (on_something g) then
                  scroll_of_scare_monsters_in_pack g.pack
                else None
              with
                Some (ch, _) ->
                  if t.t_stop_at_paradise &&
                     g.ring_of_slow_digestion_on_hand <> None
                  then
                    stop_paradise t;
                  let ds = start_drop_scare pos ch in
                  let na = NAdrop_scare_and_kill ds in Coth 'd', na, None
              | None ->
                  match scroll_of_hold_monsters_in_pack g.pack with
                    Some (ch, _) ->
                      let na = NAstring ("50.", false, NAnone) in
                      let uo = UOread_scroll (ch, RSread_what) in
                      let na = NAuse_object (uo, na) in Coth 'r', na, None
                  | None ->
                      match scroll_of_teleport_in_pack g.pack with
                        Some (ch, _) ->
                          let uo = UOread_scroll (ch, RSread_what) in
                          let na = NAuse_object (uo, prev_a) in
                          Coth 'r', na, None
                      | None ->
                          let na = NAfight (mch, false, NAnone) in
                          let (comm, na) = move_command g pos mmov na in
                          comm, na, Some mmov
      else
        match
          if jeopardized && not (on_something g) &&
             (List.length monl >= 2 || not (attacked_by_flamer g pos monl))
          then
            scroll_of_scare_monsters_in_pack g.pack
          else None
        with
          Some (ch, _) ->
            if t.t_stop_at_paradise &&
               g.ring_of_slow_digestion_on_hand <> None
            then
              stop_paradise t;
            (*
                let ds = start_drop_scare (rogue_pos g) ch in
                let na = NAdrop_scare_and_kill ds in
            *)
            let na = drop_scare_or_drop_scare_and_kill g ch prev_a in
            (**)
            Coth 'd', na, None
        | None ->
            match
              if jeopardized then scroll_of_hold_monsters_in_pack g.pack
              else None
            with
              Some (ch, _) ->
                let uo = UOread_scroll (ch, RSread_what) in
                let na = NAuse_object (uo, prev_a) in Coth 'r', na, None
            | None ->
                if g.held then
                  if random_int g 2 = 0 then Coth 's', NAnone, None
                  else
                    let na = NAfight (mch, false, NAnone) in
                    match holding_monster_around g pos with
                      Some mov -> move_command3 g pos (add_mov pos mov) na
                    | None -> random_move g pos na
                else
                  match treat_critical_situation g t prev_a with
                    Some r -> r
                  | None ->
                      match
                        if g.confused &&
                           g.level >= level_of_very_mean_monsters
                        then
                          wand_of_magic_missile_in_pack g.pack
                        else None
                      with
                        Some (ch, (_, obj)) ->
                          let na = NAzap (mmov, ch, NAnone, 1) in
                          Coth 'z', na, None
                      | None ->
                          if g.confused &&
                             g.level >= level_of_very_mean_monsters
                          then
                            random_move g pos prev_a
                          else
                            begin
                              begin match t.t_next_action with
                                NArun_away (_, _, _) -> tempo g 1.0
                              | _ -> ()
                              end;
                              let na =
                                match prev_a with
                                  NAmove_in_corridor (_, _, _) ->
                                    if in_room then NAnone else prev_a
                                | na -> na
                              in
                              let na = NAfight (mch, false, na) in
                              let (comm, na) = move_command g pos mmov na in
                              comm, na, Some mmov
                            end

(* *)

let find_all g pred =
  let rec loop list row col =
    if row = g.dung.nrow then list
    else if col = g.dung.ncol then loop list (row + 1) 0
    else
      let list =
        if pred g.dung.tab.(row).[col] then {row = row; col = col} :: list
        else list
      in
      loop list row (col + 1)
  in
  loop [] 1 0

let number_of_monsters g = List.length (find_all g is_monster)

let slow_down g t =
  if t.t_breakpoint >= 0 || t.t_slow_at_level <> None ||
     t.t_slow_at_time <> None
  then
    ()
  else g.speed <- min 2.0 g.speed

let rec start_search g t graph =
  let pos = rogue_pos g in
  if nothing_to_search graph then
    let sp = stairs_pos g in
    if sp <> [] then
      let tpos = List.nth sp (random_int g (List.length sp)) in
      go_to_stairs_rec true g t graph pos tpos false
    else let graph = make_graph g true in start_search g t graph
  else
    match path_to_closest g graph pos with
      Some (path, tpos, around) ->
        begin match path with
          pos1 :: path ->
            let gp = {epos = pos1; tpos = tpos; path = path} in
            let na = NAglobal_search1 (gp, around) in
            move_command3 g pos pos1 na
        | [] ->
            let ch = 's' in
            let na = NAglobal_search2 (graph, around, 1) in Coth ch, na, None
        end
    | None ->
        let sp = stairs_pos g in
        if sp <> [] then
          let tpos = List.nth sp (random_int g (List.length sp)) in
          go_to_stairs_rec true g t graph pos tpos false
        else random_move g pos NAnone
and go_to_stairs_rec from_search g t graph pos tpos strict =
  let graph =
    if g.nb_of_reinit_search >= 10 then make_graph g true else graph
  in
  if g.nb_of_reinit_search mod 10 = 0 then reinit_graph_search g graph;
  match
    if not g.was_hallucinated && g.mon_detected && g.map_showed_since > 0 &&
       number_of_monsters g <= 20 && g.ring_of_slow_digestion_on_hand <> None
    then
      path_to_closest_gold g t pos
    else None
  with
    Some gp ->
      let na = NAseek_gold_or_monster (gp, true) in
      slow_down g t; Coth ' ', na, None
  | None ->
      let pred _ = (=) tpos in
      match path_to_closest2 g pos pred with
        Some gp -> let na = NAgo_to_stairs (gp, strict) in Coth ' ', na, None
      | None ->
          reinit_graph_search g graph;
          g.nb_of_reinit_search <- g.nb_of_reinit_search + 1;
          start_search g t graph

let go_to_stairs = go_to_stairs_rec false

(* *)

let continue_test_scrolls g t ch_arm prev_a =
  let (monl, movl) = monsters_and_moves_around g in
  match monl with
    [] ->
      begin match unidentified_scroll_in_pack g.pack with
        Some (ch_scr, _) ->
          let ws = WSscroll_read in
          let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
          let uo = UOread_scroll (ch_scr, RSread_what) in
          let na = NAuse_object (uo, na) in Coth 'r', na, None
      | None ->
          g.after_first_pack_full <- true;
          match g.worn_armor with
            Some (ch, ar) ->
              if ar.ar_protected && is_best_armor g ch || g.armor_cursed ||
                 is_good_armor g ch ar
              then
                Coth '.', NAnone, None
              else
                begin match good_armor g with
                  Some (ch, _) -> Coth 'T', NAwear (ch, 1, NAnone), None
                | None -> Coth 's', prev_a, None
                end
          | None ->
              match good_armor g with
                Some (ch, _) -> Coth 'W', NAwear (ch, 2, NAnone), None
              | None -> Coth 's', prev_a, None
      end
  | monl ->
      if not g.armor_cursed && g.worn_armor <> None &&
         not (worn_armor_protected g) && aquator_around g
      then
        let na = NAwear (' ', 1, prev_a) in Coth 'T', na, None
      else attack_monsters g t movl monl prev_a

let close_to_stairs g =
  match g.rogue_room_and_door with
    Some (room, _) ->
      let (row, col, _, _) = room in
      let pos = {row = row; col = col} in
      begin match dist_to_closest g room pos (fun ch _ -> ch = '%') with
        Some _ -> true
      | None ->
          match stairs_pos g with
            [spos] ->
              begin match current_room g spos with
                Some sroom ->
                  let rr = room_row room in
                  let rc = room_col room in
                  let sr = room_row sroom in
                  let sc = room_col sroom in
                  if sr = rr || sc = rc then
                    let pred _ = (=) spos in
                    match direct_path_excl g [] pos pred with
                      Some (path, _) ->
                        List.for_all
                          (fun pos ->
                             match current_room g pos with
                               Some proom ->
                                 sr = rr && room_row proom = rr ||
                                 sc = rc && room_col proom = sc
                             | None -> true)
                          path
                    | None -> false
                  else false
              | None -> false
              end
          | _ -> false
      end
  | None -> false

type trap_entrance =
    TE_yes
  | TE_no
  | TE_perhaps of position
  | TE_error

let trap_at_entrance g =
  match g.rogue_room_and_door with
    Some (room, Some dir) ->
      let pos = rogue_pos g in
      let mov = one_step_to_enter_room dir in
      let pos1 = add_mov pos mov in
      if dung_char g.dung pos1 = '^' then TE_yes
      else if PosMap.mem pos1 g.trail || g.map_showed_since > 0 then TE_no
      else TE_perhaps pos1
  | Some (_, None) | None -> TE_error

let exit_visited g =
  match g.rogue_room_and_door with
    Some (room, Some dir) ->
      let pos = rogue_pos g in
      let mov = one_step_to_exit_room dir in
      let pos1 = add_mov pos mov in
      PosMap.mem pos1 g.trail || g.map_showed_since > 0
  | Some (_, None) | None -> false

let connected_with_another_room g =
  match g.rogue_room_and_door with
    Some (room, _) ->
      let dl = find_doors g room in
      List.exists
        (fun (rpos, _) ->
           let paths = paths_in_corridors_from g rpos rpos in
           List.exists
             (fun (_, tpos) ->
                match pos_room_and_door g.dung tpos with
                  Some _ -> true
                | None -> false)
             paths)
        dl
  | None -> false

let conditions_for_dropping_scare g =
  let pos = rogue_pos g in
  not (on_something g) && is_at_door g pos &&
  g.ring_of_slow_digestion_on_hand <> None &&
  scroll_of_scare_monsters_in_pack g.pack <> None && close_to_stairs g &&
  trap_at_entrance g <> TE_yes && exit_visited g && g.level >= 5 &&
  not (List.mem pos g.scare_pos) && not g.paradise &&
  (g.map_showed_since > 0 || connected_with_another_room g)

let ok_for_dropping_scare g t =
  let pos = rogue_pos g in
  match trap_at_entrance g with
    TE_yes -> assert false
  | TE_no ->
      if t.t_stop_at_paradise then stop_paradise t;
      let ch =
        match scroll_of_scare_monsters_in_pack g.pack with
          Some (ch, _) -> ch
        | None -> assert false
      in
      let ds = start_drop_scare pos ch in
      let na = NAdrop_scare_and_kill ds in Coth 'd', na, None
  | TE_perhaps pos1 ->
      let na = NAcheck_no_trap (pos, pos1) in move_command3 g pos pos1 na
  | TE_error -> failwith "trap_at_entrance"

let conditions_for_exit_level g =
  g.ring_of_slow_digestion_on_hand <> None && g.map_showed_since > 0 &&
  g.mon_detected &&
  number_of_scrolls_of_scare_monsters_in_pack g.pack >=
    (if g.level < level_of_very_mean_monsters then 2 else 4) &&
  number_of_scrolls_of_magic_mapping_in_pack g.pack >=
    (if g.level < level_of_very_mean_monsters then 1 else 3) &&
  number_of_potions_of_detect_monsters_in_pack g.pack >=
    (if g.level < level_of_very_mean_monsters then 1 else 3) &&
  (g.level < 20 || enough_anti_flamer_wand_in_pack g.pack) &&
  (g.level < level_of_very_mean_monsters || main_sword_value g >= 25)

let select_less_explorated_paths_in_corridor g paths =
  let paths =
    List.map
      (fun (path, tpos) ->
         let v = try PosMap.find tpos g.trail with Not_found -> 0 in
         (v, List.length path), (path, tpos))
      paths
  in
  let paths = List.sort compare paths in
  let less_expl = fst (List.hd paths) in
  (*
  let most_expl = fst (List.hd (List.rev paths)) in
  let _ = if fst most_expl - fst less_expl > 5 then do { display_trail g; sleep 1.0 } else () in
  *)
  let paths = List.filter (fun (v, _) -> v = less_expl) paths in
  List.map snd paths

let start_move_in_corridor g t pos =
  match paths_in_corridors_from g pos pos with
    [] ->
      begin match t.t_prev_mov with
        Some mov ->
          let from = opposite_move mov in
          let around = around_pos g pos in
          let na = NAsearch_and_back (from, around, 1) in
          Coth 's', na, t.t_prev_mov
      | None ->
          if g.blind then random_move g pos NAnone
          else
            match room_of_door g pos with
              Some (_, dir) ->
                let mov = one_step_to_exit_room dir in
                let around = around_pos g pos in
                let na = NAsearch_and_back (mov, around, 1) in
                Coth 's', na, Some mov
            | None -> Coth 's', NAnone, None
      end
  | paths ->
      let paths = select_less_explorated_paths_in_corridor g paths in
      let len = List.length paths in
      (*
      let _ = trace (sprintf "*** start move in corridor\n") in
      *)
      let (path1, tpos1) = List.nth paths (random_int g len) in
      match path1 with
        pos1 :: path1 ->
          let gp = {epos = pos1; tpos = tpos1; path = path1} in
          let na = NAmove_in_corridor (pos, gp, []) in
          move_command3 g pos pos1 na
      | [] -> failwith "start_move_in_corridor 4"

let moving_monsters_at_one_move g t pos =
  let d = if g.level >= level_of_faster_monsters then 5 else 2 in
  let pred path pos1 =
    let len = List.length path in
    len > d ||
    is_monster (dung_char g.dung pos1) &&
    (is_moving g t pos1 || len = 1 && g.attacked > 0) &&
    not (List.mem_assoc pos1 g.frozen_monsters) &&
    not (List.exists (fun pos -> List.mem pos g.scare_pos) path)
  in
  match direct_path_excl g [] pos pred with
    Some (path, mpos) ->
      let len = List.length path in
      assert (len > 0);
      if len <= d then Some (List.hd path, len mod d mod 3, len, mpos)
      else None
  | None -> None

let move_back g pos1 pos2 na =
  let mov = opposite_move (move_between pos1 pos2) in
  let pos3 = add_mov pos1 mov in
  let in_room = current_room_possibly_at_door g pos1 <> None in
  if old_can_move_to g in_room pos1 pos3 then move_command2 g pos1 pos3 na
  else
    let pred path tpos =
      match List.length path with
        0 -> false
      | 1 ->
          let pred1 _ tpos = tpos = pos2 in
          begin match direct_path_excl g [] tpos pred1 with
            Some (path, _) -> List.length path = 2
          | None -> false
          end
      | _ -> true
    in
    match direct_path_excl g [pos2] pos1 pred with
      Some (path, tpos) ->
        if List.length path = 1 then
          begin assert (pos1 <> tpos); move_command2 g pos1 tpos na end
        else Coth '.', na, None
    | None -> Coth 's', na, None

let select_random_move_to g nmov pos ch =
  let rec loop list =
    function
      mov :: nmov ->
        if dung_char g.dung (add_mov pos mov) = ch then
          loop (mov :: list) nmov
        else loop list nmov
    | [] ->
        match list with
          [] -> None
        | [mov] -> Some mov
        | list ->
            let len = List.length list in
            Some (List.nth list (random_int g len))
  in
  loop [] nmov

let move_against_monster g t pos (pos1, len, dist, mpos) tml na =
  tempo g 0.1;
  let mch = dung_char g.dung mpos in
  match len with
    0 ->
      let na = NAtest_monster ((TMstay, mpos, mch) :: tml, na) in
      Coth '.', na, None
  | 1 ->
      (*
            let na = NAtest_monster [(TMforward, mpos, mch) :: tml] na in
      *)
      move_command2 g pos pos1 na
  | 2 ->
      let na = NAtest_monster ((TMbackward, mpos, mch) :: tml, na) in
      move_back g pos pos1 na
  | _ -> failwith (sprintf "move_against_monster %d" len)

let treat_moving_monsters_at_one_move g t pos na =
  match moving_monsters_at_one_move g t pos with
    Some x -> Some (move_against_monster g t pos x [] na)
  | None -> None

let treat_moving_monsters_at_one_move2 g t =
  let pos = rogue_pos g in
  let na = t.t_next_action in treat_moving_monsters_at_one_move g t pos na

let object_around g pos =
  let list_obj_ch = interesting_objects g in
  let rec loop =
    function
      k :: kl ->
        let mov = mov_of_k k in
        let tpos = add_mov pos mov in
        if in_dung g tpos then
          let ch = dung_char g.dung tpos in
          if old_can_move_to g false pos tpos && List.mem ch list_obj_ch &&
             not (List.mem tpos g.garbage)
          then
            Some mov
          else loop kl
        else loop kl
    | [] -> None
  in
  loop run_around_list

let nothing_to_search_in_dung g = g.level < 3 || g.map_showed_since > 0

let select_ahead_moves_in_corridor g pos mov all_paths =
  let paths =
    (* strictly ahead (not perpendicular *)
    List.filter
      (fun (path, tpos) ->
         match path with
           pos1 :: _ ->
             let mov1 = move_between pos pos1 in
             if mov.di = 0 then mov.dj = mov1.dj
             else if mov.dj = 0 then mov.di = mov1.di
             else
               mov1 = mov || mov1 = {mov with di = 0} ||
               mov1 = {mov with dj = 0}
         | [] -> false)
      all_paths
  in
  if paths <> [] then paths
  else
    List.filter
      (fun (path, tpos) ->
         match path with
           pos1 :: _ ->
             let mov1 = move_between pos pos1 in
             let opp = opposite_move mov in
             if mov.di = 0 then opp.dj <> mov1.dj
             else if mov.dj = 0 then opp.di <> mov1.di
             else
               not
                 (mov1 = opp || mov1 = {opp with di = 0} ||
                  mov1 = {opp with dj = 0})
         | [] -> false)
      all_paths

let continue_move_in_corridor g t ipos pos trail =
  match paths_in_corridors_from g ipos pos with
    [] ->
      if nothing_to_search_in_dung g then start_move_in_corridor g t pos
      else
        begin match t.t_prev_mov with
          Some mov ->
            let from = opposite_move mov in
            let around = around_pos g pos in
            let na = NAsearch_and_back (from, around, 1) in Coth 's', na, None
        | None -> Coth ' ', NAnone, None
        end
  | all_paths ->
      let paths = select_less_explorated_paths_in_corridor g all_paths in
      let paths =
        match t.t_prev_mov with
          Some mov -> select_ahead_moves_in_corridor g pos mov paths
        | None -> paths
      in
      let paths = if paths = [] then all_paths else paths in
      let len = List.length paths in
      let (path, tpos) = List.nth paths (random_int g len) in
      match path with
        pos1 :: path ->
          let gp = {epos = pos1; tpos = tpos; path = path} in
          let na = NAmove_in_corridor (ipos, gp, trail) in
          move_command3 g pos pos1 na
      | [] -> failwith "not impl NAmove_in_corridor 11"

let attacked_or_held g t na =
  let pos = rogue_pos g in
  let (monl, movl) = monsters_and_moves_around g in
  if monl <> [] then attack_monsters g t movl monl na
  else
    match
      if g.attacked_by_flame > 0 then flaming_monster_dir g pos else None
    with
      Some (dir, dist, _) ->
        begin match usable_anti_flamer_wand_in_pack g.pack with
          Some (ch, _) ->
            let na = NAzap (dir, ch, na, 1) in Coth 'z', na, None
        | None -> random_move g pos na
        end
    | None ->
        (* perhaps a phantom, or a dragon, or in a maze *)
        match scroll_of_hold_monsters_in_pack g.pack with
          Some (ch, _) ->
            let uo = UOread_scroll (ch, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, None
        | None -> random_move g pos na

let move_in_corridor_starting_with_move g pos mov =
  let ipos = pos in
  let pl = paths_in_corridors_from g ipos pos in
  let pl =
    List.filter
      (fun (path, _) ->
         match path with
           [] -> false
         | pos1 :: _ -> pos1 = add_mov pos mov)
      pl
  in
  if pl = [] then
    let from = opposite_move mov in
    let around = around_pos g pos in
    let na = NAsearch_and_back (from, around, 1) in Coth 's', na, None
  else
    let pl = select_less_explorated_paths_in_corridor g pl in
    let len = List.length pl in
    let (path1, tpos1) = List.nth pl (random_int g len) in
    match path1 with
      pos1 :: path1 ->
        let gp = {epos = pos1; tpos = tpos1; path = path1} in
        let na = NAmove_in_corridor (ipos, gp, []) in
        move_command3 g pos pos1 na
    | [] -> assert false

let drop_scare ds dss = {ds with ds_state = dss}
let alone_room ar ars = {ar with ar_state = ars}

let first_monster_in_path g path =
  let rec loop rev_path =
    function
      pos1 :: path1 ->
        let ch = dung_char g.dung pos1 in
        if is_monster ch then Some (rev_path, pos1, path)
        else loop (pos1 :: rev_path) path1
    | [] -> None
  in
  loop [] path

let paths_with_monsters g paths =
  List.fold_left
    (fun tpos_list (path, tpos) ->
       let rec loop len =
         function
           pos1 :: path1 ->
             if is_monster (dung_char g.dung pos1) then
               (len, (path, tpos)) :: tpos_list
             else loop (len + 1) path1
         | [] -> tpos_list
       in
       loop 0 path)
    [] paths

let go_in_corridor_and_hit g pos =
  let paths_corr = paths_in_corridors_from g pos pos in
  let paths = paths_with_monsters g paths_corr in
  match
    if paths <> [] then
      let paths = List.sort compare paths in
      let (min_len, _) = List.hd paths in
      let tpos_list = List.filter (fun (len, _) -> len = min_len) paths in
      let len = List.length tpos_list in
      let (_, path) = List.nth tpos_list (random_int g len) in Some path
    else if paths_corr <> [] then
      let len = List.length paths_corr in
      let path = List.nth paths_corr (random_int g len) in Some path
    else None
  with
    Some (path, tpos) ->
      begin match path with
        pos1 :: path ->
          let gp = {epos = pos1; tpos = tpos; path = path} in Some gp
      | [] -> assert false
      end
  | None -> None

let ds_go_in_corridor_and_hit g pos ds =
  match go_in_corridor_and_hit g pos with
    Some gp ->
      let ce =
        let ds = drop_scare ds (DSdropped 0) in
        let na = NAdrop_scare_and_kill ds in
        {ce_base = pos; ce_state = "start"; ce_gp = gp; ce_kont = na}
      in
      let na = NAgo_in_corridor_and_hit ce in Coth 'm', na, None
  | None ->
      let ds = drop_scare ds (DSdropped 0) in
      let na = NAdrop_scare_and_kill ds in Coth '.', na, None

(* *)

let is_deadend_room g pos =
  match current_room_possibly_at_door g pos with
    Some room -> let dl = find_doors g room in List.length dl = 1
  | None -> false

let rec connected_to_rooms_without_exit g pos =
  match paths_in_corridors_from g pos pos with
    [] | _ :: _ :: _ -> false
  | [path, tpos] ->
      match room_of_door g tpos with
        Some (room, _) ->
          begin match find_doors g room with
            [_] -> true
          | [d1, _; d2, _] ->
              assert (tpos = d1 || tpos = d2);
              let d = if tpos = d1 then d2 else d1 in
              connected_to_rooms_without_exit g d
          | [] -> assert false
          | _ -> false
          end
      | None ->         (*False*)true

(* *)

let room_contents g (rmin, cmin, rmax, cmax) =
  let has_mon = ref false in
  let a =
    Array.init (rmax - rmin + 1)
      (fun i ->
         let row = rmin + i in
         Array.init (cmax - cmin + 1)
           (fun j ->
              let col = cmin + j in
              let ch = g.dung.tab.(row).[col] in
              if is_monster ch then has_mon := true; ch))
  in
  a, !has_mon

let active_monsters_in_room g t (rmin, cmin, rmax, cmax) =
  let rec loop row col =
    if row > rmax then false
    else if col > cmax then loop (row + 1) cmin
    else if
      is_monster g.dung.tab.(row).[col] &&
      not (List.mem_assoc {row = row; col = col} g.frozen_monsters)
    then
      true
    else loop row (col + 1)
  in
  loop rmin cmin

let active_monsters_in_rogue_room g t =
  match g.rogue_room_and_door with
    Some (room, _) -> active_monsters_in_room g t room
  | None -> false

let healthy_enough g = health_is_maximum g

let get_rid_of_objects g =
  match object_to_be_used_in_pack_when_scaring g with
    Some (ch, (nb, obj)) ->
      begin match obj with
        Ppotion (Ipotion pk) ->
          if pk = PKdetect_mon then g.mon_detected <- true;
          Some ('q', UOquaff_potion (ch, QSquaff_what))
      | Ppotion (Upotion _) -> Some ('q', UOquaff_potion (ch, QSquaff_what))
      | Pscroll (Iscroll sk) ->
          if sk = SKmagic_map then g.map_showed_since <- g.time;
          Some ('r', UOread_scroll (ch, RSread_what))
      | Pscroll (Uscroll _) -> Some ('r', UOread_scroll (ch, RSread_what))
      | Pweapon {we_kind = WKtwo_handed_sword} ->
          Some ('w', UOwield_sword (ch, "wield what"))
      | Pfood -> Some ('e', UOeat_food (ch, "eat what"))
      | obj -> failwith (sprintf "use %s" (not_impl "pack_obj" obj))
      end
  | None ->
      match unuseful_object_in_pack_when_scaring g with
        Some (ch, _) -> Some ('t', UOthrow_unuseful_objects (ch, 1))
      | None ->
          match
            if g.pack_full then unidentified_object_not_used_in_pack g
            else None
          with
            Some (ch, _) -> Some ('t', UOthrow_unuseful_objects (ch, 1))
          | None -> None

let get_rid_of_objects_when_scaring g ds =
  match get_rid_of_objects g with
    Some (ch, uo) ->
      let ds = drop_scare ds (DSdropped 0) in
      let na = NAdrop_scare_and_kill ds in
      let na = NAuse_object (uo, na) in Some (Coth ch, na, None)
  | None -> None

let start_alone_room room dl dpos =
  let dl =
    List.map
      (fun (pos, dir) ->
         {ard_pos = pos; ard_dir = dir; ard_trip_cnt = 0;
          ard_monster_perhaps_blocked = None})
      dl
  in
  {ar_state = ARgo_and_put_scare dpos; ar_room = room; ar_doors = dl;
   ar_trip_cnt = 0}

let monster_behind_a_hidden_door g room =
  let dl = find_doors g room in
  let possible_hidden_doors_dirs =
    let rec loop list =
      function
        (_, dir) :: rest -> loop (List.filter ((<>) dir) list) rest
      | [] -> list
    in
    loop [DoorUp; DoorDown; DoorLeft; DoorRight] dl
  in
  let (rmin, cmin, rmax, cmax) = room in
  let rec loop_dir =
    function
      dir :: rest ->
        let (minr, minc, dr, dc, tr, tc) =
          match dir with
            DoorUp -> rmin, cmin, 0, 1, -1, 0
          | DoorDown -> rmax, cmin, 0, 1, 1, 0
          | DoorLeft -> rmin, cmin, 1, 0, 0, -1
          | DoorRight -> rmin, cmax, 1, 0, 0, 1
        in
        let rec loop row col =
          if row > rmax || col > cmax then loop_dir rest
          else
            let pos1 = {row = row + 2 * tr; col = col + 2 * tc} in
            if in_dung g pos1 && is_monster (dung_char g.dung pos1) then
              let tpos = {row = minr; col = minc} in
              let mov = {di = dr; dj = dc} in
              let wallmov = {di = tr; dj = tc} in Some (tpos, mov, wallmov)
            else loop (row + dr) (col + dc)
        in
        loop minr minc
    | [] -> None
  in
  loop_dir possible_hidden_doors_dirs

let check_room g room pred =
  let (rmin, cmin, rmax, cmax) = room in
  let rec loop pos =
    if pos.row = rmax + 1 then false
    else if pos.col = cmax + 1 then loop {row = pos.row + 1; col = cmin}
    else
      let mch = dung_char g.dung pos in
      if pred mch then true else loop {pos with col = pos.col + 1}
  in
  loop {row = rmin; col = cmin}

let no_monster_in_room g room = not (check_room g room is_monster)

type step_go_corr =
    SGCway_there of move * string
  | SGCack_mess
  | SGCmove_way_there of global_path * string
  | SGCattack of move * string
  | SGCchar of char * string
  | SGCpick_and_return
  | SGCjeopardized
  | SGChome

let step_go_in_corridor_and_hit g t message base gp step =
  let pos = rogue_pos g in
  match step with
    "start" ->
      let mov = move_between pos gp.epos in SGCway_there (mov, "moved")
  | "moved" ->
      if message <> "" then begin tempo g 1.0; SGCack_mess end
      else
        let monl = monsters_around g pos in
        begin match monl with
          mov :: _ -> tempo g 0.5; SGCattack (mov, "on the way")
        | [] ->
            if pos = gp.tpos then SGCchar ('s', "return to base")
            else if pos = gp.epos then
              begin
                tempo g 0.1;
                match gp.path with
                  pos1 :: path ->
                    let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                    SGCmove_way_there (gp, "start")
                | [] -> assert false
              end
            else if g.confused then
              begin tempo g 0.1; SGCchar ('3', "confused 3") end
            else if distance pos gp.epos = 1 then
              SGCmove_way_there (gp, "start")
            else failwith "DSgo_in_corridor_and_hit 24"
        end
  | "confused 3" -> SGCchar ('3', "confused 33")
  | "confused 33" -> SGCchar ('.', "confused 33.")
  | "confused 33." ->
      if message <> "" then begin tempo g 0.5; SGCack_mess end
      else if pos = base then SGChome
      else SGCpick_and_return
  | "on the way" ->
      if message <> "" then begin tempo g 0.5; SGCack_mess end
      else
        let monl = monsters_around g pos in
        begin match monl with
          mov :: _ ->
            tempo g 0.5;
            let (mov, mch, jeopardized) = test_monsters_risk g t monl in
            if jeopardized then SGCjeopardized
            else SGCattack (mov, "on the way")
        | [] ->
            let sl =
              match g.status_line with
                Some sl -> sl
              | None -> assert false
            in
            if pos = base then SGChome
            else if sl.sl_hp <= sl.sl_max_hp / 2 then SGCpick_and_return
            else
              let paths = paths_in_corridors_from g pos pos in
              let paths = paths_with_monsters g paths in
              if paths <> [] then
                let paths = List.sort compare paths in
                let (min_len, _) = List.hd paths in
                if min_len < 9 then SGCchar ('9', "4") else SGCpick_and_return
              else
                match
                  if not g.hallucinated then
                    match g.rogue_room_and_door with
                      Some (room, Some dir) -> Some dir
                    | Some (_, None) | None -> None
                  else None
                with
                  Some dir ->
                    let mov = one_step_to_enter_room dir in
                    let pos1 = add_mov pos mov in
                    if dung_char g.dung pos1 = '*' then
                      SGCway_there (mov, "return to base")
                    else SGCpick_and_return
                | None -> SGCpick_and_return
        end
  | "return to base" ->
      if message <> "" then begin tempo g 1.0; SGCack_mess end
      else SGCpick_and_return
  | "4" -> SGCchar ('.', "9.")
  | "9." ->
      if message <> "" then begin tempo g 0.5; SGCack_mess end
      else if g.confused then SGCchar ('3', "6")
      else
        let monl = monsters_around g pos in
        begin match monl with
          mov :: _ -> SGCattack (mov, "on the way")
        | [] -> SGCpick_and_return
        end
  | "6" -> SGCchar ('3', "7")
  | "7" -> SGCchar ('.', "on the way")
  | step -> failwith (sprintf "step_go_in_corridor_and_hit step '%s'" step)

let monster_perhaps_blocked_in_corridor_by_scroll g ipos pos =
  match g.rogue_room_and_door with
    Some (_, Some _) | None ->
      let paths = paths_in_corridors_from g ipos pos in
      let paths =
        List.map
          (fun (path, tpos) ->
             match pos_room_and_door g.dung tpos with
               Some (room, Some dir) ->
                 (* a scroll of scare monster can block monsters at door *)
                 let mov = one_step_to_enter_room dir in
                 let pos1 = add_mov tpos mov in
                 let pos2 = add_mov pos1 mov in path @ [pos1; pos2], tpos
             | Some (_, None) | None -> path, tpos)
          paths
      in
      let paths =
        List.sort
          (fun (p1, _) (p2, _) -> compare (List.length p1) (List.length p2))
          paths
      in
      begin match paths with
        (path, tpos) :: _ ->
          begin match first_monster_in_path g path with
            Some (rev_bef, pos1, aft) ->
              begin match rev_bef with
                pos2 :: _ ->
                  if dung_char g.dung pos2 = '?' then
                    let rev_path = pos1 :: pos2 :: rev_bef in Some rev_path
                  else None
              | [] -> None
              end
          | None -> None
          end
      | [] -> failwith "not impl: monsters_blocked_in_corridor_by_scroll 1"
      end
  | Some (_, None) -> None

let unblocking_monster g ipos perhaps_blocked =
  let pos = rogue_pos g in
  let block =
    match monster_perhaps_blocked_in_corridor_by_scroll g ipos pos with
      Some rev_path ->
        begin match perhaps_blocked with
          Some (rev_path1, nb_times) ->
            if rev_path = rev_path1 then
              if nb_times >= 3 then
                let (path, tpos, mpos) =
                  match rev_path with
                    mpos :: tpos :: rev_path -> List.rev rev_path, tpos, mpos
                  | [_] | [] -> assert false
                in
                let gp = {epos = pos; tpos = tpos; path = path} in
                Left (gp, mpos)
              else let mpb = Some (rev_path, nb_times + 1) in Right mpb
            else Right (Some (rev_path, 1))
        | None -> Right (Some (rev_path, 1))
        end
    | None -> Right None
  in
  match block with
    Left (gp, mpos) -> Some (gp, mpos), None
  | Right mpb -> None, mpb

let can_return_without_attack g mpos gp =
  let pos = rogue_pos g in
  let paths = paths_in_corridors_from g pos pos in
  (*
    let paths =
      List.filter
        (fun (path, tpos) -> do {
           match path with
           [ [] -> False
           | [pos1 :: path] -> pos1 = mpos ]
         })
        paths
    in
  *)
  List.for_all
    (fun (path, tpos) ->
       match first_monster_in_path g path with
         Some (path_bef, _, _) ->
           let len_rog = List.length gp.path in
           let len_mon = len_rog + List.length path_bef + 1 in
           len_mon >= 2 * len_rog
       | None -> true)
    paths

let is_outside_mov dir mov =
  match dir with
    DoorUp -> mov.di < 0
  | DoorDown -> mov.di > 0
  | DoorLeft -> mov.dj < 0
  | DoorRight -> mov.dj > 0

let free_space = ['.'; '#']
let find_closest_free_space g pos =
  let pred _ tpos = List.mem (dung_char g.dung tpos) free_space in
  direct_path_excl g [] pos pred

let throw_in_the_garbage g t pos ch =
  match find_closest_free_space g pos with
    Some (path, tpos) ->
      begin match path with
        pos1 :: path ->
          let gp = {epos = pos1; tpos = tpos; path = path} in
          let na = NAthrow_in_the_garbage (ch, gp, NAnone, "move") in
          move_command3 g pos pos1 na
      | [] -> assert false
      end
  | None ->
      let na = NAthrow_away (ch, "direction") in Coth 't', na, t.t_prev_mov

let manage_full_pack g t =
  match find_object_to_be_used_when_full_pack g with
    Some (ch, (nb, obj)) ->
      begin match obj with
        Pscroll sobj ->
          begin match sobj with
            Iscroll sk -> wear_armor_and_read g t ch
          | Uscroll _ ->
              let pos = rogue_pos g in
              match g.rogue_room_and_door with
                Some (room, None) ->
                  let dl = find_doors g room in
                  if dl = [] then
                    match find_random_around g '.' with
                      Some spos ->
                        let pos = rogue_pos g in
                        let gp = {epos = spos; tpos = spos; path = []} in
                        let na = NAgo_to_shelter_and_test_scrolls gp in
                        move_command3 g pos spos na
                    | None -> failwith "manage_full_pack no room around"
                  else
                    begin match
                      begin match
                        list_find (fun (dpos, _) -> List.mem dpos g.scare_pos)
                          dl
                      with
                        Some (dpos, _) -> path_excl_from_to g [] pos dpos
                      | None -> None
                      end
                    with
                      Some gp ->
                        let na = NAgo_to_shelter_and_test_scrolls gp in
                        Coth ' ', na, t.t_prev_mov
                    | None ->
                        let dl2 =
                          List.filter
                            (fun (dpos, _) ->
                               not
                                 (List.mem (dung_char g.dung dpos)
                                    list_obj_ch))
                            dl
                        in
                        if dl2 = [] then
                          let gp = {epos = pos; tpos = pos; path = []} in
                          let na = NAgo_to_shelter_and_test_scrolls gp in
                          Coth ' ', na, None
                        else
                          let dl2 =
                            List.map
                              (fun (dpos, _) -> distance pos dpos, dpos) dl2
                          in
                          let dl2 = List.sort compare dl2 in
                          let (_, dpos) = List.hd dl2 in
                          match path_excl_from_to g [] pos dpos with
                            Some gp ->
                              let na = NAgo_to_shelter_and_test_scrolls gp in
                              Coth ' ', na, t.t_prev_mov
                          | None -> assert false
                    end
              | Some (room, Some dir) ->
                  let mov = one_step_to_enter_room dir in
                  let na = NAnone in move_command3 g pos (add_mov pos mov) na
              | None ->
                  match find_random_around g '#' with
                    Some spos ->
                      begin match path_excl_from_to g [] pos spos with
                        Some gp ->
                          let na = NAgo_to_shelter_and_test_scrolls gp in
                          Coth ' ', na, t.t_prev_mov
                      | None -> Coth ' ', NAnone, t.t_prev_mov
                      end
                  | None ->
                      let gp = {epos = pos; tpos = pos; path = []} in
                      let na = NAgo_to_shelter_and_test_scrolls gp in
                      Coth ' ', na, None
          end
      | Ppotion _ ->
          let na = NAtest_potions (ch, 1) in Coth 'q', na, t.t_prev_mov
      | Pfood ->
          let uo = UOeat_food (ch, "eat what") in
          let na = NAuse_object (uo, NAnone) in Coth 'e', na, t.t_prev_mov
      | _ -> failwith (sprintf "pack full; propose %c to use" ch)
      end
  | None ->
      match find_unuseful_object_when_full_pack g with
        Some (ch, (nb, obj)) ->
          let pos = rogue_pos g in throw_in_the_garbage g t pos ch
      | None ->
          let list =
            List.filter
              (fun (ch, (_, obj)) ->
                 if ch <= 'e' then false
                 else if ch = g.main_sword then false
                 else if g.ring_of_slow_digestion_on_hand = Some ch then false
                 else
                   match obj with
                     Pscroll (Iscroll SKscare_mon) -> false
                   | Pscroll (Uscroll _) -> false
                   | _ ->
                       match g.worn_armor with
                         Some (ch1, _) -> if ch = ch1 then false else true
                       | None -> true)
              g.pack
          in
          let pos = rogue_pos g in
          let (ch, _) = List.nth list (random_int g (List.length list)) in
          throw_in_the_garbage g t pos ch

let monster_at_entrance g =
  match g.rogue_room_and_door with
    Some (room, Some dir) ->
      let pos = rogue_pos g in
      let mov = one_step_to_enter_room dir in
      is_monster (dung_char g.dung (add_mov pos mov))
  | Some (_, None) | None -> false

let is_room_border ch = ch = '-' || ch = '|'

let pick_object g t na =
  let pos = rogue_pos g in
  g.garbage <- List.filter ((<>) pos) g.garbage;
  g.scare_pos <- List.filter ((<>) pos) g.scare_pos;
  g.on_something_at <- None;
  Coth ',', na, t.t_prev_mov

let unidentified_trap_in_room g =
  match g.rogue_room_and_door with
    Some ((rmin, cmin, rmax, cmax), _) ->
      let rec loop trow tcol =
        if trow = rmax + 1 then None
        else if tcol = cmax + 1 then loop (trow + 1) cmin
        else
          let tpos = {row = trow; col = tcol} in
          if dung_char g.dung tpos = '^' then
            match
              try Some (Hashtbl.find g.traps tpos) with Not_found -> None
            with
              Some trap_opt ->
                begin match trap_opt with
                  Some tk_opt ->
                    begin match tk_opt with
                      Some tk -> loop trow (tcol + 1)
                    | None -> Some tpos
                    end
                | None ->
                    (* should not happen, but bug in rogue (happened once) *)
                    Hashtbl.add g.traps tpos (Some None);
                    Some tpos
                end
            | None -> Hashtbl.add g.traps tpos (Some None); Some tpos
          else loop trow (tcol + 1)
      in
      loop rmin cmin
  | None -> None

let ds_return_to_base g ds gp =
  let ds =
    {ds with ds_state = DSdropped 0; ds_last_corridor_kill_time = g.time}
  in
  NAreturn_to_base (gp, NAdrop_scare_and_kill ds)

let ar_return_to_base g ar gp =
  (*
    let first_door = gp.tpos = (List.hd ar.ar_doors).ard_pos in
    let ars = ARgo_to_door first_door gp in
    let ar = alone_room ar ars in
    NAalone_in_room ar
  *)
  NAreturn_to_base (gp, NAalone_in_room ar)

let return_to_base g gp =
  function
    NAdrop_scare_and_kill ds -> ds_return_to_base g ds gp
  | NAalone_in_room ar -> ar_return_to_base g ar gp
  | _ -> assert false

let map_showed_and_some_time_spent g =
  g.map_showed_since > 0 && g.time - g.map_showed_since > 50

let is_safe_to_go_to_stairs_room g t base =
  match
    match g.sure_stairs_pos with
      Some x -> [x]
    | None -> stairs_pos g
  with
    [spos] ->
      begin match common_room_with g spos with
        Some _ -> true
      | None ->
          let pos = rogue_pos g in
          let (len, rogue_first_pos) =
            match path_excl_from_to g [] pos spos with
              Some gp -> List.length gp.path, List.hd gp.path
            | None -> max_int, pos
          in
          let mposl = find_all g is_monster in
          (**)
          let gmposl = mposl in
          let rec loop n =
            function
              mpos :: mposl ->
                if n >= 20 then
                  let _ =
                    trace t
                      (sprintf
                         "**** more than 20 monsters/%d can reach rogue\n"
                         (List.length gmposl))
                  in
                  false
                else
                  let n =
                    match monster_path g mpos rogue_first_pos with
                      Some path ->
                        if List.length path / 2 <= len then n + 1 else n
                    | None -> n
                  in
                  loop n mposl
            | [] -> true
          in
          loop 0 mposl
      end
  | [] | _ :: _ -> false

let drop_scare_and_kill g t message ds =
  let transl = transl g in
  let pos = rogue_pos g in
  let base = ds.ds_base in
  if g.ring_of_slow_digestion_on_hand <> None then
    begin g.hist_dung <- []; g.paradise <- true end;
  if t.t_slow_at_level <> Some g.level && t.t_slow_at_time = None then
    g.speed <- t.t_speed;
  match ds.ds_state with
    DSdrop ch ->
      if transl.is_message_something_there message then
        if g.pack_full then manage_full_pack g t
        else
          match g.rogue_room_and_door with
            Some (_, Some _) ->
              let na = NAdrop_scare_and_kill ds in
              let na = NAstring ("d", true, na) in pick_object g t na
          | Some (_, None) | None -> random_move g pos NAnone
      else
        begin
          if g.scare_pos <> [] then g.speed <- t.t_speed;
          let (nb, obj) = List.assoc ch g.pack in
          remove_from_pack g ch nb obj;
          g.garbage <- base :: g.garbage;
          g.scare_pos <- base :: g.scare_pos;
          g.on_something_at <- Some (pos, true);
          let dss = DSdropped 0 in
          let ds = drop_scare ds dss in
          let na = NAdrop_scare_and_kill ds in Coth ch, na, None
        end
  | DSdropped ntest ->
      if message <> "" then
        begin
          tempo g 1.0;
          let ds = drop_scare ds (DSdropped 0) in
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else if
        g.confused && g.attacked_by_flame > 0 && not (health_is_maximum g) &&
        g.level >= level_of_faster_monsters && monster_at_entrance g
      then
        let na = NAdrop_scare_and_kill ds in random_move g pos na
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused && g.attacked_by_flame = 0 then
              let _ = glup g t "3" in
              match
                if pos <> base && g.attacked > 0 && not g.blind then
                  scroll_of_hold_monsters_in_pack g.pack
                else None
              with
                Some (ch, _) ->
                  let uo = UOread_scroll (ch, RSread_what) in
                  let ds = drop_scare ds (DSdropped 0) in
                  let na = NAdrop_scare_and_kill ds in
                  let na = NAuse_object (uo, na) in Coth 'r', na, None
              | None ->
                  let ds = drop_scare ds (DSdropped 0) in
                  let na = NAdrop_scare_and_kill ds in
                  let na = NAstring ("3.", false, na) in Coth '3', na, None
            else if not g.confused && pos <> base then
              let (monl, movl) = monsters_and_moves_around g in
              if g.held then
                let na = NAdrop_scare_and_kill ds in
                attack_monsters g t movl monl na
              else if t.t_on_stairs || is_trap g pos then
                if monl <> [] then
                  let na = NAdrop_scare_and_kill ds in
                  attack_monsters g t movl monl na
                else
                  let gp = old_path_excl_from_to g [] pos base 1 in
                  let na = ds_return_to_base g ds gp in Coth ' ', na, None
              else
                let gp = old_path_excl_from_to g [] pos base 2 in
                let na = ds_return_to_base g ds gp in pick_object g t na
            else
              let monl = monsters_around g pos in
              if g.attacked_by_flame > 0 &&
                 (g.level >= level_of_very_mean_monsters ||
                  worn_armor_value g < 30) &&
                 flame_risk g t >= health_points g
              then
                match g.rogue_room_and_door with
                  Some (room, Some dir) ->
                    let mov = one_step_to_exit_room dir in
                    let na = NAdrop_scare_and_kill ds in
                    let na = NArestore_health na in
                    move_command2 g pos (add_mov pos mov) na
                | Some (_, None) | None ->
                    let (monl, movl) = monsters_and_moves_around g in
                    if monl <> [] then
                      let na = NAdrop_scare_and_kill ds in
                      attack_monsters g t movl monl na
                    else
                      match flaming_monster_dir g pos with
                        Some (mdir, dist, pos1) ->
                          begin match
                            if not (monster_before_flamer g mdir dist) then
                              usable_anti_flamer_wand_in_pack g.pack
                            else None
                          with
                            Some (ch, _) ->
                              let na = NAdrop_scare_and_kill ds in
                              let na = NAzap (mdir, ch, na, 1) in
                              Coth 'z', na, t.t_prev_mov
                          | None ->
                              match path_to g pos pos1 with
                                Some gp ->
                                  let ds =
                                    drop_scare ds (DSgo_and_hit (gp, 1))
                                  in
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth 'm', na, None
                              | None -> failwith "flames 4"
                          end
                      | None -> failwith "flames 5"
              else if monl <> [] && not g.confused then
                begin
                  tempo g 0.1;
                  let len = List.length monl in
                  let mov = List.nth monl (random_int g len) in
                  let ch = basic_command_of_move mov in
                  let ds = drop_scare ds (DSforce_kill ch) in
                  let ds =
                    match g.rogue_room_and_door with
                      Some (_, Some dir) ->
                        if mov = one_step_to_exit_room dir then
                          {ds with ds_monster_perhaps_blocked = None}
                        else ds
                    | Some (_, None) | None -> ds
                  in
                  let ds_opt =
                    match
                      if health_is_maximum g then
                        match g.rogue_room_and_door with
                          Some (_, Some dir) -> Some dir
                        | Some (_, None) | None -> None
                      else None
                    with
                      Some dir ->
                        if mov = one_step_to_exit_room dir then
                          Some {ds with ds_last_corridor_kill_time = g.time}
                        else if
                          len = 1 && ds.ds_last_corridor_kill_time > 0 &&
                          g.time > ds.ds_last_corridor_kill_time + 100
                        then
                          None
                        else Some ds
                    | None -> Some ds
                  in
                  match ds_opt with
                    Some ds ->
                      let ds = {ds with ds_outside_tested = false} in
                      let na = NAdrop_scare_and_kill ds in Coth 'F', na, None
                  | None ->
                      (* perhaps a 'scare monsters' scroll in the corridor *)
                      ds_go_in_corridor_and_hit g pos ds
                end
              else if
                g.attacked_by_flame > 0 &&
                (g.level >= level_of_very_mean_monsters ||
                 worn_armor_value g < 30)
              then
                match flaming_monster_dir g pos with
                  Some (dir, dist, pos1) ->
                    if dist = 1 && can_move_to g (add_mov pos dir) then
                      let ds = drop_scare ds (DSdropped ntest) in
                      let na = NAdrop_scare_and_kill ds in Cmov dir, na, None
                    else
                      begin match
                        if is_deadend_room g pos then
                          match g.rogue_room_and_door with
                            Some (room, Some dir) -> Some (room, dir)
                          | Some (_, None) | None -> None
                        else None
                      with
                        Some (room, dir) ->
                          let mov =
                            if inside_room room pos1 then
                              one_step_to_exit_room dir
                            else one_step_to_enter_room dir
                          in
                          let na = NAdrop_scare_and_kill ds in
                          let na = NArestore_health na in
                          move_command2 g pos (add_mov pos mov) na
                      | None ->
                          match usable_anti_flamer_wand_in_pack g.pack with
                            Some (ch, _) ->
                              let ds = drop_scare ds (DSdropped ntest) in
                              let na = NAdrop_scare_and_kill ds in
                              let na = NAzap (dir, ch, na, 1) in
                              Coth 'z', na, None
                          | None ->
                              match g.rogue_room_and_door with
                                Some (room, Some _) ->
                                  let (rc, has_mon) = room_contents g room in
                                  let ds =
                                    drop_scare ds
                                      (DStest_move (ntest + 1, rc))
                                  in
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth '.', na, None
                              | Some (_, None) | None ->
                                  let ds = drop_scare ds (DSdropped ntest) in
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth '.', na, None
                      end
                | None ->
                    if g.confused then
                      let ds = drop_scare ds (DSdropped 0) in
                      let na = NAdrop_scare_and_kill ds in
                      let na = NAstring ("3.", false, na) in
                      Coth '3', na, None
                    else
                      let ds = drop_scare ds (DSdropped ntest) in
                      let na = NAdrop_scare_and_kill ds in Coth '.', na, None
              else if
                not (is_at_door g pos) &&
                scroll_of_scare_monsters_in_pack g.pack <> None &&
                g.ring_of_slow_digestion_on_hand <> None &&
                health_is_maximum g && g.mon_detected
              then
                let na = NAnone in g.paradise <- false; pick_object g t na
              else if not g.armor_cursed && not (wearing_good_armor g) then
                match good_armor g with
                  Some (ch, _) ->
                    let ds = drop_scare ds (DSdropped ntest) in
                    let na = NAdrop_scare_and_kill ds in
                    let na = NAwear (ch, 1, na) in Coth 'T', na, None
                | None -> assert false
              else if g.level < 5 || conditions_for_exit_level g then
                if healthy_enough g && g.level >= 5 then
                  match
                    match stairs_pos g with
                      [spos] -> Some spos
                    | [] -> None
                    | spos_list ->
                        let rec loop dist closest_spos =
                          function
                            spos :: spos_list ->
                              let sdist =
                                let pred _ = (=) spos in
                                match direct_path_excl g [] pos pred with
                                  Some (path, _) -> List.length path
                                | None -> max_int
                              in
                              let (dist, closest_spos) =
                                if sdist < dist then sdist, Some spos
                                else dist, closest_spos
                              in
                              loop dist closest_spos spos_list
                          | [] -> closest_spos
                        in
                        loop max_int None spos_list
                  with
                    Some spos ->
                      slow_down g t;
                      begin match get_rid_of_objects_when_scaring g ds with
                        Some r -> r
                      | None ->
                          let graph = make_graph g false in
                          go_to_stairs g t graph pos spos true
                      end
                  | None -> Coth ' ', NAnone, t.t_prev_mov
                else
                  let ds = drop_scare ds (DSdropped ntest) in
                  let na = NAdrop_scare_and_kill ds in Coth '.', na, None
              else
                match
                  match
                    if g.ring_of_slow_digestion_on_hand <> None &&
                       map_showed_and_some_time_spent g &&
                       g.level >= level_of_very_mean_monsters &&
                       g.rogue_room_and_door <> None
                    then
                      let _ = stairs_pos g in g.sure_stairs_pos
                    else None
                  with
                    Some spos ->
                      begin match common_room_with g spos with
                        Some room ->
                          if snd (room_contents g room) then None
                          else
                            let dl = find_doors g room in
                            let nb_doors = List.length dl in
                            if nb_doors > 1 &&
                               number_of_scrolls_of_scare_monsters_in_pack
                                 g.pack >=
                                 nb_doors - 1 &&
                               List.for_all
                                 (fun (dpos, ddir) ->
                                    let mov = one_step_to_enter_room ddir in
                                    let ch =
                                      dung_char g.dung (add_mov dpos mov)
                                    in
                                    List.mem ch
                                      ('.' :: ',' :: '%' :: list_obj_ch))
                                 dl
                            then
                              Some (room, dl)
                            else None
                      | None -> None
                      end
                  | None -> None
                with
                  Some (room, dl) ->
                    (* change to the action 'alone in room' by completing scare
                       monsters in other doors. *)
                    begin match
                      list_find
                        (fun (dpos, _) -> not (List.mem dpos g.scare_pos)) dl
                    with
                      Some (dpos, _) ->
                        begin match path_in_room_to2 g room [] pos dpos with
                          Some (pos1 :: path) ->
                            let ar = start_alone_room room dl dpos in
                            let na = NAalone_in_room ar in
                            move_command3 g pos pos1 na
                        | Some [] | None -> assert false
                        end
                    | None ->
                        let ar = start_alone_room room dl pos in
                        let ar =
                          alone_room ar (ARcommand "dropped_last_scare")
                        in
                        let na = NAalone_in_room ar in
                        Coth ' ', na, t.t_prev_mov
                    end
                | None ->
                    match
                      match
                        if not (is_big_room g) &&
                           g.ring_of_slow_digestion_on_hand <> None &&
                           map_showed_and_some_time_spent g &&
                           g.level >= level_of_very_mean_monsters &&
                           g.mon_detected &&
                           not (active_monsters_in_rogue_room g t) &&
                           is_safe_to_go_to_stairs_room g t base
                        then
                          let _ = stairs_pos g in g.sure_stairs_pos
                        else None
                      with
                        Some spos ->
                          let r =
                            match current_room g spos with
                              Some room ->
                                begin match g.rogue_room_and_door with
                                  Some (room1, Some _) ->
                                    if room1 <> room then Some room else None
                                | Some (_, None) | None -> Some room
                                end
                            | None -> None
                          in
                          begin match r with
                            Some room ->
                              let dl = find_doors g room in
                              let nb_doors = List.length dl in
                              if number_of_scrolls_of_scare_monsters_in_pack
                                   g.pack >=
                                   nb_doors &&
                                 List.for_all
                                   (fun (dpos, ddir) ->
                                      let mov = one_step_to_enter_room ddir in
                                      let ch =
                                        dung_char g.dung (add_mov dpos mov)
                                      in
                                      (**)
                                      is_monster ch ||
                                      List.mem ch
                                        (' ' :: '.' :: ',' :: '%' ::
                                         list_obj_ch))
                                   dl
                              then
                                Some (room, dl)
                              else None
                          | None -> None
                          end
                      | None -> None
                    with
                      Some (room, dl) ->
                        (* remove the scare monsters and quit the current position to go
                           to the room with the stairs, (hopefully without problem) to later
                           use the action 'alone in room' *)
                        let paths =
                          List.map
                            (fun (dpos, _) ->
                               match
                                 direct_path_excl g [] pos
                                   (fun _ pos -> pos = dpos)
                               with
                                 Some (path, _) ->
                                   List.length path, path, dpos
                               | None -> assert false)
                            dl
                        in
                        let paths = List.sort compare paths in
                        let (_, path, tpos) = List.hd paths in
                        let gp = {epos = pos; tpos = tpos; path = path} in
                        let na = NAgo_to gp in
                        g.paradise <- false; pick_object g t na
                    | None ->
                        match
                          match g.rogue_room_and_door with
                            Some (room, Some _) -> Some room
                          | Some (_, None) | None -> None
                        with
                          Some room ->
                            let (rc, has_mon) = room_contents g room in
                            begin match
                              if not has_mon then
                                monster_behind_a_hidden_door g room
                              else None
                            with
                              Some (tpos, mov, wallmov) ->
                                let tpos =
                                  let rec loop tpos =
                                    if dung_char g.dung tpos = '^' then
                                      loop (add_mov tpos mov)
                                    else tpos
                                  in
                                  loop tpos
                                in
                                let gp =
                                  old_path_excl_from_to g [] pos tpos 3
                                in
                                let ds =
                                  drop_scare ds
                                    (DSfree_monster (gp, mov, wallmov, 0))
                                in
                                let na = NAdrop_scare_and_kill ds in
                                Coth ' ', na, None
                            | None ->
                                if has_mon && g.attacked_by_flame = 0 &&
                                   g.ring_of_slow_digestion_on_hand <> None &&
                                   not (health_is_maximum g)
                                then
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth '.', na, t.t_prev_mov
                                else if
                                  has_mon ||
                                  g.ring_of_slow_digestion_on_hand <> None
                                then
                                  let ntest = ntest + 1 in
                                  let ds =
                                    drop_scare ds (DStest_move (ntest, rc))
                                  in
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth '.', na, None
                                else Coth ' ', NAnone, t.t_prev_mov
                            end
                        | None ->
                            match
                              if g.ring_of_slow_digestion_on_hand <> None then
                                match g.rogue_room_and_door with
                                  Some (room, None) -> Some room
                                | Some (_, Some _) | None -> None
                              else None
                            with
                              Some room ->
                                begin match
                                  get_rid_of_objects_when_scaring g ds
                                with
                                  Some r -> r
                                | None ->
                                    match
                                      dist_to_closest g room pos
                                        (fun ch mov ->
                                           let tpos = add_mov pos mov in
                                           is_monster ch &&
                                           not (is_freezing_monster g ch) &&
                                           not (is_moving g t tpos) ||
                                           List.mem ch list_obj_ch &&
                                           not (List.mem tpos g.garbage))
                                    with
                                      Some dist ->
                                        let tpos = add_mov pos dist in
                                        let gp = old_path_to g pos tpos in
                                        let ds =
                                          drop_scare ds (DSseek_object gp)
                                        in
                                        let na = NAdrop_scare_and_kill ds in
                                        move_command2 g pos gp.epos na
                                    | None ->
                                        if is_big_room g then
                                          match stairs_pos g with
                                            [spos] ->
                                              begin match
                                                path_excl_from_to g [] pos
                                                  spos
                                              with
                                                Some gp ->
                                                  let na =
                                                    NAgo_to_stairs (gp, true)
                                                  in
                                                  Coth ' ', na, t.t_prev_mov
                                              | None -> failwith "ddd"
                                              end
                                          | [] | _ :: _ -> failwith "ccc"
                                        else if
                                          scroll_of_scare_monsters_in_pack
                                            g.pack <>
                                            None
                                        then
                                          Coth ' ', NAnone, t.t_prev_mov
                                        else if
                                          connected_with_another_room g
                                        then
                                          let na = NAdrop_scare_and_kill ds in
                                          Coth '.', na, t.t_prev_mov
                                        else
                                          let dposl =
                                            List.map fst (find_doors g room)
                                          in
                                          match dposl with
                                            [] -> failwith "shit alors 1"
                                          | dpos :: dposl ->
                                              let gp =
                                                old_path_excl_from_to g [] pos
                                                  dpos 47
                                              in
                                              let fa =
                                                {fa_state = "go to door";
                                                 fa_gp = gp; fa_doors = dposl;
                                                 fa_base = base}
                                              in
                                              let na =
                                                NAfind_another_room_and_return
                                                  fa
                                              in
                                              Coth ' ', na, t.t_prev_mov
                                end
                            | None ->
                                if g.ring_of_slow_digestion_on_hand <> None
                                then
                                  if ds.ds_nb_attempt < 5 &&
                                     (ds.ds_nb_killed_in_corr = 0 ||
                                      ds.ds_nb_killed_in_corr > 3)
                                  then
                                    let ds = drop_scare ds DScheck_monsters in
                                    let ds =
                                      let n = ds.ds_nb_attempt + 1 in
                                      {ds with ds_nb_killed_in_corr = 0;
                                       ds_nb_attempt = n}
                                    in
                                    let na = NAdrop_scare_and_kill ds in
                                    let na = NAstring ("99.", false, na) in
                                    Coth '9', na, None
                                  else
                                    let ds =
                                      {ds with ds_nb_killed_in_corr = 0;
                                       ds_nb_attempt = 0}
                                    in
                                    ds_go_in_corridor_and_hit g pos ds
                                else
                                  match t.t_prev_mov with
                                    Some mov ->
                                      let mov = opposite_move mov in
                                      move_in_corridor_starting_with_move g
                                        pos mov
                                  | None -> start_move_in_corridor g t pos
        end
  | DSfree_monster (gp, mov, wallmov, ntimes) ->
      if message <> "" then
        begin
          tempo g 0.5;
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else
        begin match moving_monsters_at_one_move g t pos with
          Some x ->
            let gp = old_path_excl_from_to g [] pos base 22 in
            let na = ds_return_to_base g ds gp in
            move_against_monster g t pos x [] na
        | None ->
            if pos = gp.tpos then
              let hpos = add_mov pos wallmov in
              if is_room_border (dung_char g.dung hpos) then
                if ntimes >= 5 then
                  let tpos = add_mov pos mov in
                  let tpos =
                    let rec loop tpos =
                      if dung_char g.dung tpos = '^' then
                        loop (add_mov tpos mov)
                      else tpos
                    in
                    loop tpos
                  in
                  let continue =
                    match g.rogue_room_and_door with
                      Some ((_, rmax, _, cmax), _) ->
                        tpos.row <= rmax || tpos.col <= cmax
                    | None -> false
                  in
                  if continue then
                    let gp = old_path_excl_from_to g [] pos tpos 22 in
                    let ds =
                      drop_scare ds (DSfree_monster (gp, mov, wallmov, 0))
                    in
                    let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
                  else
                    let gp = old_path_excl_from_to g [] pos base 21 in
                    let na = ds_return_to_base g ds gp in Coth ' ', na, None
                else
                  let ntimes = ntimes + 1 in
                  let ds =
                    drop_scare ds (DSfree_monster (gp, mov, wallmov, ntimes))
                  in
                  let na = NAdrop_scare_and_kill ds in Coth 's', na, None
              else
                let gp = old_path_excl_from_to g [] pos base 4 in
                let na = ds_return_to_base g ds gp in Coth ' ', na, None
            else if pos = gp.epos then
              begin
                tempo g 0.1;
                match gp.path with
                  epos :: path ->
                    let gp = {epos = epos; tpos = gp.tpos; path = path} in
                    let ds =
                      drop_scare ds
                        (DSfree_monster (gp, mov, wallmov, ntimes))
                    in
                    let na = NAdrop_scare_and_kill ds in
                    move_command3 g pos epos na
                | [] -> failwith "not impl DSfree_monster 2"
              end
            else
              match path_excl_from_to g [] pos gp.tpos with
                Some gp ->
                  let ds =
                    drop_scare ds (DSfree_monster (gp, mov, wallmov, ntimes))
                  in
                  let na = NAdrop_scare_and_kill ds in
                  Coth ' ', na, t.t_prev_mov
              | None -> Coth ' ', NAnone, t.t_prev_mov
        end
  | DSforce_kill ch ->
      let ds = drop_scare ds (DSdropped 0) in
      let na = NAdrop_scare_and_kill ds in Coth ch, na, None
  | DScheck_monsters ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 0.5;
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if pos <> base then
              begin
                tempo g 0.1;
                if g.confused then
                  if g.attacked = 0 then
                    let na = NAdrop_scare_and_kill ds in Coth '.', na, None
                  else
                    let monl = monsters_around g pos in
                    match monl with
                      [mov] ->
                        let pos1 = add_mov pos mov in
                        let na = NAdrop_scare_and_kill ds in
                        move_command3 g pos pos1 na
                    | [] ->
                        let na = NAdrop_scare_and_kill ds in
                        random_move g pos na
                    | _ -> failwith "DScheck_monsters not at base 3"
                else
                  let gp = old_path_excl_from_to g [] pos base 5 in
                  let na = ds_return_to_base g ds gp in Coth ' ', na, None
              end
            else if g.confused && g.attacked_by_flame = 0 then
              let _ = failwith "41" in
              let ds = drop_scare ds DScheck_monsters in
              let na = NAdrop_scare_and_kill ds in
              let na = NAstring ("3.", false, na) in Coth '3', na, None
            else if g.confused && g.attacked_by_flame > 0 then
              let _ = failwith "42" in
              match g.rogue_room_and_door with
                Some (room, Some dir) ->
                  let mov = one_step_to_enter_room dir in
                  let pos1 = add_mov pos mov in
                  if is_monster (dung_char g.dung pos1) then
                    let ds = drop_scare ds DScheck_monsters in
                    let na = NAdrop_scare_and_kill ds in
                    move_command3 g pos pos1 na
                  else if
                    no_monster_in_room g room &&
                    is_monster
                      (dung_char g.dung
                         (add_mov pos (one_step_to_exit_room dir)))
                  then
                    let pos1 = add_mov pos (one_step_to_exit_room dir) in
                    let ds = drop_scare ds DScheck_monsters in
                    let na = NAdrop_scare_and_kill ds in
                    move_command3 g pos pos1 na
                  else let na = NAdrop_scare_and_kill ds in Coth '.', na, None
              | Some (_, None) | None ->
                  failwith "DScheck_monsters confused flamed 4"
            else
              match
                match g.rogue_room_and_door with
                  Some (room, None) ->
                    let rec loop =
                      function
                        k :: kl ->
                          let mov = mov_of_k k in
                          let tpos = add_mov pos mov in
                          if is_monster (dung_char g.dung tpos) &&
                             can_move_to g tpos
                          then
                            Some mov
                          else loop kl
                      | [] -> None
                    in
                    loop run_around_list
                | Some (room, Some dir) ->
                    let mov1 = one_step_to_enter_room dir in
                    let mov2 = one_step_to_exit_room dir in
                    if is_monster (dung_char g.dung (add_mov pos mov1)) then
                      Some mov1
                    else if
                      is_monster (dung_char g.dung (add_mov pos mov2))
                    then
                      Some mov2
                    else None
                | None -> None
              with
                Some mov ->
                  let tpos = add_mov pos mov in
                  let ds = drop_scare ds (DSdropped 0) in
                  let na = NAdrop_scare_and_kill ds in
                  move_command3 g pos tpos na
              | None ->
                  if not (health_is_maximum g) then
                    let ds = drop_scare ds (DSdropped 0) in
                    let na = NAdrop_scare_and_kill ds in
                    Coth ' ', na, t.t_prev_mov
                  else
                    match g.rogue_room_and_door with
                      Some (room, _) ->
                        begin match
                          dist_to_closest g room pos
                            (fun ch _ ->
                               is_monster ch &&
                               not (is_freezing_monster g ch))
                        with
                          Some dist ->
                            let tpos = add_mov pos dist in
                            let (pos1, path) =
                              match path_in_room_to2 g room [] pos tpos with
                                Some (pos1 :: path) -> pos1, path
                              | Some [] | None -> assert false
                            in
                            let gp =
                              {epos = pos1; tpos = tpos; path = path}
                            in
                            let ds = drop_scare ds (DSgo_and_hit (gp, 1)) in
                            let na = NAdrop_scare_and_kill ds in
                            Coth 'm', na, None
                        | None ->
                            if is_at_door g base then
                              ds_go_in_corridor_and_hit g pos ds
                            else Coth ' ', NAnone, t.t_prev_mov
                        end
                    | None ->
                        match get_rid_of_objects_when_scaring g ds with
                          Some r -> r
                        | None ->
                            let monl = monsters_around g base in
                            if monl <> [] then
                              let mov = List.hd monl in
                              let ch = basic_command_of_move mov in
                              let ds =
                                {ds with ds_monster_perhaps_blocked = None}
                              in
                              let ds = drop_scare ds (DSforce_kill ch) in
                              let nb_killed_in_corr =
                                ds.ds_nb_killed_in_corr + 1
                              in
                              let ds =
                                {ds with ds_nb_killed_in_corr =
                                  nb_killed_in_corr}
                              in
                              let ds = {ds with ds_outside_tested = false} in
                              let na = NAdrop_scare_and_kill ds in
                              Coth 'F', na, None
                            else
                              let ds = drop_scare ds (DSdropped 0) in
                              let na = NAdrop_scare_and_kill ds in
                              Coth '.', na, None
        end
  | DSgive_them_chance (stime, step) ->
      begin match step with
        1 ->
          if stime <> "" then
            let len = String.length stime in
            let ch = stime.[0] in
            let stime = String.sub stime 1 (len - 1) in
            let ds = drop_scare ds (DSgive_them_chance (stime, 1)) in
            let na = NAdrop_scare_and_kill ds in Coth ch, na, None
          else if message <> "" then
            begin
              tempo g 1.0;
              let ds = drop_scare ds (DSgive_them_chance ("", 1)) in
              let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
            end
          else
            let ds = drop_scare ds (DSgive_them_chance ("", 2)) in
            let na = NAdrop_scare_and_kill ds in Coth '.', na, None
      | 2 ->
          let monl = monsters_around g pos in
          if monl <> [] then
            let ds = drop_scare ds (DSdropped 0) in
            let na = NAdrop_scare_and_kill ds in Coth '.', na, None
          else
            begin match g.rogue_room_and_door with
              Some (room, _) ->
                begin match
                  dist_to_closest g room pos
                    (fun ch mov ->
                       is_monster ch && not (is_freezing_monster g ch))
                with
                  Some dist ->
                    let tpos = add_mov pos dist in
                    let gp = old_path_to g pos tpos in
                    let ds = drop_scare ds (DSgo_and_hit (gp, 1)) in
                    let na = NAdrop_scare_and_kill ds in Coth 'm', na, None
                | None ->
                    let ds = drop_scare ds (DSdropped 0) in
                    let na = NAdrop_scare_and_kill ds in Coth '.', na, None
                end
            | None -> assert false
            end
      | step -> failwith (sprintf "DSgive_them_chance step %d" step)
      end
  | DSgo_and_hit (gp, step) ->
      begin match step with
        1 ->
          let ds = drop_scare ds (DSgo_and_hit (gp, 2)) in
          let na = NAdrop_scare_and_kill ds in
          let mov = move_between pos gp.epos in Cmov mov, na, Some mov
      | 2 ->
          if message <> "" then
            begin
              tempo g 0.5;
              let ds = drop_scare ds (DSgo_and_hit (gp, 2)) in
              let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
            end
          else
            begin match treat_critical_situation g t t.t_next_action with
              Some r -> r
            | None ->
                if g.confused then
                  if g.attacked > 0 then
                    match scroll_of_hold_monsters_in_pack g.pack with
                      Some (ch, _) ->
                        let uo = UOread_scroll (ch, RSread_what) in
                        let ds = drop_scare ds (DSdropped 0) in
                        let na = NAdrop_scare_and_kill ds in
                        let na = NAuse_object (uo, na) in Coth 'r', na, None
                    | None -> failwith "confused and attacked"
                  else if pos = base then
                    let ds = drop_scare ds (DSdropped 0) in
                    let na = NAdrop_scare_and_kill ds in
                    let na = NAstring ("3.", false, na) in Coth '3', na, None
                  else
                    let gp = old_path_excl_from_to g [] pos base 6 in
                    let na = ds_return_to_base g ds gp in
                    let na = NAstring ("3.", false, na) in Coth '3', na, None
                else
                  match treat_moving_monsters_at_one_move2 g t with
                    Some x -> x
                  | None ->
                      if pos = base then
                        let ds = drop_scare ds (DSdropped 0) in
                        let na = NAdrop_scare_and_kill ds in
                        Coth ' ', na, t.t_prev_mov
                      else
                        let monl = monsters_around g pos in
                        match monl with
                          mov :: _ ->
                            let mov =
                              match holding_monster_around g pos with
                                Some mov -> mov
                              | None ->
                                  match flaming_monster_around g pos with
                                    Some mov -> mov
                                  | None -> mov
                            in
                            let mpos = add_mov pos mov in
                            let ds = drop_scare ds (DSgo_and_hit (gp, 3)) in
                            let na = NAdrop_scare_and_kill ds in
                            move_command3 g pos mpos na
                        | [] ->
                            if pos = gp.tpos then
                              let gp =
                                old_path_excl_from_to g [] pos base 7
                              in
                              let na = ds_return_to_base g ds gp in
                              pick_object g t na
                            else if pos = gp.epos then
                              match gp.path with
                                pos1 :: path ->
                                  let gp =
                                    {epos = pos1; tpos = gp.tpos; path = path}
                                  in
                                  let ds =
                                    drop_scare ds (DSgo_and_hit (gp, 1))
                                  in
                                  let na = NAdrop_scare_and_kill ds in
                                  Coth 'm', na, None
                              | [] -> assert false
                            else
                              match path_excl_from_to g [] pos base with
                                Some gp ->
                                  let na = ds_return_to_base g ds gp in
                                  pick_object g t na
                              | None ->
                                  (* lost... probably due to teleport trap *)
                                  Coth ' ', NAnone, None
            end
      | 3 ->
          if message <> "" then
            begin
              tempo g 0.5;
              let ds = drop_scare ds (DSgo_and_hit (gp, 3)) in
              let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
            end
          else
            let monl = monsters_around g pos in
            begin match monl with
              mov :: _ ->
                tempo g 0.5;
                let mov_opt =
                  if on_scare_monster g then Some mov
                  else
                    match holding_monster_around g pos with
                      Some mov -> Some mov
                    | None ->
                        let (mmov, mch, jeopardized) =
                          test_monsters_risk g t monl
                        in
                        if jeopardized then None else Some mmov
                in
                begin match mov_opt with
                  Some mov ->
                    let ds = drop_scare ds (DSgo_and_hit (gp, 3)) in
                    let na = NAdrop_scare_and_kill ds in
                    move_command3 g pos (add_mov pos mov) na
                | None ->
                    let gp = old_path_excl_from_to g [] pos base 9 in
                    let na = ds_return_to_base g ds gp in pick_object g t na
                end
            | [] ->
                if pos = base then
                  let ds = drop_scare ds (DSdropped 0) in
                  let na = NAdrop_scare_and_kill ds in Coth '.', na, None
                else
                  let gp = old_path_excl_from_to g [] pos base 10 in
                  let na = ds_return_to_base g ds gp in pick_object g t na
            end
      | step -> failwith (sprintf "DSgo_and_hit step %d" step)
      end
  | DSseek_object gp ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 0.5;
          let ds = drop_scare ds (DSseek_object gp) in
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else
        begin match holding_monster_around g pos with
          Some mov ->
            tempo g 0.5;
            let ds = drop_scare ds (DSseek_object gp) in
            let na = NAdrop_scare_and_kill ds in
            move_command2 g pos (add_mov pos mov) na
        | None ->
            match
              if pos <> base && g.attacked > 0 then
                path_excl_from_to g [] pos base
              else None
            with
              Some gp ->
                let na = ds_return_to_base g ds gp in Coth ' ', na, None
            | None ->
                match treat_moving_monsters_at_one_move2 g t with
                  Some x -> x
                | None ->
                    if g.pack_full || pos = gp.tpos then
                      let gp = old_path_excl_from_to g [] pos base 17 in
                      let na = ds_return_to_base g ds gp in Coth ' ', na, None
                    else if pos = gp.epos then
                      begin
                        tempo g 0.1;
                        match gp.path with
                          pos1 :: path ->
                            let gp =
                              {epos = pos1; tpos = gp.tpos; path = path}
                            in
                            let ds = drop_scare ds (DSseek_object gp) in
                            let na = NAdrop_scare_and_kill ds in
                            move_command2 g pos pos1 na
                        | [] -> assert false
                      end
                    else if
                      distance pos gp.epos = 1 && can_move_to g gp.epos
                    then
                      begin
                        tempo g 0.1;
                        let ds = drop_scare ds (DSseek_object gp) in
                        let na = NAdrop_scare_and_kill ds in
                        move_command2 g pos gp.epos na
                      end
                    else if pos = base then
                      let ds = drop_scare ds (DSdropped 0) in
                      let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
                    else
                      match path_excl_from_to g [] pos base with
                        Some gp ->
                          let na = ds_return_to_base g ds gp in
                          pick_object g t na
                      | None -> Coth ' ', NAnone, None
        end
  | DStest_move (ntest, prev_rc) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let ds = drop_scare ds (DStest_move (ntest, prev_rc)) in
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused then
              if pos <> base then
                let na = NAdrop_scare_and_kill ds in random_move g pos na
              else if
                g.level > level_of_very_mean_monsters &&
                g.attacked_by_flame > 0
              then
                let _ = glup g t "7" in
                match scroll_of_hold_monsters_in_pack g.pack with
                  Some (ch, _) ->
                    let dss = DSdropped 0 in
                    let ds = drop_scare ds dss in
                    let na = NAdrop_scare_and_kill ds in
                    let uo = UOread_scroll (ch, RSread_what) in
                    let na = NAuse_object (uo, na) in Coth 'r', na, None
                | None -> failwith "c'est la merde"
              else
                let ds = drop_scare ds (DStest_move (ntest, prev_rc)) in
                let na = NAdrop_scare_and_kill ds in
                let na = NAstring ("3.", false, na) in Coth '3', na, None
            else
              match room_of_door g base with
                Some (room, dir) ->
                  let max_test =
                    let (rmin, cmin, rmax, cmax) = room in
                    2 * max (rmax - rmin) (cmax - cmin)
                  in
                  let (rc, _) = room_contents g room in
                  let monl = monsters_around g base in
                  if ntest < max_test && (prev_rc <> rc || monl <> []) then
                    let ds = drop_scare ds (DSdropped ntest) in
                    let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
                  else
                    begin match
                      if is_at_door g pos && not g.pack_full &&
                         ds.ds_outside_tested
                      then
                        match room_of_door g pos with
                          Some (room, dir) ->
                            let mov = one_step_to_enter_room dir in
                            let pos1 = add_mov pos mov in
                            if dung_char g.dung pos1 = '?' then Some mov
                            else None
                        | None -> None
                      else None
                    with
                      Some mov ->
                        let pos1 = add_mov pos mov in
                        (*
                                  let gp = {epos = pos1; tpos = base; path = [base]} in
                                  let ds = drop_scare ds (DSreturn_to_base gp) in
                                  let na = NAdrop_scare_and_kill ds in
                                  let mov = move_between pos pos1 in
                                  (Cmov mov, na, Some mov)
                        *)
                        let gp = {epos = pos1; tpos = pos1; path = []} in
                        let ds = drop_scare ds (DSgo_and_hit (gp, 1)) in
                        let na = NAdrop_scare_and_kill ds in
                        Coth 'm', na, t.t_prev_mov
                    | None ->
                        if g.ring_of_slow_digestion_on_hand <> None &&
                           ds.ds_outside_tested
                        then
                          if g.attacked_by_flame > 0 &&
                             g.level >= level_of_very_mean_monsters &&
                             g.frozen_monsters = [] && is_deadend_room g base
                          then
                            let mov = one_step_to_enter_room dir in
                            let na = NAdrop_scare_and_kill ds in
                            move_command2 g pos (add_mov pos mov) na
                          else if
                            g.attacked_by_flame > 0 &&
                            g.level >= level_of_very_mean_monsters &&
                            g.frozen_monsters = []
                          then
                            match flaming_monster_dir g pos with
                              Some (mdir, dist, _) ->
                                begin match
                                  usable_anti_flamer_wand_in_pack g.pack
                                with
                                  Some (ch, _) ->
                                    let na = NAdrop_scare_and_kill ds in
                                    let na = NAzap (mdir, ch, na, 1) in
                                    Coth 'z', na, None
                                | None ->
                                    if pos = base && is_outside_mov dir mdir
                                    then
                                      let mov = one_step_to_enter_room dir in
                                      let pos1 = add_mov pos mov in
                                      let na = NAdrop_scare_and_kill ds in
                                      move_command3 g pos pos1 na
                                    else
                                      match
                                        if dist <= 2 then
                                          scroll_of_hold_monsters_in_pack
                                            g.pack
                                        else None
                                      with
                                        Some (ch, _) ->
                                          let na = NAdrop_scare_and_kill ds in
                                          let uo =
                                            UOread_scroll (ch, RSread_what)
                                          in
                                          let na = NAuse_object (uo, na) in
                                          Coth 'r', na, None
                                      | None ->
                                          if not (is_at_door g pos) then
                                            failwith "mouais"
                                          else
                                            let mov =
                                              one_step_to_exit_room dir
                                            in
                                            let pos1 = add_mov pos mov in
                                            let na =
                                              NAdrop_scare_and_kill ds
                                            in
                                            move_command3 g pos pos1 na
                                end
                            | None -> failwith "invisible dragon?"
                          else
                            match get_rid_of_objects_when_scaring g ds with
                              Some r -> r
                            | None ->
                                match
                                  dist_to_closest g room pos
                                    (fun ch mov ->
                                       is_monster ch &&
                                       not (is_freezing_monster g ch) ||
                                       List.mem ch list_obj_ch &&
                                       not
                                         (List.mem (add_mov pos mov)
                                            g.garbage))
                                with
                                  Some dist ->
                                    let tpos = add_mov pos dist in
                                    let gp = old_path_to g pos tpos in
                                    if is_monster (dung_char g.dung tpos) then
                                      if List.mem_assoc tpos g.frozen_monsters
                                      then
                                        let ds =
                                          drop_scare ds (DSgo_and_hit (gp, 1))
                                        in
                                        let na = NAdrop_scare_and_kill ds in
                                        Coth 'm', na, t.t_prev_mov
                                      else
                                        let ds =
                                          drop_scare ds
                                            (DSgive_them_chance ("3", 1))
                                        in
                                        let na = NAdrop_scare_and_kill ds in
                                        Coth '3', na, None
                                    else if not (health_is_maximum g) then
                                      if g.attacked_by_flame > 0 then
                                        match flaming_monster_dir g pos with
                                          Some (mdir, _, _) ->
                                            let mov =
                                              if not (is_outside_mov dir mdir)
                                              then
                                                one_step_to_exit_room dir
                                              else one_step_to_enter_room dir
                                            in
                                            let pos1 = add_mov pos mov in
                                            let na =
                                              NAdrop_scare_and_kill ds
                                            in
                                            let na = NArestore_health na in
                                            move_command3 g pos pos1 na
                                        | None -> assert false
                                      else
                                        let dss =
                                          DStest_move (ntest, prev_rc)
                                        in
                                        let ds = drop_scare ds dss in
                                        let na = NAdrop_scare_and_kill ds in
                                        Coth '.', na, None
                                    else
                                      let ds =
                                        drop_scare ds (DSseek_object gp)
                                      in
                                      let na = NAdrop_scare_and_kill ds in
                                      move_command2 g pos gp.epos na
                                | None ->
                                    match
                                      if map_showed_and_some_time_spent g then
                                        unidentified_trap_in_room g
                                      else None
                                    with
                                      Some tpos ->
                                        begin match
                                          path_excl_from_to g [] pos tpos
                                        with
                                          Some gp ->
                                            let na =
                                              NAdrop_scare_and_kill ds
                                            in
                                            let na =
                                              NAgo_identify_trap
                                                (gp, "move", na)
                                            in
                                            Coth ' ', na, None
                                        | None -> assert false
                                        end
                                    | None ->
                                        let perhaps_blocked =
                                          ds.ds_monster_perhaps_blocked
                                        in
                                        let (block, mpb) =
                                          unblocking_monster g pos
                                            perhaps_blocked
                                        in
                                        let ds =
                                          {ds with ds_monster_perhaps_blocked =
                                            mpb}
                                        in
                                        match block with
                                          Some (gp, mpos) ->
                                            let ds =
                                              {ds with ds_state = DSdropped 0}
                                            in
                                            let na =
                                              NAdrop_scare_and_kill ds
                                            in
                                            let um =
                                              {um_base = pos; um_mpos = mpos;
                                               um_gp = gp; um_kont = na}
                                            in
                                            let na =
                                              NAgo_unblock_monster um
                                            in
                                            Coth ' ', na, None
                                        | None ->
                                            let ds =
                                              drop_scare ds DScheck_monsters
                                            in
                                            let na =
                                              NAdrop_scare_and_kill ds
                                            in
                                            let na =
                                              NAstring ("99.", false, na)
                                            in
                                            Coth '9', na, None
                        else if
                          is_at_door g base && not ds.ds_outside_tested &&
                          snd (room_contents g room)
                        then
                          let move = one_step_to_exit_room dir in
                          let ds =
                            drop_scare ds (DStest_out_in (move, rc, 1))
                          in
                          let na = NAdrop_scare_and_kill ds in
                          let (comm, na) = move_command g base move na in
                          comm, na, Some move
                        else
                          let dss = DSdropped (ntest + 1) in
                          let ds = drop_scare ds dss in
                          let ds = {ds with ds_outside_tested = true} in
                          let na = NAdrop_scare_and_kill ds in
                          Coth '.', na, t.t_prev_mov
                    end
              | None -> assert false
        end
  | DStest_out_in (move, rc, step) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let ds = drop_scare ds (DStest_out_in (move, rc, step)) in
          let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
        end
      else
        match step with
          1 ->
            tempo g 1.0;
            let ds = drop_scare ds (DStest_out_in (move, rc, 2)) in
            let na = NAdrop_scare_and_kill ds in Coth 'm', na, None
        | 2 ->
            let ds = {ds with ds_outside_tested = true} in
            let ds = drop_scare ds (DStest_move (0, rc)) in
            let na = NAdrop_scare_and_kill ds in
            let (comm, na) =
              move_command g base {di = -move.di; dj = -move.dj} na
            in
            comm, na, None
        | _ -> failwith (sprintf "not impl DStest_out_in step %d" step)

let set_ar_door_trip_cnt pos ar =
  let dl =
    List.map
      (fun ard ->
         if ard.ard_pos = pos then {ard with ard_trip_cnt = ar.ar_trip_cnt}
         else ard)
      ar.ar_doors
  in
  {ar with ar_doors = dl}

let get_ar_door_trip_cnt pos ar =
  let rec loop =
    function
      ard :: rest -> if pos = ard.ard_pos then ard.ard_trip_cnt else loop rest
    | [] -> assert false
  in
  loop ar.ar_doors

(* perhaps a merge to do with 'monsters_blocked_in_corridor_by_scroll' *)
let test_possible_obstruction_in_next_room g pos =
  let paths_corr = paths_in_corridors_from g pos pos in
  let paths = paths_with_monsters g paths_corr in
  if paths <> [] then None
  else
    match paths_corr with
      [path, tpos] ->
        begin match room_of_door g tpos with
          Some (room, dir) ->
            let mov = one_step_to_enter_room dir in
            let pos1 = add_mov tpos mov in
            if dung_char g.dung pos1 = '?' then Some (path, tpos, pos1)
            else None
        | None -> None
        end
    | [] -> assert false
    | _ :: _ -> None

let ard_of_pos ar pos = List.find (fun ard -> ard.ard_pos = pos) ar.ar_doors

let ar_with_ard ar ard =
  let ar_doors =
    List.map (fun ard1 -> if ard.ard_pos = ard1.ard_pos then ard else ard1)
      ar.ar_doors
  in
  {ar with ar_doors = ar_doors}

let alone_in_room g t message ar =
  let transl = transl g in
  let pos = rogue_pos g in
  if g.ring_of_slow_digestion_on_hand <> None then
    begin g.hist_dung <- []; g.paradise <- true end;
  (*
    if t.t_slow_level <> Some g.level then g.speed := t.t_speed else ();
  *)
  match ar.ar_state with
    ARgo_and_put_scare dpos ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else if pos = dpos then
        begin
          tempo g 1.0;
          match scroll_of_scare_monsters_in_pack g.pack with
            Some _ ->
              let ar = alone_room ar (ARcommand "drop_a_scare") in
              let na = NAalone_in_room ar in Coth 'd', na, None
          | None ->
              match
                list_find (fun ard -> List.mem ard.ard_pos g.scare_pos)
                  ar.ar_doors
              with
                Some ard ->
                  let base = ard.ard_pos in
                  let ds = start_drop_scare base ' ' in
                  begin match path_excl_from_to g [] pos base with
                    Some gp ->
                      let na = ds_return_to_base g ds gp in Coth ' ', na, None
                  | None -> assert false
                  end
              | None -> assert false
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused then
              let monl = monsters_around g pos in
              match
                if monl <> [] then
                  let (mmov, mch, jeopardized) =
                    test_monsters_risk g t monl
                  in
                  if jeopardized &&
                     List.exists
                       (fun mov ->
                          let pos1 = add_mov pos mov in
                          not (List.mem_assoc pos1 g.frozen_monsters))
                       monl
                  then
                    scroll_of_hold_monsters_in_pack g.pack
                  else None
                else None
              with
                Some (ch, _) ->
                  let _ = glup g t "8" in
                  let na = NAalone_in_room ar in
                  let uo = UOread_scroll (ch, RSread_what) in
                  let na = NAuse_object (uo, na) in Coth 'r', na, None
              | None ->
                  let na = NAalone_in_room ar in
                  let na = NAstring ("3.", false, na) in Coth '3', na, None
            else
              begin
                tempo g 0.1;
                let (monl, movl) = monsters_and_moves_around g in
                match monl with
                  mov :: _ ->
                    let na = NAalone_in_room ar in
                    attack_monsters g t movl monl na
                | [] ->
                    let (pos1, path) = one_step_to2 g dpos in
                    let ar = alone_room ar (ARgo_and_put_scare dpos) in
                    let na = NAalone_in_room ar in move_command3 g pos pos1 na
              end
        end
  | ARcommand "drop_a_scare" ->
      if transl.is_message_something_there message then
        begin
          tempo g 1.0;
          let ars = ARgo_and_put_scare pos in
          let ar = alone_room ar ars in
          let na = NAalone_in_room ar in pick_object g t na
        end
      else
        begin match scroll_of_scare_monsters_in_pack g.pack with
          Some (ch, (nb, obj)) ->
            remove_from_pack g ch nb obj;
            g.garbage <- pos :: g.garbage;
            g.scare_pos <- pos :: g.scare_pos;
            let ars =
              match
                list_find (fun ard -> not (List.mem ard.ard_pos g.scare_pos))
                  ar.ar_doors
              with
                Some ard -> ARgo_and_put_scare ard.ard_pos
              | None -> ARcommand "dropped_last_scare"
            in
            let ar = alone_room ar ars in
            let na = NAalone_in_room ar in Coth ch, na, None
        | None -> assert false
        end
  | ARcommand "dropped_last_scare" ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            match
              if g.confused && g.attacked_by_flame > 0 then
                scroll_of_hold_monsters_in_pack g.pack
              else None
            with
              Some (ch, _) ->
                let _ = failwith "9" in
                let uo = UOread_scroll (ch, RSread_what) in
                let ar = alone_room ar (ARcommand "dropped_last_scare") in
                let na = NAalone_in_room ar in
                let na = NAuse_object (uo, na) in Coth 'r', na, None
            | None ->
                if g.confused then
                  let ar = alone_room ar (ARcommand "dropped_last_scare") in
                  let na = NAalone_in_room ar in
                  let na = NAstring ("3.", false, na) in Coth '3', na, None
                else
                  match
                    if g.attacked_by_flame = 0 then get_rid_of_objects g
                    else None
                  with
                    Some (ch, uo) ->
                      let ar =
                        alone_room ar (ARcommand "dropped_last_scare")
                      in
                      let na = NAalone_in_room ar in
                      let na = NAuse_object (uo, na) in Coth ch, na, None
                  | None ->
                      match
                        dist_to_closest g ar.ar_room pos
                          (fun ch mov ->
                             is_monster ch ||
                             List.mem ch list_obj_ch &&
                             not (List.mem (add_mov pos mov) g.garbage))
                      with
                        Some dist ->
                          let tpos = add_mov pos dist in
                          let ch = dung_char g.dung tpos in
                          let gp =
                            path_in_room_excl_mon g ar.ar_room pos tpos
                          in
                          if is_monster ch then
                            let ar = alone_room ar (ARgo_and_kill gp) in
                            let na = NAalone_in_room ar in
                            move_command3 g pos gp.epos na
                          else
                            let ar = alone_room ar (ARseek_object gp) in
                            let na = NAalone_in_room ar in
                            move_command3 g pos gp.epos na
                      | None ->
                          let ard = List.hd ar.ar_doors in
                          if pos = ard.ard_pos then
                            let gp =
                              {epos = ard.ard_pos; tpos = ard.ard_pos;
                               path = []}
                            in
                            let ar =
                              alone_room ar (ARgo_to_door (true, gp))
                            in
                            let na = NAalone_in_room ar in Coth ' ', na, None
                          else
                            let ar =
                              {ar with ar_trip_cnt = ar.ar_trip_cnt + 1}
                            in
                            let gp =
                              path_in_room_excl_mon g ar.ar_room pos
                                ard.ard_pos
                            in
                            let ar =
                              alone_room ar (ARgo_to_door (true, gp))
                            in
                            let na = NAalone_in_room ar in
                            move_command3 g pos gp.epos na
        end
  | ARseek_object gp ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else if g.attacked > 0 && not (List.mem pos g.scare_pos) then
        let (monl, movl) = monsters_and_moves_around g in
        if monl <> [] then
          let na = NAalone_in_room ar in attack_monsters g t movl monl na
        else
          let dist_to_dragon =
            match flaming_monster_dir g pos with
              Some (mdir, dist, pos1) -> dist
            | None -> assert false
          in
          let dist_to_base =
            List.map
              (fun ard ->
                 match path_excl_from_to g [] pos ard.ard_pos with
                   Some gp -> List.length gp.path, gp
                 | None -> assert false)
              ar.ar_doors
          in
          let s =
            List.fold_left (fun s (len, gp) -> sprintf "%s len %d" s len)
              (sprintf "drag %d" dist_to_dragon) dist_to_base
          in
          failwith s
      else if g.pack_full then
        let ard = List.hd ar.ar_doors in
        let gp = path_in_room_excl_mon g ar.ar_room pos ard.ard_pos in
        let ar = alone_room ar (ARgo_to_door (true, gp)) in
        let na = NAalone_in_room ar in move_command3 g pos gp.epos na
      else if pos = gp.tpos then
        match
          dist_to_closest g ar.ar_room pos
            (fun ch mov ->
               List.mem ch list_obj_ch &&
               not (List.mem (add_mov pos mov) g.garbage))
        with
          Some dist ->
            let tpos = add_mov pos dist in
            let gp = path_in_room_excl_mon g ar.ar_room pos tpos in
            let ar = alone_room ar (ARseek_object gp) in
            let na = NAalone_in_room ar in move_command2 g pos gp.epos na
        | None ->
            let ard = List.hd ar.ar_doors in
            let gp = path_in_room_excl_mon g ar.ar_room pos ard.ard_pos in
            let ar = alone_room ar (ARgo_to_door (true, gp)) in
            let na = NAalone_in_room ar in move_command2 g pos gp.epos na
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused && g.attacked > 0 then
              failwith "ARseek_object confused attacked"
            else if pos = gp.epos then
              begin
                tempo g 0.1;
                match gp.path with
                  pos1 :: path ->
                    let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                    let ar = alone_room ar (ARseek_object gp) in
                    let na = NAalone_in_room ar in move_command2 g pos pos1 na
                | [] -> assert false
              end
            else
              match holding_monster_around g pos with
                Some mov ->
                  tempo g 0.5;
                  let tpos = add_mov pos mov in
                  let gp = {epos = tpos; tpos = tpos; path = []} in
                  let ar = alone_room ar (ARgo_and_kill gp) in
                  let na = NAalone_in_room ar in move_command2 g pos tpos na
              | None ->
                  let tpos = gp.tpos in
                  let gp = path_in_room_excl_mon g ar.ar_room pos tpos in
                  let ar = alone_room ar (ARseek_object gp) in
                  let na = NAalone_in_room ar in
                  move_command2 g pos gp.epos na
        end
  | ARgo_and_kill gp ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else
        begin match treat_moving_monsters_at_one_move2 g t with
          Some x -> x
        | None ->
            match treat_critical_situation g t t.t_next_action with
              Some r -> r
            | None ->
                if g.confused && g.attacked > 0 then
                  let _ = failwith "10" in
                  match scroll_of_hold_monsters_in_pack g.pack with
                    Some (ch, _) ->
                      let na = NAalone_in_room ar in
                      let uo = UOread_scroll (ch, RSread_what) in
                      let na = NAuse_object (uo, na) in Coth 'r', na, None
                  | None -> failwith "ARgo_and_kill confused attacked"
                else if pos = gp.epos && pos <> gp.tpos then
                  match gp.path with
                    pos1 :: path ->
                      (*
                                  let (pos1, path) =
                                    circle_trap_if_possible2 g (Some ar.ar_room) pos pos1 path
                                  in
                      *)
                      let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                      let ar = alone_room ar (ARgo_and_kill gp) in
                      let na = NAalone_in_room ar in
                      move_command3 g pos pos1 na
                  | [] -> assert false
                else
                  let ard = List.hd ar.ar_doors in
                  let gp =
                    path_in_room_excl_mon g ar.ar_room pos ard.ard_pos
                  in
                  let ar = alone_room ar (ARgo_to_door (true, gp)) in
                  let na = NAalone_in_room ar in
                  move_command3 g pos gp.epos na
        end
  | ARgo_to_door (first_door, gp) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused then
              match
                if g.attacked_by_flame > 0 then
                  usable_anti_flamer_wand_in_pack g.pack
                else None
              with
                Some (ch, _) ->
                  tempo g 1.0;
                  begin match flaming_monster_dir g pos with
                    Some (mdir, dist, _) ->
                      let na = NAzap (mdir, ch, NAalone_in_room ar, 1) in
                      Coth 'z', na, None
                  | None ->
                      let ar =
                        alone_room ar (ARgo_to_door (first_door, gp))
                      in
                      let na = NAalone_in_room ar in random_move g pos na
                  end
              | None ->
                  let ar = alone_room ar (ARgo_to_door (first_door, gp)) in
                  let na = NAalone_in_room ar in
                  if g.attacked = 0 then
                    let na = NAstring ("3.", false, na) in Coth '3', na, None
                  else begin tempo g 0.1; random_move g pos na end
            else if g.held then
              let (monl, movl) = monsters_and_moves_around g in
              let na = NAalone_in_room ar in attack_monsters g t movl monl na
            else if conditions_for_exit_level g then
              begin
                slow_down g t;
                let spos =
                  match g.sure_stairs_pos with
                    Some spos -> spos
                  | None -> assert false
                in
                if pos = spos then
                  begin tempo g 1.0; Coth '>', NAnone, None end
                else
                  let graph = make_graph g false in
                  go_to_stairs g t graph pos spos true
              end
            else if pos = gp.tpos then
              begin
                tempo g 0.1;
                let monl = monsters_around g pos in
                match monl with
                  mov :: _ ->
                    let ard = ard_of_pos ar pos in
                    let ard = {ard with ard_monster_perhaps_blocked = None} in
                    let ar = ar_with_ard ar ard in
                    let ar = set_ar_door_trip_cnt pos ar in
                    let ars = ARcommand "kill_monsters" in
                    let ar = alone_room ar (ARforce_kill (mov, ars)) in
                    let na = NAalone_in_room ar in Coth 'F', na, None
                | [] ->
                    let last_trip_cnt = get_ar_door_trip_cnt pos ar in
                    match
                      if g.attacked_by_flame = 0 then get_rid_of_objects g
                      else None
                    with
                      Some (ch, uo) ->
                        let na = NAalone_in_room ar in
                        let na = NAuse_object (uo, na) in Coth ch, na, None
                    | None ->
                        if not (health_is_maximum g) &&
                           g.attacked_by_flame = 0
                        then
                          let na = NAalone_in_room ar in Coth '.', na, None
                        else
                          let ard = ard_of_pos ar pos in
                          let perhaps_blocked =
                            ard.ard_monster_perhaps_blocked
                          in
                          let (block, mpb) =
                            unblocking_monster g pos perhaps_blocked
                          in
                          let ard =
                            {ard with ard_monster_perhaps_blocked = mpb}
                          in
                          let ar = ar_with_ard ar ard in
                          let blocked =
                            match block with
                              Some (gp, mpos) -> Some (gp, mpos, ard)
                            | None -> None
                          in
                          match blocked with
                            Some (gp, mpos, ard) ->
                              let na = NAalone_in_room ar in
                              let um =
                                {um_base = pos; um_mpos = mpos; um_gp = gp;
                                 um_kont = na}
                              in
                              let na = NAgo_unblock_monster um in
                              Coth ' ', na, None
                          | None ->
                              match
                                if scroll_of_hold_monsters_in_pack g.pack <>
                                     None
                                then
                                  test_possible_obstruction_in_next_room g pos
                                else None
                              with
                                Some (path, tpos, pos1) ->
                                  let gp =
                                    {epos = pos; tpos = pos1;
                                     path = path @ [pos1]}
                                  in
                                  let ce =
                                    let na = NAalone_in_room ar in
                                    {ce_base = pos; ce_gp = gp;
                                     ce_state = "moved"; ce_kont = na}
                                  in
                                  let na = NAgo_in_corridor_and_hit ce in
                                  Coth ' ', na, None
                              | None ->
                                  if scroll_of_hold_monsters_in_pack g.pack <>
                                       None &&
                                     last_trip_cnt > 0 &&
                                     ar.ar_trip_cnt - last_trip_cnt > 5
                                  then
                                    let ar = set_ar_door_trip_cnt pos ar in
                                    match go_in_corridor_and_hit g pos with
                                      Some gp ->
                                        let ce =
                                          let na = NAalone_in_room ar in
                                          {ce_base = pos; ce_gp = gp;
                                           ce_state = "start"; ce_kont = na}
                                        in
                                        let na =
                                          NAgo_in_corridor_and_hit ce
                                        in
                                        Coth 'm', na, None
                                    | None ->
                                        failwith "cannot go in corridor 1"
                                  else if first_door then
                                    let ar =
                                      alone_room ar
                                        (ARcommand "kill_monsters")
                                    in
                                    let na = NAalone_in_room ar in
                                    let na = NAstring ("99.", false, na) in
                                    Coth '9', na, None
                                  else
                                    let next_door =
                                      let rec loop =
                                        function
                                          ard :: dl ->
                                            if pos = ard.ard_pos then dl
                                            else loop dl
                                        | [] -> []
                                      in
                                      loop ar.ar_doors
                                    in
                                    let rec loop =
                                      function
                                        ard :: rest ->
                                          let skip_it =
                                            if g.mon_detected then
                                              let mov =
                                                one_step_to_exit_room
                                                  ard.ard_dir
                                              in
                                              let ch =
                                                dung_char g.dung
                                                  (add_mov ard.ard_pos mov)
                                              in
                                              if is_monster ch then false
                                              else
                                                let last_trip_cnt =
                                                  ard.ard_trip_cnt
                                                in
                                                if last_trip_cnt > 0 &&
                                                   ar.ar_trip_cnt -
                                                   last_trip_cnt >
                                                     5
                                                then
                                                  false
                                                else true
                                            else false
                                          in
                                          if skip_it && rest <> [] then
                                            loop rest
                                          else
                                            let gp =
                                              path_in_room_excl_mon g
                                                ar.ar_room pos ard.ard_pos
                                            in
                                            let ar =
                                              alone_room ar
                                                (ARgo_to_door (false, gp))
                                            in
                                            let na = NAalone_in_room ar in
                                            move_command3 g pos gp.epos na
                                      | [] ->
                                          let ars =
                                            ARcommand "dropped_last_scare"
                                          in
                                          let ar = alone_room ar ars in
                                          let na = NAalone_in_room ar in
                                          Coth ' ', na, None
                                    in
                                    loop next_door
              end
            else if
              not (health_is_maximum g) && inside_room ar.ar_room pos
            then
              let na = NAalone_in_room ar in
              let na = NArestore_health na in Coth '.', na, t.t_prev_mov
            else if pos = gp.epos then
              begin
                tempo g 0.1;
                let (monl, movl) = monsters_and_moves_around g in
                match monl with
                  mon :: _ ->
                    let na = NAalone_in_room ar in
                    attack_monsters g t movl monl na
                | [] ->
                    match gp.path with
                      pos1 :: path ->
                        (*
                                        let (pos1, path) =
                                          circle_trap_if_possible2 g (Some ar.ar_room) pos pos1 path
                                        in
                        *)
                        let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                        let ar =
                          alone_room ar (ARgo_to_door (first_door, gp))
                        in
                        let na = NAalone_in_room ar in
                        move_command3 g pos pos1 na
                    | [] -> assert false
              end
            else
              let gp =
                match g.rogue_room_and_door with
                  Some (room, None) ->
                    path_in_room_excl_mon g room pos gp.tpos
                | Some (_, Some _) | None -> old_path_to g pos gp.tpos
              in
              let ar = alone_room ar (ARgo_to_door (false, gp)) in
              let na = NAalone_in_room ar in move_command3 g pos gp.epos na
        end
  | ARcommand "kill_monsters" ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAalone_in_room ar in Coth ' ', na, None
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.confused then
              let _ = failwith "13" in
              match
                if g.attacked_by_flame > 0 then
                  usable_anti_flamer_wand_in_pack g.pack
                else None
              with
                Some (ch, _) ->
                  tempo g 1.0;
                  begin match flaming_monster_dir g pos with
                    Some (mdir, dist, _) ->
                      let na = NAzap (mdir, ch, NAalone_in_room ar, 1) in
                      Coth 'z', na, None
                  | None -> assert false
                  end
              | None ->
                  let ar = alone_room ar (ARcommand "kill_monsters") in
                  let na = NAalone_in_room ar in
                  let na = NAstring ("3.", false, na) in Coth '3', na, None
            else
              let monl = monsters_around g pos in
              match monl with
                mov :: _ ->
                  let ard = ard_of_pos ar pos in
                  let ard = {ard with ard_monster_perhaps_blocked = None} in
                  let ar = ar_with_ard ar ard in
                  let ar = set_ar_door_trip_cnt pos ar in
                  let ars = ARcommand "kill_monsters" in
                  let ar = alone_room ar (ARforce_kill (mov, ars)) in
                  let na = NAalone_in_room ar in Coth 'F', na, None
              | [] ->
                  let last_trip_cnt =
                    if not (is_at_door g pos) then 0
                    else get_ar_door_trip_cnt pos ar
                  in
                  if last_trip_cnt > 0 && ar.ar_trip_cnt - last_trip_cnt > 5
                  then
                    match go_in_corridor_and_hit g pos with
                      Some gp ->
                        let ce =
                          let na = NAalone_in_room ar in
                          {ce_base = pos; ce_gp = gp; ce_state = "start";
                           ce_kont = na}
                        in
                        let na = NAgo_in_corridor_and_hit ce in
                        Coth 'm', na, None
                    | None -> failwith "cannot go in corridor"
                  else
                    let next_door =
                      let rec loop =
                        function
                          ard :: dl ->
                            if pos = ard.ard_pos then dl else loop dl
                        | [] -> []
                      in
                      loop ar.ar_doors
                    in
                    match next_door with
                      ard :: _ ->
                        let gp =
                          path_in_room_excl_mon g ar.ar_room pos ard.ard_pos
                        in
                        let ars = ARgo_to_door (false, gp) in
                        let ar = alone_room ar ars in
                        let na = NAalone_in_room ar in
                        move_command3 g pos gp.epos na
                    | [] ->
                        let ar =
                          alone_room ar (ARcommand "dropped_last_scare")
                        in
                        let na = NAalone_in_room ar in Coth ' ', na, None
        end
  | ARforce_kill (mov, ars) ->
      let ar = alone_room ar ars in
      let na = NAalone_in_room ar in Cmov mov, na, None
  | ARcommand s -> failwith (sprintf "not impl ARcommand '%s'" s)

let no_monster_in_line_of_sight g mpos cand_dir pos_dir =
  let rec loop pos =
    if pos = cand_dir then pos
    else if not (in_dung g (pos_dir pos)) then cand_dir
    else if is_monster (dung_char g.dung (pos_dir pos)) then pos
    else loop (pos_dir pos)
  in
  loop mpos

let furthest_pos_in_room_to g (rmin, cmin, rmax, cmax) mpos =
  let (rmin, cmin, rmax, cmax) =
    (* for the "big" room *)
    max rmin (mpos.row - 20), max cmin (mpos.col - 20),
    min rmax (mpos.row + 20), min cmax (mpos.col + 20)
  in
  let rmin =
    let rec loop rmin =
      if g.dung.tab.(rmin).[mpos.col] = '^' then loop (rmin + 1) else rmin
    in
    loop rmin
  in
  let cmin =
    let rec loop cmin =
      if g.dung.tab.(mpos.row).[cmin] = '^' then loop (cmin + 1) else cmin
    in
    loop cmin
  in
  let rmax =
    let rec loop rmax =
      if g.dung.tab.(rmax).[mpos.col] = '^' then loop (rmax - 1) else rmax
    in
    loop rmax
  in
  let cmax =
    let rec loop cmax =
      if g.dung.tab.(mpos.row).[cmax] = '^' then loop (cmax - 1) else cmax
    in
    loop cmax
  in
  let cand_left =
    let pos_door = {mpos with col = cmin - 1} in
    if dung_char g.dung pos_door = '+' then pos_door
    else {mpos with col = cmin}
  in
  let cand_right =
    let pos_door = {mpos with col = cmax + 1} in
    if dung_char g.dung pos_door = '+' then pos_door
    else {mpos with col = cmax}
  in
  let cand_up =
    let pos_door = {mpos with row = rmin - 1} in
    if dung_char g.dung pos_door = '+' then pos_door
    else {mpos with row = rmin}
  in
  let cand_down =
    let pos_door = {mpos with row = rmax + 1} in
    if dung_char g.dung pos_door = '+' then pos_door
    else {mpos with row = rmax}
  in
  let cand_left = no_monster_in_line_of_sight g mpos cand_left pos_left in
  let cand_right = no_monster_in_line_of_sight g mpos cand_right pos_right in
  let cand_up = no_monster_in_line_of_sight g mpos cand_up pos_up in
  let cand_down = no_monster_in_line_of_sight g mpos cand_down pos_down in
  List.sort
    (fun pos1 pos2 -> compare (distance pos2 mpos) (distance pos1 mpos))
    [cand_left; cand_right; cand_up; cand_down]

let attack_at_distance g t room mov =
  let pos = rogue_pos g in
  let mpos = add_mov pos mov in
  let excl =
    let (rmin, cmin, rmax, cmax) = room in
    let rec loop excl pos =
      if pos.row = rmax + 1 then excl
      else if pos.col = cmax + 1 then
        loop excl {row = pos.row + 1; col = cmin}
      else
        let mch = dung_char g.dung pos in
        let excl =
          if is_monster mch then
            if is_mean_monster g mch then
              let rec loop excl =
                function
                  k :: kl -> loop (add_mov pos (mov_of_k k) :: excl) kl
                | [] -> excl
              in
              loop excl run_around_list
            else pos :: excl
          else excl
        in
        loop excl {pos with col = pos.col + 1}
    in
    loop [] {row = rmin; col = cmin}
  in
  let mch = dung_char g.dung mpos in
  let tpos_list = furthest_pos_in_room_to g room mpos in
  let tpos_list =
    let rec loop =
      function
        tpos :: tpos_list ->
          if distance mpos tpos < 4 then [] else tpos :: loop tpos_list
      | [] -> []
    in
    loop tpos_list
  in
  let rec loop =
    function
      tpos :: tpos_list ->
        begin match path_in_room_to g room excl pos tpos with
          Some (mov :: path) ->
            let na = NAmove_throw1 (path, mpos, mch) in
            let (comm, na) = move_command g pos mov na in comm, na, Some mov
        | Some [] ->
            if g.weapon_cursed || short_bow_in_pack g.pack = None then
              let na = NAmove_throw2 (mpos, mch, "direction") in
              Coth 't', na, None
            else
              let na = NAmove_throw2 (mpos, mch, "what bow") in
              Coth 'w', na, None
        | None -> loop tpos_list
        end
    | [] ->
        if max (abs mov.di) (abs mov.dj) = 2 then
          let (mov, _) = one_step_to g mpos in
          let na = NAlet_come (mch, mov) in Coth '.', na, None
        else if is_at_door g pos then start_move_in_corridor g t pos
        else if mpos.row = pos.row || mpos.col = pos.col then
          let (mov, _) = one_step_to g mpos in
          move_command3 g pos (add_mov pos mov) NAnone
        else
          let (mov, _) = one_step_to g mpos in
          let na = NAlet_come (mch, mov) in Coth '.', na, None
  in
  loop tpos_list

let dist_between pos1 pos2 =
  {di = pos2.row - pos1.row; dj = pos2.col - pos1.col}

let monster_in_dir g pos mch mpos =
  let rec loop pos =
    if pos.row = mpos.row then
      if dung_char g.dung pos = mch then Some pos
      else if pos.col > mpos.col then loop (pos_left pos)
      else if pos.col < mpos.col then loop (pos_right pos)
      else None
    else if pos.col = mpos.col then
      if dung_char g.dung pos = mch then Some pos
      else if pos.row > mpos.row then loop (pos_up pos)
      else if pos.row < mpos.row then loop (pos_down pos)
      else None
    else None
  in
  loop pos

let at_dot = ['@'; '.']

let around_diff a1 a2 =
  let rec loop k =
    if k = String.length a1.ar then None
    else if
      a1.ar.[k] = a2.ar.[k] || List.mem a1.ar.[k] at_dot ||
      List.mem a2.ar.[k] at_dot
    then
      loop (k + 1)
    else Some k
  in
  loop 0

let close_to_room g pos =
  let rec loop =
    function
      pos_dir :: rest ->
        let pos1 = pos_dir pos in
        if in_dung g pos1 && List.mem (dung_char g.dung pos1) ['|'; '-'] then
          true
        else loop rest
    | [] -> false
  in
  loop [pos_up; pos_down; pos_left; pos_right]

let count_accessible_things_around g pos in_room =
  let (row, col) = pos.row, pos.col in
  let rec loop nmov nmon1 nmon2 nobj di dj =
    let mov = {di = di; dj = dj} in
    if di = 2 then nmov, nmon1, nmon2, nobj
    else if dj = 2 then loop nmov nmon1 nmon2 nobj (di + 1) (-1)
    else if di = 0 && dj = 0 then loop nmov nmon1 nmon2 nobj 0 1
    else if not (old_can_move_to g in_room pos (add_mov pos mov)) then
      loop nmov nmon1 nmon2 nobj di (dj + 1)
    else
      let ch = g.dung.tab.(row+di).[col+dj] in
      if List.mem ch list_mov_ch then
        loop (mov :: nmov) nmon1 nmon2 nobj di (dj + 1)
      else if is_attackable_monster g ch || g.held then
        loop nmov (mov :: nmon1) nmon2 nobj di (dj + 1)
      else if is_monster_attackable_at_distance g ch then
        loop nmov nmon1 (mov :: nmon2) nobj di (dj + 1)
      else if List.mem ch list_obj_ch then
        let (nmov, nobj) =
          if List.mem {row = row + di; col = col + dj} g.garbage then
            mov :: nmov, nobj
          else nmov, mov :: nobj
        in
        loop nmov nmon1 nmon2 nobj di (dj + 1)
      else loop nmov nmon1 nmon2 nobj di (dj + 1)
  in
  loop [] [] [] [] (-1) (-1)

let access_blocked_by_other_monster g room pos mov =
  let f_excl pos = is_monster (dung_char g.dung pos) in
  let mpos = add_mov pos mov in
  match gen_path_in_room_to g room f_excl pos mpos with
    Some (_, []) | None -> true
  | Some _ -> false

let sign x = if x >= 0 then 1 else -1

let find_something_to_do g t =
  let prev_mov = t.t_prev_mov in
  let pos = rogue_pos g in
  let in_room = g.rogue_room_and_door <> None in
  if in_room then set_regrets g;
  let (nmov, nmon1, nmon2, nobj) =
    count_accessible_things_around g pos in_room
  in
  if conditions_for_dropping_scare g then ok_for_dropping_scare g t
  else if nmon1 <> [] then
    let (monl, movl) = monsters_and_moves_around g in
    attack_monsters g t movl monl NAnone
  else
    match treat_critical_situation g t t.t_next_action with
      Some r -> r
    | None ->
        if nobj <> [] then
          let mov = List.nth nobj (random_int g (List.length nobj)) in
          let (comm, na) = move_command g pos mov NAnone in comm, na, Some mov
        else
          match g.rogue_room_and_door with
            Some (room, _) ->
              let rr = room_row room in
              let rc = room_col room in
              g.visited.(rr).(rc) <- true;
              let is_new_level =
                match t.t_prev_pos with
                  Some old_g -> old_g.level <> g.level
                | None -> true
              in
              if g.confused && is_at_door g pos then Coth '.', NAnone, None
              else if
                g.confused && g.level >= level_of_very_mean_monsters
              then
                glup g t "15"
              else
                begin match
                  dist_to_closest g room pos
                    (fun ch mov ->
                       is_monster ch &&
                       (is_attackable_monster g ch ||
                        is_moving g t (add_mov pos mov) && not is_new_level ||
                        t.t_prev_pos = None) &&
                       not (access_blocked_by_other_monster g room pos mov))
                with
                  Some mov ->
                    let mpos = add_mov pos mov in
                    let thing = dung_char g.dung mpos in
                    begin match
                      treat_moving_monsters_at_one_move g t pos NAnone
                    with
                      Some r -> r
                    | None ->
                        if is_mean_monster g thing &&
                           not (is_moving g t (add_mov pos mov)) &&
                           arrows_in_pack g.pack <> None && not g.confused
                        then
                          attack_at_distance g t room mov
                        else if
                          is_mean_monster g thing &&
                          max (abs mov.di) (abs mov.dj) > 4 &&
                          arrows_in_pack g.pack <> None
                        then
                          let mch = thing in
                          if mov.di = 0 || mov.dj = 0 then
                            if g.weapon_cursed ||
                               short_bow_in_pack g.pack = None
                            then
                              let na =
                                NAmove_throw2 (mpos, mch, "direction")
                              in
                              Coth 't', na, None
                            else
                              let na =
                                NAmove_throw2 (mpos, mch, "what bow")
                              in
                              Coth 'w', na, None
                          else if is_at_door g pos then
                            match room_of_door g pos with
                              Some (_, dir) ->
                                let test_moving =
                                  let horizontal = abs mov.di < abs mov.dj in
                                  match dir with
                                    DoorUp | DoorDown -> not horizontal
                                  | DoorLeft | DoorRight -> horizontal
                                in
                                if test_moving then
                                  if short_bow_in_pack g.pack <> None then
                                    let na =
                                      NAwield_bow_test_moving (mpos, mch, 1)
                                    in
                                    Coth 'w', na, None
                                  else
                                    let na =
                                      NAwield_bow_test_moving (mpos, mch, 2)
                                    in
                                    Coth '.', na, None
                                else
                                  let mov = one_step_to_enter_room dir in
                                  move_command3 g pos (add_mov pos mov) NAnone
                            | None ->
                                failwith "attack at distance immediately 17"
                          else
                            let tpos =
                              if abs mov.di <= abs mov.dj then
                                {row = pos.row + sign mov.di;
                                 col = pos.col - sign mov.dj}
                              else
                                {row = pos.row - sign mov.di;
                                 col = pos.col + sign mov.dj}
                            in
                            if can_move_to g tpos then
                              move_command3 g pos tpos NAnone
                            else
                              let (mov, path) =
                                one_step_to g (add_mov pos mov)
                              in
                              move_command3 g pos (add_mov pos mov) NAnone
                        else
                          let tpos = add_mov pos mov in
                          let gp = path_in_room_excl_mon g room pos tpos in
                          let na = NAgo_and_kill (gp, thing) in
                          move_command3 g pos gp.epos na
                    end
                | None ->
                    match
                      dist_to_closest g room pos
                        (fun ch mov ->
                           List.mem ch list_obj_ch &&
                           not (List.mem (add_mov pos mov) g.garbage) &&
                           not (will_take_not_interesting_object g ch))
                    with
                      Some mov ->
                        let tpos = add_mov pos mov in
                        let gp = path_in_room_excl_mon g room pos tpos in
                        let na = NAseek_object gp in
                        move_command3 g pos gp.epos na
                    | None ->
                        match
                          if arrows_in_pack g.pack = None || g.confused then
                            None
                          else
                            dist_to_closest g room pos
                              (fun ch _ ->
                                 is_monster_attackable_at_distance g ch ||
                                 is_mean_monster g ch)
                        with
                          Some mov -> attack_at_distance g t room mov
                        | None ->
                            match
                              path_to_closest2 g pos
                                (fun _ pos -> List.mem pos g.regrets)
                            with
                              Some gp ->
                                let na = NAseek_object gp in
                                Coth ' ', na, t.t_prev_mov
                            | None ->
                                match
                                  if is_big_room g ||
                                     g.time_in_level >= min_time_in_level &&
                                     doors_not_explorated_in_current_room g =
                                       []
                                  then
                                    path_to_closest2 g pos
                                      (fun _ pos ->
                                         dung_char g.dung pos = '%')
                                  else None
                                with
                                  Some gp ->
                                    let na = NAgo_to_stairs (gp, true) in
                                    Coth ' ', na, t.t_prev_mov
                                | None ->
                                    let doors = find_doors g room in
                                    let inilen = List.length doors in
                                    let rr = room_row room in
                                    let rc = room_col room in
                                    let selected_doors =
                                      let dl =
                                        doors_not_explorated g room doors
                                      in
                                      (* rather doors not towards center *)
                                      if rr = 1 then
                                        let dl2 =
                                          List.filter
                                            (fun (tpos, _) ->
                                               match room_of_door g tpos with
                                                 Some (_, dir) ->
                                                   dir <> DoorRight &&
                                                   dir <> DoorLeft
                                               | None -> assert false)
                                            dl
                                        in
                                        if dl2 <> [] then dl2 else dl
                                      else if rc = 1 then
                                        let dl2 =
                                          List.filter
                                            (fun (tpos, _) ->
                                               match room_of_door g tpos with
                                                 Some (_, dir) ->
                                                   dir <> DoorUp &&
                                                   dir <> DoorDown
                                               | None -> assert false)
                                            dl
                                        in
                                        if dl2 <> [] then dl2 else dl
                                      else dl
                                    in
                                    let selected_doors =
                                      if selected_doors = [] then []
                                      else
                                        let dl =
                                          List.filter
                                            (fun (tpos, _) ->
                                               match room_of_door g tpos with
                                                 Some (_, dir) ->
                                                   let (neigh_rr, neigh_rc) =
                                                     match dir with
                                                       DoorUp -> rr - 1, rc
                                                     | DoorDown -> rr + 1, rc
                                                     | DoorLeft -> rr, rc - 1
                                                     | DoorRight -> rr, rc + 1
                                                   in
                                                   not
                                                     g.visited.(neigh_rr)
                                                     .(neigh_rc)
                                               | None -> true)
                                            selected_doors
                                        in
                                        if dl = [] then selected_doors else dl
                                    in
                                    let doors =
                                      if selected_doors = [] then doors
                                      else selected_doors
                                    in
                                    let doors =
                                      match prev_mov with
                                        Some mov ->
                                          let from = opposite_move mov in
                                          let dl =
                                            List.filter
                                              (fun (tpos, _) ->
                                                 pos <> tpos &&
                                                 (let (mov1, _) =
                                                    one_step_to g tpos
                                                  in
                                                  mov1 <> from))
                                              doors
                                          in
                                          if dl = [] then doors else dl
                                      | None -> doors
                                    in
                                    let len = List.length doors in
                                    if len = 0 ||
                                       inilen = 1 &&
                                       connected_to_rooms_without_exit g pos
                                    then
                                      begin
                                        for i = 0 to 2 do
                                          for j = 0 to 2 do
                                            g.visited.(i).(j) <- false
                                          done
                                        done;
                                        let graph = make_graph g false in
                                        start_search g t graph
                                      end
                                    else
                                      let (tpos, _) =
                                        List.nth doors (random_int g len)
                                      in
                                      if tpos = pos then
                                        start_move_in_corridor g t pos
                                      else
                                        let gp =
                                          path_in_room_excl_mon g room pos
                                            tpos
                                        in
                                        let na = NAgo_to_door gp in
                                        move_command3 g pos gp.epos na
                end
          | None ->
              match
                match
                  if List.length nmov <= 1 then None
                  else select_random_move_to g nmov pos '+'
                with
                  Some mov -> Some mov
                | None ->
                    match prev_mov with
                      Some mov ->
                        if can_move_to g (add_mov pos mov) then Some mov
                        else
                          begin match nmov with
                            [mov] ->
                              if random_int g 5 <> 0 then Some no_move
                              else Some mov
                          | [] -> None
                          | _ ->
                              let nmov_without_from =
                                let from = opposite_move mov in
                                List.filter ((<>) from) nmov
                              in
                              let nmov =
                                if nmov_without_from = [] then nmov
                                else nmov_without_from
                              in
                              select_random_move_to g nmov pos '#'
                          end
                    | None -> None
              with
                Some mov ->
                  let (comm, na) = move_command g pos mov NAnone in
                  comm, na, Some mov
              | None ->
                  if g.blind then random_move g pos NAnone
                  else start_move_in_corridor g t pos

let rec choose_dir g pos =
  function
    [] -> assert false
  | [mov] -> mov
  | mov :: movl ->
      if can_move_to g (add_mov pos mov) then mov else choose_dir g pos movl

let turn_right g pos =
  function
    {di = 0; dj = 1} ->
      choose_dir g pos
        [{di = 1; dj = 0}; {di = 0; dj = 1}; {di = -1; dj = 0};
         {di = 0; dj = -1}]
  | {di = 0; dj = (-1)} ->
      choose_dir g pos
        [{di = -1; dj = 0}; {di = 0; dj = -1}; {di = 1; dj = 0};
         {di = 0; dj = 1}]
  | {di = 1; dj = 0} ->
      choose_dir g pos
        [{di = 0; dj = -1}; {di = 1; dj = 0}; {di = 0; dj = 1};
         {di = -1; dj = 0}]
  | {di = (-1); dj = 0} ->
      choose_dir g pos
        [{di = 0; dj = 1}; {di = -1; dj = 0}; {di = 0; dj = -1};
         {di = 1; dj = 0}]
  | mov -> failwith (sprintf "turn_right %d %d" mov.di mov.dj)

let apply g t message =
  let transl = transl g in
  match t.t_next_action with
    NAfind_another_room_and_return fa ->
      let pos = rogue_pos g in
      begin match fa.fa_state with
        "go to door" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAfind_another_room_and_return fa in
              Coth ' ', na, t.t_prev_mov
            end
          else if pos = fa.fa_gp.tpos then
            begin
              tempo g 0.1;
              let fa = {fa with fa_state = "at door"} in
              let na = NAfind_another_room_and_return fa in
              Coth ' ', na, t.t_prev_mov
            end
          else if pos = fa.fa_gp.epos then
            begin
              tempo g 0.1;
              match fa.fa_gp.path with
                epos :: path ->
                  let gp = {fa.fa_gp with epos = epos; path = path} in
                  let fa = {fa with fa_gp = gp} in
                  let na = NAfind_another_room_and_return fa in
                  move_command3 g pos epos na
              | [] -> assert false
            end
          else failwith "NAfind_another_room_and_return 3"
      | "at door" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAfind_another_room_and_return fa in
              Coth ' ', na, t.t_prev_mov
            end
          else
            begin
              tempo g 0.1;
              let mov =
                match t.t_prev_mov with
                  Some mov -> mov
                | None -> assert false
              in
              let fa = {fa with fa_state = "in corridor"} in
              let na = NAfind_another_room_and_return fa in
              move_command3 g pos (add_mov pos mov) na
            end
      | "in corridor" ->
          tempo g 0.1;
          let prev_mov =
            match t.t_prev_mov with
              Some mov -> mov
            | None -> assert false
          in
          let mov = turn_right g pos prev_mov in
          let pos1 = add_mov pos mov in
          if is_inside_room g pos1 then
            if in_same_rooms g pos1 fa.fa_base then
              match fa.fa_doors with
                [] -> failwith "aaaaa 3"
              | dpos :: dposl ->
                  let gp = old_path_excl_from_to g [] pos dpos 48 in
                  let fa =
                    {fa_state = "go to door"; fa_gp = gp; fa_doors = dposl;
                     fa_base = fa.fa_base}
                  in
                  let na = NAfind_another_room_and_return fa in
                  Coth ' ', na, t.t_prev_mov
            else
              let gp = old_path_excl_from_to g [] pos fa.fa_base 48 in
              let fa = {fa with fa_state = "returning"; fa_gp = gp} in
              let na = NAfind_another_room_and_return fa in
              Coth ' ', na, t.t_prev_mov
          else
            let na = NAfind_another_room_and_return fa in
            move_command3 g pos pos1 na
      | "returning" ->
          tempo g 0.1;
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAfind_another_room_and_return fa in
              Coth ' ', na, t.t_prev_mov
            end
          else if pos = fa.fa_gp.tpos then
            let ds = drop_scare_with fa.fa_gp.tpos (DSdropped 0) in
            let na = NAdrop_scare_and_kill ds in Coth ' ', na, t.t_prev_mov
          else if pos = fa.fa_gp.epos then
            match fa.fa_gp.path with
              epos :: path ->
                let gp = {fa.fa_gp with epos = epos; path = path} in
                let fa = {fa with fa_gp = gp} in
                let na = NAfind_another_room_and_return fa in
                move_command3 g pos epos na
            | [] -> assert false
          else
            let na = NAfind_another_room_and_return fa in
            move_command3 g pos fa.fa_gp.epos na
      | state ->
          failwith
            (sprintf "not impl NAfind_another_room_and_return '%s'" state)
      end
  | NAgo_and_kill (gp, ch) ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAgo_and_kill (gp, ch) in Coth ' ', na, t.t_prev_mov
        end
      else if g.attacked > 0 then Coth ' ', NAnone, t.t_prev_mov
      else if dung_char g.dung gp.tpos <> ch then
        Coth ' ', NAnone, t.t_prev_mov
      else if pos = gp.tpos then
        failwith (sprintf "NAgo_and_kill '%c' error" ch)
      else if pos = gp.epos then
        match gp.path with
          epos :: path ->
            let gp = {epos = epos; tpos = gp.tpos; path = path} in
            let na = NAgo_and_kill (gp, ch) in move_command3 g pos epos na
        | [] -> assert false
      else Coth ' ', NAnone, t.t_prev_mov
  | NAgo_identify_trap (gp, step, na) ->
      let pos = rogue_pos g in
      begin match step with
        "move" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAgo_identify_trap (gp, step, na) in
              Coth ' ', na, t.t_prev_mov
            end
          else
            begin match treat_moving_monsters_at_one_move2 g t with
              Some x -> x
            | None ->
                if pos = gp.epos && gp.path <> [] then
                  match gp.path with
                    [] -> assert false
                  | [_] ->
                      let mov = move_between pos gp.tpos in
                      let ch = basic_command_of_move mov in
                      let str = String.make 1 ch in
                      let na = NAgo_identify_trap (gp, "ask", na) in
                      let na = NAstring (str, false, na) in
                      Coth '^', na, t.t_prev_mov
                  | epos :: path ->
                      tempo g 0.1;
                      let gp = {epos = epos; tpos = gp.tpos; path = path} in
                      let na = NAgo_identify_trap (gp, step, na) in
                      move_command3 g pos epos na
                else
                  match path_excl_from_to g [] pos gp.tpos with
                    Some gp ->
                      let na = NAgo_identify_trap (gp, step, na) in
                      Coth ' ', na, t.t_prev_mov
                  | None -> failwith "not impl: NAgo_identify_trap 3"
            end
      | "ask" ->
          let tk =
            if transl.is_trap_door message then TKtrap_door else TKother
          in
          Hashtbl.replace g.traps gp.tpos (Some (Some tk));
          Coth ' ', na, t.t_prev_mov
      | step -> failwith (sprintf "NAgo_identify_trap '%s'" step)
      end
  | NAgo_in_corridor_and_hit ce ->
      let pos = rogue_pos g in
      let base = ce.ce_base in
      let gp = ce.ce_gp in
      let step = ce.ce_state in
      begin match step_go_in_corridor_and_hit g t message base gp step with
        SGCway_there (mov, step) ->
          let ce = {ce with ce_state = step} in
          let na = NAgo_in_corridor_and_hit ce in Cmov mov, na, Some mov
      | SGCack_mess ->
          let na = NAgo_in_corridor_and_hit ce in Coth ' ', na, None
      | SGCmove_way_there (gp, step) ->
          let ce = {ce with ce_gp = gp; ce_state = step} in
          let na = NAgo_in_corridor_and_hit ce in Coth 'm', na, None
      | SGCattack (mov, step) ->
          let gp =
            let pred _ = (=) base in
            let excl = [add_mov pos mov] in
            match direct_path_excl g excl pos pred with
              Some (path, _) -> {epos = pos; tpos = base; path = path}
            | None -> failwith "SGCattack"
          in
          let na = return_to_base g gp ce.ce_kont in
          move_command3 g pos (add_mov pos mov) na
      | SGCchar (ch, step) ->
          let ce = {ce with ce_gp = gp; ce_state = step} in
          let na = NAgo_in_corridor_and_hit ce in Coth ch, na, None
      | SGCpick_and_return ->
          let (monl, movl) = monsters_and_moves_around g in
          begin match
            if on_scare_monster g then
              match monl with
                mov :: _ -> Some mov
              | [] -> None
            else None
          with
            Some mov ->
              let pos1 = add_mov pos mov in
              let na = NAgo_in_corridor_and_hit ce in
              move_command2 g pos pos1 na
          | None ->
              if common_room_with g ce.ce_base <> None then
                let gp =
                  let pred _ = (=) base in
                  match direct_path_excl g [] pos pred with
                    Some (path, _) -> {epos = pos; tpos = base; path = path}
                  | None -> failwith "SGCpick_and_return 1"
                in
                let na = return_to_base g gp ce.ce_kont in
                Coth ' ', na, t.t_prev_mov
              else
                let gp =
                  let excl =
                    match g.rogue_room_and_door with
                      Some (_, Some dir) ->
                        let mov = one_step_to_enter_room dir in
                        [add_mov pos mov]
                    | Some (_, None) | None -> []
                  in
                  let pred _ = (=) base in
                  match direct_path_excl g excl pos pred with
                    Some (path, _) -> {epos = pos; tpos = base; path = path}
                  | None -> failwith "SGCpick_and_return 2"
                in
                let na = return_to_base g gp ce.ce_kont in pick_object g t na
          end
      | SGCjeopardized ->
          (*
                    let gp = old_path_excl_from_to g [] pos base 13 in
                    let ds = drop_scare ds (DSreturn_to_base gp) in
                    let na = NAdrop_scare_and_kill ds in
                    (Coth ' ', na, None)
          *)
          failwith "NAgo_in_corridor_and_hit 7"
      | SGChome -> Coth ' ', ce.ce_kont, t.t_prev_mov
      end
  | NAgo_to_door gp ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAgo_to_door gp in Coth ' ', na, t.t_prev_mov
        end
      else if pos = gp.tpos then
        if conditions_for_dropping_scare g then ok_for_dropping_scare g t
        else start_move_in_corridor g t pos
      else if g.held then
        if g.blind then random_move g pos NAnone
        else
          let (monl, movl) = monsters_and_moves_around g in
          let na = NAgo_to_door gp in attack_monsters g t movl monl na
      else
        begin match
          if g.attacked > 0 then
            let (monl, movl) = monsters_and_moves_around g in
            if monl <> [] then Some (monl, movl) else None
          else None
        with
          Some (monl, movl) ->
            let na = NAgo_to_door gp in attack_monsters g t movl monl na
        | None ->
            let na = NAgo_to_door gp in
            match treat_moving_monsters_at_one_move g t pos na with
              Some x -> x
            | None ->
                if pos = gp.epos then
                  begin
                    tempo g 0.1;
                    match gp.path with
                      pos1 :: path ->
                        let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                        let na = NAgo_to_door gp in
                        move_command3 g pos pos1 na
                    | [] -> assert false
                  end
                else Coth ' ', NAnone, t.t_prev_mov
        end
  | NAgo_to_shelter_and_test_scrolls gp ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAgo_to_shelter_and_test_scrolls gp in
          Coth ' ', na, t.t_prev_mov
        end
      else if g.held then
        let (monl, movl) = monsters_and_moves_around g in
        let na = NAgo_to_shelter_and_test_scrolls gp in
        attack_monsters g t movl monl na
      else if pos = gp.tpos then
        begin
          tempo g 0.1;
          set_regrets g;
          match unidentified_scroll_in_pack g.pack with
            Some (ch, _) -> wear_armor_and_read g t ch
          | None -> failwith "NAgo_to_shelter_and_test_scrolls 3"
        end
      else if pos = gp.epos then
        match gp.path with
          epos :: path ->
            let gp = {epos = epos; tpos = gp.tpos; path = path} in
            let na = NAgo_to_shelter_and_test_scrolls gp in
            move_command2 g pos epos na
        | [] -> assert false
      else
        begin match path_excl_from_to g [] pos gp.tpos with
          Some gp ->
            let na = NAgo_to_shelter_and_test_scrolls gp in
            Coth ' ', na, t.t_prev_mov
        | None ->
            (* probably teleported and no access *)
            Coth ' ', NAnone, t.t_prev_mov
        end
  | NAmove (mov, na) -> Cmov mov, na, t.t_prev_mov
  | NAput_ring (ch, na, step) ->
      begin match step with
        1 -> let na = NAput_ring (ch, na, 2) in Coth ch, na, t.t_prev_mov
      | 2 -> let na = NAput_ring (ch, na, 3) in Cansw_left, na, t.t_prev_mov
      | 3 ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAput_ring (ch, na, 3) in
              g.ring_of_slow_digestion_on_hand <- Some ch;
              Coth ' ', na, t.t_prev_mov
            end
          else Coth ' ', na, t.t_prev_mov
      | step -> failwith (sprintf "NAput_ring %d" step)
      end
  | NAread_scroll_for_best_armor (ch_scr, prev_a, state) ->
      begin match state with
        WStoken_off ->
          if message <> "" then
            begin
              tempo g 1.0;
              if transl.is_message_cursed message then g.armor_cursed <- true;
              let ws = WStoken_off in
              let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
              Coth ' ', na, t.t_prev_mov
            end
          else if g.armor_cursed then
            let ws = WSscroll_read in
            let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
            let uo = UOread_scroll (ch_scr, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
          else
            let ws = WSwear_what in
            let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
            Coth 'W', na, t.t_prev_mov
      | WSwear_what ->
          begin match best_armor g with
            Some (ch, _) ->
              let ws = WSarmor_worn ch in
              let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
              Coth ch, na, t.t_prev_mov
          | None -> assert false
          end
      | WSarmor_worn ch_arm ->
          if message <> "" then
            begin
              tempo g 1.0;
              g.worn_armor <- armor_of_ch g ch_arm;
              let ws = WSarmor_worn ch_arm in
              let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
              Coth ' ', na, t.t_prev_mov
            end
          else
            let ws = WSscroll_read in
            let na = NAread_scroll_for_best_armor (ch_scr, prev_a, ws) in
            let uo = UOread_scroll (ch_scr, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
      | WSscroll_read ->
          if wearing_good_armor g || g.armor_cursed then
            Coth ' ', prev_a, t.t_prev_mov
          else
            match good_armor g with
              Some (ch, _) ->
                begin match g.worn_armor with
                  Some _ -> Coth 'T', NAwear (ch, 1, prev_a), t.t_prev_mov
                | None -> Coth 'W', NAwear (ch, 2, prev_a), t.t_prev_mov
                end
            | None -> assert false
      end
  | NArestore_health na ->
      g.hist_dung <- [];
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NArestore_health na in Coth ' ', na, t.t_prev_mov
        end
      else if health_is_maximum g then Coth ' ', na, t.t_prev_mov
      else if g.attacked > 0 then
        let (monl, movl) = monsters_and_moves_around g in
        if monl <> [] then attack_monsters g t movl monl na
        else let na = NArestore_health na in Coth '.', na, t.t_prev_mov
      else
        begin
          tempo g 0.1;
          let na = NArestore_health na in Coth '.', na, t.t_prev_mov
        end
  | NAreturn_to_base (gp, akont) ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAreturn_to_base (gp, akont) in Coth ' ', na, None
        end
      else if pos = gp.tpos then Coth ' ', akont, t.t_prev_mov
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            match
              if g.level < level_of_very_mean_monsters then None
              else treat_moving_monsters_at_one_move2 g t
            with
              Some x -> x
            | None ->
                if pos = gp.epos then
                  begin
                    tempo g 0.1;
                    match gp.path with
                      [_] -> move_command2 g pos gp.tpos akont
                    | _ ->
                        let (monl, _) =
                          if g.level < level_of_faster_monsters then [], []
                          else monsters_and_moves_around g
                        in
                        match monl with
                          mov :: _ ->
                            let jeopardized = test_jeopardized g t monl in
                            begin match
                              if g.confused && jeopardized &&
                                 List.exists
                                   (fun mov ->
                                      let pos1 = add_mov pos mov in
                                      not
                                        (List.mem_assoc pos1
                                           g.frozen_monsters))
                                   monl
                              then
                                scroll_of_hold_monsters_in_pack g.pack
                              else None
                            with
                              Some (ch, _) ->
                                (*
                                let _ = failwith "6" in
                                                    let ds = drop_scare ds (DSreturn_to_base gp) in
                                                    let na = NAdrop_scare_and_kill ds in
                                                    let uo = UOread_scroll ch RSread_what in
                                                    let na = NAuse_object uo na in
                                                    (Coth 'r', na, None)
                                *)
                                failwith "NAreturn_to base 4.1"
                            | None ->
                                if jeopardized &&
                                   List.mem_assoc (add_mov pos mov)
                                     g.frozen_monsters
                                then
                                  match gp.path with
                                    pos1 :: path ->
                                      let gp =
                                        {epos = pos1; tpos = gp.tpos;
                                         path = path}
                                      in
                                      let na = NAreturn_to_base (gp, akont) in
                                      move_command3 g pos gp.epos na
                                  | [] -> assert false
                                else
                                  let na = NAreturn_to_base (gp, akont) in
                                  move_command3 g pos (add_mov pos mov) na
                            end
                        | [] ->
                            match gp.path with
                              pos1 :: path ->
                                let gp =
                                  {epos = pos1; tpos = gp.tpos; path = path}
                                in
                                let na = NAreturn_to_base (gp, akont) in
                                move_command3 g pos pos1 na
                            | [] -> assert false
                  end
                else if g.attacked > 0 || g.held then
                  let na = NAreturn_to_base (gp, akont) in
                  attacked_or_held g t na
                else
                  let pred _ = (=) gp.tpos in
                  match direct_path_excl g [] pos pred with
                    Some (path, _) ->
                      let gp = {epos = pos; tpos = gp.tpos; path = path} in
                      let na = NAreturn_to_base (gp, akont) in
                      pick_object g t na
                  | None -> let na = NAnone in Coth ' ', na, None
        end
  | NAseek_object gp ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = if pos = gp.tpos then NAnone else NAseek_object gp in
          Coth ' ', na, None
        end
      else if pos = gp.tpos then let na = NAnone in Coth ' ', na, None
      else if g.held then
        let (monl, movl) = monsters_and_moves_around g in
        let na = NAseek_object gp in attack_monsters g t movl monl na
      else
        let (monl, movl) = monsters_and_moves_around g in
        let monl =
          List.filter
            (fun mov ->
               is_attackable_monster g (dung_char g.dung (add_mov pos mov)))
            monl
        in
        if monl <> [] then attack_monsters g t movl monl NAnone
        else
          begin match
            if g.attacked_by_flame > 0 then
              match flaming_monster_dir g pos with
                Some (mdir, _, _) ->
                  begin match usable_anti_flamer_wand_in_pack g.pack with
                    Some (ch, _) -> Some (mdir, ch)
                  | None -> None
                  end
              | None -> None
            else None
          with
            Some (mdir, ch) ->
              let na = NAseek_object gp in
              let na = NAzap (mdir, ch, na, 1) in Coth 'z', na, None
          | None ->
              if pos = gp.epos then
                begin
                  tempo g 0.1;
                  match gp.path with
                    pos1 :: path ->
                      let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                      let na = NAseek_object gp in move_command2 g pos pos1 na
                  | [] -> Coth ' ', NAnone, None
                end
              else
                match common_room_with g gp.tpos with
                  Some room ->
                    let gp = path_in_room_excl_mon g room pos gp.tpos in
                    let na = NAseek_object gp in
                    move_command2 g pos gp.epos na
                | None -> Coth ' ', NAnone, None
          end
  | NAstring (str, skip_mess, na) ->
      if skip_mess && message <> "" then
        begin
          tempo g 1.0;
          let na = NAstring (str, skip_mess, na) in Coth ' ', na, t.t_prev_mov
        end
      else
        let ch = str.[0] in
        let na =
          if String.length str = 1 then na
          else
            let str = String.sub str 1 (String.length str - 1) in
            NAstring (str, skip_mess, na)
        in
        Coth ch, na, t.t_prev_mov
  | NAtest_monster (tml, na) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAtest_monster (tml, na) in Coth ' ', na, t.t_prev_mov
        end
      else
        let pos = rogue_pos g in
        begin match moving_monsters_at_one_move g t pos with
          Some x -> move_against_monster g t pos x tml na
        | None -> Coth ' ', na, t.t_prev_mov
        end
  | NAtest_potions (ch, step) ->
      begin match step with
        1 ->
          if transl.is_message_nothing_appropriate message then
            begin
              (* probably stolen by a nymph *)
              tempo g 1.0;
              let (nb, obj) = List.assoc ch g.pack in
              remove_from_pack g ch nb obj;
              let na = NAtest_potions (ch, 3) in Coth ' ', na, t.t_prev_mov
            end
          else let na = NAtest_potions (ch, 2) in Coth ch, na, t.t_prev_mov
      | 2 ->
          tempo g 1.0;
          if transl.is_message_there_is_no message then
            let (nb, obj) = List.assoc ch g.pack in
            remove_from_pack g ch nb obj;
            let na = NAtest_potions (ch, 3) in Coth ' ', na, t.t_prev_mov
          else
            let (nb, _) = List.assoc ch g.pack in
            let obj = Ppotion (quaffed_potion_of_message g message) in
            remove_from_pack g ch nb obj;
            let na = NAtest_potions (ch, 3) in Coth ' ', na, t.t_prev_mov
      | 3 ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAtest_potions (ch, 3) in Coth ' ', na, t.t_prev_mov
            end
          else
            begin match unidentified_potion_in_pack g.pack with
              Some (ch, _) ->
                let na = NAtest_potions (ch, 1) in Coth 'q', na, t.t_prev_mov
            | None -> let na = NAnone in pick_object g t na
            end
      | _ -> failwith "not impl test_potions"
      end
  | NAthrow_away (ch, step) ->
      begin match step with
        "direction" ->
          let na = NAthrow_away (ch, "object") in Coth 'h', na, t.t_prev_mov
      | "object" ->
          let (nb, obj) = List.assoc ch g.pack in
          remove_from_pack g ch nb obj;
          let na = NAnone in Coth ch, na, t.t_prev_mov
      | step -> failwith (sprintf "NAthrow_away '%s'" step)
      end
  | NAthrow_in_the_garbage (ch, gp, prev_a, step) ->
      let pos = rogue_pos g in
      begin match step with
        "move" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAthrow_in_the_garbage (ch, gp, prev_a, "move") in
              Coth ' ', na, t.t_prev_mov
            end
          else
            begin match treat_critical_situation g t t.t_next_action with
              Some t -> t
            | None ->
                match treat_moving_monsters_at_one_move2 g t with
                  Some x -> x
                | None ->
                    if pos = gp.tpos then
                      let na =
                        NAthrow_in_the_garbage (ch, gp, prev_a, "drop")
                      in
                      Coth 'd', na, None
                    else if pos = gp.epos then
                      match gp.path with
                        pos1 :: path2 ->
                          let gp =
                            {epos = pos1; tpos = gp.tpos; path = path2}
                          in
                          let na =
                            NAthrow_in_the_garbage (ch, gp, prev_a, "move")
                          in
                          move_command3 g pos pos1 na
                      | [] -> assert false
                    else
                      let (monl, movl) = monsters_and_moves_around g in
                      if monl <> [] then
                        let na =
                          NAthrow_in_the_garbage (ch, gp, prev_a, "move")
                        in
                        attack_monsters g t movl monl na
                      else throw_in_the_garbage g t pos ch
            end
      | "drop" ->
          if transl.is_message_something_there message then
            throw_in_the_garbage g t pos ch
          else
            begin
              g.garbage <- gp.tpos :: g.garbage;
              g.on_something_at <- Some (pos, false);
              let (nb, obj) = List.assoc ch g.pack in
              remove_from_pack g ch nb obj;
              let na = NAthrow_in_the_garbage (ch, gp, prev_a, "dropped") in
              Coth ch, na, t.t_prev_mov
            end
      | "dropped" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAthrow_in_the_garbage (ch, gp, prev_a, "dropped") in
              Coth ' ', na, t.t_prev_mov
            end
          else
            let (monl, movl) = monsters_and_moves_around g in
            if monl <> [] then
              let na = NAthrow_in_the_garbage (ch, gp, prev_a, "dropped") in
              attack_monsters g t movl monl na
            else
              begin match find_unuseful_object_when_full_pack g with
                Some (ch, (nb, obj)) -> throw_in_the_garbage g t pos ch
              | None -> Coth ' ', prev_a, t.t_prev_mov
              end
      | step -> failwith (sprintf "NAthrow_in_the_garbage '%s'" step)
      end
  | NAuse_object (uo, na) ->
      let uoo = use_object g t message na uo in
      let (ch, na) =
        match uoo with
          Some (ch, na) -> ch, na
        | None -> ' ', na
      in
      Coth ch, na, t.t_prev_mov
  | NAwear (ch, step, na) ->
      begin match step with
        1 ->
          if transl.is_message_cursed message then
            begin
              tempo g 1.0;
              g.armor_cursed <- true;
              Coth ' ', na, t.t_prev_mov
            end
          else if message <> "" then
            begin tempo g 1.0; Coth ' ', NAwear (ch, 1, na), t.t_prev_mov end
          else
            begin
              g.worn_armor <- None;
              if ch = ' ' then Coth ' ', na, t.t_prev_mov
              else Coth 'W', NAwear (ch, 2, na), t.t_prev_mov
            end
      | 2 -> Coth ch, NAwear (ch, 3, na), t.t_prev_mov
      | 3 ->
          tempo g 1.0;
          if transl.is_message_there_is_no message then
            begin
              (* probably stolen by a nymph *)
              g.pack <- List.remove_assoc ch g.pack;
              match good_armor g with
                Some (ch, _) -> Coth 'W', NAwear (ch, 2, na), t.t_prev_mov
              | None -> Coth 's', na, t.t_prev_mov
            end
          else
            let ar =
              match snd (List.assoc ch g.pack) with
                Parmor ar -> ar
              | _ -> assert false
            in
            let ar = {ar with ar_value = armor_value message} in
            g.worn_armor <- Some (ch, ar);
            redefine_in_pack g ch (Parmor ar);
            Coth ' ', na, t.t_prev_mov
      | step -> failwith (sprintf "NAwear step %d" step)
      end
  | NAwear_armor_and_test_scrolls (ch_arm, ch_scr, state) ->
      begin match state with
        WStoken_off ->
          if message <> "" then
            begin
              tempo g 1.0;
              if transl.is_message_cursed message then g.armor_cursed <- true;
              let ws = WStoken_off in
              let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
              Coth ' ', na, t.t_prev_mov
            end
          else if g.armor_cursed then
            let ws = WSscroll_read in
            let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
            let uo = UOread_scroll (ch_scr, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
          else
            let ws = WSwear_what in
            let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
            g.worn_armor <- None; Coth 'W', na, t.t_prev_mov
      | WSwear_what ->
          let ws = WSarmor_worn ch_arm in
          let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
          Coth ch_arm, na, t.t_prev_mov
      | WSarmor_worn ch_arm ->
          if message <> "" then
            begin
              tempo g 1.0;
              g.worn_armor <- armor_of_ch g ch_arm;
              let ws = WSarmor_worn ch_arm in
              let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
              Coth ' ', na, t.t_prev_mov
            end
          else
            let ws = WSscroll_read in
            let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
            let uo = UOread_scroll (ch_scr, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
      | WSscroll_read ->
          if message <> "" then
            begin
              tempo g 1.0;
              let ws = WSscroll_read in
              let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
              Coth ' ', na, t.t_prev_mov
            end
          else continue_test_scrolls g t ch_arm t.t_next_action
      end
  | NAmove_in_corridor (ipos, gp, trail) ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAmove_in_corridor (ipos, gp, trail) in
          Coth ' ', na, t.t_prev_mov
        end
      else if is_at_door g pos then
        begin
          tempo g 0.2;
          if pos <> ipos then
            List.iter
              (fun pos ->
                 let r = pos.row, pos.col, pos.row, pos.col in
                 match gen_room_row r, gen_room_col r with
                   Some rr, Some rc -> g.visited.(rr).(rc) <- true
                 | _ -> ())
              trail;
          Coth ' ', NAnone, t.t_prev_mov
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if g.held then
              let na = NAmove_in_corridor (ipos, gp, trail) in
              let (monl, movl) = monsters_and_moves_around g in
              if monl <> [] then attack_monsters g t movl monl na
              else Coth 's', na, t.t_prev_mov
            else
              begin
                tempo g 0.1;
                let trail = if pos = gp.epos then pos :: trail else trail in
                let (monl, movl) = monsters_and_moves_around g in
                if monl <> [] then
                  let na = NAmove_in_corridor (ipos, gp, trail) in
                  attack_monsters g t movl monl na
                else if g.pack_full then manage_full_pack g t
                else
                  match
                    if g.hallucinated then None else object_around g pos
                  with
                    Some mov ->
                      let na = NAmove_in_corridor (ipos, gp, trail) in
                      move_command3 g pos (add_mov pos mov) na
                  | None ->
                      if g.held then Coth 's', NAnone, None
                      else if pos = gp.tpos then
                        match movl with
                          [] -> assert false
                        | [mov] ->
                            if nothing_to_search_in_dung g then
                              start_move_in_corridor g t pos
                            else
                              let from = mov in
                              let around = around_pos g pos in
                              let na = NAsearch_and_back (from, around, 1) in
                              Coth 's', na, None
                        | [mov1; mov2] ->
                            let mov =
                              match t.t_prev_mov with
                                Some mov ->
                                  if mov1 = opposite_move mov then mov2
                                  else mov1
                              | None ->
                                  if random_int g 2 = 0 then mov2 else mov1
                            in
                            let tpos = add_mov pos mov in
                            let gp = {epos = tpos; tpos = tpos; path = []} in
                            let na = NAmove_in_corridor (ipos, gp, trail) in
                            move_command3 g pos tpos na
                        | movl ->
                            let ipos = pos in
                            continue_move_in_corridor g t ipos pos trail
                      else if pos = gp.epos then
                        match gp.path with
                          pos1 :: path ->
                            if monster_moving_to g t pos1 then
                              let na = NAmove_in_corridor (ipos, gp, trail) in
                              Coth '.', na, None
                            else
                              let gp =
                                {epos = pos1; tpos = gp.tpos; path = path}
                              in
                              let na = NAmove_in_corridor (ipos, gp, trail) in
                              move_command3 g pos pos1 na
                        | [] -> failwith "not impl NAmove_in_corridor 15"
                      else if g.confused then
                        let na = NAstring ("3.", false, NAnone) in
                        Coth '3', na, None
                      else start_move_in_corridor g t pos
              end
        end
  | NAseek_gold_or_monster (gp, gold) ->
      let pos = rogue_pos g in
      let na = NAseek_gold_or_monster (gp, gold) in
      if message <> "" then begin tempo g 1.0; Coth ' ', na, None end
      else if
        g.attacked > 0 && on_scare_monster g && t.t_prev_mov <> None
      then
        let ds = start_drop_scare pos ' ' in
        let ds = drop_scare ds (DSdropped 0) in
        let na = NAdrop_scare_and_kill ds in Coth ' ', na, None
      else if g.attacked > 0 || g.held then attacked_or_held g t na
      else
        begin match treat_moving_monsters_at_one_move g t pos na with
          Some x -> x
        | None ->
            if g.pack_full then manage_full_pack g t
            else if pos = gp.epos && pos <> gp.tpos then
              begin
                tempo g 0.1;
                match gp.path with
                  pos1 :: path ->
                    let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                    let na = NAseek_gold_or_monster (gp, gold) in
                    move_command3 g pos pos1 na
                | [] -> failwith "NAseek_gold_or_monster 2"
              end
            else
              match
                if gold then path_to_closest_gold g t pos
                else path_to_closest_static_monster g t pos
              with
                Some gp ->
                  let na = NAseek_gold_or_monster (gp, gold) in
                  Coth ' ', na, None
              | None ->
                  let graph = make_graph g false in
                  let sp = stairs_pos g in
                  if sp <> [] then
                    let spos = List.nth sp (random_int g (List.length sp)) in
                    go_to_stairs g t graph pos spos false
                  else start_search g t graph
        end
  | NAgo_to gp ->
      let pos = rogue_pos g in
      if message <> "" then
        begin tempo g 1.0; let na = NAgo_to gp in Coth ' ', na, None end
      else if pos = gp.tpos then begin tempo g 1.0; Coth ' ', NAnone, None end
      else
        begin
          tempo g 0.1;
          if g.attacked > 0 || g.held then
            let na = NAgo_to gp in attacked_or_held g t na
          else
            match
              let na = NAgo_to gp in
              treat_moving_monsters_at_one_move g t pos na
            with
              Some x -> x
            | None ->
                if pos = gp.epos then
                  match gp.path with
                    epos :: path ->
                      let gp = {epos = epos; tpos = gp.tpos; path = path} in
                      let na = NAgo_to gp in move_command3 g pos epos na
                  | [] -> assert false
                else
                  begin
                    tempo g 0.5;
                    match
                      direct_path_excl g [] pos (fun _ pos -> pos = gp.tpos)
                    with
                      Some (path, _) ->
                        let gp = {epos = pos; tpos = gp.tpos; path = path} in
                        let na = NAgo_to gp in Coth ' ', na, None
                    | None -> failwith "eh merde"
                  end
        end
  | NAgo_to_stairs (gp, strict) ->
      let pos = rogue_pos g in
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAgo_to_stairs (gp, strict) in Coth ' ', na, None
        end
      else if
        on_scare_monster g && g.attacked_by_flame = 0 &&
        not (health_is_maximum g)
      then
        let na = NAgo_to_stairs (gp, strict) in Coth '.', na, None
      else if pos = gp.tpos then begin tempo g 1.0; Coth '>', NAnone, None end
      else
        begin
          tempo g 0.1;
          let dl =
            if strict then [] else doors_not_explorated_in_current_room g
          in
          if g.attacked > 0 || g.held then
            let na = NAgo_to_stairs (gp, strict) in attacked_or_held g t na
          else if
            g.pack_full && g.mon_detected && number_of_monsters g < 10
          then
            manage_full_pack g t
          else if dl = [] then
            match
              match common_room_with g gp.tpos with
                Some room ->
                  let list_obj_ch = interesting_objects g in
                  dist_to_closest g room pos
                    (fun ch dist ->
                       if ch = '*' || is_gold_seeker_monster g ch then true
                       else
                         not g.pack_full &&
                         not (List.mem (add_mov pos dist) g.garbage) &&
                         List.mem ch list_obj_ch)
              | None -> None
            with
              Some dist ->
                let tpos = add_mov pos dist in
                let (pos1, path) = one_step_to2 g tpos in
                (*
                            let (pos1, path) =
                              let room = current_room g pos in
                              circle_trap_and_mon_if_possible2 g room pos pos1 path
                            in
                *)
                let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                let na = NAgo_to_stairs (gp, strict) in
                move_command3 g pos pos1 na
            | None ->
                if pos = gp.epos && gp.path <> [] then
                  let na = NAgo_to_stairs (gp, strict) in
                  match treat_moving_monsters_at_one_move g t pos na with
                    Some x -> x
                  | None ->
                      match gp.path with
                        pos1 :: path ->
                          let gp =
                            {epos = pos1; tpos = gp.tpos; path = path}
                          in
                          let na = NAgo_to_stairs (gp, strict) in
                          move_command3 g pos pos1 na
                      | [] -> assert false
                else
                  let graph = make_graph g false in
                  go_to_stairs g t graph pos gp.tpos strict
          else
            let len = List.length dl in
            let (tpos, _) = List.nth dl (random_int g len) in
            if pos = tpos then start_move_in_corridor g t pos
            else
              match common_room_with g tpos with
                Some room ->
                  let gp = path_in_room_excl_mon g room pos tpos in
                  let na = NAgo_to_door gp in move_command3 g pos gp.epos na
              | None -> assert false
        end
  | NAgo_unblock_monster um ->
      let pos = rogue_pos g in
      let gp = um.um_gp in
      let base = um.um_base in
      let mpos = um.um_mpos in
      let akont = um.um_kont in
      if message <> "" then
        begin
          tempo g 0.5;
          let na = NAgo_unblock_monster um in Coth ' ', na, None
        end
      else if g.confused && g.attacked = 0 then
        let na = NAgo_unblock_monster um in
        let na = NAstring ("33.", false, na) in Coth ' ', na, t.t_prev_mov
      else if g.attacked = 0 && not (health_is_maximum g) then
        let gp = old_path_excl_from_to g [] pos base 20 in
        let na = return_to_base g gp akont in Coth ' ', na, t.t_prev_mov
      else if pos = gp.tpos then
        if not (on_scare_monster g) then
          let gp = old_path_excl_from_to g [] pos base 14 in
          let na = return_to_base g gp akont in pick_object g t na
        else if is_monster (dung_char g.dung mpos) then
          let na =
            match um.um_kont with
              NAdrop_scare_and_kill ds ->
                let ds = {ds with ds_monster_perhaps_blocked = None} in
                NAdrop_scare_and_kill ds
            | NAalone_in_room ar ->
                let dl =
                  List.map
                    (fun ard ->
                       if ard.ard_pos = base then
                         {ard with ard_monster_perhaps_blocked = None}
                       else ard)
                    ar.ar_doors
                in
                let ar = {ar with ar_doors = dl} in NAalone_in_room ar
            | _ -> assert false
          in
          let um = {um with um_kont = na} in
          let na = NAgo_unblock_monster um in move_command2 g pos mpos na
        else
          let gp = old_path_excl_from_to g [] pos base 15 in
          if g.level < level_of_faster_monsters ||
             can_return_without_attack g mpos gp
          then
            let na = return_to_base g gp um.um_kont in pick_object g t na
          else
            let ipos = base in
            let (block, na) =
              match um.um_kont with
                NAdrop_scare_and_kill ds ->
                  let perhaps_blocked = ds.ds_monster_perhaps_blocked in
                  let (block, mpb) =
                    unblocking_monster g ipos perhaps_blocked
                  in
                  let ds = {ds with ds_monster_perhaps_blocked = mpb} in
                  let na = NAdrop_scare_and_kill ds in block, na
              | NAalone_in_room ar ->
                  let (block, dl) =
                    List.fold_right
                      (fun ard (block, dl) ->
                         if base = ard.ard_pos then
                           let ipos = ard.ard_pos in
                           let perhaps_blocked =
                             ard.ard_monster_perhaps_blocked
                           in
                           let (block, mpb) =
                             unblocking_monster g ipos perhaps_blocked
                           in
                           let ard =
                             {ard with ard_monster_perhaps_blocked = mpb}
                           in
                           block, ard :: dl
                         else block, ard :: dl)
                      ar.ar_doors (None, [])
                  in
                  let ar = {ar with ar_doors = dl} in
                  let na = NAalone_in_room ar in block, na
              | _ -> assert false
            in
            let um = {um with um_kont = na} in
            match block with
              Some (gp, mpos) ->
                let um = {um with um_gp = gp; um_mpos = mpos} in
                let na = NAgo_unblock_monster um in Coth ' ', na, t.t_prev_mov
            | None ->
                let na = NAgo_unblock_monster um in Coth '.', na, t.t_prev_mov
      else if pos = gp.epos then
        match gp.path with
          epos :: path ->
            tempo g 0.1;
            let gp = {epos = epos; tpos = gp.tpos; path = path} in
            let um = {um with um_gp = gp} in
            let na = NAgo_unblock_monster um in
            let mov = move_between pos epos in
            let na = NAmove (mov, na) in Coth 'm', na, t.t_prev_mov
        | [] -> assert false
      else
        let gp = old_path_excl_from_to g [] pos gp.tpos 25 in
        let um = {um with um_gp = gp} in
        let na = NAgo_unblock_monster um in Coth ' ', na, None
  | NAlet_come (mch, move) ->
      tempo g 1.0;
      let pos = rogue_pos g in
      let (monl, movl) = monsters_and_moves_around g in
      let monl =
        List.filter
          (fun mov ->
             let ch_mon = dung_char g.dung (add_mov pos mov) in
             ch_mon = mch ||
             not (is_not_attackable_monster g ch_mon) &&
             not (is_mean_monster g ch_mon))
          monl
      in
      if monl = [] then move_command3 g pos (add_mov pos move) NAnone
      else attack_monsters g t movl monl NAnone
  | NAfight (mch, answered, prev_a) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAfight (mch, true, prev_a) in Coth ' ', na, t.t_prev_mov
        end
      else
        begin match treat_critical_situation g t t.t_next_action with
          Some r -> r
        | None ->
            if not g.confused && not answered then
              let na = NAfight (mch, false, prev_a) in
              Coth 's', na, t.t_prev_mov
            else
              let pos = rogue_pos g in
              let in_room = current_room_possibly_at_door g pos <> None in
              let (nmon, nmov) = monsters_and_moves_around g in
              let nmon =
                List.filter
                  (fun mov ->
                     let mch = dung_char g.dung (add_mov pos mov) in
                     not (is_not_attackable_monster g mch))
                  nmon
              in
              if nmon = [] then
                match prev_a with
                  NAalone_in_room _ | NAdrop_scare_and_kill _ |
                  NAglobal_search1 (_, _) | NAglobal_search2 (_, _, _) |
                  NAgo_to _ | NAgo_to_door _ | NAgo_to_stairs (_, _) |
                  NArestore_health _ | NAreturn_to_base (_, _) |
                  NAseek_gold_or_monster (_, _) | NAseek_object _ |
                  NAthrow_in_the_garbage (_, _, _, _) |
                  NAwear_armor_and_test_scrolls (_, _, _) ->
                    Coth ' ', prev_a, t.t_prev_mov
                | NAnone ->
                    if in_room then Coth ' ', NAnone, t.t_prev_mov
                    else
                      begin match
                        let pred _ pos = List.mem pos g.regrets in
                        path_to_closest2 g pos pred
                      with
                        Some gp ->
                          let na = NAseek_object gp in
                          Coth ' ', na, t.t_prev_mov
                      | None ->
                          match t.t_prev_mov with
                            Some mov ->
                              let mov = opposite_move mov in
                              move_in_corridor_starting_with_move g pos mov
                          | None -> Coth ' ', NAnone, t.t_prev_mov
                      end
                | NAmove_in_corridor (_, _, trail) ->
                    continue_move_in_corridor g t pos pos trail
                | _ -> failwith (not_impl "prev_a" prev_a)
              else attack_monsters g t nmov nmon prev_a
        end
  | NArun_away (mch, room_path, prev_a) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NArun_away (mch, room_path, prev_a) in
          Coth ' ', na, t.t_prev_mov
        end
      else if t.t_on_stairs then begin tempo g 1.0; Coth '>', NAnone, None end
      else
        let pos = rogue_pos g in
        let in_room = g.rogue_room_and_door <> None in
        let (nmon, nmov) = monsters_and_moves_around g in
        if nmon = [] then
          match prev_a with
            NAwear_armor_and_test_scrolls (_, _, _) |
            NAthrow_in_the_garbage (_, _, _, _) | NAglobal_search1 (_, _) |
            NAglobal_search2 (_, _, _) ->
              Coth ' ', prev_a, t.t_prev_mov
          | NAnone ->
              if in_room then Coth ' ', NAnone, t.t_prev_mov
              else
                begin match t.t_prev_mov with
                  Some mov -> move_in_corridor_starting_with_move g pos mov
                | None -> start_move_in_corridor g t pos
                end
          | NAmove_in_corridor (_, _, trail) ->
              continue_move_in_corridor g t pos pos trail
          | _ -> failwith (not_impl "prev_a 1" prev_a)
        else
          begin match room_path with
            mov :: path ->
              tempo g 0.1;
              let na = NArun_away (mch, path, NAnone) in
              let tpos = add_mov pos mov in move_command3 g pos tpos na
          | [] -> attack_monsters g t nmov nmon prev_a
          end
  | NAzap (mov, ch, na, step) ->
      begin match step with
        1 ->
          let ch_dir = basic_command_of_move mov in
          let na = NAzap (mov, ch, na, 2) in Coth ch_dir, na, None
      | 2 -> let na = NAzap (mov, ch, na, 3) in Coth ch, na, None
      | 3 ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAzap (mov, ch, na, 3) in Coth ' ', na, None
            end
          else
            begin
              begin match List.assoc ch g.pack with
                _, Pwand (Iwand (wk, Some n)) ->
                  if n > 0 then
                    let obj = Pwand (Iwand (wk, Some (n - 1))) in
                    redefine_in_pack g ch obj
                  else assert false
              | _ -> assert false
              end;
              Coth ' ', na, t.t_prev_mov
            end
      | step -> failwith (sprintf "NAzap step %d not impl" step)
      end
  | NAcheck_no_trap (pos, pos1) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAcheck_no_trap (pos, pos1) in Coth ' ', na, None
        end
      else if rogue_pos g = pos1 then
        begin tempo g 0.5; move_command3 g pos1 pos NAnone end
      else Coth ' ', NAnone, t.t_prev_mov
  | NAdrop_scare (ch_opt, na) ->
      let pos = rogue_pos g in
      if g.ring_of_slow_digestion_on_hand <> None then g.hist_dung <- [];
      begin match ch_opt with
        Some ch ->
          if transl.is_message_something_there message then
            begin
              g.on_something_at <- Some (pos, false);
              match scroll_of_hold_monsters_in_pack g.pack with
                Some (ch, _) ->
                  let na = NAdrop_scare (None, na) in
                  let uo = UOread_scroll (ch, RSread_what) in
                  let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
              | None -> random_move g pos na
            end
          else
            let (nb, obj) = List.assoc ch g.pack in
            remove_from_pack g ch nb obj;
            g.garbage <- pos :: g.garbage;
            g.scare_pos <- pos :: g.scare_pos;
            g.on_something_at <- Some (pos, true);
            let na = NAdrop_scare (None, na) in Coth ch, na, t.t_prev_mov
      | None ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAdrop_scare (None, na) in Coth ' ', na, t.t_prev_mov
            end
          else if g.attacked_by_flame > 0 || health_is_maximum g then
            match na with
              NAalone_in_room ar ->
                (*
                                let base = (List.hd ar.ar_doors).ard_pos in
                                let gp =
                                  let excl =
                                    let monl = monsters_around g pos in
                                    List.map (add_mov pos) monl
                                  in
                                  match path_excl_from_to g excl pos base with
                                  [ Some gp -> gp
                                  | None -> failwith "faich 1" ]
                                in
                                let ars = ARgo_to_door True gp in
                                let na = NAalone_in_room ar in
                                match gp.path with
                                [ [pos1 :: path] -> do {
                                    let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                                    move_command3 g pos gp.epos na
                                  }
                                | [] -> do {
                                    (Coth ' ', na, t.t_prev_mov)
                                  } ]
                *)
                let na = NAalone_in_room ar in Coth ' ', na, t.t_prev_mov
            | NAdrop_scare_and_kill ds ->
                let base = ds.ds_base in
                let gp = old_path_excl_from_to g [] pos base 1 in
                let na = ds_return_to_base g ds gp in Coth ' ', na, None
            | _ -> assert false
          else let na = NAdrop_scare (None, na) in Coth '.', na, t.t_prev_mov
      end
  | NAdrop_scare_and_kill ds -> drop_scare_and_kill g t message ds
  | NAalone_in_room ar -> alone_in_room g t message ar
  | NAwield_bow_test_moving (mpos, mch, step) ->
      begin match step with
        1 ->
          if message <> "" && transl.is_message_cursed message then
            begin
              tempo g 1.0;
              g.weapon_cursed <- true;
              let na = NAwield_bow_test_moving (mpos, mch, 2) in
              Coth ' ', na, None
            end
          else
            begin match short_bow_in_pack g.pack with
              Some (ch, _) ->
                let na = NAwield_bow_test_moving (mpos, mch, 2) in
                Coth ch, na, None
            | None -> failwith "no bow"
            end
      | 2 ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAwield_bow_test_moving (mpos, mch, 2) in
              Coth ' ', na, None
            end
          else
            let pos = rogue_pos g in
            if dung_char g.dung mpos <> mch then
              let mpos_list =
                let rec loop list =
                  function
                    k :: kl ->
                      let pos = add_mov mpos (mov_of_k k) in
                      let list =
                        if in_dung g pos && dung_char g.dung pos = mch then
                          pos :: list
                        else list
                      in
                      loop list kl
                  | [] -> list
                in
                loop [] run_around_list
              in
              match mpos_list with
                [mpos] ->
                  if mpos.row = pos.row || mpos.col = pos.col then
                    let na = NAmove_throw2 (mpos, mch, "direction") in
                    Coth 't', na, None
                  else
                    begin
                      tempo g 0.5;
                      let na = NAwield_bow_test_moving (mpos, mch, 2) in
                      Coth '.', na, None
                    end
              | _ ->
                  let ch = g.main_sword in
                  let uo = UOwield_sword (ch, "wield what") in
                  let na = NAuse_object (uo, NAnone) in Coth 'w', na, None
            else
              let room =
                match current_room_possibly_at_door g pos with
                  Some room -> room
                | None -> assert false
              in
              attack_at_distance g t room (dist_between pos mpos)
      | step -> failwith (sprintf "not impl NAwield_bow_test_moving %d" step)
      end
  | NAmove_throw1 (path, mpos, mch) ->
      let pos = rogue_pos g in
      let (monl, movl) = monsters_and_moves_around g in
      let monl =
        List.filter
          (fun mov ->
             is_attackable_monster g (dung_char g.dung (add_mov pos mov)))
          monl
      in
      if message <> "" then
        begin
          tempo g 1.0;
          let na =           (*next_action*)NAnone in Coth ' ', na, None
        end
      else if g.move_result = MRteleported then Coth ' ', NAnone, t.t_prev_mov
      else if List.length monl > 0 then
        let na = NAnone in attack_monsters g t movl monl na
      else if is_moving g t mpos then
        begin
          tempo g 1.0;
          let mpos =
            match current_room_possibly_at_door g pos with
              Some room ->
                begin match
                  dist_to_closest g room pos (fun ch _ -> ch = mch)
                with
                  Some mov -> add_mov pos mov
                | None -> mpos
                end
            | None -> failwith "oops"
          in
          if g.weapon_cursed || short_bow_in_pack g.pack = None then
            let na = NAmove_throw2 (mpos, mch, "direction") in
            Coth 't', na, None
          else
            let na = NAmove_throw2 (mpos, mch, "what bow") in
            Coth 'w', na, None
        end
      else
        begin match path with
          [] ->
            tempo g 1.0;
            if g.weapon_cursed || short_bow_in_pack g.pack = None then
              let na = NAmove_throw2 (mpos, mch, "direction") in
              Coth 't', na, None
            else
              let na = NAmove_throw2 (mpos, mch, "what bow") in
              Coth 'w', na, None
        | mov :: path ->
            tempo g 0.1;
            let na = NAmove_throw1 (path, mpos, mch) in
            let (comm, na) = move_command g pos mov na in comm, na, Some mov
        end
  | NAmove_throw2 (mpos, mch, step) ->
      begin match step with
        "what bow" ->
          if message <> "" && transl.is_message_cursed message then
            begin
              tempo g 1.0;
              g.weapon_cursed <- true;
              let na = NAmove_throw2 (mpos, mch, "ready") in
              Coth ' ', na, None
            end
          else
            begin match short_bow_in_pack g.pack with
              Some (ch, _) ->
                begin match arrows_in_pack g.pack with
                  Some _ ->
                    let na = NAmove_throw2 (mpos, mch, "ready") in
                    Coth ch, na, None
                | None -> failwith "no arrows not impl"
                end
            | None -> failwith "no bow NAmove_throw2"
            end
      | "ready" ->
          if message <> "" then
            begin
              tempo g 1.0;
              if transl.is_message_there_is_no message then
                begin match short_bow_in_pack g.pack with
                  Some (ch, _) -> g.pack <- List.remove_assoc ch g.pack
                | None -> ()
                end;
              let na = NAmove_throw2 (mpos, mch, step) in Coth ' ', na, None
            end
          else
            let na = NAmove_throw2 (mpos, mch, "direction") in
            Coth 't', na, None
      | "direction" ->
          let pos = rogue_pos g in
          let ch =
            if pos.row = mpos.row then if pos.col < mpos.col then 'l' else 'h'
            else if pos.row < mpos.row then 'j'
            else 'k'
          in
          let na = NAmove_throw2 (mpos, mch, "throw what") in
          Coth ch, na, None
      | "throw what" ->
          let na = NAmove_throw2 (mpos, mch, "thrown") in
          begin match arrows_in_pack g.pack with
            Some (ch_weap, (nb, obj)) ->
              remove_from_pack g ch_weap nb obj; Coth ch_weap, na, None
          | None -> assert false
          end
      | "thrown" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAmove_throw2 (mpos, mch, step) in Coth ' ', na, None
            end
          else
            let pos = rogue_pos g in
            begin match monster_in_dir g pos mch mpos with
              Some mpos ->
                begin match arrows_in_pack g.pack with
                  Some _ ->
                    if distance pos mpos = 2 && not g.weapon_cursed &&
                       short_bow_in_pack g.pack <> None
                    then
                      let step = "wield sword and attack" in
                      let na = NAmove_throw2 (mpos, mch, step) in
                      Coth 'w', na, None
                    else if distance pos mpos = 1 then
                      let na =
                        NAmove_throw2 (mpos, mch, "attack with sword")
                      in
                      move_command3 g pos mpos na
                    else
                      let na = NAmove_throw2 (mpos, mch, "direction") in
                      Coth 't', na, None
                | None ->
                    if g.weapon_cursed then Coth ' ', NAnone, t.t_prev_mov
                    else
                      let na = NAmove_throw2 (mpos, mch, "wield sword") in
                      Coth 'w', na, None
                end
            | None ->
                if g.weapon_cursed then Coth ' ', NAnone, t.t_prev_mov
                else
                  let na = NAmove_throw2 (mpos, mch, "wield sword") in
                  Coth 'w', na, None
            end
      | "wield sword" -> g.hist_dung <- []; Coth g.main_sword, NAnone, None
      | "wield sword and attack" ->
          g.hist_dung <- [];
          let na = NAmove_throw2 (mpos, mch, "attack with sword") in
          Coth g.main_sword, na, None
      | "attack with sword" ->
          if message <> "" then
            begin
              tempo g 1.0;
              let na = NAmove_throw2 (mpos, mch, step) in Coth ' ', na, None
            end
          else
            let pos = rogue_pos g in
            begin match monster_in_dir g pos mch mpos with
              Some mpos ->
                if distance pos mpos = 1 then
                  let na = NAmove_throw2 (mpos, mch, step) in
                  move_command3 g pos mpos na
                else begin tempo g 1.0; Coth ' ', NAnone, t.t_prev_mov end
            | None -> tempo g 1.0; Coth ' ', NAnone, t.t_prev_mov
            end
      | step -> failwith (sprintf "NAmove_throw2 step '%s'" step)
      end
  | NAglobal_search1 (gp, around) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAglobal_search1 (gp, around) in Coth ' ', na, None
        end
      else
        begin match treat_moving_monsters_at_one_move2 g t with
          Some x -> x
        | None ->
            let pos = rogue_pos g in
            (*
                    let current_room = current_room g pos in
            *)
            let (monl, movl) = monsters_and_moves_around g in
            if monl = [] then
              begin
                tempo g 0.1;
                g.hist_dung <- [];
                if pos = gp.tpos then
                  let graph =
                    let insist = g.nb_of_reinit_search > 10 in
                    make_graph g insist
                  in
                  let na = NAglobal_search2 (graph, around, 1) in
                  Coth 's', na, None
                else if
                  g.confused && monsters_around g pos = [] && g.attacked = 0
                then
                  let na = NAglobal_search1 (gp, around) in Coth 's', na, None
                else if pos = gp.epos then
                  match gp.path with
                    pos1 :: path ->
                      (*
                                      let (pos1, path) =
                                        circle_trap_if_possible2 g current_room pos pos1 path
                                      in
                      *)
                      let gp = {epos = pos1; tpos = gp.tpos; path = path} in
                      let na = NAglobal_search1 (gp, around) in
                      move_command3 g pos pos1 na
                  | [] -> failwith "NAglobal_search1 7 not impl"
                else
                  let graph = make_graph g false in
                  match path_to_closest g graph pos with
                    Some (path, tpos, around) ->
                      begin match path with
                        pos1 :: path ->
                          let gp = {epos = pos1; tpos = tpos; path = path} in
                          let na = NAglobal_search1 (gp, around) in
                          move_command3 g pos pos1 na
                      | [] ->
                          (* already there, likely after being teleported *)
                          let na = NAglobal_search2 (graph, around, 1) in
                          Coth 's', na, None
                      end
                  | None ->
                      (* happened one day when teleported in an isolated room *)
                      start_search g t graph
              end
            else
              begin
                g.nb_of_reinit_search <- g.nb_of_reinit_search + 1;
                let na = NAglobal_search1 (gp, around) in
                (*
                          attack_monsters g t movl monl na
                *)
                let mov = List.nth monl (random_int g (List.length monl)) in
                move_command2 g pos (add_mov pos mov) na
              end
        end
  | NAglobal_search2 (graph, around, ntimes) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAglobal_search2 (graph, around, ntimes) in
          Coth ' ', na, None
        end
      else
        let pos = rogue_pos g in
        let room = current_room g pos in
        let in_room = room <> None in
        let (monl, movl) = monsters_and_moves_around g in
        if monl = [] then
          begin
            g.hist_dung <- [];
            let new_around = around_pos g pos in
            match around_diff new_around around with
              None ->
                if ntimes < 5 then
                  let na = NAglobal_search2 (graph, around, ntimes + 1) in
                  Coth 's', na, None
                else if ntimes = 5 then
                  begin
                    (* test possible invisible monster *)
                    tempo g 0.1;
                    let na = NAglobal_search2 (graph, around, ntimes + 1) in
                    let rec loop movl k =
                      if k = 8 then
                        if movl = [] then Coth ' ', na, None
                        else
                          let len = List.length movl in
                          let mov = List.nth movl (random_int g len) in
                          move_command2 g pos (add_mov pos mov) na
                      else
                        let mov = mov_of_k k in
                        let pos1 = add_mov pos mov in
                        if in_dung g pos1 && dung_char g.dung pos1 = ' ' then
                          loop (mov :: movl) (k + 1)
                        else loop movl (k + 1)
                    in
                    loop [] 0
                  end
                else
                  begin
                    tempo g 0.1;
                    graph.(pos.row).(pos.col).search <- SearchFailed;
                    match path_to_closest g graph pos with
                      Some (path, tpos, around) ->
                        begin match path with
                          pos1 :: path ->
                            (*
                                                    let (pos1, path) =
                                                      circle_trap_if_possible2 g room pos pos1 path
                                                    in
                            *)
                            let gp =
                              {epos = pos1; tpos = tpos; path = path}
                            in
                            let na = NAglobal_search1 (gp, around) in
                            move_command3 g pos pos1 na
                        | [] -> assert false
                        end
                    | None ->
                        match
                          if g.nb_of_reinit_search > 5 then
                            Some (stairs_pos g)
                          else None
                        with
                          Some [] | None ->
                            g.nb_of_reinit_search <-
                              g.nb_of_reinit_search + 1;
                            reinit_graph_search g graph;
                            start_search g t graph
                        | Some sp ->
                            let tpos =
                              List.nth sp (random_int g (List.length sp))
                            in
                            go_to_stairs g t graph pos tpos false
                  end
            | Some k ->
                tempo g 0.5;
                let mov = mov_of_k k in
                if in_room then
                  if mov.di = 0 || mov.dj = 0 then
                    let na = NAnone in
                    move_command3 g pos (add_mov pos mov) na
                  else Coth ' ', NAnone, t.t_prev_mov
                else move_in_corridor_starting_with_move g pos mov
          end
        else
          let na = NAglobal_search2 (graph, around, ntimes) in
          attack_monsters g t movl monl na
  | NAsearch_and_back (from, around, ntimes) ->
      if message <> "" then
        begin
          tempo g 1.0;
          let na = NAsearch_and_back (from, around, ntimes) in
          Coth ' ', na, t.t_prev_mov
        end
      else
        let pos = rogue_pos g in
        let (monl, movl) = monsters_and_moves_around g in
        if monl <> [] then attack_monsters g t movl monl NAnone
        else if g.blind || g.attacked > 0 then
          match t.t_prev_mov with
            Some mov -> move_command3 g pos (add_mov pos mov) NAnone
          | None -> random_move g pos NAnone
        else
          let new_around = around_pos g pos in
          begin match around_diff around new_around with
            Some k ->
              let mov = mov_of_k k in
              let tpos = add_mov pos mov in
              if can_move_to g tpos then
                let gp = {epos = tpos; tpos = tpos; path = []} in
                let na = NAmove_in_corridor (pos, gp, []) in
                move_command3 g pos tpos na
              else
                let pred _ = (=) tpos in
                begin match direct_path_excl g [] pos pred with
                  Some (pos1 :: path, tpos) ->
                    let gp = {epos = pos1; tpos = tpos; path = path} in
                    let na = NAmove_in_corridor (pos, gp, []) in
                    move_command3 g pos pos1 na
                | Some ([], _) -> assert false
                | None -> start_move_in_corridor g t pos
                end
          | None ->
              if ntimes >= 5 && not (close_to_room g pos && ntimes < 20) then
                match paths_in_corridors_from g pos pos with
                  [] ->
                    (* likely at door; restarting the search *)
                    let na = NAsearch_and_back (from, around, 0) in
                    Coth 's', na, t.t_prev_mov
                | all_paths ->
                    let paths =
                      select_less_explorated_paths_in_corridor g all_paths
                    in
                    (*
                                    let mov = opposite_move from in
                                    let paths = select_ahead_moves_in_corridor g pos mov paths in
                    *)
                    let paths = if paths = [] then all_paths else paths in
                    let len = List.length paths in
                    let (path, tpos) = List.nth paths (random_int g len) in
                    match path with
                      pos1 :: path ->
                        let gp = {epos = pos1; tpos = tpos; path = path} in
                        let na = NAmove_in_corridor (pos, gp, []) in
                        move_command3 g pos pos1 na
                    | [] -> failwith "NAsearch_and_back 8"
              else
                begin
                  tempo g 0.1;
                  let na = NAsearch_and_back (from, around, ntimes + 1) in
                  Coth 's', na, t.t_prev_mov
                end
          end
  | NAnone ->
      let pos = rogue_pos g in
      if message <> "" then begin tempo g 1.0; Coth ' ', NAnone, None end
      else if g.blind then random_move g pos NAnone
      else
        match
          if g.armor_cursed then None
          else scroll_of_enchant_armor_in_pack g.pack
        with
          Some (ch_scr, _) ->
            if wearing_best_armor g then
              let uo = UOread_scroll (ch_scr, RSread_what) in
              let na = NAuse_object (uo, NAnone) in Coth 'r', na, t.t_prev_mov
            else
              let ws = WStoken_off in
              let na = NAread_scroll_for_best_armor (ch_scr, NAnone, ws) in
              Coth 'T', na, t.t_prev_mov
        | None ->
            match scroll_of_enchant_weapon_in_pack g.pack with
              Some (ch, _) ->
                let uo = UOread_scroll (ch, RSread_what) in
                let na = NAuse_object (uo, NAnone) in
                Coth 'r', na, t.t_prev_mov
            | None ->
                if g.pack_full && g.attacked = 0 && monsters_around g pos = []
                then
                  manage_full_pack g t
                else
                  match
                    if g.armor_cursed ||
                       worn_armor_protected g && worn_armor_value g >= 10
                    then
                      None
                    else unidentified_armor_in_pack g.pack
                  with
                    Some (ch, _) ->
                      Coth 'T', NAwear (ch, 1, NAnone), t.t_prev_mov
                  | None ->
                      if not g.armor_cursed && g.worn_armor <> None &&
                         not (worn_armor_protected g) && aquator_around g
                      then
                        Coth 'T', NAwear (' ', 1, NAnone), t.t_prev_mov
                      else if
                        not (wielding_a_two_handed_sword g) &&
                        two_handed_swords_in_pack g <> []
                      then
                        let list = two_handed_swords_in_pack g in
                        let len = List.length list in
                        let (ch, _) = List.nth list (random_int g len) in
                        let uo = UOwield_sword (ch, "wield what") in
                        let na = NAuse_object (uo, NAnone) in
                        Coth 'w', na, t.t_prev_mov
                      else
                        match
                          if g.ring_of_slow_digestion_on_hand <> None ||
                             unidentified_ring_in_pack g.pack = None
                          then
                            None
                          else scroll_of_identification_in_pack g.pack
                        with
                          Some (ch, _) ->
                            let uo = UOread_scroll (ch, RSread_what) in
                            let na = NAuse_object (uo, NAnone) in
                            Coth 'r', na, t.t_prev_mov
                        | None ->
                            if not g.blind && not (wearing_best_armor g) &&
                               scroll_of_protect_armor_in_pack g.pack <> None
                            then
                              match best_armor g with
                                Some (ch_arm, _) ->
                                  let ch_scr =
                                    match
                                      scroll_of_protect_armor_in_pack g.pack
                                    with
                                      Some (ch, _) -> ch
                                    | None -> assert false
                                  in
                                  let na =
                                    NAwear_armor_and_test_scrolls
                                      (ch_arm, ch_scr, WStoken_off)
                                  in
                                  Coth 'T', na, t.t_prev_mov
                              | None -> failwith "no armor"
                            else
                              match
                                if not g.after_first_pack_full then None
                                else if not g.teleport_discovered then None
                                else unidentified_scroll_in_pack g.pack
                              with
                                Some (ch_scr, _) ->
                                  if wearing_best_armor g then
                                    let uo =
                                      UOread_scroll (ch_scr, RSread_what)
                                    in
                                    let na = NAuse_object (uo, NAnone) in
                                    Coth 'r', na, t.t_prev_mov
                                  else
                                    begin match best_armor g with
                                      Some (ch_arm, _) ->
                                        let na =
                                          NAwear_armor_and_test_scrolls
                                            (ch_arm, ch_scr, WStoken_off)
                                        in
                                        Coth 'T', na, t.t_prev_mov
                                    | None -> failwith "no armors"
                                    end
                              | None ->
                                  if t.t_on_stairs &&
                                     g.time_in_level > min_time_in_level &&
                                     doors_not_explorated_in_current_room g =
                                       [] &&
                                     (health_is_maximum g ||
                                      g.ring_of_slow_digestion_on_hand = None)
                                  then
                                    begin
                                      tempo g 1.0;
                                      Coth '>', NAnone, None
                                    end
                                  else if
                                    g.time_in_level > max_time_in_level &&
                                    stairs_pos g <> [] &&
                                    nothing_interesting_in_current_room g &&
                                    g.sure_stairs_pos <> Some (rogue_pos g)
                                  then
                                    begin
                                      tempo g 1.0;
                                      let sp = stairs_pos g in
                                      let graph = make_graph g false in
                                      let tpos =
                                        List.nth sp
                                          (random_int g (List.length sp))
                                      in
                                      go_to_stairs g t graph pos tpos false
                                    end
                                  else
                                    begin
                                      tempo g 0.1;
                                      find_something_to_do g t
                                    end
