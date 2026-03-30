(* $Id: rob_path.ml,v 1.7 2015/04/09 15:14:20 deraugla Exp $ *)

open Rob_def
open Rob_misc
open Rob_position
open Printf

let run_around_list2 = [0, 3; 1, 2; 2, 3; 3, 2; 4, 2; 5, 3; 6, 2; 7, 3]

let compare_dist (d1, _, _) (d2, _, _) = compare d1 d2

let f_inc_dd g interesting_objects pos2 =
  if is_trap g pos2 then 1000
  else if
    not (List.mem pos2 g.garbage) &&
    List.mem (dung_char g.dung pos2) interesting_objects
  then
    0
  else if List.mem pos2 g.garbage then 20
  else if List.mem (dung_char g.dung pos2) list_obj_ch then 20
  else 10

let gen_path g f_excl f_connected f_inc_dd pos pred =
  let rec loop scanned =
    function
      (dist, pos1, path) :: rest ->
        if pred path pos1 then Some (pos1, path)
        else if PosSet.mem pos1 scanned then loop scanned rest
        else
          let scanned = PosSet.add pos1 scanned in
          if f_excl pos1 then loop scanned rest
          else if List.mem pos1 path then loop scanned rest
          else
            let path = pos1 :: path in
            let rest =
              let rec loop rest =
                function
                  (k, ddist) :: kl ->
                    let mov = mov_of_k k in
                    let pos2 = add_mov pos1 mov in
                    let ddist =
                      if not (in_dung g pos2) then ddist
                      else ddist + f_inc_dd pos2
                    in
                    let rest =
                      if not (PosSet.mem pos2 scanned) &&
                         f_connected pos1 pos2 k
                      then
                        (dist + ddist, pos2, path) :: rest
                      else rest
                    in
                    loop rest kl
                | [] -> rest
              in
              loop rest run_around_list2
            in
            loop scanned (List.sort compare_dist rest)
    | [] -> None
  in
  loop PosSet.empty [0, pos, []]

let monster_path g pos tpos =
  let f_excl pos = List.mem pos g.scare_pos in
  let f_connected pos1 pos2 k =
    let in_room_or_at_door = current_room_possibly_at_door g pos1 <> None in
    old_can_move_to g in_room_or_at_door pos1 pos2
  in
  let f_inc_dd _ = 0 in
  let pred pos = (=) tpos in
  match gen_path g f_excl f_connected f_inc_dd pos pred with
    Some (tpos, rev_path) -> Some (List.tl (List.rev (tpos :: rev_path)))
  | None -> None

let direct_path_excl g excl pos pred =
  let f_excl pos = List.mem pos excl in
  let f_connected pos1 pos2 k =
    let in_room_or_at_door = current_room_possibly_at_door g pos1 <> None in
    old_can_move_to g in_room_or_at_door pos1 pos2
  in
  let interesting_objects = Rob_object.interesting_objects g in
  let f_inc_dd = f_inc_dd g interesting_objects in
  match gen_path g f_excl f_connected f_inc_dd pos pred with
    Some (tpos, rev_path) ->
      let path = List.tl (List.rev (tpos :: rev_path)) in Some (path, tpos)
  | None -> None

let gen_path_in_room_to g room f_excl pos tpos =
  let f_connected pos1 pos2 k =
    (inside_room room pos2 || is_at_door g pos2) &&
    old_can_move_to g true pos1 pos2
  in
  let pred _ = (=) tpos in
  let interesting_objects = Rob_object.interesting_objects g in
  let f_inc_dd = f_inc_dd g interesting_objects in
  gen_path g f_excl f_connected f_inc_dd pos pred

let move_path_of_position_path =
  let rec loop path opos =
    function
      pos :: rest ->
        let mov = {di = opos.row - pos.row; dj = opos.col - pos.col} in
        loop (mov :: path) pos rest
    | [] -> path
  in
  loop []

let path_in_room_to2 g room excl pos tpos =
  match
    gen_path_in_room_to g room (fun pos -> List.mem pos excl) pos tpos
  with
    Some (tpos, rev_path) -> Some (List.tl (List.rev (tpos :: rev_path)))
  | None -> None

let path_in_room_to g room excl pos tpos =
  match
    gen_path_in_room_to g room (fun pos -> List.mem pos excl) pos tpos
  with
    Some (tpos, rev_path) -> Some (move_path_of_position_path tpos rev_path)
  | None -> None

let one_step_to g tpos =
  let pos = rogue_pos g in
  match g.rogue_room_and_door with
    Some (room, Some dir) -> let mov = one_step_to_enter_room dir in mov, []
  | Some (room, None) ->
      begin match path_in_room_to g room [] pos tpos with
        Some (mov :: path) -> mov, path
      | Some [] | None ->
          failwith
            (sprintf "one_step_to (%d,%d)->(%d,%d)" pos.row pos.col tpos.row
               tpos.col)
      end
  | None -> assert false

let one_step_to2 g tpos =
  let pos = rogue_pos g in
  match g.rogue_room_and_door with
    Some (room, Some dir) ->
      let mov = one_step_to_enter_room dir in add_mov pos mov, []
  | Some (room, None) ->
      begin match path_in_room_to2 g room [] pos tpos with
        Some (pos1 :: path) -> pos1, path
      | Some [] | None ->
          failwith
            (sprintf "one_step_to2 (%d,%d)->(%d,%d)" pos.row pos.col tpos.row
               tpos.col)
      end
  | None -> assert false

let path_excl_from_to g excl pos tpos =
  let pred _ = (=) tpos in
  match direct_path_excl g excl pos pred with
    Some (path, _) -> Some {epos = pos; tpos = tpos; path = path}
  | None -> None
let old_path_excl_from_to g excl pos tpos s =
  match path_excl_from_to g excl pos tpos with
    Some gp -> gp
  | None -> failwith (sprintf "old_path_excl_from_to %d" s)

let path_to g pos tpos =
  match path_excl_from_to g [] pos tpos with
    Some gp ->
      begin match gp.path with
        pos1 :: path -> Some {epos = pos1; tpos = tpos; path = path}
      | [] -> Some gp
      end
  | None -> None
let old_path_to g pos tpos =
  match path_to g pos tpos with
    Some gp -> gp
  | None -> failwith "old_path_to"

let path_in_room_excl_mon g room pos tpos =
  let excl =
    let (rmin, cmin, rmax, cmax) = room in
    let rec loop excl pos =
      if pos.row > rmax then excl
      else if pos.col > cmax then loop excl {row = pos.row + 1; col = cmin}
      else
        let ch = dung_char g.dung pos in
        let excl = if is_monster ch then pos :: excl else excl in
        loop excl (pos_right pos)
    in
    loop [] {row = rmin; col = cmin}
  in
  let path =
    match path_in_room_to2 g room excl pos tpos with
      Some path -> path
    | None ->
        match path_in_room_to2 g room [] pos tpos with
          Some path -> path
        | None -> assert false
  in
  match path with
    pos1 :: path -> {epos = pos1; tpos = tpos; path = path}
  | [] -> assert false

let paths_in_corridors_from g ipos pos =
  let rec loop paths_ended paths_running =
    function
      (lpos, path) :: rest ->
        let (paths_ended, paths_running) =
          if path <> [] && lpos = ipos then paths_ended, paths_running
          else
            let list =
              let rec loop list =
                function
                  k :: kl ->
                    let mov = mov_of_k k in
                    let npos = add_mov lpos mov in
                    if in_dung g npos then
                      let already =
                        List.exists
                          (fun (pos, path) -> List.mem npos (pos :: path))
                          paths_running ||
                        List.exists
                          (fun (pos, path) -> List.mem npos (pos :: path))
                          rest
                      in
                      if not (old_can_move_to g false lpos npos) ||
                         is_inside_room g npos || List.mem npos path ||
                         already ||
                         List.exists
                           (fun (pos, path) -> List.mem npos (pos :: path))
                           paths_ended
                      then
                        loop list kl
                      else if already then []
                      else loop (npos :: list) kl
                    else loop list kl
                | [] -> list
              in
              loop [] (shuffle g run_around_list)
            in
            match list with
              [] -> (lpos, path) :: paths_ended, paths_running
            | _ ->
                let paths_running =
                  List.fold_right
                    (fun npos paths_running ->
                       (npos, lpos :: path) :: paths_running)
                    list paths_running
                in
                paths_ended, paths_running
        in
        loop paths_ended paths_running rest
    | [] ->
        if paths_running = [] then
          let pl = paths_ended in
          List.fold_left
            (fun pl (tpos, path) ->
               if tpos = ipos then pl
               else if List.exists (fun (_, tpos1) -> tpos = tpos1) pl then pl
               else
                 let path = List.tl (List.rev (tpos :: path)) in
                 (path, tpos) :: pl)
            [] pl
        else loop paths_ended [] paths_running
  in
  loop [] [] [pos, []]

let path_to_closest2 g pos pred =
  match direct_path_excl g [] pos pred with
    Some (path, tpos) -> Some {epos = pos; tpos = tpos; path = path}
  | None -> None

let list_cannot_move_to = ['-'; '|']

let has_door_above g (rmin, cmin, rmax, cmax) =
  let rec loop col =
    if col > cmax then false
    else if g.dung.tab.(rmin-1).[col] = '-' then loop (col + 1)
    else true
  in
  loop cmin

let has_door_below g (rmin, cmin, rmax, cmax) =
  let rec loop col =
    if col > cmax then false
    else if g.dung.tab.(rmax+1).[col] = '-' then loop (col + 1)
    else true
  in
  loop cmin

let has_door_at_left g (rmin, cmin, rmax, cmax) =
  let rec loop row =
    if row > rmax then false
    else if g.dung.tab.(row).[cmin-1] = '|' then loop (row + 1)
    else true
  in
  loop rmin

let has_door_at_right g (rmin, cmin, rmax, cmax) =
  let rec loop row =
    if row > rmax then false
    else if g.dung.tab.(row).[cmax+1] = '|' then loop (row + 1)
    else true
  in
  loop rmin

let make_graph g insist =
  let graph =
    Array.init g.dung.nrow
      (fun row ->
         Array.init g.dung.ncol
           (fun col ->
              let pos = {row = row; col = col} in
              if row = 0 || row = g.dung.nrow - 1 then
                let conn = Array.make 8 false in
                {connection = conn; search = NotToSearch}
              else
                let room = current_room g pos in
                let in_room = room <> None in
                let at_door = is_at_door g pos in
                if List.mem g.dung.tab.(row).[col] list_cannot_move_to ||
                   g.dung.tab.(row).[col] = ' ' && not in_room
                then
                  let conn = Array.make 8 false in
                  {connection = conn; search = NotToSearch}
                else
                  let conn = Array.make 8 true in
                  for k = 0 to 7 do
                    let mov = mov_of_k k in
                    let tpos = add_mov pos mov in
                    if in_dung g tpos then
                      if old_can_move_to g (in_room || at_door) pos tpos then
                        ()
                      else conn.(k) <- false
                    else conn.(k) <- false
                  done;
                  let to_search =
                    match room with
                      Some (rmin, cmin, rmax, cmax as room) ->
                        let rr = room_row room in
                        let rc = room_col room in
                        g.level >= 3 && g.dung.tab.(row).[col] <> '^' &&
                        (row = rmin && rr <> 0 &&
                         (insist || not g.visited.(rr-1).(rc)) &&
                         not (has_door_above g room) ||
                         col = cmin && rc <> 0 &&
                         (insist || not g.visited.(rr).(rc-1)) &&
                         not (has_door_at_left g room) ||
                         row = rmax && rr <> 2 &&
                         (insist || not g.visited.(rr+1).(rc)) &&
                         not (has_door_below g room) ||
                         col = cmax && rc <> 2 &&
                         (insist || not g.visited.(rr).(rc+1)) &&
                         not (has_door_at_right g room))
                    | None ->
                        if insist then true
                        else
                          let n =
                            List.fold_left
                              (fun cnt connected ->
                                 if connected then cnt + 1 else cnt)
                              0 (Array.to_list conn)
                          in
                          n = 1 || n = 3
                  in
                  let search = if to_search then ToSearch else NotToSearch in
                  {connection = conn; search = search}))
  in
  for row = 1 to g.dung.nrow - 2 do
    for col = 0 to g.dung.ncol - 1 do
      let pos = {row = row; col = col} in
      if is_at_door g pos then
        if in_dung g (pos_left pos) &&
           dung_char g.dung (pos_left pos) = ' ' &&
           not graph.(row).(col-1).connection.(4) ||
           in_dung g (pos_right pos) &&
           dung_char g.dung (pos_right pos) = ' ' &&
           not graph.(row).(col+1).connection.(3) ||
           in_dung g (pos_up pos) && dung_char g.dung (pos_up pos) = ' ' &&
           not graph.(row-1).(col).connection.(6) ||
           in_dung g (pos_down pos) &&
           dung_char g.dung (pos_down pos) = ' ' &&
           not graph.(row+1).(col).connection.(1)
        then
          graph.(row).(col).search <- ToSearch
    done
  done;
  begin match g.graph with
    Some gr ->
      for row = 1 to g.dung.nrow - 2 do
        for col = 0 to g.dung.ncol - 1 do
          if graph.(row).(col).search = ToSearch &&
             gr.(row).(col).search = SearchFailed
          then
            graph.(row).(col).search <- SearchFailed
        done
      done
  | None -> ()
  end;
  g.graph <- Some graph;
  graph

let reinit_graph_search g graph =
  for row = 1 to g.dung.nrow - 2 do
    for col = 0 to g.dung.ncol - 1 do
      if graph.(row).(col).search <> NotToSearch then
        graph.(row).(col).search <- ToSearch
    done
  done

let nothing_to_search graph =
  List.for_all
    (fun line ->
       List.for_all (fun node -> node.search <> ToSearch)
         (Array.to_list line))
    (Array.to_list graph)

let path_to_closest g graph pos =
  if nothing_to_search graph then None
  else
    let pred _ tpos =
      let node = graph.(tpos.row).(tpos.col) in node.search = ToSearch
    in
    let f_excl pos = false in
    let f_connected pos1 pos2 k =
      graph.(pos1.row).(pos1.col).connection.(k)
    in
    let interesting_objects = Rob_object.interesting_objects g in
    let f_inc_dd = f_inc_dd g interesting_objects in
    match gen_path g f_excl f_connected f_inc_dd pos pred with
      Some (tpos, rev_path) ->
        let path = List.tl (List.rev (tpos :: rev_path)) in
        Some (path, tpos, around_pos g tpos)
    | None -> None

let path_to_closest_gold g t pos =
  let pred _ pos =
    let ch = dung_char g.dung pos in
    ch = '*' || is_gold_seeker_monster g ch && not (is_moving g t pos)
  in
  path_to_closest2 g pos pred

let path_to_closest_static_monster g t pos =
  let pred _ pos =
    let ch = dung_char g.dung pos in is_monster ch && not (is_moving g t pos)
  in
  path_to_closest2 g pos pred

let find_random_around g ch =
  let pos = rogue_pos g in
  let rec loop =
    function
      k :: kl ->
        let mov = mov_of_k k in
        let pos1 = add_mov pos mov in
        if in_dung g pos1 && dung_char g.dung pos1 = ch then Some pos1
        else loop kl
    | [] -> None
  in
  loop (shuffle g run_around_list)
