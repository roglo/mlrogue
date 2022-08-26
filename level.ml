(* $Id: level.ml,v 1.67 2010/04/27 11:46:10 deraugla Exp $ *)

#use "rogue.def";
#load "pa_more.cmo";

open Rogue;
open Rfield;
open Imisc;

type trap_string = { t_title : string; t_mess : string };

value trap_strings =
  [(TrapDoor,
    {t_title = "Trap door.";
     t_mess = "You fell down a trap."});
   (BearTrap,
    {t_title = "Bear trap.";
     t_mess = "You are caught in a bear trap."});
   (TeleTrap,
    {t_title = "Teleport trap.";
     t_mess = ""});
   (DartTrap,
    {t_title = "Poison dart trap.";
     t_mess = "A small dart just hit you in the shoulder."});
   (SleepingGasTrap,
    {t_title = "Sleeping gas trap.";
     t_mess = "A strange white mist envelops you and you fall asleep."});
   (RustTrap,
    {t_title = "Rust trap.";
     t_mess = "A gush of water hits you on the head."})]
;

value nb_traps = List.length trap_strings;

value trap_string t = (List.assoc t trap_strings).t_title;
value trap_mess t = (List.assoc t trap_strings).t_mess;

value clear_level g = do {
  for i = 0 to MAXROOMS - 1 do {
    g.rooms.(i).is_room := R_NOTHING;
    for j = 0 to 3 do { g.rooms.(i).doors.(j) := None };
  };
  for i = 0 to MAX_TRAPS - 1 do { g.traps.(i) := None };
  for i = 0 to DROWS - 1 do {
    for j = 0 to DCOLS - 1 do { g.dungeon.(i).(j) := NOTHING };
  };
  g.rogue.detect_monster := False;
  g.rogue.see_invisible := False;
  g.rogue.being_held := False;
  g.rogue.bear_trap := 0;
  g.party_room := None;
  g.rogue.row := -1;
  g.rogue.col := -1;
  g.level_monsters := [];
  g.level_objects := []
};

type dir = [ Up | Right | Down | Left ];

value rec make_maze g r c tr br lc rc = do {
  let dirs = [| Up; Down; Left; Right |] in
  g.dungeon.(r).(c) := TUNNEL;
  if rand_percent 33 then
    for i = 0 to 9 do {
      let t1 = get_rand 0 3 in
      let t2 = get_rand 0 3 in
      let d = dirs.(t1) in
      dirs.(t1) := dirs.(t2);
      dirs.(t2) := d;
    }
  else ();
  for i = 0 to 3 do {
    match dirs.(i) with
    [ Up ->
        if r - 1 >= tr && g.dungeon.(r-1).(c) <> TUNNEL &&
           (c - 1 < lc || g.dungeon.(r-1).(c-1) <> TUNNEL) &&
           (c + 1 > rc || g.dungeon.(r-1).(c+1) <> TUNNEL) &&
           g.dungeon.(r-2).(c) <> TUNNEL
        then
          make_maze g (r - 1) c tr br lc rc
        else ()
    | Down ->
        if r + 1 <= br && g.dungeon.(r+1).(c) <> TUNNEL &&
           (c - 1 < lc || g.dungeon.(r+1).(c-1) <> TUNNEL) &&
           (c + 1 > rc || g.dungeon.(r+1).(c+1) <> TUNNEL) &&
           g.dungeon.(r+2).(c) <> TUNNEL
        then
          make_maze g (r + 1) c tr br lc rc
        else ()
    | Left ->
        if c - 1 >= lc && g.dungeon.(r).(c-1) <> TUNNEL &&
           g.dungeon.(r-1).(c-1) <> TUNNEL &&
           g.dungeon.(r+1).(c-1) <> TUNNEL &&
           (c - 2 < lc || g.dungeon.(r).(c-2) <> TUNNEL)
        then
          make_maze g r (c - 1) tr br lc rc
        else ()
    | Right ->
        if c + 1 <= rc && g.dungeon.(r).(c+1) <> TUNNEL &&
           g.dungeon.(r-1).(c+1) <> TUNNEL &&
           g.dungeon.(r+1).(c+1) <> TUNNEL &&
           (c + 2 > rc || g.dungeon.(r).(c+2) <> TUNNEL)
        then
          make_maze g r (c + 1) tr br lc rc
        else () ];
  }
};

value hide_boxed_passage g row1 col1 row2 col2 n =
  if g.cur_level > 2 then
    let (row1, row2) = (min row1 row2, max row1 row2) in
    let (col1, col2) = (min col1 col2, max col1 col2) in
    let h = row2 - row1 in
    let w = col2 - col1 in
    if w >= 5 || h >= 5 then
      let row_cut = if h >= 2 then 1 else 0 in
      let col_cut = if w >= 2 then 1 else 0 in
      for i = 0 to n - 1 do {
        let rec loop_j j =
          if j < 10 then
            let row = get_rand (row1 + row_cut) (row2 - row_cut) in
            let col = get_rand (col1 + col_cut) (col2 - col_cut) in
            if g.dungeon.(row).(col) = TUNNEL then
              g.dungeon.(row).(col) or_eq HIDDEN
            else loop_j (j + 1)
          else ()
        in
        loop_j 0;
      }
    else ()
  else ()
;

value add_mazes g =
  if g.cur_level > 1 then
    let start = get_rand 0 (MAXROOMS - 1) in
    let maze_percent =
      let v = g.cur_level * 5 / 4 in
      if g.cur_level > 15 then v + g.cur_level else v
    in
    for i = 0 to MAXROOMS - 1 do {
      let j = (start + i) mod MAXROOMS in
      let rm = g.rooms.(j) in
      if rm.is_room land R_NOTHING <> 0 then
        if rand_percent maze_percent then do {
          rm.is_room := R_MAZE;
          make_maze g
            (get_rand (rm.top_row + 1) (rm.bottom_row - 1))
            (get_rand (rm.left_col + 1) (rm.right_col - 1))
            rm.top_row rm.bottom_row rm.left_col rm.right_col;
          hide_boxed_passage g rm.top_row rm.left_col rm.bottom_row
            rm.right_col (get_rand 0 2)
        }
        else ()
      else ();
    }
  else ()
;

value make_room g rn r1 r2 r3 = do {
  let (left_col, right_col, top_row, bottom_row) =
    match rn with
    [ 0 -> (0, COL1 - 1, MIN_ROW, ROW1 - 1)
    | 1 -> (COL1 + 1, COL2 - 1, MIN_ROW, ROW1 - 1)
    | 2 -> (COL2 + 1, DCOLS - 1, MIN_ROW, ROW1 - 1)
    | 3 -> (0, COL1 - 1, ROW1 + 1, ROW2 - 1)
    | 4 -> (COL1 + 1, COL2 - 1, ROW1 + 1, ROW2 - 1)
    | 5 -> (COL2 + 1, DCOLS - 1, ROW1 + 1, ROW2 - 1)
    | 6 -> (0, COL1 - 1, ROW2 + 1, DROWS - 2)
    | 7 -> (COL1 + 1, COL2 - 1, ROW2 + 1, DROWS - 2)
    | 8 -> (COL2 + 1, DCOLS - 1, ROW2 + 1, DROWS - 2)
    | BIG_ROOM ->
        (get_rand 0 10, get_rand (DCOLS - 11) (DCOLS - 1),
         get_rand MIN_ROW (MIN_ROW + 5),
         get_rand (DROWS - 7) (DROWS - 2))
    | _ -> assert False ]
  in
  let (left_col, right_col, top_row, bottom_row) =
    if rn <> BIG_ROOM then
      let height = get_rand 4 (bottom_row - top_row + 1) in
      let width = get_rand 7 (right_col - left_col - 2) in
      let row_offset = get_rand 0 (bottom_row - top_row - height + 1) in
      let col_offset = get_rand 0 (right_col - left_col - width + 1) in
      let top_row = top_row + row_offset in
      let left_col = left_col + col_offset in
      (left_col, left_col + width - 1, top_row, top_row + height - 1)
    else
      (left_col, right_col, top_row, bottom_row)
  in
  let rn0 = if rn = BIG_ROOM then 0 else rn in
  if rn <> BIG_ROOM && rn <> r1 && rn <> r2 && rn <> r3 && rand_percent 40
  then
    ()
  else do {
    g.rooms.(rn0).is_room := R_ROOM;
    for i = top_row to bottom_row do {
      for j = left_col to right_col do {
        g.dungeon.(i).(j) :=
          if i = top_row || i = bottom_row then HORWALL
          else if
            i <> top_row && i <> bottom_row &&
            (j = left_col || j = right_col)
          then
            VERTWALL
          else FLOOR;
      };
    }
  };
  g.rooms.(rn0).top_row := top_row;
  g.rooms.(rn0).bottom_row := bottom_row;
  g.rooms.(rn0).left_col := left_col;
  g.rooms.(rn0).right_col := right_col
};

value mix_random_rooms g =
  for i = 0 to 3 * MAXROOMS - 1 do {
    let (x, y) =
      loop () where rec loop () =
        let x = get_rand 0 (MAXROOMS - 1) in
        let y = get_rand 0 (MAXROOMS - 1) in
        if x = y then loop () else (x, y)
    in
    let v = g.random_rooms.(x) in
    g.random_rooms.(x) := g.random_rooms.(y);
    g.random_rooms.(y) := v;
  }
;

value same_row room1 room2 = room1 / 3 = room2 / 3;
value same_col room1 room2 = room1 mod 3 = room2 mod 3;

value put_door g rm dir = do {
  let wall_width = if rm.is_room land R_MAZE <> 0 then 0 else 1 in
  let (row, col) =
    match dir with
    [ Up | Down ->
        let row = if dir = Up then rm.top_row else rm.bottom_row in
        loop () where rec loop () =
          let col =
            get_rand (rm.left_col + wall_width) (rm.right_col - wall_width)
          in
          if g.dungeon.(row).(col) land (HORWALL lor TUNNEL) = 0 then loop ()
          else (row, col)
    | Right | Left ->
        let col = if dir = Left then rm.left_col else rm.right_col in
        loop () where rec loop () =
          let row =
            get_rand (rm.top_row + wall_width) (rm.bottom_row - wall_width)
          in
          if g.dungeon.(row).(col) land (VERTWALL lor TUNNEL) = 0 then loop ()
          else (row, col) ]
  in
  if rm.is_room land R_ROOM <> 0 then g.dungeon.(row).(col) := DOOR else ();
  if g.cur_level > 2 && rand_percent HIDE_PERCENT then
    g.dungeon.(row).(col) or_eq HIDDEN
  else ();
  (row, col)
};

value draw_simple_passage g row1 col1 row2 col2 dir =
  let (row1, col1, row2, col2) =
    if dir = Left || dir = Right then do {
      let (row1, col1, row2, col2) =
        if col1 > col2 then (row2, col2, row1, col1)
        else (row1, col1, row2, col2)
      in
      let middle = get_rand (col1 + 1) (col2 - 1) in
      for i = col1 + 1 to middle - 1 do {
        g.dungeon.(row1).(i) := TUNNEL;
      };
      if row1 > row2 then
        for i = row1 downto row2 + 1 do {
          g.dungeon.(i).(middle) := TUNNEL;
        }
      else
        for i = row1 to row2 - 1 do {
          g.dungeon.(i).(middle) := TUNNEL;
        };
      for i = middle to col2 - 1 do {
        g.dungeon.(row2).(i) := TUNNEL;
      };
      (row1, col1, row2, col2)
    }
    else do {
      let (row1, col1, row2, col2) =
        if row1 > row2 then (row2, col2, row1, col1)
        else (row1, col1, row2, col2)
      in
      let middle = get_rand (row1 + 1) (row2 - 1) in
      for i = row1 + 1 to middle - 1 do {
        g.dungeon.(i).(col1) := TUNNEL;
      };
      if col1 > col2 then
        for i = col1 downto col2 + 1 do {
          g.dungeon.(middle).(i) := TUNNEL;
        }
      else
        for i = col1 to col2 - 1 do {
          g.dungeon.(middle).(i) := TUNNEL;
        };
      for i = middle to row2 - 1 do {
        g.dungeon.(i).(col2) := TUNNEL;
      };
      (row1, col1, row2, col2)
    }
  in
  if rand_percent HIDE_PERCENT then hide_boxed_passage g row1 col1 row2 col2 1
  else ()
;

value not_impl x = failwith ("not implemented: " ^ x);

value connect_rooms g room1 room2 =
  if g.rooms.(room1).is_room land (R_ROOM lor R_MAZE) = 0 ||
     g.rooms.(room2).is_room land (R_ROOM lor R_MAZE) = 0
  then
    False
  else
    let r =
      if same_row room1 room2 &&
         g.rooms.(room1).left_col > g.rooms.(room2).right_col
      then
        not_impl "connect_rooms 1"
      else if
        same_row room1 room2 &&
        g.rooms.(room2).left_col > g.rooms.(room1).right_col
      then
        let (row1, col1) = put_door g g.rooms.(room1) Right in
        let (row2, col2) = put_door g g.rooms.(room2) Left in
        Some (row1, col1, row2, col2, Right, 1, 3)
      else if
        same_col room1 room2 &&
        g.rooms.(room1).top_row > g.rooms.(room2).bottom_row
      then
        not_impl "connect_rooms 3"
      else if
        same_col room1 room2 &&
        g.rooms.(room2).top_row > g.rooms.(room1).bottom_row
      then
        let (row1, col1) = put_door g g.rooms.(room1) Down in
        let (row2, col2) = put_door g g.rooms.(room2) Up in
        Some (row1, col1, row2, col2, Down, 2, 0)
      else None
    in
    match r with
    [ Some (row1, col1, row2, col2, dir, dir1, dir2) -> do {
        let rec loop () = do {
          draw_simple_passage g row1 col1 row2 col2 dir;
          if rand_percent 4 then loop () else ()
        }
        in
        loop ();
        g.rooms.(room1).doors.(dir1) :=
          Some
            {oth_room = room2; oth_dir = dir2; oth_row = row2;
             oth_col = col2; door_row = row1; door_col = col1};
        g.rooms.(room2).doors.(dir2) :=
          Some
            {oth_room = room1; oth_dir = dir1; oth_row = row1;
             oth_col = col1; door_row = row2; door_col = col2};
        True
      }
    | None -> False ]
;

value rec visit_rooms g rooms_visited rn = do {
  rooms_visited.(rn) := True;
  for i = 0 to 3 do {
    match g.rooms.(rn).doors.(i) with
    [ Some {oth_room = oth_rn} ->
        if not rooms_visited.(oth_rn) then
          visit_rooms g rooms_visited oth_rn
        else ()
    | None -> () ];
  }
};

value is_all_connected g = do {
  let rooms_visited = Array.make MAXROOMS False in
  let starting_room =
    loop 0 where rec loop i =
      if i = MAXROOMS then i
      else if g.rooms.(i).is_room land (R_ROOM lor R_MAZE) <> 0 then i
      else loop (i + 1)
  in
  visit_rooms g rooms_visited starting_room;
  let rec loop i =
    if i = MAXROOMS then True
    else if
      g.rooms.(i).is_room land (R_ROOM lor R_MAZE) <> 0 &&
      not rooms_visited.(i)
    then
      False
    else loop (i + 1)
  in
  loop 0
};

value mask_room g rn mask =
  loop_i g.rooms.(rn).top_row where rec loop_i i =
    if i > g.rooms.(rn).bottom_row then None
    else
      let rec loop_j j =
        if j > g.rooms.(rn).right_col then loop_i (i + 1)
        else if g.dungeon.(i).(j) land mask <> 0 then Some (i, j)
        else loop_j (j + 1)
      in
      loop_j g.rooms.(rn).left_col
;

value rec recursive_deadend g rn offsets srow scol = do {
  g.rooms.(rn).is_room := R_DEADEND;
  g.dungeon.(srow).(scol) := TUNNEL;
  for i = 0 to 3 do {
    let de = rn + offsets.(i) in
    if de < 0 || de >= MAXROOMS || not (same_row rn de || same_col rn de) then
      ()
    else if g.rooms.(de).is_room land R_NOTHING = 0 then ()
    else do {
      let drow = (g.rooms.(de).top_row + g.rooms.(de).bottom_row) / 2 in
      let dcol = (g.rooms.(de).left_col + g.rooms.(de).right_col) / 2 in
      let tunnel_dir =
        if same_row rn de then
          if g.rooms.(rn).left_col < g.rooms.(de).left_col then Right
          else Left
        else if g.rooms.(rn).top_row < g.rooms.(de).top_row then Down
        else Up
      in
      draw_simple_passage g srow scol drow dcol tunnel_dir;
      g.r_de := Some de;
      recursive_deadend g de offsets drow dcol
    };
  }
};

(*
value offsets = [| -1; 1; 3; -3 |];
*)

value fill_it g rn do_rec_de = do {
  let rooms_found = ref 0 in
  let did_this = ref False in
  let offsets =
    f_array.Efield.get g.env "fill_it_offset" [| -1; 1; 3; -3 |]
  in
  for i = 0 to 9 do {
    let srow = get_rand 0 3 in
    let scol = get_rand 0 3 in
    let t = offsets.(srow) in
    offsets.(srow) := offsets.(scol);
    offsets.(scol) := t;
  };
  f_array.Efield.set g.env "fill_it_offset" offsets;
  let rec loop i =
    if i >= 4 then ()
    else
      let target_room = rn + offsets.(i) in
      if target_room < 0 || target_room >= MAXROOMS ||
         not (same_row rn target_room || same_col rn target_room) ||
         g.rooms.(target_room).is_room land (R_ROOM lor R_MAZE) = 0
      then
        loop (i + 1)
      else
        let tunnel_dir =
          if same_row rn target_room then
            if g.rooms.(rn).left_col < g.rooms.(target_room).left_col then
              Right
            else Left
          else if g.rooms.(rn).top_row < g.rooms.(target_room).top_row then
            Down
          else Up
        in
        let (k, door_dir) =
          match tunnel_dir with
          [ Up -> (2, Down)
          | Right -> (3, Left)
          | Down -> (0, Up)
          | Left -> (1, Right) ]
        in
        if g.rooms.(target_room).doors.(k) <> None then loop (i + 1)
        else do {
          let (nrow, ncol) =
            match
              if not do_rec_de || did_this.val then None
              else mask_room g rn TUNNEL
            with
            [ Some v -> v
            | None ->
                ((g.rooms.(rn).top_row + g.rooms.(rn).bottom_row) / 2,
                 (g.rooms.(rn).left_col + g.rooms.(rn).right_col) / 2) ]
          in
          let (drow, dcol) = put_door g g.rooms.(target_room) door_dir in
          incr rooms_found;
          draw_simple_passage g nrow ncol drow dcol tunnel_dir;
          g.rooms.(rn).is_room := R_DEADEND;
          g.dungeon.(nrow).(ncol) := TUNNEL;
          if i < 3 && not did_this.val then do {
            did_this.val := True;
            if coin_toss () then loop (i + 1) else ()
          }
          else ();
          if rooms_found.val < 2 && do_rec_de then
            recursive_deadend g rn offsets nrow ncol
          else ()
        }
  in
  loop 0
};

value fill_out_level g = do {
  mix_random_rooms g;
  g.r_de := None;
  for i = 0 to MAXROOMS - 1 do {
    let rn = g.random_rooms.(i) in
    if g.rooms.(rn).is_room land R_NOTHING <> 0 ||
       g.rooms.(rn).is_room land R_CROSS <> 0 && coin_toss ()
    then
      fill_it g rn True
    else ();
  };
  match g.r_de with
  [ Some rn -> fill_it g rn False
  | None -> () ]
};

value place_at g obj row col = do {
  obj.ob_row := row;
  obj.ob_col := col;
  g.dungeon.(row).(col) or_eq OBJECT;
  g.level_objects := g.level_objects @ [obj]
};

value rand_place g obj =
  let (row, col, _) = gr_row_col g (FLOOR lor TUNNEL) 0 in
  place_at g obj row col
;

value put_amulet g = rand_place g (Object.get_amulet None);

value make g =
  let (must_exist1, must_exist2, must_exist3) =
    match get_rand 0 5 with
    [ 0 -> (0, 1, 2)
    | 1 -> (3, 4, 5)
    | 2 -> (6, 7, 8)
    | 3 -> (0, 3, 6)
    | 4 -> (1, 4, 7)
    | 5 -> (2, 5, 8)
    | _ -> assert False ]
  in
  let big_room = g.cur_level = g.party_counter && rand_percent 1 in
  if big_room then make_room g BIG_ROOM 0 0 0
  else do {
    for i = 0 to MAXROOMS - 1 do {
      make_room g i must_exist1 must_exist2 must_exist3;
    };
    add_mazes g;
    mix_random_rooms g;
    let rec loop j =
      if j = MAXROOMS then ()
      else do {
        let i = g.random_rooms.(j) in
        if i < MAXROOMS - 1 then
          let _ : bool = connect_rooms g i (i + 1) in
          ()
        else ();
        if i < MAXROOMS - 3 then
          let _ : bool = connect_rooms g i (i + 3) in
          ()
        else ();
        if i < MAXROOMS - 2 then
          if g.rooms.(i+1).is_room land R_NOTHING <> 0 then
            if connect_rooms g i (i + 2) then g.rooms.(i+1).is_room := R_CROSS
            else ()
          else ()
        else ();
        if i < MAXROOMS - 6 then
          if g.rooms.(i+3).is_room land R_NOTHING <> 0 then
            if connect_rooms g i (i + 6) then g.rooms.(i+3).is_room := R_CROSS
            else ()
          else ()
        else ();
        if is_all_connected g then () else loop (j + 1)
      }
    in
    loop 0;
    fill_out_level g
  }
;

value gr_room g =
  loop () where rec loop () =
    let i = get_rand 0 (MAXROOMS - 1) in
    if g.rooms.(i).is_room land (R_ROOM lor R_MAZE) = 0 then loop () else i
;

value party_objects g rn =
  let rm = g.rooms.(rn) in
  let nn =
    (rm.bottom_row - rm.top_row - 1) * (rm.right_col - rm.left_col - 1)
  in
  let n =
    let n = get_rand 5 10 in
    if n > nn then nn - 2 else n
  in
  loop_i 0 0 where rec loop_i nf i =
    if i = n then nf
    else
      let rec loop_j nf j =
        if j < 250 then
          let row = get_rand (rm.top_row + 1) (rm.bottom_row - 1) in
          let col = get_rand (rm.left_col + 1) (rm.right_col - 1) in
          let found =
            g.dungeon.(row).(col) = FLOOR || g.dungeon.(row).(col) = TUNNEL
          in
          if found then do {
            let obj = Object.gr_object g in
            place_at g obj row col;
            loop_i (nf + 1) (i + 1)
          }
          else loop_j nf (j + 1)
        else loop_i nf (i + 1)
      in
      loop_j nf 0
;

value no_room_for_monster g rn =
  loop_i (g.rooms.(rn).top_row + 1) where rec loop_i i =
    if i < g.rooms.(rn).bottom_row then
      let rec loop_j j =
        if j < g.rooms.(rn).right_col then
          if g.dungeon.(i).(j) land MONSTER = 0 then False else loop_j (j + 1)
        else loop_i (i + 1)
      in
      loop_j (g.rooms.(rn).left_col + 1)
    else True
;

value party_monsters g rn n =
  let rm = g.rooms.(rn) in
  let n = n + n in
  let shift_lev = g.cur_level mod 3 in
  loop_i 0 where rec loop_i i =
    if i < n then
      if no_room_for_monster g rn then ()
      else
        let rec loop_j j =
          if j < 250 then
            let row = get_rand (rm.top_row + 1) (rm.bottom_row - 1) in
            let col = get_rand (rm.left_col + 1) (rm.right_col - 1) in
            if g.dungeon.(row).(col) land MONSTER = 0 &&
               g.dungeon.(row).(col) land (FLOOR lor TUNNEL) <> 0
            then do {
              let monster = Imonster.gr_monster g (Some shift_lev) in
              if monster.mn_flags land IMITATES = 0 then
                monster.mn_flags or_eq WAKENS
              else ();
              put_m_at g row col monster;
              loop_i (i + 1)
            }
            else loop_j (j + 1)
          else loop_i (i + 1)
        in
        loop_j 0
    else ()
;

value make_party g = do {
  let rn = gr_room g in
  g.party_room := Some rn;
  let n = if rand_percent 99 then party_objects g rn else 11 in
  if rand_percent 99 then party_monsters g rn n else ()
};

value next_party g =
  let n =
    loop g.cur_level where rec loop n =
      if n mod PARTY_TIME <> 0 then loop (n + 1) else n
  in
  get_rand (n + 1) (n + PARTY_TIME)
;

value plant_gold g row col is_maze =
  let q = get_rand (2 * g.cur_level) (16 * g.cur_level) in
  let q = if is_maze then q + q / 2 else q in
  let obj = Object.get_gold (Some q) in
  place_at g obj row col
;

value put_gold g =
  for i = 0 to MAXROOMS - 1 do {
    let rn = g.rooms.(i) in
    let is_maze = rn.is_room land R_MAZE <> 0 in
    let is_room = rn.is_room land R_ROOM <> 0 in
    if not (is_room || is_maze) then ()
    else if is_maze || rand_percent GOLD_PERCENT then
      let rec loop j =
        if j = 50 then ()
        else
          let row = get_rand (rn.top_row + 1) (rn.bottom_row - 1) in
          let col = get_rand (rn.left_col + 1) (rn.right_col - 1) in
          if g.dungeon.(row).(col) = FLOOR || g.dungeon.(row).(col) = TUNNEL
          then
            plant_gold g row col is_maze
          else loop (j + 1)
      in
      loop 0
    else ();
  }
;

value put_objects g =
  if g.cur_level < g.max_level then ()
  else do {
    let n = if coin_toss () then get_rand 2 4 else get_rand 3 5 in
    let n =
      loop n where rec loop n = if rand_percent 33 then loop (n + 1) else n
    in
    if g.cur_level = g.party_counter then do {
      make_party g;
      g.party_counter := next_party g
    }
    else ();
    for i = 0 to n - 1 do {
      let obj = Object.gr_object g in
      rand_place g obj;
    };
    put_gold g
  }
;

value add_traps g =
  let n =
    if g.cur_level <= 2 then 0
    else if g.cur_level <= 7 then get_rand 0 2
    else if g.cur_level <= 11 then get_rand 1 2
    else if g.cur_level <= 16 then get_rand 2 3
    else if g.cur_level <= 21 then get_rand 2 4
    else if g.cur_level <= AMULET_LEVEL then get_rand 3 5
    else get_rand 5 MAX_TRAPS
  in
  for i = 0 to n - 1 do {
    let tt =
      let i = get_rand 0 (nb_traps - 1) in
      fst (List.nth trap_strings i)
    in
    let (row, col) =
      if i = 0 then
        match g.party_room with
        [ Some rn ->
            let rm = g.rooms.(rn) in
            let (tries, row, col) =
              loop 1 where rec loop tries =
                let row = get_rand (rm.top_row + 1) (rm.bottom_row - 1) in
                let col = get_rand (rm.left_col + 1) (rm.right_col - 1) in
                if (g.dungeon.(row).(col) land
                    (OBJECT lor STAIRS lor TRAP lor TUNNEL) <>
                      0 ||
                    g.dungeon.(row).(col) = NOTHING) &&
                   tries < 15
                then
                  loop (tries + 1)
                else (tries, row, col)
            in
            if tries >= 15 then
              let (row, col, _) = gr_row_col g (FLOOR lor MONSTER) 0 in
              (row, col)
            else (row, col)
        | None ->
            let (row, col, _) = gr_row_col g (FLOOR lor MONSTER) 0 in
            (row, col) ]
      else
        let (row, col, _) = gr_row_col g (FLOOR lor MONSTER) 0 in
        (row, col)
    in
    let trap = {trap_type = tt; trap_row = row; trap_col = col} in
    g.traps.(i) := Some trap;
    g.dungeon.(row).(col) or_eq TRAP lor HIDDEN;
  }
;

value put_stairs g =
  let (row, col, _) = gr_row_col g (FLOOR lor TUNNEL) 0 in
  g.dungeon.(row).(col) or_eq STAIRS
;

value put_mons g =
  let n = get_rand 4 6 in
  for i = 0 to n - 1 do {
    let monster = Imonster.gr_monster g (Some 0) in
    if monster.mn_flags land WANDERS <> 0 && coin_toss () then wake_up monster
    else ();
    let (row, col, _) =
      gr_row_col g (FLOOR lor TUNNEL lor STAIRS lor OBJECT) 0
    in
    put_m_at g row col monster;
  }
;

value put_player g nr = do {
  let (row, col, rn) =
    match nr with
    [ Some nr ->
        loop nr 0 0 0 where rec loop rn row col misses =
          if misses < 2 && rn = nr then
            let (row, col, rn) =
              gr_row_col g (FLOOR lor TUNNEL lor OBJECT lor STAIRS) 0
            in
            loop rn row col (misses + 1)
          else (row, col, rn)
    | None -> gr_row_col g (FLOOR lor TUNNEL lor OBJECT lor STAIRS) 0 ]
  in
  g.rogue.row := row;
  g.rogue.col := col;
  g.cur_room :=
    if g.dungeon.(row).(col) land TUNNEL <> 0 then None else Some rn
};

value create g = do {
  if g.cur_level < LAST_DUNGEON then g.cur_level ++ else ();
  if g.cur_level > g.max_level then g.max_level := g.cur_level else ();
  let rec loop () = do {
    clear_level g;
    make g;
    if is_all_connected g then () else loop ()
  }
  in
  loop ();
  if not (has_amulet g) && g.cur_level >= AMULET_LEVEL then put_amulet g
  else ();
  put_objects g;
  put_stairs g;
  add_traps g;
  put_mons g;
  put_player g g.party_room
};
