(* $Id: imisc.ml,v 1.26 2010/04/29 03:48:29 deraugla Exp $ *)

#use "rogue.def";

open Rogue;
open Rfield;
open Printf;

value get_rand x y =
  let (x, y) = if x < y then (x, y) else (y, x) in
  Random.int (y - x + 1) + x
;

value rand_percent p = get_rand 1 100 <= p;

value coin_toss () = Random.int 2 = 0;

value get_room_number g row col =
  loop_i 0 where rec loop_i i =
    if i < MAXROOMS then
      if row >= g.rooms.(i).top_row && row <= g.rooms.(i).bottom_row &&
         col >= g.rooms.(i).left_col && col <= g.rooms.(i).right_col
      then Some i
      else loop_i (i + 1)
    else None
;

value gr_row_col g mask m =
  loop m where rec loop i =
    if m > 0 && i < 0 then raise Not_found
    else
      let r = get_rand MIN_ROW (DROWS - 2) in
      let c = get_rand 0 (DCOLS - 1) in
      match get_room_number g r c with
      [ None -> loop (i - 1)
      | Some rn ->
          if g.dungeon.(r).(c) land mask = 0 ||
             g.dungeon.(r).(c) land (lnot mask) <> 0 ||
             g.rooms.(rn).is_room land (R_ROOM lor R_MAZE) = 0 ||
             r = g.rogue.row && c = g.rogue.col
          then loop (i - 1)
          else (r, c, rn) ]
;

value has_amulet g =
  List.exists (fun (_, ob) -> ob.ob_kind = Amulet) g.rogue.pack
;

value gr_obj_char () =
  let rs = "%!?]=/):*" in
  let r = get_rand 0 (String.length rs - 1) in
  rs.[r]
;

value hp_raise g = if g.wizard then 10 else get_rand 3 10;

value rogue_is_around g row col =
  let rdif = row - g.rogue.row in
  let cdif = col - g.rogue.col in
  rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1
;

value rogue_can_see g row col =
  if g.rogue.blind = 0 then
    match get_room_number g row col with
    [ Some rn ->
        g.cur_room = Some rn && g.rooms.(rn).is_room land R_MAZE = 0 ||
        rogue_is_around g row col
    | None -> rogue_is_around g row col ]
  else False
;

value get_damage g (n, d, kl) r =
  let total =
    loop 0 0 where rec loop total j =
      if j < n then loop (total + if r then get_rand 1 d else d) (j + 1)
      else total
  in
  match kl with
  [ Some (n, d) ->
      loop total 0 where rec loop total j =
        if j < n then loop (total + if r then get_rand 1 d else d) (j + 1)
        else total
  | None -> total ]
;

value rand_around g i r c = do {
  if i = 0 then do {
    f_int.Efield.set g.env "rand_around_row" r;
    f_int.Efield.set g.env "rand_around_col" c;
    let o = get_rand 1 8 in
    for j = 0 to 4 do {
      let x = get_rand 0 8 in
      let y = (x + o) mod 9 in
      let pos =
        f_array.Efield.get g.env "rand_around_pos"
          [| 8; 7; 1; 3; 4; 5; 2; 6; 0 |]
      in
      let t = pos.(x) in
      pos.(x) := pos.(y);
      pos.(y) := t;
      f_array.Efield.set g.env "rand_around_pos" pos;
    }
  }
  else ();
  let pos =
    f_array.Efield.get g.env "rand_around_pos" [| 8; 7; 1; 3; 4; 5; 2; 6; 0 |]
  in
  let row = f_int.Efield.get g.env "rand_around_row" 0 in
  let col = f_int.Efield.get g.env "rand_around_col" 0 in
  match pos.(i) with
  [ 0 -> (row + 1, col + 1)
  | 1 -> (row + 1, col - 1)
  | 2 -> (row - 1, col + 1)
  | 3 -> (row - 1, col - 1)
  | 4 -> (row, col + 1)
  | 5 -> (row + 1, col)
  | 6 -> (row, col)
  | 7 -> (row - 1, col)
  | 8 -> (row, col - 1)
  | _ -> assert False ]
};

value rec nth_field s n =
  loop 0 n where rec loop i n =
    match try Some (String.index_from s i '/') with [ Not_found -> None ] with
    [ Some j ->
        if n = 0 then String.sub s i j else loop (j + 1) (n - 1)
    | None ->
        if n = 0 then String.sub s i (String.length s - i)
        else nth_field s 0 ]
;

value pack_count g new_obj =
  loop 0 g.rogue.pack where rec loop count =
    fun
    [ [(_, obj) :: pack] ->
        match obj.ob_kind with
        [ Weapon w ->
            match new_obj with
            [ Some {ob_kind = Weapon nw} ->
                if w.we_kind = Arrow || w.we_kind = Dagger || w.we_kind = Dart
                || w.we_kind = Shuriken then
                  if w.we_kind = nw.we_kind && nw.we_quiver = w.we_quiver then
                    loop count pack
                  else loop (count + 1) pack
                else loop (count + 1) pack
            | _ -> loop (count + 1) pack ]
        | _ -> loop (count + obj.ob_quantity) pack ]
    | [] -> count ]
;

value same_objects obj1 obj2 =
  match (obj1.ob_kind, obj2.ob_kind) with
  [ (Weapon {we_quiver = q1}, Weapon {we_quiver = q2}) -> q1 <> 0 && q1 = q2
  | (Food Ration, Food Ration) -> True
  | (Potion p1, Potion p2) -> p1 = p2
  | (Scroll s1, Scroll s2) -> s1 = s2
  | (_, _) -> False ]
;

value insert_object obj =
  loop [] where rec loop rpack =
    fun
    [ [(c, obj1) :: pack] ->
        if same_objects obj obj1 then
          let obj1 = {(obj1) with ob_quantity = obj1.ob_quantity + 1} in
          (rpack, [(c, obj1) :: pack])
        else loop [(c, obj1) :: rpack] pack
    | [] -> (rpack, []) ]
;

value first_avail_ichar pack =
  loop 'a' where rec loop c =
    if c > 'z' then '?'
    else if List.mem_assoc c pack then loop (Char.chr (Char.code c + 1))
    else c
;

value add_to_pack g obj = do {
  let (rpack, pack) = insert_object obj g.rogue.pack in
  let (obj, c, pack) =
    match pack with
    [ [] ->
        let c = first_avail_ichar rpack in
        (obj, c, [(c, obj)])
    | [(c, obj) :: _] -> (obj, c, pack) ]
  in
  g.rogue.pack := List.rev_append rpack pack;
  (c, obj)
};

value do_wear g c a = do {
  g.rogue.armor := Some (c, a);
  a.ar_in_use := True;
  a.ar_identified := True;
};

value do_wield g c w = do {
  g.rogue.weapon := Some (c, w);
  w.we_in_use := True;
};

value get_armor_class =
  fun
  [ Some (_, a) -> a.ar_class + a.ar_enchant
  | None -> 0 ]
;

value string_eq s i t j =
  loop i j where rec loop i j len =
    if len <= 0 then True
    else if i = String.length s then False
    else if j = String.length t then False
    else if s.[i] = t.[j] then loop (i + 1) (j + 1) (len - 1)
    else False
;

value aim_monster g monster =
  let rn = get_room_number g monster.mn_row monster.mn_col in
  let r = get_rand 0 12 in
  loop 0 where rec loop i =
    if i < 4 then
      let d = (r + i) mod 4 in
      match rn with
      [ Some rn ->
          match g.rooms.(rn).doors.(d) with
          [ Some door ->
              monster.mn_target := Some (door.door_row, door.door_col)
          | None -> loop (i + 1) ]
      | None -> loop (i + 1) ]
    else ()
;

value put_m_at g row col monster = do {
  monster.mn_row := row;
  monster.mn_col := col;
  g.dungeon.(row).(col) := g.dungeon.(row).(col) lor MONSTER;
  g.level_monsters := g.level_monsters @ [monster];
  aim_monster g monster;
};

value wake_up monster =
  if monster.mn_flags land NAPPING = 0 then
    monster.mn_flags :=
      monster.mn_flags land lnot (ASLEEP lor IMITATES lor WAKENS)
  else ()
;
