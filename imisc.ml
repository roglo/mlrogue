(* $Id: imisc.ml,v 1.26 2010/04/29 03:48:29 deraugla Exp $ *)

(* #use "rogue.def" *)(* $Id: imisc.ml,v 1.26 2010/04/29 03:48:29 deraugla Exp $ *)



open Rogue
open Rfield
open Printf

let get_rand x y =
  let (x, y) = if x < y then x, y else y, x in Random.int (y - x + 1) + x

let rand_percent p = get_rand 1 100 <= p

let coin_toss () = Random.int 2 = 0

let get_room_number g row col =
  let rec loop_i i =
    if i < 9 then
      if row >= g.rooms.(i).top_row && row <= g.rooms.(i).bottom_row &&
         col >= g.rooms.(i).left_col && col <= g.rooms.(i).right_col
      then
        Some i
      else loop_i (i + 1)
    else None
  in
  loop_i 0

let gr_row_col g mask m =
  let rec loop i =
    if m > 0 && i < 0 then raise Not_found
    else
      let r = get_rand 1 (24 - 2) in
      let c = get_rand 0 (80 - 1) in
      match get_room_number g r c with
        None -> loop (i - 1)
      | Some rn ->
          if g.dungeon.(r).(c) land mask = 0 ||
             g.dungeon.(r).(c) land lnot mask <> 0 ||
             g.rooms.(rn).is_room land (0o2 lor 0o4) = 0 ||
             r = g.rogue.row && c = g.rogue.col
          then
            loop (i - 1)
          else r, c, rn
  in
  loop m

let has_amulet g =
  List.exists (fun (_, ob) -> ob.ob_kind = Amulet) g.rogue.pack

let gr_obj_char () =
  let rs = "%!?]=/):*" in let r = get_rand 0 (String.length rs - 1) in rs.[r]

let hp_raise g = if g.wizard then 10 else get_rand 3 10

let rogue_is_around g row col =
  let rdif = row - g.rogue.row in
  let cdif = col - g.rogue.col in
  rdif >= -1 && rdif <= 1 && cdif >= -1 && cdif <= 1

let rogue_can_see g row col =
  if g.rogue.blind = 0 then
    match get_room_number g row col with
      Some rn ->
        g.cur_room = Some rn && g.rooms.(rn).is_room land 0o4 = 0 ||
        rogue_is_around g row col
    | None -> rogue_is_around g row col
  else false

let get_damage g (n, d, kl) r =
  let total =
    let rec loop total j =
      if j < n then loop (total + (if r then get_rand 1 d else d)) (j + 1)
      else total
    in
    loop 0 0
  in
  match kl with
    Some (n, d) ->
      let rec loop total j =
        if j < n then loop (total + (if r then get_rand 1 d else d)) (j + 1)
        else total
      in
      loop total 0
  | None -> total

let rand_around g i r c =
  if i = 0 then
    begin
      f_int.Efield.set g.env "rand_around_row" r;
      f_int.Efield.set g.env "rand_around_col" c;
      let o = get_rand 1 8 in
      for j = 0 to 4 do
        let x = get_rand 0 8 in
        let y = (x + o) mod 9 in
        let pos =
          f_array.Efield.get g.env "rand_around_pos"
            [| 8; 7; 1; 3; 4; 5; 2; 6; 0 |]
        in
        let t = pos.(x) in
        pos.(x) <- pos.(y);
        pos.(y) <- t;
        f_array.Efield.set g.env "rand_around_pos" pos
      done
    end;
  let pos =
    f_array.Efield.get g.env "rand_around_pos" [| 8; 7; 1; 3; 4; 5; 2; 6; 0 |]
  in
  let row = f_int.Efield.get g.env "rand_around_row" 0 in
  let col = f_int.Efield.get g.env "rand_around_col" 0 in
  match pos.(i) with
    0 -> row + 1, col + 1
  | 1 -> row + 1, col - 1
  | 2 -> row - 1, col + 1
  | 3 -> row - 1, col - 1
  | 4 -> row, col + 1
  | 5 -> row + 1, col
  | 6 -> row, col
  | 7 -> row - 1, col
  | 8 -> row, col - 1
  | _ -> assert false

let rec nth_field s n =
  let rec loop i n =
    match try Some (String.index_from s i '/') with Not_found -> None with
      Some j -> if n = 0 then String.sub s i j else loop (j + 1) (n - 1)
    | None ->
        if n = 0 then String.sub s i (String.length s - i) else nth_field s 0
  in
  loop 0 n

let pack_count g new_obj =
  let rec loop count =
    function
      (_, obj) :: pack ->
        begin match obj.ob_kind with
          Weapon w ->
            begin match new_obj with
              Some {ob_kind = Weapon nw} ->
                if w.we_kind = Arrow || w.we_kind = Dagger ||
                   w.we_kind = Dart || w.we_kind = Shuriken
                then
                  if w.we_kind = nw.we_kind && nw.we_quiver = w.we_quiver then
                    loop count pack
                  else loop (count + 1) pack
                else loop (count + 1) pack
            | _ -> loop (count + 1) pack
            end
        | _ -> loop (count + obj.ob_quantity) pack
        end
    | [] -> count
  in
  loop 0 g.rogue.pack

let same_objects obj1 obj2 =
  match obj1.ob_kind, obj2.ob_kind with
    Weapon {we_quiver = q1}, Weapon {we_quiver = q2} -> q1 <> 0 && q1 = q2
  | Food Ration, Food Ration -> true
  | Potion p1, Potion p2 -> p1 = p2
  | Scroll s1, Scroll s2 -> s1 = s2
  | _, _ -> false

let insert_object obj =
  let rec loop rpack =
    function
      (c, obj1) :: pack ->
        if same_objects obj obj1 then
          let obj1 = {obj1 with ob_quantity = obj1.ob_quantity + 1} in
          rpack, (c, obj1) :: pack
        else loop ((c, obj1) :: rpack) pack
    | [] -> rpack, []
  in
  loop []

let first_avail_ichar pack =
  let rec loop c =
    if c > 'z' then '?'
    else if List.mem_assoc c pack then loop (Char.chr (Char.code c + 1))
    else c
  in
  loop 'a'

let add_to_pack g obj =
  let (rpack, pack) = insert_object obj g.rogue.pack in
  let (obj, c, pack) =
    match pack with
      [] -> let c = first_avail_ichar rpack in obj, c, [c, obj]
    | (c, obj) :: _ -> obj, c, pack
  in
  g.rogue.pack <- List.rev_append rpack pack; c, obj

let do_wear g c a =
  g.rogue.armor <- Some (c, a); a.ar_in_use <- true; a.ar_identified <- true

let do_wield g c w = g.rogue.weapon <- Some (c, w); w.we_in_use <- true

let get_armor_class =
  function
    Some (_, a) -> a.ar_class + a.ar_enchant
  | None -> 0

let string_eq s i t j =
  let rec loop i j len =
    if len <= 0 then true
    else if i = String.length s then false
    else if j = String.length t then false
    else if s.[i] = t.[j] then loop (i + 1) (j + 1) (len - 1)
    else false
  in
  loop i j

let aim_monster g monster =
  let rn = get_room_number g monster.mn_row monster.mn_col in
  let r = get_rand 0 12 in
  let rec loop i =
    if i < 4 then
      let d = (r + i) mod 4 in
      match rn with
        Some rn ->
          begin match g.rooms.(rn).doors.(d) with
            Some door ->
              monster.mn_target <- Some (door.door_row, door.door_col)
          | None -> loop (i + 1)
          end
      | None -> loop (i + 1)
  in
  loop 0

let put_m_at g row col monster =
  monster.mn_row <- row;
  monster.mn_col <- col;
  g.dungeon.(row).(col) <- g.dungeon.(row).(col) lor 0o2;
  g.level_monsters <- g.level_monsters @ [monster];
  aim_monster g monster

let wake_up monster =
  if monster.mn_flags land 0o200000000 = 0 then
    monster.mn_flags <-
      monster.mn_flags land lnot (0o10 lor 0o20000000 lor 0o20)
