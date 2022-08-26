(* $Id: rob_monster.ml,v 1.13 2015/04/09 15:14:20 deraugla Exp $ *)

#load "pa_if_match.cmo";
#load "pa_extend.cmo";

open Printf;
open Rob_def;
open Rob_misc;

type monster_power = list (int * list (int * int));

value monster_power_list = ref None;

value gram = Grammar.gcreate (Plexer.gmake ());
value mon_pow_line = Grammar.Entry.create gram "mon_pow_line";

EXTEND
  GLOBAL: mon_pow_line;
  mon_pow_line:
    [ [ ch = UIDENT; lev = INT; v = LIST1 armv_pow SEP "/"; EOI ->
          let ch = Plexing.eval_char ch in
          let lev = int_of_string lev in
          ((ch, lev), v) ] ]
  ;
  armv_pow:
    [ [ armv = signed_int; ","; pow = INT -> (armv, int_of_string pow) ] ]
  ;
  signed_int:
    [ [ i = INT -> int_of_string i
      | "-"; i = INT -> - int_of_string i ] ]
  ;
END;

value parse_mon_pow_line line =
  Grammar.Entry.parse mon_pow_line (Stream.of_string line)
;

value read_monster_power_list t = do {
  let monpow_fname = t.t_monpow_fname in
  let mpt = Array.make 26 [] in
  if_match try Some (open_in monpow_fname) with [ Sys_error _ -> None ]
  with_some ic -> do {
    loop () where rec loop () =
      if_match
        try
          let line = input_line ic in
          let r = parse_mon_pow_line line in
          Some r
        with
        [ End_of_file -> None
        | Ploc.Exc loc _ -> None ]
      with_some ((ch, lev), v) -> do {
        let i = Char.code ch - Char.code 'A' in
        mpt.(i) := [(lev, v) :: mpt.(i)];
        loop ()
      }
      else ();
    close_in ic
  }
  else ();
  for i = 0 to 25 do { mpt.(i) := List.rev mpt.(i) };
  mpt
};

value write_monster_power_list_fname mpt fname = do {
  let list =
    loop [] 0 where rec loop rev_list i =
      if i = 26 then List.rev rev_list
      else
        let ch = Char.chr (Char.code 'A' + i) in
        loop1 rev_list mpt.(i) where rec loop1 rev_list =
          fun
          [ [(lev, v) :: rest] -> loop1 [(ch, lev, v) :: rev_list] rest
          | [] -> loop rev_list (i + 1) ]
  in
  let rev_list =
    List.fold_left
      (fun rev_list (ch, lev, v) ->
         loop v where rec loop =
           fun
           [ [(x1, y1) :: rest1] -> do {
               match rest1 with
               [ [(x2, y2) :: _] -> do {
                   if x1 >= x2 || y1 <= y2 then rev_list
                   else loop rest1
                 }
               | [] -> [(ch, lev, v) :: rev_list] ]
             }
           | [] -> [(ch, lev, v) :: rev_list] ])
    [] list
  in
  let list = List.rev rev_list in
  let oc = open_out (fname ^ ".new") in
  List.iter
    (fun (mch, lev, v) ->
       fprintf oc "%c %d %s\n" mch lev
         (List.fold_left
            (fun s (armv, pow) ->
               let armv = min 99 armv in
               let sep = if s = "" then "" else "/" in
               sprintf "%s%s%d,%d" s sep armv pow)
            "" v))
    list;
  close_out oc;
  try Sys.rename (fname ^ ".new") fname with
  [ Sys_error _ -> () ];
};

value write_monster_power_list t mpt = do {
  write_monster_power_list_fname mpt t.t_monpow_fname
};

value get_monster_power_list t =
  if_match monster_power_list.val with_some list -> list
  else do {
    let list = read_monster_power_list t in
    monster_power_list.val := Some list;
    list
  }
;

value set_monster_power g t mch new_power = do {
  let mch = monster g mch in
  let mpt = get_monster_power_list t in
  let i = Char.code mch - Char.code 'A' in
  let v = try List.assoc g.level mpt.(i) with [ Not_found -> [] ] in
  let armv = Rob_object.worn_armor_value g in
  let new_v =
    loop v where rec loop =
      fun
      [ [(armv1, pow1) :: rest] ->
          if armv < armv1 then
            if new_power > pow1 then
              [(armv, new_power); (armv1, pow1) :: rest]
            else
              [(armv1, new_power) :: rest]
          else if new_power >= pow1 then loop rest
          else [(armv1, pow1) :: loop rest]
      | [] -> [(armv, new_power)] ]
  in
  let new_v =
    loop new_v where rec loop =
      fun
      [ [(x1, y1) :: rest1] ->
          match rest1 with
          [ [(x2, y2) :: rest2] ->
              if x1 = x2 || y1 = y2 then [(x2, y2) :: rest2]
              else [(x1, y1) :: loop rest1]
          | [] -> [(x1, y1)] ]
      | [] -> [] ]
  in
  let mpti = List.remove_assoc g.level mpt.(i) in
  let mpti = List.sort compare [(g.level, new_v) :: mpti] in
(*
trace (sprintf "*** set monster power (lev %d arm %d pow %d)\n" g.level armv new_power);
trace "*** before:\n";
List.iter
  (fun (lev, v) -> do {
     trace (sprintf "%c %d" mch lev);
     List.iter (fun (arm, pow) -> trace (sprintf " %d,%d" arm pow)) v;
     trace "\n";
   })
  mpt.(i);
trace "*** interm:\n";
List.iter
  (fun (lev, v) -> do {
     trace (sprintf "%c %d" mch lev);
     List.iter (fun (arm, pow) -> trace (sprintf " %d,%d" arm pow)) v;
     trace "\n";
   })
  mpti;
*)
  let mpti =
    loop [] mpti where rec loop rev_list =
      fun
      [ [(lev1, v1); (lev2, v2) :: rest] ->
          let rev_v2 =
            loop [] v1 v2 where rec loop rev_v2 v1 v2 =
              match (v1, v2) with
              [ ([(arm1, pow1) :: rest1], [(arm2, pow2) :: rest2]) ->
                  if arm1 < arm2 then
                    if pow1 <= pow2 then loop rev_v2 rest1 v2
                    else loop [(arm1, pow1) :: rev_v2] rest1 v2
                  else if arm1 > arm2 then
                    loop [(arm2, pow2) :: rev_v2] v1 rest2
                  else
                    loop [(arm2, max pow1 pow2) :: rev_v2] rest1 rest2
              | ([(arm1, pow1) :: rest1], []) ->
                  List.rev_append v1 rev_v2
              | ([], [(arm2, pow2) :: rest1]) ->
                  List.rev_append v2 rev_v2
              | ([], []) ->
                  rev_v2 ]
          in
          let v2 =
            loop [] rev_v2 where rec loop v2 =
              fun
              [ [(arm1, pow1); (arm2, pow2) :: rest] ->
                  if arm1 = arm2 then
                    loop v2 [(arm1, pow1) :: rest]
                  else if pow1 = pow2 then
                    loop v2 [(arm1, pow1) :: rest]
                  else if pow1 > pow2 then
                    loop v2 [(arm1, pow1) :: rest]
                  else
                    loop [(arm1, pow1) :: v2] [(arm2, pow2) :: rest]
              | [x] -> [x :: v2]
              | [] -> v2 ]
          in
          loop [(lev1, v1) :: rev_list] [(lev2, v2) :: rest]
      | [x] -> List.rev [x :: rev_list]
      | [] -> List.rev rev_list ]
  in
(*
trace "*** after:\n";
List.iter
  (fun (lev, v) -> do {
     trace (sprintf "%c %d" mch lev);
     List.iter (fun (arm, pow) -> trace (sprintf " %d,%d" arm pow)) v;
     trace "\n";
   })
  mpti;
*)
  mpt.(i) := mpti;
  monster_power_list.val := Some mpt;
  write_monster_power_list t mpt;
};

value basic_monster_power g t mch default = do {
  let mch = monster g mch in
  let armv = Rob_object.worn_armor_value g in
  let mpt = get_monster_power_list t in
  let i = Char.code mch - Char.code 'A' in
  let v =
    try List.assoc g.level mpt.(i) with [ Not_found -> default mpt.(i) ]
  in
  loop v where rec loop =
    fun
    [ [(_, pow)] -> pow
    | [(armv1, pow) :: rest] -> if armv <= armv1 then pow else loop rest
    | [] -> 0 ]
};

value monster_power g t mch = do {
  let default list =
    match List.rev list with
    [ [(_, v) :: _] -> v
    | [] -> [] ]
  in
  basic_monster_power g t mch default
};

value monster_power_at_level g t mch =
  let mp = monster_power g t mch in
  if g.level >= level_of_faster_monsters then mp * 2 else mp
;

(* *)

value is_aquator g mch = monster g mch = 'A';
value is_flaming_monster g mch = monster g mch = 'D';
value is_fliting_monster g mch = List.mem (monster g mch) ['B'; 'P'];
value is_freezing_monster g mch = monster g mch = 'I';
value is_holding_monster g mch = monster g mch = 'F';
value is_mean_monster g ch = List.mem (monster g ch) ['C'; 'Q'; 'R'; 'T'];

value is_not_attackable_monster g ch =
  List.mem (monster g ch) ['I'; 'L'; 'N']
;

value is_attackable_monster g ch =
  is_monster ch && not g.hallucinated && not (is_not_attackable_monster g ch)
;
value is_monster_attackable_at_distance g ch =
  is_monster ch && g.hallucinated || is_not_attackable_monster g ch
;

(* *)

value monsters_and_moves_around g = do {
  let pos = rogue_pos g in
  loop [] [] (-1) (-1) where rec loop monl movl di dj =
    let mov = {di = di; dj = dj} in
    if di = 2 then (monl, movl)
    else if dj = 2 then loop monl movl (di + 1) (-1)
    else if di = 0 && dj = 0 then loop monl movl 0 1
    else if can_move_to g (add_mov pos mov) then
      if is_monster (dung_char g.dung (add_mov pos mov)) then
        loop [mov :: monl] movl di (dj + 1)
      else
        loop monl [mov :: movl] di (dj + 1)
    else loop monl movl di (dj + 1)
};

value monsters_around g pos = do {
  let in_room = current_room_possibly_at_door g pos <> None in
  loop [] (-1) (-1) where rec loop monl di dj =
    let mov = {di = di; dj = dj} in
    if di = 2 then monl
    else if dj = 2 then loop monl (di + 1) (-1)
    else if di = 0 && dj = 0 then loop monl 0 1
    else
      let pos1 = add_mov pos mov in
      if in_dung g pos1 && is_monster (dung_char g.dung pos1) &&
         old_can_move_to g in_room pos pos1
      then loop [mov :: monl] di (dj + 1)
      else loop monl di (dj + 1)
};

value monster_around pred g pos =
  let monl = monsters_around g pos in
  loop monl where rec loop =
    fun
    [ [mov :: rest] ->
        if pred (dung_char g.dung (add_mov pos mov)) then Some mov
        else loop rest
    | [] -> None ]
;

value holding_monster_around g = monster_around (is_holding_monster g) g;
value flaming_monster_around g = monster_around (is_flaming_monster g) g;

value aquator_around g =
  if g.hallucinated then False
  else
    let pos = rogue_pos g in
    let monl = monsters_around g pos in
    List.exists (fun mov -> is_aquator g (dung_char g.dung (add_mov pos mov)))
      monl
;

value flaming_monster_dir g pos =
  loop 1 run_around_list where rec loop dist =
    fun
    [ [k :: kl] ->
        let dir = mov_of_k k in
        let dmov = {di = dir.di * dist; dj = dir.dj * dist} in
        let pos1 = add_mov pos dmov in
        if in_dung g pos1 && is_flaming_monster g (dung_char g.dung pos1) &&
           not (List.mem_assoc pos1 g.frozen_monsters)
        then
          Some (dir, dist, pos1)
        else
          loop dist kl
    | [] ->
        if dist > 10 then None
        else loop (dist + 1) run_around_list ]
;

value monster_moving_to g t pos1 =
  loop 0 where rec loop k =
    if k = 8 then False
    else do {
      let mov = mov_of_k k in
      let pos = add_mov pos1 mov in
      if in_dung g pos then do {
        let ch = dung_char g.dung pos in
        if is_monster ch && is_moving g t pos then True
        else loop (k + 1)
      }
      else loop (k + 1)
    }
;
