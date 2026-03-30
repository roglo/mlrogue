(* $Id: misc.ml,v 1.107 2018/04/26 09:52:37 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

(* #use "rogue.def" *)


(* #use "keyboard.def" *)


open Rogue
open Rfield
open Printf
open Translate

let string_create = Bytes.create
let string_length = Bytes.length
let string_of_bytes = Bytes.to_string

let level_points =
  [| 10; 20; 40; 80; 160; 320; 640; 1300; 2600; 5200; 10000; 20000; 40000;
     80000; 160000; 320000; 1000000; 3333333; 6666666; 10000000; 99900000 |]

let is_passable g row col =
  if row < 1 || row > 24 - 2 || col < 0 || col > 80 - 1 then false
  else if g.dungeon.(row).(col) land 0o1000 <> 0 then
    if g.dungeon.(row).(col) land 0o400 <> 0 then true else false
  else if
    g.dungeon.(row).(col) land (0o100 lor 0o200 lor 0o40 lor 0o4 lor 0o400) <>
      0
  then
    true
  else false

let object_at g row col =
  try
    List.find (fun ob -> ob.ob_row = row && ob.ob_col = col) g.level_objects
  with Not_found -> invalid_arg "object_at"

let monster_at g row col =
  try
    List.find (fun mn -> mn.mn_row = row && mn.mn_col = col) g.level_monsters
  with Not_found -> invalid_arg "monster_at"

let trap_at g row col =
  let rec loop_i i =
    if i < 10 then
      match g.traps.(i) with
        None -> loop_i (i + 1)
      | Some t ->
          if t.trap_row = row && t.trap_col = col then Some t.trap_type
          else loop_i (i + 1)
    else None
  in
  loop_i 0

let gold_at g row col =
  try
    let obj =
      List.find (fun ob -> ob.ob_row = row && ob.ob_col = col) g.level_objects
    in
    obj.ob_kind = Gold
  with Not_found -> false

let imitating g row col =
  if g.dungeon.(row).(col) land 0o2 <> 0 then
    let monster = monster_at g row col in
    if monster.mn_flags land 0o20000000 <> 0 then true else false
  else false

let take_from_pack g ch =
  g.rogue.pack <- List.filter (fun (c, _) -> ch <> c) g.rogue.pack

let take_from_monsters g monster =
  g.level_monsters <-
    List.filter (fun mn -> mn.mn_unique_id <> monster.mn_unique_id)
      g.level_monsters

let tgmc g c =
  let t = fast_transl g.lang "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let m = Char.code c - Char.code 'A' in
  if String.length t = 26 && t.[m] >= 'A' && t.[m] <= 'Z' then t.[m] else c

let itgmc g ch =
  let t = fast_transl g.lang "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let rec loop_c c =
    if c > 'Z' then ch
    else
      let cc =
        let m = Char.code c - Char.code 'A' in
        if String.length t = 26 && t.[m] >= 'A' && t.[m] <= 'Z' then t.[m]
        else c
      in
      if cc = ch then c else loop_c (Char.chr (Char.code c + 1))
  in
  loop_c 'A'

let gmc g monster =
  if not
       (g.rogue.detect_monster || g.rogue.see_invisible ||
        g.rogue.r_see_invisible) &&
     monster.mn_flags land 0o4 <> 0 ||
     g.rogue.blind > 0
  then
    monster.mn_trail_char
  else if monster.mn_flags land 0o20000000 <> 0 then monster.mn_disguise
  else tgmc g monster.mn_char

let get_mask_char obj =
  match obj.ob_kind with
    Amulet -> ','
  | Armor _ -> ']'
  | Food _ -> ':'
  | Gold -> '*'
  | Potion _ -> '!'
  | Ring _ -> '='
  | Scroll _ -> '?'
  | Wand _ -> '/'
  | Weapon _ -> ')'

let get_dungeon_char g row col =
  let mask = g.dungeon.(row).(col) in
  if mask land 0o2 <> 0 then gmc g (monster_at g row col)
  else if mask land 0o1 <> 0 then get_mask_char (object_at g row col)
  else if mask land 0o4 <> 0 then '%'
  else if mask land 0o200 <> 0 && mask land 0o1000 = 0 then '#'
  else if mask land 0o10 <> 0 then '-'
  else if mask land 0o20 <> 0 then '|'
  else if mask land 0o100 <> 0 then
    if mask land 0o400 <> 0 && mask land 0o1000 = 0 then '^' else '.'
  else if mask land 0o40 <> 0 then
    if mask land 0o1000 <> 0 then
      if col > 0 && g.dungeon.(row).(col-1) land 0o10 <> 0 ||
         col < 80 - 1 && g.dungeon.(row).(col+1) land 0o10 <> 0
      then
        '-'
      else '|'
    else '+'
  else ' '

let get_exp_level e =
  let rec loop_i i =
    if i < 21 - 1 then if level_points.(i) > e then i + 1 else loop_i (i + 1)
    else i + 1
  in
  loop_i 0

let show_rogue g =
  let rogue = g.rogue in
  (*
    if rogue.hp_current <= rogue.hp_max / 2 then Curses.color_set 1 (-1)
    else ();
  *)
  if rogue.confused > 0 then Curses.color_set 2 (-1);
  (**)
  Curses.mvaddch rogue.row rogue.col rogue.fchar;
  (*
    if rogue.hp_current <= rogue.hp_max / 2 then Curses.color_set (-1) (-1)
    else ()
  *)
  if rogue.confused > 0 then Curses.color_set (-1) (-1)

let show_monster g row col monster ch =
  if ch >= 'A' && ch <= 'Z' then
    let i = Char.code monster.mn_char - Char.code 'A' in
    let init_hp = Imonster.mon_init_hp g i in
    let hp = monster.mn_hp_to_kill in
    if hp < init_hp then Curses.color_set 1 (-1);
    Curses.mvaddch row col ch;
    (if hp < init_hp then Curses.color_set (-1) (-1))
  else Curses.mvaddch row col ch

let show_trap i j t =
  (*
    match t with
    [ TrapDoor -> Curses.color_set 1 (-1)
    | BearTrap -> Curses.color_set 2 (-1)
    | TeleTrap -> Curses.color_set 3 (-1)
    | DartTrap -> Curses.color_set 4 (-1)
    | SleepingGasTrap -> Curses.color_set 5 (-1)
    | RustTrap -> Curses.color_set 6 (-1) ];
  *)
  Curses.mvaddch i j '^'

let add_exp g e promotion =
  let rogue = g.rogue in
  rogue.exp_points <- rogue.exp_points + e;
  if rogue.exp_points >= level_points.(rogue.exp - 1) then
    let new_exp = get_exp_level rogue.exp_points in
    if rogue.exp_points > 10000000 then rogue.exp_points <- 10000000 + 1;
    for i = rogue.exp + 1 to new_exp do
      let mbuf =
        sprintf (ftransl g.lang "Welcome to experience level %d!") i
      in
      Dialogue.message g mbuf false;
      let hp = promotion g in
      rogue.hp_current <- rogue.hp_current + hp;
      rogue.hp_max <- rogue.hp_max + hp;
      rogue.exp <- i;
      Dialogue.print_stats g (0o4 lor 0o40);
      show_rogue g
    done
  else Dialogue.print_stats g 0o40

type saved = game * char array array
let save_magic = "RGSV0005"

module OLD_GAME =
  struct
    type t =
      { saved_uid : int;
        true_uid : int;
        mutable cur_level : int;
        mutable max_level : int;
        mutable cur_room : int option;
        mutable lang : string;
        mutable score_only : bool;
        save_file : string;
        nick_name : string;
        login_name : string;
        fruit : string;
        ask_quit : bool;
        show_skull : bool;
        jump : bool;
        mutable party_counter : int;
        mutable party_room : int option;
        mutable foods : int;
        mutable r_de : int option;
        mutable trap_door : bool;
        mutable interrupted : bool;
        mutable can_int : bool;
        mutable reg_search : bool;
        mutable monsters_count : int;
        mutable mon_disappeared : bool;
        mutable level_objects : objet list;
        mutable level_monsters : monster list;
        mutable new_level_message : string;
        mutable hunger_str : string;
        mutable hit_message : string;
        mutable msg_cleared : bool;
        mutable msg_line : string;
        mutable msg_col : int;
        mutable same_msg : int;
        mutable m_moves : int;
        mutable wizard : bool;
        mutable experimented_pick_up_scare_monster : bool;
        rogue : fighter;
        random_rooms : int array;
        id_potions : id array;
        id_rings : id array;
        id_scrolls : id array;
        id_wands : id array;
        is_wood : bool array;
        rooms : room array;
        traps : trap option array;
        dungeon : int array array }
  end

let g_of_old_g g =
  {saved_uid = g.OLD_GAME.saved_uid; true_uid = g.OLD_GAME.true_uid;
   cur_level = g.OLD_GAME.cur_level; max_level = g.OLD_GAME.max_level;
   cur_room = g.OLD_GAME.cur_room; lang = g.OLD_GAME.lang;
   score_only = g.OLD_GAME.score_only; save_file = g.OLD_GAME.save_file;
   nick_name = g.OLD_GAME.nick_name; login_name = g.OLD_GAME.login_name;
   fruit = g.OLD_GAME.fruit; ask_quit = g.OLD_GAME.ask_quit;
   show_skull = g.OLD_GAME.show_skull; jump = g.OLD_GAME.jump;
   party_counter = g.OLD_GAME.party_counter;
   party_room = g.OLD_GAME.party_room; foods = g.OLD_GAME.foods;
   r_de = g.OLD_GAME.r_de; trap_door = g.OLD_GAME.trap_door;
   interrupted = g.OLD_GAME.interrupted; can_int = g.OLD_GAME.can_int;
   reg_search = g.OLD_GAME.reg_search;
   monsters_count = g.OLD_GAME.monsters_count;
   mon_disappeared = g.OLD_GAME.mon_disappeared;
   level_objects = g.OLD_GAME.level_objects;
   level_monsters = g.OLD_GAME.level_monsters;
   new_level_message = g.OLD_GAME.new_level_message;
   hunger_str = g.OLD_GAME.hunger_str; hit_message = g.OLD_GAME.hit_message;
   msg_cleared = g.OLD_GAME.msg_cleared; msg_line = g.OLD_GAME.msg_line;
   msg_col = g.OLD_GAME.msg_col; same_msg = g.OLD_GAME.same_msg;
   m_moves = g.OLD_GAME.m_moves; wizard = g.OLD_GAME.wizard;
   experimented_pick_up_scare_monster =
     g.OLD_GAME.experimented_pick_up_scare_monster;
   rogue = g.OLD_GAME.rogue; random_rooms = g.OLD_GAME.random_rooms;
   id_potions = g.OLD_GAME.id_potions; id_rings = g.OLD_GAME.id_rings;
   id_scrolls = g.OLD_GAME.id_scrolls; id_wands = g.OLD_GAME.id_wands;
   is_wood = g.OLD_GAME.is_wood; rooms = g.OLD_GAME.rooms;
   traps = g.OLD_GAME.traps; dungeon = g.OLD_GAME.dungeon;
   env = Efield.make ()}

type old_saved = OLD_GAME.t * char array array
let old_save_magic = "RGSV0004"

let save_into_file g fname =
  if g.score_only then
    f_random.Efield.set g.env "random" (Some (Random.get_state ()));
  let oc = open_out_bin fname in
  let buf =
    Array.init 24 (fun i -> Array.init 80 (fun j -> Curses.mvinch i j))
  in
  output_string oc save_magic; output_value oc (g, buf : saved); close_out oc

let display_dungeon g buf =
  for i = 0 to Array.length buf - 1 do
    let line = buf.(i) in
    for j = 0 to Array.length line - 1 do
      if line.(j) >= 'A' && line.(j) <= 'Z' && g.dungeon.(i).(j) land 0o2 <> 0
      then
        let monster = monster_at g i j in
        show_monster g i j monster (gmc g monster)
      else if line.(j) = '^' then
        match trap_at g i j with
          Some trap -> show_trap i j trap
        | None -> Curses.mvaddch i j line.(j)
      else Curses.mvaddch i j line.(j)
    done
  done

let restore fname =
  let ic = open_in_bin fname in
  let b = string_create (String.length save_magic) in
  really_input ic b 0 (string_length b);
  let b = string_of_bytes b in
  if b = save_magic then
    let (g, buf) = (input_value ic : saved) in
    display_dungeon g buf; close_in ic; Sys.remove fname; g
  else if b = old_save_magic then
    let (old_g, buf) = (input_value ic : old_saved) in
    let g = g_of_old_g old_g in
    display_dungeon g buf; close_in ic; Sys.remove fname; g
  else
    begin close_in ic; failwith (sprintf "not a mlrogue saved file %s" b) end

let get_dir_rc dir row col allow_off_screen =
  match dir with
    'h' -> row, (if allow_off_screen || col > 0 then col - 1 else col)
  | 'j' -> (if allow_off_screen || row < 24 - 2 then row + 1 else row), col
  | 'k' -> (if allow_off_screen || row > 1 then row - 1 else row), col
  | 'l' -> row, (if allow_off_screen || col < 80 - 1 then col + 1 else col)
  | 'y' ->
      if allow_off_screen || row > 1 && col > 0 then row - 1, col - 1
      else row, col
  | 'u' ->
      if allow_off_screen || row > 1 && col < 80 - 1 then row - 1, col + 1
      else row, col
  | 'b' ->
      if allow_off_screen || row < 24 - 2 && col > 0 then row + 1, col - 1
      else row, col
  | 'n' ->
      if allow_off_screen || row < 24 - 2 && col < 80 - 1 then
        row + 1, col + 1
      else row, col
  | _ -> invalid_arg "get_dir_rc"

let is_direction =
  function
    'h' | 'j' | 'k' | 'l' | 'b' | 'y' | 'u' | 'n' | '\027' -> true
  | _ -> false

let can_move g row1 col1 row2 col2 =
  if not (is_passable g row2 col2) then false
  else if row1 <> row2 && col1 <> col2 then
    if g.dungeon.(row1).(col1) land 0o40 <> 0 ||
       g.dungeon.(row2).(col2) land 0o40 <> 0 ||
       g.dungeon.(row1).(col2) = 0 || g.dungeon.(row2).(col1) = 0
    then
      false
    else true
  else true

let light_passage g row col =
  if g.rogue.blind > 0 then ()
  else
    let i_end = if row < 24 - 2 then 1 else 0 in
    let j_end = if col < 80 - 1 then 1 else 0 in
    for i = if row > 1 then -1 else 0 to i_end do
      for j = if col > 0 then -1 else 0 to j_end do
        if can_move g row col (row + i) (col + j) then
          let i = row + i in
          let j = col + j in
          if g.dungeon.(i).(j) land 0o2 <> 0 then
            let monster = monster_at g i j in
            show_monster g i j monster (gmc g monster)
          else Curses.mvaddch i j (get_dungeon_char g i j)
      done
    done

let light_up_room g rn =
  if g.rogue.blind = 0 then
    let rm = g.rooms.(rn) in
    for i = rm.top_row to rm.bottom_row do
      for j = rm.left_col to rm.right_col do
        if g.dungeon.(i).(j) land 0o2 <> 0 then
          let monster = monster_at g i j in
          g.dungeon.(i).(j) <- g.dungeon.(i).(j) land lnot 0o2;
          monster.mn_trail_char <- get_dungeon_char g i j;
          g.dungeon.(i).(j) <- g.dungeon.(i).(j) lor 0o2;
          show_monster g i j monster (gmc g monster)
        else
          let ch = get_dungeon_char g i j in
          if ch = '^' then
            match trap_at g i j with
              Some t -> show_trap i j t
            | None -> Curses.mvaddch i j ch
          else Curses.mvaddch i j ch
      done
    done;
    show_rogue g

let relight g =
  begin match g.cur_room with
    None -> light_passage g g.rogue.row g.rogue.col
  | Some rn -> light_up_room g rn
  end;
  show_rogue g

let ring_stats g =
  g.rogue.stealthy <- 0;
  g.rogue.r_rings <- 0;
  g.rogue.e_rings <- 0;
  g.rogue.r_teleport <- false;
  g.rogue.sustain_strength <- false;
  g.rogue.add_strength <- 0;
  g.rogue.regeneration <- 0;
  g.rogue.ring_exp <- 0;
  g.rogue.r_see_invisible <- false;
  g.rogue.maintain_armor <- false;
  g.rogue.auto_search <- 0;
  for i = 0 to 1 do
    let ring = if i = 0 then g.rogue.left_ring else g.rogue.right_ring in
    match ring with
      None -> ()
    | Some ring ->
        g.rogue.r_rings <- g.rogue.r_rings + 1;
        g.rogue.e_rings <- g.rogue.e_rings + 1;
        match ring.rg_kind with
          Stealth -> g.rogue.stealthy <- g.rogue.stealthy + 1
        | RTeleport -> g.rogue.r_teleport <- true
        | Regeneration -> g.rogue.regeneration <- g.rogue.regeneration + 1
        | SlowDigest -> g.rogue.e_rings <- g.rogue.e_rings - 2
        | AddStrength ->
            g.rogue.add_strength <- g.rogue.add_strength + ring.rg_class
        | SustainStrength -> g.rogue.sustain_strength <- true
        | Dexterity -> g.rogue.ring_exp <- g.rogue.ring_exp + ring.rg_class
        | Adornment -> ()
        | RSeeInvisible -> g.rogue.r_see_invisible <- true
        | MaintainArmor -> g.rogue.maintain_armor <- true
        | Searching -> g.rogue.auto_search <- g.rogue.auto_search + 2
  done

let unwield g =
  match g.rogue.weapon with
    Some (_, w) -> w.we_in_use <- false; g.rogue.weapon <- None
  | None -> ()

let unwear g =
  match g.rogue.armor with
    Some (_, a) -> a.ar_in_use <- false; g.rogue.armor <- None
  | None -> ()

let un_put_on g ring =
  begin match ring.rg_in_use with
    Some LeftHand -> g.rogue.left_ring <- None
  | Some RightHand -> g.rogue.right_ring <- None
  | None -> ()
  end;
  ring.rg_in_use <- None;
  ring_stats g;
  Dialogue.print_stats g 0o10;
  relight g

let vanish g ch obj =
  if obj.ob_quantity > 1 then obj.ob_quantity <- obj.ob_quantity - 1
  else
    begin
      begin match obj.ob_kind with
        Weapon w -> if w.we_in_use then unwield g
      | Armor a -> if a.ar_in_use then unwear g
      | Ring r -> if r.rg_in_use <> None then un_put_on g r
      | _ -> ()
      end;
      take_from_pack g ch
    end

let show_monsters g =
  g.rogue.detect_monster <- true;
  if g.rogue.blind > 0 then ()
  else
    List.iter
      (fun monster ->
         show_monster g monster.mn_row monster.mn_col monster
           (tgmc g monster.mn_char);
         if monster.mn_flags land 0o20000000 <> 0 then
           begin
             monster.mn_flags <- monster.mn_flags land lnot 0o20000000;
             monster.mn_flags <- monster.mn_flags lor 0o20
           end)
      g.level_monsters

let get_letter_object g ch mess_try_again =
  try Some (List.assoc ch g.rogue.pack) with
    Not_found ->
      Dialogue.message g
        (if mess_try_again then transl g.lang "No such item. Try again."
         else transl g.lang "No such item.")
        false;
      None

let fast g = f_bool.Efield.get g.env "fast" false
