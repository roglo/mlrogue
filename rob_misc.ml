(* $Id: rob_misc.ml,v 1.28 2018/04/26 09:52:37 deraugla Exp $ *)

(* #load "pa_if_match.cmo" *)

open Printf
open Scanf
open Rob_def
open Rob_position

let string_create = Bytes.create
let string_set = Bytes.set
let string_of_bytes = Bytes.to_string

(* language dependent *)

let end_with str s =
  let len = String.length s in
  String.length str >= len && String.sub str (String.length str - len) len = s

let contains str s =
  let len = String.length s in
  let rec loop i j =
    if j = len then true
    else if i = String.length str then false
    else if str.[i] = s.[j] then loop (i + 1) (j + 1)
    else loop (i - j + 1) 0
  in
  loop 0 0

let contains_yn has has_not s =
  (has = [] || List.exists (contains s) has) &&
  not (List.exists (contains s) has_not)

let lang_en =
  {scan_status_line =
    (fun line ->
       sscanf line
         "Level: %d Gold: %d Hp: %d(%d) Str: %d(%d) Arm: %d  Exp: %d/%d %s"
         (fun lev gold hp max_hp stren max_stren arm exp max_exp hunger ->
            {sl_level = lev; sl_gold = gold; sl_hp = hp; sl_max_hp = max_hp;
             sl_stren = stren; sl_max_stren = max_stren; sl_exp = exp;
             sl_max_exp = max_exp; sl_hunger = hunger}));
   answer_left_hand = 'l'; answer_yes = 'y'; flaming_monster = 'D';
   is_armor =
     contains_yn [" armor"; " mail"] ["ring of"; "scroll of"; "scrolls of"];
   is_arrow = contains_yn [" arrow"] [];
   is_fallen_down = contains_yn ["down a trap"] [];
   is_food = contains_yn ["food"; "mold"] [];
   is_identified_potion = contains_yn ["potion of"; "potions of"] [];
   is_identified_ring = contains_yn ["ring of"] [];
   is_identified_scroll = contains_yn [] ["entitled"];
   is_identified_wand_kind = contains_yn ["staff of"; "wand of"] [];
   is_leather_armor = contains_yn ["leather armor"] [];
   is_long_sword = contains_yn ["long sword"] [];
   is_mace = contains_yn ["mace"] [];
   is_message_about_fliting_monster = contains_yn [" bat "; " phantom "] [];
   is_message_aggressive_monster =
     contains_yn [" hit"; " miss"; "weaker"]
       ["hit on"; "hits you on"; "missile"; "hit you in"; "hits the ground"];
   is_message_armor_blue = (fun s -> contains s " armor glows");
   is_message_attacked_by_flame = (fun s -> contains s "flame");
   is_message_better = (fun s -> contains s "feel better");
   is_message_boring = (fun s -> contains s "boring");
   is_message_cosmic = (fun s -> contains s "cosmic");
   is_message_cursed = (fun s -> contains s " cursed");
   is_message_darkness = (fun s -> contains s "cloak of");
   is_message_dart = (fun s -> contains s "small dart");
   is_message_dead = contains_yn ["Died of"; "Killed"] [];
   is_message_faint = (fun s -> contains s "ou faint");
   is_message_faster = (fun s -> contains s "much faster");
   is_message_gold_shield = (fun s -> contains s " gold shield");
   is_message_has_been_confused =
     contains_yn ["feel confused"; "has confused"] [];
   is_message_held = (fun s -> contains s "being held");
   is_message_hold_monster = contains_yn ["strange sense"; "freeze"] [];
   is_message_less_confused = (fun s -> contains s "feel less");
   is_message_maniacal_laughter = (fun s -> contains s "maniacal laughter");
   is_message_move_again = (fun s -> contains s "move again");
   is_message_moved_onto = (fun s -> contains s "Moved onto");
   is_message_much_better = (fun s -> contains s "feel much");
   is_message_no_darkness = (fun s -> contains s "veil of");
   is_message_nothing_appropriate = (fun s -> contains s "appropriate");
   is_message_pack_full = (fun s -> contains s " too full");
   is_message_pitched_noise = (fun s -> contains s "pitched humming noise");
   is_message_really_pick = (fun s -> contains s "pick up");
   is_message_shows_a_map = (fun s -> contains s "have a map");
   is_message_something_attacked = (fun s -> contains s "The something");
   is_message_something_there = (fun s -> contains s "something there");
   is_message_stronger = (fun s -> contains s "muscles");
   is_message_tastes_like = (fun s -> contains s "tastes like");
   is_message_there_is_no = (fun s -> contains s "No such item");
   is_message_warm_all_over = (fun s -> contains s "warm all");
   is_message_watching_over = (fun s -> contains s "watching over");
   is_message_weaken_armor = (fun s -> contains s "armor weakens");
   is_message_weapon_blue = contains_yn [" glow"] ["armor"];
   is_message_welcome_to_level = (fun s -> contains s "Welcome to");
   is_potion = (fun s -> contains s " potion");
   is_potion_of_blindness = (fun s -> contains s "blindness");
   is_potion_of_extra_healing = (fun s -> contains s "extra healing");
   is_potion_of_hallucination = (fun s -> contains s "of hallucination");
   is_potion_of_haste_self = (fun s -> contains s "haste self");
   is_potion_of_healing = (fun s -> contains s "of healing");
   is_potion_of_increase_strength = (fun s -> contains s "increase strength");
   is_potion_of_monster_detection = (fun s -> contains s "detect monster");
   is_potion_of_object_detection = (fun s -> contains s "detect things");
   is_potion_of_raise_level = (fun s -> contains s "raise level");
   is_potion_of_restore_strength = (fun s -> contains s "restore strength");
   is_potion_of_see_invisible = (fun s -> contains s "see invisible");
   is_ring = (fun s -> contains s " ring");
   is_ring_of_slow_digestion = (fun s -> contains s "slow digestion");
   is_scroll = (fun s -> contains s " scroll");
   is_scroll_of_aggravate_monsters = (fun s -> contains s "aggravate");
   is_scroll_of_enchant_armor = (fun s -> contains s "enchant armor");
   is_scroll_of_enchant_weapon = (fun s -> contains s "enchant weapon");
   is_scroll_of_hold_monsters = (fun s -> contains s "hold monster");
   is_scroll_of_identification = (fun s -> contains s "of identify");
   is_scroll_of_magic_mapping = (fun s -> contains s " magic mapping");
   is_scroll_of_protection = (fun s -> contains s "protect armor");
   is_scroll_of_scare_monsters = (fun s -> contains s "scare");
   is_scroll_of_remove_curse = (fun s -> contains s "remove curse");
   is_scroll_of_teleport = (fun s -> contains s "teleportation");
   is_short_bow = (fun s -> contains s "short bow");
   is_trap_door = (fun s -> contains s "Trap door");
   is_two_handed_sword = (fun s -> contains s "two-handed");
   is_very_hungry = (fun s -> s = "faint");
   is_wand = contains_yn [" staff"; " wand"] [];
   is_wand_of_cancellation = (fun s -> contains s "cancellation");
   is_wand_of_magic_missile = (fun s -> contains s " magic missile");
   is_weapon =
     contains_yn
       [" arrow"; " short bow"; " dagger"; " dart"; " mace"; " shuriken";
        " spear"; " sword"]
       [];
   message_more =
     (fun s ->
        let rec loop =
          function
            m :: rest -> if end_with s m then m else loop rest
          | [] -> ""
        in
        loop [" -- More --"; "--More--"]);
   monsters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"}

let lang_fr =
  let list_armor = [" armure "; " cotte "; " armor "] in
  let list_dead = ["Mort d'hypo"; "Tué par"; "Mort de faim"] in
  let list_food = ["champignon"; "nourriture"] in
  let list_wand = [" baguette "; " bâton "] in
  let list_wand_of =
    ["monstre"; "invisibilité"; "transformation"; "sommeil"; "missile";
     "annulation"; "fait rien"]
  in
  let list_weapons =
    [" arc "; " épée"; " massue"; " flèche"; " churikène"; " poignard";
     " dard"]
  in
  let move_onto_list = ["passez sur"; "passes sur"] in
  {scan_status_line =
    (fun line ->
       sscanf line
         "Niveau: %d Or: %d Pv: %d(%d) Forc: %d(%d) Arm: %d Exp: %d/%d %s"
         (fun lev gold hp max_hp stren max_stren arm exp max_exp hunger ->
            {sl_level = lev; sl_gold = gold; sl_hp = hp; sl_max_hp = max_hp;
             sl_stren = stren; sl_max_stren = max_stren; sl_exp = exp;
             sl_max_exp = max_exp; sl_hunger = hunger}));
   answer_left_hand = 'g'; answer_yes = 'o'; flaming_monster = 'D';
   is_armor = (fun s -> List.exists (contains s) list_armor);
   is_arrow = (fun s -> contains s " flèche");
   is_fallen_down = (fun s -> contains s "dans une trappe");
   is_food = (fun s -> List.exists (contains s) list_food);
   is_identified_potion =
     (fun s ->
        List.exists (contains s)
          ["potion de"; "potion d'"; "potion hallu"; "potions de";
           "potions d'"; "potions hallu"]);
   is_identified_ring = (fun txt -> not (contains txt " avec un"));
   is_identified_scroll = (fun s -> not (contains s "intitulé"));
   is_identified_wand_kind = (fun s -> List.exists (contains s) list_wand_of);
   is_leather_armor = (fun s -> contains s "armure de cuir");
   is_long_sword = (fun s -> contains s "longue épée");
   is_mace = (fun s -> contains s "massue");
   is_message_about_fliting_monster =
     contains_yn [" wombat "; " fantôme "] [];
   is_message_aggressive_monster =
     (fun s ->
        List.exists (contains s) ["rate"; "touche"; "faiblir"] &&
        not
          (List.exists (contains s)
             ["amulette"; "arc court"; "armure"; "bague"; "bâton";
              "churikène"; "dard"; "épée"; "flèche"; "massue"; "parchemin";
              "poignard"; "potion"]) &&
        not
          (List.exists ((=) s)
             ["Tu rates."; "Tu touches."; "Vous ratez."; "Vous touchez."]));
   is_message_armor_blue = (fun s -> contains s " armure devient");
   is_message_attacked_by_flame = (fun s -> contains s "flamme");
   is_message_better = (fun s -> contains s "sentir mieux");
   is_message_boring = (fun s -> contains s "ennuyeux");
   is_message_cosmic = (fun s -> contains s "bizarre");
   is_message_cursed = (fun s -> contains s " maudit");
   is_message_darkness = (fun s -> contains s "cape de");
   is_message_dart = (fun s -> contains s "petit dard");
   is_message_dead = (fun s -> List.exists (contains s) list_dead);
   is_message_faint = (fun s -> contains s "écroule");
   is_message_faster = (fun s -> contains s " accél");
   is_message_gold_shield = (fun s -> contains s " bouclier d'or");
   is_message_has_been_confused =
     (fun s ->
        List.exists (contains s)
          ["a déconcerté"; "sens déconcerté"; "sentez déconcerté"]);
   is_message_held = (fun s -> contains s "immobilisé");
   is_message_hold_monster =
     (fun txt -> List.exists (contains txt) ["impression d'avoir"; "gèle"]);
   is_message_less_confused =
     (fun s -> List.exists (contains s) ["sens moins"; "sentez moins"]);
   is_message_maniacal_laughter = (fun s -> contains s "rires maniaques");
   is_message_move_again = (fun s -> contains s "bouger de nouveau");
   is_message_moved_onto = (fun s -> List.exists (contains s) move_onto_list);
   is_message_much_better = (fun s -> contains s "sentir beaucoup");
   is_message_no_darkness = (fun s -> contains s "voile de");
   is_message_nothing_appropriate = (fun s -> contains s "approprié");
   is_message_pack_full = (fun s -> contains s " trop plein");
   is_message_pitched_noise = (fun s -> contains s "bourdonnement");
   is_message_really_pick = (fun s -> contains s "prendre le");
   is_message_shows_a_map = (fun s -> contains s "montrer un plan");
   is_message_something_attacked = (fun s -> contains s "Quelque chose");
   is_message_something_there = (fun s -> contains s "quelque chose");
   is_message_stronger = (fun s -> contains s "muscles");
   is_message_tastes_like = (fun s -> contains s "un goût d");
   is_message_there_is_no = (fun s -> contains s "Il n'y en a pas");
   is_message_warm_all_over = (fun s -> contains s "tout chaud");
   is_message_watching_over = (fun s -> contains s "veille sur");
   is_message_weaken_armor = (fun s -> contains s "armure faiblit");
   is_message_weapon_blue =
     (fun txt -> contains txt " devient" && not (contains txt "armure"));
   is_message_welcome_to_level = (fun s -> contains s "venue au niveau");
   is_potion = (fun s -> contains s " potion");
   is_potion_of_blindness = (fun s -> contains s "cécité");
   is_potion_of_extra_healing =
     (fun s -> contains s "guérison supplémentaire");
   is_potion_of_hallucination = (fun s -> contains s "hallucinogène");
   is_potion_of_haste_self = (fun s -> contains s "accélération");
   is_potion_of_healing =
     (fun s -> contains s "guérison" && not (contains s "suppl"));
   is_potion_of_increase_strength = (fun s -> contains s "renforcement");
   is_potion_of_monster_detection =
     (fun s -> contains s "détection des monstres");
   is_potion_of_object_detection =
     (fun s -> contains s "détection des objets");
   is_potion_of_raise_level = (fun s -> contains s "passage au niveau");
   is_potion_of_restore_strength = (fun s -> contains s "restauration");
   is_potion_of_see_invisible = (fun s -> contains s "vision de");
   is_ring = (fun s -> contains s " bague ");
   is_ring_of_slow_digestion = (fun s -> contains s " digestion lente");
   is_scroll = (fun s -> contains s " parchemin");
   is_scroll_of_aggravate_monsters = (fun s -> contains s "aggravation");
   is_scroll_of_enchant_armor = (fun s -> contains s "enchantement d'armure");
   is_scroll_of_enchant_weapon = (fun s -> contains s "enchantement d'arme");
   is_scroll_of_hold_monsters = (fun s -> contains s "immobilisation");
   is_scroll_of_identification = (fun s -> contains s "d'identification");
   is_scroll_of_magic_mapping = (fun s -> contains s "plan magique");
   is_scroll_of_protection = (fun s -> contains s "protection");
   is_scroll_of_scare_monsters = (fun s -> contains s "effraie");
   is_scroll_of_remove_curse = (fun s -> contains s "de malédiction");
   is_scroll_of_teleport = (fun s -> contains s "téléportation");
   is_short_bow = (fun s -> contains s "arc court");
   is_trap_door =
     (fun s ->
        if List.exists (contains s)
             ["dard empoi"; " douche"; " loup"; " à téléportation";
              "gaz soporifique"]
        then
          false
        else if s = "Trappe." then true
        else failwith "is_trap_door fr not impl");
   is_two_handed_sword = (fun s -> contains s "épée à deux");
   is_very_hungry = (fun s -> s = "épuisé");
   is_wand = (fun s -> List.exists (contains s) list_wand);
   is_wand_of_cancellation = (fun s -> contains s "annulation");
   is_wand_of_magic_missile = (fun s -> contains s " missile magique");
   is_weapon = (fun s -> List.exists (contains s) list_weapons);
   message_more =
     (fun s -> let m = " -- Suite --" in if end_with s m then m else "");
   monsters = "AFXDEPILWJCHMNOGQRSTUVBKYZ"}

let transl g =
  let s = g.lang in
  if s = "en" then lang_en
  else if s = "fr" || s = "fr-tt" then lang_fr
  else failwith (sprintf "lang '%s' not impl" s)

(* *)

let min_time_in_level = 300
let max_time_in_level = 500

let not_impl name x =
  let desc =
    if Obj.tag (Obj.repr x) = Obj.tag (Obj.repr "") then
      "\"" ^ Obj.magic x ^ "\""
    else if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  sprintf "\"not impl: %s; %s\"" name (String.escaped desc)

let trace t txt =
  if t.t_move_trace then begin eprintf "%s" txt; flush stderr end

let is_monster ch = ch >= 'A' && ch <= 'Z'

(* *)

let sleep x = let _ = Unix.select [Unix.stdin] [] [] x in ()
let tempo g x = sleep (x /. g.speed)

exception NoRogue
let rogue_pos g =
  match g.rogue_pos with
    Some pos -> pos
  | None -> raise NoRogue

let pos_in_dung dung pos =
  pos.row > 0 && pos.row < dung.nrow - 1 && pos.col >= 0 &&
  pos.col < dung.ncol
let dung_char dung pos = dung.tab.(pos.row).[pos.col]

let minus_bar = ['-'; '|']
let minus_bar_space = ['-'; '|'; ' ']

let dung_char_eq dung pos ch = pos_in_dung dung pos && dung_char dung pos = ch

let add_mov pos mov = {row = pos.row + mov.di; col = pos.col + mov.dj}
let opposite_move mov = {di = -mov.di; dj = -mov.dj}
let perpendicular_move mov = {di = mov.dj; dj = mov.di}

(* *)

let pos_up pos = {pos with row = pos.row - 1}
let pos_down pos = {pos with row = pos.row + 1}
let pos_left pos = {pos with col = pos.col - 1}
let pos_right pos = {pos with col = pos.col + 1}

let pos_is_at_door dung pos =
  if dung_char dung pos = '+' then true
  else if List.mem (dung_char dung pos) minus_bar_space then false
  else if
    pos_in_dung dung (pos_up pos) && pos_in_dung dung (pos_down pos) &&
    List.mem (dung_char dung (pos_up pos)) minus_bar &&
    List.mem (dung_char dung (pos_down pos)) minus_bar &&
    not
      (dung_char dung (pos_up pos) = '-' &&
       dung_char dung (pos_down pos) = '-')
  then
    true
  else if
    dung_char_eq dung (pos_left pos) '-' &&
    dung_char_eq dung (pos_right pos) '-' &&
    not
      (dung_char_eq dung (add_mov pos {di = -2; dj = 1}) '|' &&
       dung_char_eq dung (add_mov pos {di = 2; dj = -1}) '|') &&
    not
      (dung_char_eq dung (add_mov pos {di = -2; dj = 1}) '|' &&
       dung_char_eq dung (add_mov pos {di = 1; dj = -1}) '|') &&
    not
      (dung_char_eq dung (add_mov pos {di = -1; dj = 1}) '|' &&
       dung_char_eq dung (add_mov pos {di = 2; dj = -1}) '|') &&
    not
      (dung_char_eq dung (add_mov pos {di = -1; dj = 1}) '|' &&
       dung_char_eq dung (add_mov pos {di = 1; dj = -1}) '|')
  then
    true
  else false

let pos_is_inside_room dung pos =
  if pos_is_at_door dung pos || List.mem (dung_char dung pos) ['|'; '-'] then
    false
  else
    let rec loop intersect pos =
      if pos_in_dung dung pos then
        let is_border =
          dung_char dung pos = '|' ||
          dung_char dung pos = '+' && dung_char dung (pos_left pos) <> '-' &&
          dung_char dung (pos_right pos) <> '-' &&
          (dung_char dung (pos_up pos) = '|' ||
           dung_char dung (pos_down pos) = '|') ||
          dung_char dung pos <> '-' &&
          (pos_in_dung dung (pos_up pos) &&
           dung_char dung (pos_up pos) = '|' ||
           pos_in_dung dung (pos_down pos) &&
           dung_char dung (pos_down pos) = '|')
        in
        let intersect = if is_border then intersect + 1 else intersect in
        loop intersect (pos_right pos)
      else intersect mod 2 = 1
    in
    loop 0 (pos_right pos)

let pos_current_room dung pos =
  if pos_is_inside_room dung pos then
    let rmin =
      let rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if
          dung_char dung pos = '-' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_left pos) &&
          dung_char dung (pos_left pos) = '-' ||
          pos_in_dung dung (pos_right pos) &&
          dung_char dung (pos_right pos) = '-'
        then
          Some (pos.row + 1)
        else loop (pos_up pos)
      in
      loop (pos_up pos)
    in
    let cmin =
      let rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if
          dung_char dung pos = '|' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_up pos) &&
          dung_char dung (pos_up pos) = '|' ||
          pos_in_dung dung (pos_down pos) &&
          dung_char dung (pos_down pos) = '|'
        then
          Some (pos.col + 1)
        else loop (pos_left pos)
      in
      loop (pos_left pos)
    in
    let rmax =
      let rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if
          dung_char dung pos = '-' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_left pos) &&
          dung_char dung (pos_left pos) = '-' ||
          pos_in_dung dung (pos_right pos) &&
          dung_char dung (pos_right pos) = '-'
        then
          Some (pos.row - 1)
        else loop (pos_down pos)
      in
      loop (pos_down pos)
    in
    let cmax =
      let rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if
          dung_char dung pos = '|' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_up pos) &&
          dung_char dung (pos_up pos) = '|' ||
          pos_in_dung dung (pos_down pos) &&
          dung_char dung (pos_down pos) = '|'
        then
          Some (pos.col - 1)
        else loop (pos_right pos)
      in
      loop (pos_right pos)
    in
    match rmin, cmin, rmax, cmax with
      Some rmin, Some cmin, Some rmax, Some cmax ->
        let room = rmin, cmin, rmax, cmax in Some room
    | _ -> None
  else None

let pos_room_and_door dung pos =
  if pos_is_at_door dung pos then
    let rec loop =
      function
        (pos, dir) :: rest ->
          if pos_in_dung dung pos then
            match pos_current_room dung pos with
              Some room -> Some (room, Some dir)
            | None -> loop rest
          else loop rest
      | [] -> None
    in
    loop
      [pos_up pos, DoorDown; pos_down pos, DoorUp; pos_left pos, DoorRight;
       pos_right pos, DoorLeft]
  else
    match pos_current_room dung pos with
      Some room -> Some (room, None)
    | None -> None

let in_dung g pos = pos_in_dung g.dung pos

let room_and_door g pos =
  if pos = rogue_pos g then g.rogue_room_and_door
  else pos_room_and_door g.dung pos

let current_room g pos =
  match room_and_door g pos with
    Some (room, None) -> Some room
  | Some (_, Some _) | None -> None

let current_room_possibly_at_door g pos =
  match room_and_door g pos with
    Some (room, _) -> Some room
  | None -> None

let is_inside_room g pos =
  match room_and_door g pos with
    Some (_, None) -> true
  | Some (_, Some _) | None -> false

let is_at_door g pos =
  match room_and_door g pos with
    Some (_, Some _) -> true
  | Some (_, None) | None -> false

let room_of_door g pos =
  match room_and_door g pos with
    Some (room, Some dir) -> Some (room, dir)
  | Some (_, None) | None -> None

let common_room_with g pos2 =
  match g.rogue_room_and_door, room_and_door g pos2 with
    Some (room, _), Some (room2, _) ->
      if room = room2 then Some room else None
  | _ -> None

let in_same_rooms g pos1 pos2 =
  match room_and_door g pos1, room_and_door g pos2 with
    Some (room1, _), Some (room2, _) -> room1 = room2
  | _ -> false

(* *)

let list_border = [' '; '|'; '-']
let list_border_in_room = ['|'; '-']

let old_can_move_to g in_room_or_at_door pos tpos =
  let list_border =
    if in_room_or_at_door then list_border_in_room else list_border
  in
  if not (in_dung g tpos) then false
  else if
    List.mem (dung_char g.dung tpos) list_border ||
    List.mem g.dung.tab.(tpos.row).[pos.col] list_border ||
    List.mem g.dung.tab.(pos.row).[tpos.col] list_border ||
    dung_char g.dung tpos = ' ' && current_room g tpos = None
  then
    false
  else true

let can_move_to g tpos =
  let pos = rogue_pos g in
  old_can_move_to g (g.rogue_room_and_door <> None) pos tpos

let level_of_faster_monsters = 29
let level_of_very_mean_monsters = 52

let health_points g =
  match g.status_line with
    Some sl -> sl.sl_hp
  | None -> assert false

let max_health_points g =
  match g.status_line with
    Some sl -> sl.sl_max_hp
  | None -> assert false

let health_is_maximum g =
  match g.status_line with
    Some sl -> sl.sl_hp = sl.sl_max_hp
  | None -> true

let random_int g n = if n = 1 then 0 else Random.State.int g.random_state n

let mov_of_k k =
  if k < 3 then {di = -1; dj = k - 1}
  else if k < 5 then {di = 0; dj = 2 * k - 7}
  else {di = 1; dj = k - 6}

let around_pos g pos =
  let s = string_create 8 in
  for k = 0 to 7 do
    let mov = mov_of_k k in
    let pos = add_mov pos mov in
    string_set s k (if in_dung g pos then dung_char g.dung pos else ' ')
  done;
  {ar = string_of_bytes s}

let no_move = {di = 0; dj = 0}

let move_between pos1 pos2 =
  let mov = {di = pos2.row - pos1.row; dj = pos2.col - pos1.col} in
  assert (abs mov.di <= 1 && abs mov.dj <= 1); assert (mov <> no_move); mov

let basic_command_of_move move =
  if move.di = -1 then
    if move.dj = -1 then 'y' else if move.dj = 0 then 'k' else 'u'
  else if move.di = 0 then
    if move.dj = -1 then 'h' else if move.dj = 0 then 's' else 'l'
  else if move.dj = -1 then 'b'
  else if move.dj = 0 then 'j'
  else 'n'

let move_command2 g pos1 pos2 na =
  let mov = move_between pos1 pos2 in
  if mov = no_move then Coth 's', na, Some mov
  else if List.mem pos2 g.garbage || g.confused then
    Coth 'm', NAmove (mov, na), Some mov
  else Cmov mov, na, Some mov

let list_mov_ch = ['.'; '+'; '#'; '%']
let list_obj_ch = ['*'; '!'; '?'; '/'; ']'; ')'; '='; ':']
let list_obj_ch2 = ['*'; '!'; '?'; '/'; ']'; ')']

let list_find f l = try Some (List.find f l) with Not_found -> None

let rec list_extract_nth n =
  function
    x :: l ->
      if n = 0 then x, l
      else let (y, l) = list_extract_nth (n - 1) l in y, x :: l
  | [] -> invalid_arg "list_extract_nth"

let shuffle g list =
  let rec loop r list =
    let len = List.length list in
    if len = 0 then r
    else
      let n = random_int g len in
      let (x, l) = list_extract_nth n list in loop (x :: r) l
  in
  loop [] list

(* *)

let armor_value s =
  try
    let i = String.index s '[' in
    let j = String.index_from s i ']' in
    let v = int_of_string (String.sub s (i + 1) (j - i - 1)) in Some v
  with Not_found | Failure _ -> None

let main_sword_value g =
  let (_, obj) = List.assoc g.main_sword g.pack in
  match obj with
    Pweapon {we_kind = WKtwo_handed_sword; we_value = Some n} -> n
  | _ -> 0

let wand_value = armor_value

let weapon_value s =
  try
    let i = String.index s ',' in
    let j = String.rindex_from s i ' ' in
    let k = String.index_from s i ' ' in
    let s1 =
      if s.[j+1] = '+' then 1
      else if s.[j+1] = '-' then -1
      else raise Not_found
    in
    let s2 =
      if s.[i+1] = '+' then 1
      else if s.[i+1] = '-' then -1
      else raise Not_found
    in
    let v1 = int_of_string (String.sub s (j + 2) (i - j - 2)) in
    let v2 = int_of_string (String.sub s (i + 2) (k - i - 2)) in
    Some (s1 * v1 + s2 * v2)
  with Not_found | Failure _ -> None

let move_command g pos mov na =
  let tpos = add_mov pos mov in
  if (List.mem tpos g.garbage || g.confused) && mov <> no_move then
    Coth 'm', NAmove (mov, na)
  else if mov = no_move then Coth 's', na
  else Cmov mov, na

let find_doors g (rmin, cmin, rmax, cmax) =
  let list = [] in
  let list =
    let rec loop list pos =
      if pos.col > cmax then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '-' then
            (pos, DoorUp) :: list
          else list
        in
        loop list (pos_right pos)
    in
    loop list {row = rmin - 1; col = cmin}
  in
  let list =
    let rec loop list pos =
      if pos.row > rmax then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '|' then
            (pos, DoorRight) :: list
          else list
        in
        loop list (pos_down pos)
    in
    loop list {row = rmin; col = cmax + 1}
  in
  let list =
    let rec loop list pos =
      if pos.col < cmin then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '-' then
            (pos, DoorDown) :: list
          else list
        in
        loop list (pos_left pos)
    in
    loop list {row = rmax + 1; col = cmax}
  in
  let list =
    let rec loop list pos =
      if pos.row < rmin then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '|' then
            (pos, DoorLeft) :: list
          else list
        in
        loop list (pos_up pos)
    in
    loop list {row = rmax; col = cmin - 1}
  in
  list

let inside_room (rmin, cmin, rmax, cmax) pos =
  rmin <= pos.row && pos.row <= rmax && cmin <= pos.col && pos.col <= cmax

let inside_room_or_at_door (rmin, cmin, rmax, cmax) pos =
  rmin - 1 <= pos.row && pos.row <= rmax + 1 && cmin - 1 <= pos.col &&
  pos.col <= cmax + 1

let dist_to_closest g room pos pred =
  let rec loop_dist d =
    let rec loop found di dj =
      let mov = {di = di; dj = dj} in
      if di = d + 1 then if found then loop_dist (d + 1) else None
      else if dj = d + 1 then loop found (di + 1) (-d)
      else if di > -d && di < d && dj = -d + 1 then loop found di d
      else if inside_room_or_at_door room (add_mov pos mov) then
        let ch = dung_char g.dung (add_mov pos mov) in
        if pred ch mov then Some mov
        else
          let found =
            found || List.mem ch list_mov_ch || List.mem ch list_obj_ch
          in
          loop found di (dj + 1)
      else loop found di (dj + 1)
    in
    loop false (-d) (-d)
  in
  loop_dist 1

let room_row_min = [| 1; 8; 16 |]
let room_col_min = [| 0; 27; 53 |]
let room_row_max = [| 6; 14; 22 |]
let room_col_max = [| 25; 51; 79 |]

let gen_room_row (rmin, cmin, rmax, cmax) =
  let rec loop i =
    if i = 3 then None
    else if
      room_row_min.(i) <= rmin && rmin <= room_row_max.(i) &&
      room_row_min.(i) <= rmax && rmax <= room_row_max.(i)
    then
      Some i
    else loop (i + 1)
  in
  loop 0
let room_row room =
  match gen_room_row room with
    Some i -> i
  | None -> 0

let gen_room_col (rmin, cmin, rmax, cmax) =
  let rec loop j =
    if j = 3 then None
    else if
      room_col_min.(j) <= cmin && cmin <= room_col_max.(j) &&
      room_col_min.(j) <= cmax && cmax <= room_col_max.(j)
    then
      Some j
    else loop (j + 1)
  in
  loop 0
let room_col room =
  match gen_room_col room with
    Some i -> i
  | None -> 0

let is_big_room g =
  match g.rogue_room_and_door with
    Some (room, None) -> gen_room_row room = None
  | Some (_, Some _) | None -> false

let number_of_visited_rooms g =
  let rec loop n rr rc =
    if rr = 3 then n
    else if rc = 3 then loop n (rr + 1) 0
    else if g.visited.(rr).(rc) then loop (n + 1) rr (rc + 1)
    else loop n rr (rc + 1)
  in
  loop 0 0 0

let one_step_to_exit_room =
  function
    DoorUp -> {di = -1; dj = 0}
  | DoorDown -> {di = 1; dj = 0}
  | DoorLeft -> {di = 0; dj = -1}
  | DoorRight -> {di = 0; dj = 1}

let one_step_to_enter_room =
  function
    DoorUp -> {di = 1; dj = 0}
  | DoorDown -> {di = -1; dj = 0}
  | DoorLeft -> {di = 0; dj = 1}
  | DoorRight -> {di = 0; dj = -1}

let doors_not_explorated g room dl =
  List.fold_left
    (fun dl (pos, dd) ->
       let neighbourg_already_explorated =
         let rr = room_row room in
         let rc = room_col room in
         match dd with
           DoorUp -> g.visited.(rr-1).(rc)
         | DoorDown -> g.visited.(rr+1).(rc)
         | DoorLeft -> g.visited.(rr).(rc-1)
         | DoorRight -> g.visited.(rr).(rc+1)
       in
       if neighbourg_already_explorated then dl
       else
         let mov = one_step_to_exit_room dd in
         let pos2 = add_mov pos mov in
         let ch = dung_char g.dung pos2 in
         if ch <> ' ' then
           let nspaces =
             (* if just a '#' beside the door (nspaces = 3 below), count it
                also as not explorated *)
             List.fold_left
               (fun n pos -> if dung_char g.dung pos = ' ' then n + 1 else n)
               0 [pos_up pos2; pos_down pos2; pos_left pos2; pos_right pos2]
           in
           if nspaces = 3 then (pos, dd) :: dl else dl
         else (pos, dd) :: dl)
    [] dl

let doors_not_explorated_in_current_room g =
  match g.rogue_room_and_door with
    Some (room, _) ->
      let doors = find_doors g room in doors_not_explorated g room doors
  | None -> []

let nothing_interesting_in_current_room g =
  let pos = rogue_pos g in
  match g.rogue_room_and_door with
    Some (room, _) ->
      let doors = find_doors g room in
      doors_not_explorated g room doors = [] &&
      dist_to_closest g room pos
        (fun ch mov ->
           is_monster ch ||
           List.mem ch list_obj_ch &&
           not (List.mem (add_mov pos mov) g.garbage)) =
        None
  | None -> true

(* paths *)

let is_trap g pos =
  if in_dung g pos then
    match try Some (Hashtbl.find g.traps pos) with Not_found -> None with
      Some trap_opt -> trap_opt <> None
    | None ->
        let ch = dung_char g.dung pos in
        if ch = '^' then
          begin Hashtbl.replace g.traps pos (Some None); true end
        else if ch = '.' || ch = ' ' then
          let v = try PosMap.find pos g.trail with Not_found -> 0 in
          if g.map_showed_since > 0 || v > 0 then
            Hashtbl.replace g.traps pos None;
          false
        else
          begin
            if is_monster ch || ch = '@' then ()
            else Hashtbl.replace g.traps pos None;
            false
          end
  else false

let run_around_list = [1; 3; 4; 6; 0; 2; 5; 7]

let is_moving g t pos =
  match t.t_prev_pos with
    Some old_g ->
      old_g.level <> g.level ||
      dung_char old_g.dung pos <> dung_char g.dung pos
  | None -> false

(* *)

let distance pos1 pos2 =
  max (abs (pos1.row - pos2.row)) (abs (pos1.col - pos2.col))

(* *)

let identified_scroll_kind_of_message g s =
  let transl = transl g in
  if transl.is_scroll_of_aggravate_monsters s then SKaggr_mon
  else if transl.is_scroll_of_enchant_armor s then SKench_ar
  else if transl.is_scroll_of_enchant_weapon s then SKench_wea
  else if transl.is_scroll_of_hold_monsters s then SKhold_mon
  else if transl.is_scroll_of_identification s then SKident
  else if transl.is_scroll_of_magic_mapping s then SKmagic_map
  else if transl.is_scroll_of_protection s then SKprotect
  else if transl.is_scroll_of_scare_monsters s then SKscare_mon
  else if transl.is_scroll_of_teleport s then
    begin g.teleport_discovered <- true; SKteleport end
  else if transl.is_scroll_of_remove_curse s then SKuncurse
  else SKother

let quaffed_potion_of_message g s =
  let transl = transl g in
  if transl.is_message_faster s then Ipotion PKacceler
  else if transl.is_message_stronger s then Ipotion PKincr_stren
  else if transl.is_message_darkness s then
    begin g.blindness_discovered <- true; Ipotion PKblindness end
  else if transl.is_message_much_better s then Ipotion PKextra_heal
  else if transl.is_message_cosmic s then
    begin g.hallucination_discovered <- true; Ipotion PKhallucination end
  else if transl.is_message_better s then Ipotion PKhealing
  else if transl.is_message_welcome_to_level s then Ipotion PKraise_level
  else if transl.is_message_warm_all_over s then Ipotion PKrestore_str
  else if transl.is_message_tastes_like s then Ipotion PKsee_invis
  else Ipotion PKother

let read_scroll_kind_of_message g s =
  let transl = transl g in
  if transl.is_message_pitched_noise s then SKaggr_mon
  else if transl.is_message_armor_blue s then SKench_ar
  else if transl.is_message_weapon_blue s then SKench_wea
  else if transl.is_message_hold_monster s then SKhold_mon
  else if transl.is_scroll_of_identification s then SKident
  else if transl.is_message_shows_a_map s then SKmagic_map
  else if transl.is_message_gold_shield s then SKprotect
  else if transl.is_message_maniacal_laughter s then SKscare_mon
  else if transl.is_message_watching_over s then SKuncurse
  else SKother

let identified_potion_kind_of_message g s =
  let transl = transl g in
  if transl.is_potion_of_haste_self s then PKacceler
  else if transl.is_potion_of_increase_strength s then PKincr_stren
  else if transl.is_potion_of_blindness s then
    begin g.blindness_discovered <- true; PKblindness end
  else if transl.is_potion_of_monster_detection s then PKdetect_mon
  else if transl.is_potion_of_object_detection s then PKdetect_obj
  else if transl.is_potion_of_hallucination s then
    begin g.hallucination_discovered <- true; PKhallucination end
  else if transl.is_potion_of_healing s then PKhealing
  else if transl.is_potion_of_extra_healing s then PKextra_heal
  else if transl.is_potion_of_raise_level s then PKraise_level
  else if transl.is_potion_of_restore_strength s then PKrestore_str
  else if transl.is_potion_of_see_invisible s then PKsee_invis
  else PKother

let weapon_kind_of_message g s =
  let transl = transl g in
  if transl.is_two_handed_sword s then WKtwo_handed_sword
  else if transl.is_long_sword s then WKlong_sword
  else if transl.is_mace s then WKmace
  else if transl.is_short_bow s then WKshort_bow
  else if transl.is_arrow s then WKarrows
  else WKother

(* *)

let stairs_pos g =
  let rec loop list row col =
    if row = g.dung.nrow then
      begin
        if not g.was_hallucinated && List.length list = 1 then
          g.sure_stairs_pos <- Some (List.hd list);
        list
      end
    else if col = g.dung.ncol then loop list (row + 1) 0
    else
      let pos = {row = row; col = col} in
      let list =
        if List.mem pos g.garbage then list
        else if dung_char g.dung pos = '%' then pos :: list
        else list
      in
      loop list row (col + 1)
  in
  loop [] 1 0

let monster g ch =
  if is_monster ch then
    let i = Char.code ch - Char.code 'A' in
    let ch2 = (transl g).monsters.[i] in
    if ch2 >= 'A' && ch2 <= 'Z' then ch2
    else failwith (sprintf "monster '%c' not yet translated" ch)
  else ch

let is_gold_seeker_monster g mch = monster g mch = 'O'
