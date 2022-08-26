(* $Id: rob_misc.ml,v 1.28 2018/04/26 09:52:37 deraugla Exp $ *)

#load "pa_if_match.cmo";

open Printf;
open Scanf;
open Rob_def;
open Rob_position;

value string_create = Bytes.create;
value string_set = Bytes.set;
value string_of_bytes = Bytes.to_string;

(* language dependent *)

value end_with str s =
  let len = String.length s in
  String.length str >= len && String.sub str (String.length str - len) len = s
;

value contains str s =
  let len = String.length s in
  loop 0 0 where rec loop i j =
    if j = len then True
    else if i = String.length str then False
    else if str.[i] = s.[j] then loop (i + 1) (j + 1)
    else loop (i - j + 1) 0
;

value contains_yn has has_not s =
  (has = [] || List.exists (contains s) has) &&
  not (List.exists (contains s) has_not)
;

value lang_en =
  {scan_status_line line =
     sscanf line
       "Level: %d Gold: %d Hp: %d(%d) Str: %d(%d) Arm: %d  Exp: %d/%d %s"
       (fun lev gold hp max_hp stren max_stren arm exp max_exp hunger ->
          {sl_level = lev; sl_gold = gold; sl_hp = hp; sl_max_hp = max_hp;
           sl_stren = stren; sl_max_stren = max_stren; sl_exp = exp;
           sl_max_exp = max_exp; sl_hunger = hunger});
   answer_left_hand = 'l';
   answer_yes = 'y';
   flaming_monster = 'D';
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
   is_message_armor_blue s = contains s " armor glows";
   is_message_attacked_by_flame s = contains s "flame";
   is_message_better s = contains s "feel better";
   is_message_boring s = contains s "boring";
   is_message_cosmic s = contains s "cosmic";
   is_message_cursed s = contains s " cursed";
   is_message_darkness s = contains s "cloak of";
   is_message_dart s = contains s "small dart";
   is_message_dead = contains_yn ["Died of"; "Killed"] [];
   is_message_faint s = contains s "ou faint";
   is_message_faster s = contains s "much faster";
   is_message_gold_shield s = contains s " gold shield";
   is_message_has_been_confused =
     contains_yn ["feel confused"; "has confused"] [];
   is_message_held s = contains s "being held";
   is_message_hold_monster = contains_yn ["strange sense"; "freeze"] [];
   is_message_less_confused s = contains s "feel less";
   is_message_maniacal_laughter s = contains s "maniacal laughter";
   is_message_move_again s = contains s "move again";
   is_message_moved_onto s = contains s "Moved onto";
   is_message_much_better s = contains s "feel much";
   is_message_no_darkness s = contains s "veil of";
   is_message_nothing_appropriate s = contains s "appropriate";
   is_message_pack_full s = contains s " too full";
   is_message_pitched_noise s = contains s "pitched humming noise";
   is_message_really_pick s = contains s "pick up";
   is_message_shows_a_map s = contains s "have a map";
   is_message_something_attacked s = contains s "The something";
   is_message_something_there s = contains s "something there";
   is_message_stronger s = contains s "muscles";
   is_message_tastes_like s = contains s "tastes like";
   is_message_there_is_no s = contains s "No such item";
   is_message_warm_all_over s = contains s "warm all";
   is_message_watching_over s = contains s "watching over";
   is_message_weaken_armor s = contains s "armor weakens";
   is_message_weapon_blue = contains_yn [" glow"] ["armor"];
   is_message_welcome_to_level s = contains s "Welcome to";
   is_potion s = contains s " potion";
   is_potion_of_blindness s = contains s "blindness";
   is_potion_of_extra_healing s = contains s "extra healing";
   is_potion_of_hallucination s = contains s "of hallucination";
   is_potion_of_haste_self s = contains s "haste self";
   is_potion_of_healing s = contains s "of healing";
   is_potion_of_increase_strength s = contains s "increase strength";
   is_potion_of_monster_detection s = contains s "detect monster";
   is_potion_of_object_detection s = contains s "detect things";
   is_potion_of_raise_level s = contains s "raise level";
   is_potion_of_restore_strength s = contains s "restore strength";
   is_potion_of_see_invisible s = contains s "see invisible";
   is_ring s = contains s " ring";
   is_ring_of_slow_digestion s = contains s "slow digestion";
   is_scroll s = contains s " scroll";
   is_scroll_of_aggravate_monsters s = contains s "aggravate";
   is_scroll_of_enchant_armor s = contains s "enchant armor";
   is_scroll_of_enchant_weapon s = contains s "enchant weapon";
   is_scroll_of_hold_monsters s = contains s "hold monster";
   is_scroll_of_identification s = contains s "of identify";
   is_scroll_of_magic_mapping s = contains s " magic mapping";
   is_scroll_of_protection s = contains s "protect armor";
   is_scroll_of_scare_monsters s = contains s "scare";
   is_scroll_of_remove_curse s = contains s "remove curse";
   is_scroll_of_teleport s = contains s "teleportation";
   is_short_bow s = contains s "short bow";
   is_trap_door s = contains s "Trap door";
   is_two_handed_sword s = contains s "two-handed";
   is_very_hungry s = s = "faint";
   is_wand = contains_yn [" staff"; " wand"] [];
   is_wand_of_cancellation s = contains s "cancellation";
   is_wand_of_magic_missile s = contains s " magic missile";
   is_weapon =
     contains_yn
       [" arrow"; " short bow"; " dagger"; " dart"; " mace"; " shuriken";
        " spear"; " sword"]
      [];
   message_more s =
     loop [" -- More --"; "--More--"] where rec loop =
       fun
       [ [m :: rest] -> if end_with s m then m else loop rest
       | [] -> "" ];
   monsters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"}
;

value lang_fr =
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
  {scan_status_line line =
     sscanf line
       "Niveau: %d Or: %d Pv: %d(%d) Forc: %d(%d) Arm: %d Exp: %d/%d %s"
       (fun lev gold hp max_hp stren max_stren arm exp max_exp hunger ->
          {sl_level = lev; sl_gold = gold; sl_hp = hp; sl_max_hp = max_hp;
           sl_stren = stren; sl_max_stren = max_stren; sl_exp = exp;
           sl_max_exp = max_exp; sl_hunger = hunger});
   answer_left_hand = 'g';
   answer_yes = 'o';
   flaming_monster = 'D';
   is_armor s = List.exists (contains s) list_armor;
   is_arrow s = contains s " flèche";
   is_fallen_down s = contains s "dans une trappe";
   is_food s = List.exists (contains s) list_food;
   is_identified_potion s =
      List.exists (contains s)
        ["potion de"; "potion d'"; "potion hallu"; "potions de"; "potions d'";
         "potions hallu"]
   ;
   is_identified_ring txt = not (contains txt " avec un");
   is_identified_scroll s = not (contains s "intitulé");
   is_identified_wand_kind s = List.exists (contains s) list_wand_of;
   is_leather_armor s = contains s "armure de cuir";
   is_long_sword s = contains s "longue épée";
   is_mace s = contains s "massue";
   is_message_about_fliting_monster =
     contains_yn [" wombat "; " fantôme "] [];
   is_message_aggressive_monster s =
     List.exists (contains s) ["rate"; "touche"; "faiblir"] &&
     not (List.exists (contains s)
            ["amulette"; "arc court"; "armure"; "bague"; "bâton"; "churikène";
             "dard"; "épée"; "flèche"; "massue"; "parchemin"; "poignard";
             "potion"]) &&
     not (List.exists (\= s)
            ["Tu rates."; "Tu touches."; "Vous ratez."; "Vous touchez."])
   ;
   is_message_armor_blue s = contains s " armure devient";
   is_message_attacked_by_flame s = contains s "flamme";
   is_message_better s = contains s "sentir mieux";
   is_message_boring s = contains s "ennuyeux";
   is_message_cosmic s = contains s "bizarre";
   is_message_cursed s = contains s " maudit";
   is_message_darkness s = contains s "cape de";
   is_message_dart s = contains s "petit dard";
   is_message_dead s = List.exists (contains s) list_dead;
   is_message_faint s = contains s "écroule";
   is_message_faster s = contains s " accél";
   is_message_gold_shield s = contains s " bouclier d'or";
   is_message_has_been_confused s =
     List.exists (contains s)
       ["a déconcerté"; "sens déconcerté"; "sentez déconcerté"]
   ;
   is_message_held s = contains s "immobilisé";
   is_message_hold_monster txt =
     List.exists (contains txt) ["impression d'avoir"; "gèle"]
   ;
   is_message_less_confused s =
     List.exists (contains s) ["sens moins"; "sentez moins"]
   ;
   is_message_maniacal_laughter s = contains s "rires maniaques";
   is_message_move_again s = contains s "bouger de nouveau";
   is_message_moved_onto s = List.exists (contains s) move_onto_list;
   is_message_much_better s = contains s "sentir beaucoup";
   is_message_no_darkness s = contains s "voile de";
   is_message_nothing_appropriate s = contains s "approprié";
   is_message_pack_full s = contains s " trop plein";
   is_message_pitched_noise s = contains s "bourdonnement";
   is_message_really_pick s = contains s "prendre le";
   is_message_shows_a_map s = contains s "montrer un plan";
   is_message_something_attacked s = contains s "Quelque chose";
   is_message_something_there s = contains s "quelque chose";
   is_message_stronger s = contains s "muscles";
   is_message_tastes_like s = contains s "un goût d";
   is_message_there_is_no s = contains s "Il n'y en a pas";
   is_message_warm_all_over s = contains s "tout chaud";
   is_message_watching_over s = contains s "veille sur";
   is_message_weaken_armor s = contains s "armure faiblit";
   is_message_weapon_blue txt =
     contains txt " devient" && not (contains txt "armure")
   ;
   is_message_welcome_to_level s = contains s "venue au niveau";
   is_potion s = contains s " potion";
   is_potion_of_blindness s = contains s "cécité";
   is_potion_of_extra_healing s = contains s "guérison supplémentaire";
   is_potion_of_hallucination s = contains s "hallucinogène";
   is_potion_of_haste_self s = contains s "accélération";
   is_potion_of_healing s = contains s "guérison" && not (contains s "suppl");
   is_potion_of_increase_strength s = contains s "renforcement";
   is_potion_of_monster_detection s = contains s "détection des monstres";
   is_potion_of_object_detection s = contains s "détection des objets";
   is_potion_of_raise_level s = contains s "passage au niveau";
   is_potion_of_restore_strength s = contains s "restauration";
   is_potion_of_see_invisible s = contains s "vision de";
   is_ring s = contains s " bague ";
   is_ring_of_slow_digestion s = contains s " digestion lente";
   is_scroll s = contains s " parchemin";
   is_scroll_of_aggravate_monsters s = contains s "aggravation";
   is_scroll_of_enchant_armor s = contains s "enchantement d'armure";
   is_scroll_of_enchant_weapon s = contains s "enchantement d'arme";
   is_scroll_of_hold_monsters s = contains s "immobilisation";
   is_scroll_of_identification s = contains s "d'identification";
   is_scroll_of_magic_mapping s = contains s "plan magique";
   is_scroll_of_protection s = contains s "protection";
   is_scroll_of_scare_monsters s = contains s "effraie";
   is_scroll_of_remove_curse s = contains s "de malédiction";
   is_scroll_of_teleport s = contains s "téléportation";
   is_short_bow s = contains s "arc court";
   is_trap_door s = do {
     if List.exists (contains s)
          ["dard empoi"; " douche"; " loup"; " à téléportation";
           "gaz soporifique"]
     then
       False
     else if s = "Trappe." then True
     else failwith "is_trap_door fr not impl"
   };
   is_two_handed_sword s = contains s "épée à deux";
   is_very_hungry s = s = "épuisé";
   is_wand s = List.exists (contains s) list_wand;
   is_wand_of_cancellation s = contains s "annulation";
   is_wand_of_magic_missile s = contains s " missile magique";
   is_weapon s = List.exists (contains s) list_weapons;
   message_more s = do {
     let m = " -- Suite --" in
     if end_with s m then m else ""
   };
   monsters = "AFXDEPILWJCHMNOGQRSTUVBKYZ"}
;

value transl g =
  let s = g.lang in
  if s = "en" then lang_en
  else if s = "fr" || s = "fr-tt" then lang_fr
  else failwith (sprintf "lang '%s' not impl" s)
;

(* *)

value min_time_in_level = 300;
value max_time_in_level = 500;

value not_impl name x =
  let desc =
    if Obj.tag (Obj.repr x) = Obj.tag (Obj.repr "") then
      "\"" ^ Obj.magic x ^ "\""
    else if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  sprintf "\"not impl: %s; %s\"" name (String.escaped desc)
;

value trace t txt = do {
  if t.t_move_trace then do {
    eprintf "%s" txt;
    flush stderr
  }
  else ()
};

value is_monster ch = ch >= 'A' && ch <= 'Z';

(* *)

value sleep x = let _ = Unix.select [Unix.stdin] [] [] x in ();
value tempo g x = sleep (x /. g.speed);

exception NoRogue;
value rogue_pos g =
  match g.rogue_pos with
  [ Some pos -> pos
  | None -> raise NoRogue ]
;

value pos_in_dung dung pos =
  pos.row > 0 && pos.row < dung.nrow - 1 &&
  pos.col >= 0 && pos.col < dung.ncol
;
value dung_char dung pos = dung.tab.(pos.row).[pos.col];

value minus_bar = ['-'; '|'];
value minus_bar_space = ['-'; '|'; ' '];

value dung_char_eq dung pos ch =
  pos_in_dung dung pos && dung_char dung pos = ch
;

value add_mov pos mov = {row = pos.row + mov.di; col = pos.col + mov.dj};
value opposite_move mov = {di = -mov.di; dj = -mov.dj};
value perpendicular_move mov = {di = mov.dj; dj = mov.di};

(* *)

value pos_up pos = {(pos) with row = pos.row-1};
value pos_down pos = {(pos) with row = pos.row+1};
value pos_left pos = {(pos) with col = pos.col-1};
value pos_right pos = {(pos) with col = pos.col+1};

value pos_is_at_door dung pos =
  if dung_char dung pos = '+' then True
  else if List.mem (dung_char dung pos) minus_bar_space then False
  else if
    pos_in_dung dung (pos_up pos) &&
    pos_in_dung dung (pos_down pos) &&
    List.mem (dung_char dung (pos_up pos)) minus_bar &&
    List.mem (dung_char dung (pos_down pos)) minus_bar &&
    not (dung_char dung (pos_up pos) = '-' &&
         dung_char dung (pos_down pos) = '-')
  then True
  else if
    dung_char_eq dung (pos_left pos) '-' &&
    dung_char_eq dung (pos_right pos) '-' &&
    not (dung_char_eq dung (add_mov pos {di = -2; dj = 1}) '|' &&
         dung_char_eq dung (add_mov pos {di = 2; dj = -1}) '|') &&
    not (dung_char_eq dung (add_mov pos {di = -2; dj = 1}) '|' &&
         dung_char_eq dung (add_mov pos {di = 1; dj = -1}) '|') &&
    not (dung_char_eq dung (add_mov pos {di = -1; dj = 1}) '|' &&
         dung_char_eq dung (add_mov pos {di = 2; dj = -1}) '|') &&
    not (dung_char_eq dung (add_mov pos {di = -1; dj = 1}) '|' &&
         dung_char_eq dung (add_mov pos {di = 1; dj = -1}) '|')
  then True
  else False
;

value pos_is_inside_room dung pos =
  if pos_is_at_door dung pos || List.mem (dung_char dung pos) ['|'; '-']
  then
    False
  else
    loop 0 (pos_right pos) where rec loop intersect pos =
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
;

value pos_current_room dung pos = do {
  if pos_is_inside_room dung pos then
    let rmin =
      loop (pos_up pos) where rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if dung_char dung pos = '-' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_left pos) &&
            dung_char dung (pos_left pos) = '-' ||
          pos_in_dung dung (pos_right pos) &&
            dung_char dung (pos_right pos) = '-'
        then Some (pos.row + 1)
        else loop (pos_up pos)
    in
    let cmin =
      loop (pos_left pos) where rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if dung_char dung pos = '|' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_up pos) &&
            dung_char dung (pos_up pos) = '|' ||
          pos_in_dung dung (pos_down pos) &&
            dung_char dung (pos_down pos) = '|'
        then Some (pos.col + 1)
        else loop (pos_left pos)
    in
    let rmax =
      loop (pos_down pos) where rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if dung_char dung pos = '-' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_left pos) &&
            dung_char dung (pos_left pos) = '-' ||
          pos_in_dung dung (pos_right pos) &&
            dung_char dung (pos_right pos) = '-'
        then Some (pos.row - 1)
        else loop (pos_down pos)
    in
    let cmax =
      loop (pos_right pos) where rec loop pos =
        if not (pos_in_dung dung pos) then None
        else if dung_char dung pos = '|' || dung_char dung pos = '+' ||
          pos_in_dung dung (pos_up pos) &&
            dung_char dung (pos_up pos) = '|' ||
          pos_in_dung dung (pos_down pos) &&
            dung_char dung (pos_down pos) = '|'
        then Some (pos.col - 1)
        else loop (pos_right pos)
    in
    match (rmin, cmin, rmax, cmax) with
    [ (Some rmin, Some cmin, Some rmax, Some cmax) ->
        let room = (rmin, cmin, rmax, cmax) in
        Some room
    | _ -> None ]
  else None
};

value pos_room_and_door dung pos = do {
  if pos_is_at_door dung pos then do {
    loop
      [(pos_up pos, DoorDown); (pos_down pos, DoorUp);
       (pos_left pos, DoorRight); (pos_right pos, DoorLeft)]
    where rec loop =
      fun
      [ [(pos, dir) :: rest] ->
          if pos_in_dung dung pos then
            match pos_current_room dung pos with
            [ Some room -> Some (room, Some dir)
            | None -> loop rest ]
          else loop rest
      | [] -> None ]
  }
  else do {
    match pos_current_room dung pos with
    [ Some room -> Some (room, None)
    | None -> None ]
  }
};

value in_dung g pos = pos_in_dung g.dung pos;

value room_and_door g pos =
  if pos = rogue_pos g then g.rogue_room_and_door
  else pos_room_and_door g.dung pos
;

value current_room g pos =
  match room_and_door g pos with
  [ Some (room, None) -> Some room
  | Some (_, Some _) | None -> None ]
;

value current_room_possibly_at_door g pos =
  match room_and_door g pos with
  [ Some (room, _) -> Some room
  | None -> None ]
;

value is_inside_room g pos =
  match room_and_door g pos with
  [ Some (_, None) -> True
  | Some (_, Some _) | None -> False ]
;

value is_at_door g pos =
  match room_and_door g pos with
  [ Some (_, Some _) -> True
  | Some (_, None) | None -> False ]
;

value room_of_door g pos =
  match room_and_door g pos with
  [ Some (room, Some dir) -> Some (room, dir)
  | Some (_, None) | None -> None ]
;

value common_room_with g pos2 =
  match (g.rogue_room_and_door, room_and_door g pos2) with
  [ (Some (room, _), Some (room2, _)) ->
      if room = room2 then Some room else None
  | _ -> None ]
;

value in_same_rooms g pos1 pos2 =
  match (room_and_door g pos1, room_and_door g pos2) with
  [ (Some (room1, _), Some (room2, _)) -> room1 = room2
  | _ -> False ]
;

(* *)

value list_border = [' '; '|'; '-'];
value list_border_in_room = ['|'; '-'];

value old_can_move_to g in_room_or_at_door pos tpos =
  let list_border =
    if in_room_or_at_door then list_border_in_room else list_border
  in
  if not (in_dung g tpos) then False
  else if
    List.mem (dung_char g.dung tpos) list_border ||
    List.mem g.dung.tab.(tpos.row).[pos.col] list_border ||
    List.mem g.dung.tab.(pos.row).[tpos.col] list_border ||
    dung_char g.dung tpos = ' ' && current_room g tpos = None
  then False
  else True
;

value can_move_to g tpos = do {
  let pos = rogue_pos g in
  old_can_move_to g (g.rogue_room_and_door <> None) pos tpos
};

value level_of_faster_monsters = 29;
value level_of_very_mean_monsters = 52;

value health_points g =
  match g.status_line with
  [ Some sl -> sl.sl_hp
  | None -> assert False ]
;

value max_health_points g =
  match g.status_line with
  [ Some sl -> sl.sl_max_hp
  | None -> assert False ]
;

value health_is_maximum g =
  match g.status_line with
  [ Some sl -> sl.sl_hp = sl.sl_max_hp
  | None -> True ]
;

value random_int g n =
  if n = 1 then 0 else Random.State.int g.random_state n
;

value mov_of_k k =
  if k < 3 then {di = -1; dj = k-1}
  else if k < 5 then {di = 0; dj = 2*k-7}
  else {di = 1; dj = k-6}
;

value around_pos g pos = do {
  let s = string_create 8 in
  for k = 0 to 7 do {
    let mov = mov_of_k k in
    let pos = add_mov pos mov in
    string_set s k (if in_dung g pos then dung_char g.dung pos else ' ');
  };
  {ar = string_of_bytes s}
};

value no_move = {di = 0; dj = 0};

value move_between pos1 pos2 = do {
  let mov = {di = pos2.row - pos1.row; dj = pos2.col - pos1.col} in
  assert (abs mov.di <= 1 && abs mov.dj <= 1);
  assert (mov <> no_move);
  mov
};

value basic_command_of_move move =
  if move.di = -1 then
    if move.dj = -1 then 'y'
    else if move.dj = 0 then 'k'
    else 'u'
  else if move.di = 0 then
    if move.dj = -1 then 'h'
    else if move.dj = 0 then 's'
    else 'l'
  else
    if move.dj = -1 then 'b'
    else if move.dj = 0 then 'j'
    else 'n'
;

value move_command2 g pos1 pos2 na = do {
  let mov = move_between pos1 pos2 in
  if mov = no_move then
    (Coth 's', na, Some mov)
  else if List.mem pos2 g.garbage || g.confused then
    (Coth 'm', NAmove mov na, Some mov)
  else
    (Cmov mov, na, Some mov)
};

value list_mov_ch = ['.'; '+'; '#'; '%'];
value list_obj_ch = ['*'; '!'; '?'; '/'; ']'; ')'; '='; ':'];
value list_obj_ch2 = ['*'; '!'; '?'; '/'; ']'; ')'];

value list_find f l = try Some (List.find f l) with [ Not_found -> None ];

value rec list_extract_nth n =
  fun
  [ [x :: l] ->
      if n = 0 then (x, l)
      else
        let (y, l) = list_extract_nth (n - 1) l in
        (y, [x :: l])
  | [] -> invalid_arg "list_extract_nth" ]
;

value shuffle g list =
  loop [] list where rec loop r list =
    let len = List.length list in
    if len = 0 then r
    else
      let n = random_int g len in
      let (x, l) = list_extract_nth n list in
      loop [x :: r] l
;

(* *)

value armor_value s =
  try
    let i = String.index s '[' in
    let j = String.index_from s i ']' in
    let v = int_of_string (String.sub s (i + 1) (j - i - 1)) in
    Some v
  with
  [ Not_found | Failure _ -> None ]
;

value main_sword_value g =
  let (_, obj) = List.assoc g.main_sword g.pack in
  match obj with
  [ Pweapon {we_kind = WKtwo_handed_sword; we_value = Some n} -> n
  | _ -> 0 ]
;

value wand_value = armor_value;

value weapon_value s =
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
    let v1 = int_of_string (String.sub s (j+2) (i-j-2)) in
    let v2 = int_of_string (String.sub s (i+2) (k-i-2)) in
    Some (s1 * v1 + s2 * v2)
  with
  [ Not_found | Failure _ -> None ]
;

value move_command g pos mov na =
  let tpos = add_mov pos mov in
  if (List.mem tpos g.garbage || g.confused) && mov <> no_move then
    (Coth 'm', NAmove mov na)
  else if mov = no_move then
    (Coth 's', na)
  else
    (Cmov mov, na)
;

value find_doors g (rmin, cmin, rmax, cmax) =
  let list = [] in
  let list =
    loop list {row = rmin-1; col = cmin} where rec loop list pos =
      if pos.col > cmax then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '-' then
            [(pos, DoorUp) :: list]
          else list
        in
        loop list (pos_right pos)
  in
  let list =
    loop list {row = rmin; col = cmax + 1} where rec loop list pos =
      if pos.row > rmax then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '|' then
            [(pos, DoorRight) :: list]
          else list
        in
        loop list (pos_down pos)
  in
  let list =
    loop list {row = rmax + 1; col = cmax} where rec loop list pos =
      if pos.col < cmin then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '-' then
            [(pos, DoorDown) :: list]
          else list
        in
        loop list (pos_left pos)
  in
  let list =
    loop list {row = rmax; col = cmin - 1} where rec loop list pos =
      if pos.row < rmin then list
      else
        let list =
          if in_dung g pos && dung_char g.dung pos <> '|' then
            [(pos, DoorLeft) :: list]
          else list
        in
        loop list (pos_up pos)
  in
  list
;

value inside_room (rmin, cmin, rmax, cmax) pos =
  rmin <= pos.row && pos.row <= rmax && cmin <= pos.col && pos.col <= cmax
;

value inside_room_or_at_door (rmin, cmin, rmax, cmax) pos =
  rmin - 1 <= pos.row && pos.row <= rmax + 1 && cmin - 1 <= pos.col &&
  pos.col <= cmax + 1
;

value dist_to_closest g room pos pred =
  loop_dist 1 where rec loop_dist d =
    loop False (-d) (-d) where rec loop found di dj =
      let mov = {di = di; dj = dj} in
      if di = d + 1 then
        if found then loop_dist (d + 1)
        else None
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
;

value room_row_min = [| 1; 8; 16 |];
value room_col_min = [| 0; 27; 53 |];
value room_row_max = [| 6; 14; 22 |];
value room_col_max = [| 25; 51; 79 |];

value gen_room_row (rmin, cmin, rmax, cmax) =
  loop 0 where rec loop i =
    if i = 3 then
      (* probable big room *)
      None
    else if
      room_row_min.(i) <= rmin && rmin <= room_row_max.(i) &&
      room_row_min.(i) <= rmax && rmax <= room_row_max.(i)
    then Some i
    else loop (i + 1)
;
value room_row room =
  match gen_room_row room with
  [ Some i -> i
  | None -> 0 ]
;

value gen_room_col (rmin, cmin, rmax, cmax) =
  loop 0 where rec loop j =
    if j = 3 then
      (* probable big room *)
      None
    else if
      room_col_min.(j) <= cmin && cmin <= room_col_max.(j) &&
      room_col_min.(j) <= cmax && cmax <= room_col_max.(j)
    then Some j
    else loop (j + 1)
;
value room_col room =
  match gen_room_col room with
  [ Some i -> i
  | None -> 0 ]
;

value is_big_room g =
  match g.rogue_room_and_door with
  [ Some (room, None) -> gen_room_row room = None
  | Some (_, Some _) | None -> False ]
;

value number_of_visited_rooms g =
  loop 0 0 0 where rec loop n rr rc =
    if rr = 3 then n
    else if rc = 3 then loop n (rr + 1) 0
    else if g.visited.(rr).(rc) then loop (n + 1) rr (rc + 1)
    else loop n rr (rc + 1)
;

value one_step_to_exit_room =
  fun
  [ DoorUp -> {di = -1; dj = 0}
  | DoorDown -> {di = 1; dj = 0}
  | DoorLeft -> {di = 0; dj = -1}
  | DoorRight -> {di = 0; dj = 1} ]
;

value one_step_to_enter_room =
  fun
  [ DoorUp -> {di = 1; dj = 0}
  | DoorDown -> {di = -1; dj = 0}
  | DoorLeft -> {di = 0; dj = 1}
  | DoorRight -> {di = 0; dj = -1} ]
;

value doors_not_explorated g room dl =
  List.fold_left
    (fun dl (pos, dd) ->
       let neighbourg_already_explorated =
         let rr = room_row room in
         let rc = room_col room in
         match dd with
         [ DoorUp -> g.visited.(rr-1).(rc)
         | DoorDown -> g.visited.(rr+1).(rc)
         | DoorLeft -> g.visited.(rr).(rc-1)
         | DoorRight -> g.visited.(rr).(rc+1) ]
       in
       if neighbourg_already_explorated then dl
       else do {
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
           if nspaces = 3 then [(pos, dd) :: dl]
           else dl
         else [(pos, dd) :: dl]
       })
    [] dl
;

value doors_not_explorated_in_current_room g =
  match g.rogue_room_and_door with
  [ Some (room, _) -> do {
      let doors = find_doors g room in
      doors_not_explorated g room doors
    }
  | None -> [] ]
;

value nothing_interesting_in_current_room g =
  let pos = rogue_pos g in
  if_match g.rogue_room_and_door with_some (room, _) ->
    let doors = find_doors g room in
    doors_not_explorated g room doors = [] &&
    dist_to_closest g room pos
      (fun ch mov ->
         is_monster ch ||
         List.mem ch list_obj_ch &&
         not (List.mem (add_mov pos mov) g.garbage)) = None
  else True
;

(* paths *)

value is_trap g pos =
  if in_dung g pos then
    match try Some (Hashtbl.find g.traps pos) with [ Not_found -> None ] with
    [ Some trap_opt -> trap_opt <> None
    | None -> do {
        let ch = dung_char g.dung pos in
        if ch = '^' then do {
          Hashtbl.replace g.traps pos (Some None);
          True
        }
        else if ch = '.' || ch = ' ' then do {
          let v = try PosMap.find pos g.trail with [ Not_found -> 0 ] in
          if g.map_showed_since > 0 || v > 0 then
            Hashtbl.replace g.traps pos None
          else ();
          False
        }
        else do {
          if is_monster ch || ch = '@' then ()
          else Hashtbl.replace g.traps pos None;
          False
        }
      } ]
  else False
;

value run_around_list = [1;3;4;6;0;2;5;7];

value is_moving g t pos =
  match t.t_prev_pos with
  [ Some old_g -> do {
      old_g.level <> g.level ||
      dung_char old_g.dung pos <> dung_char g.dung pos
    }
  | None -> False ]
;

(* *)

value distance pos1 pos2 =
  max (abs (pos1.row - pos2.row)) (abs (pos1.col - pos2.col))
;

(* *)

value identified_scroll_kind_of_message g s =
  let transl = transl g in
  if transl.is_scroll_of_aggravate_monsters s then SKaggr_mon
  else if transl.is_scroll_of_enchant_armor s then SKench_ar
  else if transl.is_scroll_of_enchant_weapon s then SKench_wea
  else if transl.is_scroll_of_hold_monsters s then SKhold_mon
  else if transl.is_scroll_of_identification s then SKident
  else if transl.is_scroll_of_magic_mapping s then SKmagic_map
  else if transl.is_scroll_of_protection s then SKprotect
  else if transl.is_scroll_of_scare_monsters s then SKscare_mon
  else if transl.is_scroll_of_teleport s then do {
    g.teleport_discovered := True;
    SKteleport
  }
  else if transl.is_scroll_of_remove_curse s then SKuncurse
  else SKother
;

value quaffed_potion_of_message g s =
  let transl = transl g in
  if transl.is_message_faster s then Ipotion PKacceler
  else if transl.is_message_stronger s then Ipotion PKincr_stren
  else if transl.is_message_darkness s then do {
    g.blindness_discovered := True;
    Ipotion PKblindness
  }
  else if transl.is_message_much_better s then Ipotion PKextra_heal
  else if transl.is_message_cosmic s then do {
    g.hallucination_discovered := True;
    Ipotion PKhallucination
  }
  else if transl.is_message_better s then Ipotion PKhealing
  else if transl.is_message_welcome_to_level s then Ipotion PKraise_level
  else if transl.is_message_warm_all_over s then Ipotion PKrestore_str
  else if transl.is_message_tastes_like s then Ipotion PKsee_invis
  else Ipotion PKother
;

value read_scroll_kind_of_message g s =
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
;

value identified_potion_kind_of_message g s =
  let transl = transl g in
  if transl.is_potion_of_haste_self s then PKacceler
  else if transl.is_potion_of_increase_strength s then PKincr_stren
  else if transl.is_potion_of_blindness s then do {
    g.blindness_discovered := True;
    PKblindness
  }
  else if transl.is_potion_of_monster_detection s then PKdetect_mon
  else if transl.is_potion_of_object_detection s then PKdetect_obj
  else if transl.is_potion_of_hallucination s then do {
    g.hallucination_discovered := True;
    PKhallucination
  }
  else if transl.is_potion_of_healing s then PKhealing
  else if transl.is_potion_of_extra_healing s then PKextra_heal
  else if transl.is_potion_of_raise_level s then PKraise_level
  else if transl.is_potion_of_restore_strength s then PKrestore_str
  else if transl.is_potion_of_see_invisible s then PKsee_invis
  else PKother
;

value weapon_kind_of_message g s =
  let transl = transl g in
  if transl.is_two_handed_sword s then WKtwo_handed_sword
  else if transl.is_long_sword s then WKlong_sword
  else if transl.is_mace s then WKmace
  else if transl.is_short_bow s then WKshort_bow
  else if transl.is_arrow s then WKarrows
  else WKother
;

(* *)

value stairs_pos g =
  loop [] 1 0 where rec loop list row col =
    if row = g.dung.nrow then do {
      if not g.was_hallucinated && List.length list = 1 then
        g.sure_stairs_pos := Some (List.hd list)
      else ();
      list
    }
    else if col = g.dung.ncol then loop list (row + 1) 0
    else
      let pos = {row = row; col = col} in
      let list =
        if List.mem pos g.garbage then list
        else if dung_char g.dung pos = '%' then [pos :: list]
        else list
      in
      loop list row (col + 1)
;

value monster g ch =
  if is_monster ch then do {
    let i = Char.code ch - Char.code 'A' in
    let ch2 = (transl g).monsters.[i] in
    if ch2 >= 'A' && ch2 <= 'Z' then ch2
    else failwith (sprintf "monster '%c' not yet translated" ch)
  }
  else ch
;

value is_gold_seeker_monster g mch = monster g mch = 'O';
