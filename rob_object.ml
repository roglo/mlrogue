(* $Id: rob_object.ml,v 1.18 2010/07/02 00:16:09 deraugla Exp $ *)

#load "pa_if_match.cmo";

open Rob_def;
open Rob_misc;
open Rob_position;
open Printf;

value list_all_obj_ch = ['*'; '!'; '?'; '/'; ']'; ')'; '='; ':'; ','];

value is_object ch = List.mem ch list_all_obj_ch;

(* pack *)

type pack_item = (char * (int * pack_obj));
type pack = list pack_item;

value remove_from_pack g ch nb obj = do {
  g.pack := List.remove_assoc ch g.pack;
  if nb > 1 then g.pack := [(ch, (nb - 1, obj)) :: g.pack] else ();
  match obj with
  [ Pweapon _ -> if nb = 1 then g.pack_full := False else ()
  | _ -> g.pack_full := False ]
};

value redefine_in_pack g ch obj = do {
  let (nb, _) = List.assoc ch g.pack in
  g.pack := List.remove_assoc ch g.pack;
  g.pack := [(ch, (nb, obj)) :: g.pack];
};

value pack_list_find = (list_find : _ -> pack -> _);

value find_potion_in_pack f =
  pack_list_find
    (fun (_, (nb, obj)) ->
       match obj with
       [ Ppotion (Ipotion s) -> f nb s
       | _ -> False ])
;
value potion_of_increase_strength_in_pack =
  find_potion_in_pack
    (fun _ ->
       fun
       [ PKincr_stren -> True
       | _ -> False ])
;
value potion_of_detect_monsters_in_pack =
  find_potion_in_pack
    (fun _ ->
       fun
       [ PKdetect_mon -> True
       | _ -> False ])
;
value potion_of_healing_in_pack =
  find_potion_in_pack
    (fun _ ->
       fun
       [ PKhealing -> True
       | _ -> False ])
;
value several_potions_of_healing_in_pack =
  find_potion_in_pack
    (fun nb ->
       fun
       [ PKhealing -> nb > 1
       | _ -> False ])
;
value potion_of_extra_healing_in_pack =
  find_potion_in_pack
    (fun _ ->
       fun
       [ PKextra_heal -> True
       | _ -> False ])
;
value several_potions_of_extra_healing_in_pack =
  find_potion_in_pack
    (fun nb ->
       fun
       [ PKextra_heal -> nb > 1
       | _ -> False ])
;
value potion_of_see_invisible_in_pack =
  find_potion_in_pack
    (fun _ ->
       fun
       [ PKsee_invis -> True
       | _ -> False ])
;

value find_scroll_in_pack f =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Pscroll (Iscroll s) -> f s
       | _ -> False ])
;

value scroll_of_enchant_armor_in_pack =
  find_scroll_in_pack
    (fun
     [ SKench_ar -> True
     | _ -> False ])
;
value scroll_of_enchant_weapon_in_pack =
  find_scroll_in_pack
    (fun
     [ SKench_wea -> True
     | _ -> False ])
;
value scroll_of_hold_monsters_in_pack =
  find_scroll_in_pack
    (fun
     [ SKhold_mon -> True
     | _ -> False ])
;
value scroll_of_identification_in_pack =
  find_scroll_in_pack
    (fun
     [ SKident -> True
     | _ -> False ])
;
value scroll_of_magic_mapping_in_pack =
  find_scroll_in_pack
    (fun
     [ SKmagic_map -> True
     | _ -> False ])
;
value scroll_of_protect_armor_in_pack =
  find_scroll_in_pack
    (fun
     [ SKprotect -> True
     | _ -> False ])
;
value scroll_of_remove_curse_in_pack =
  find_scroll_in_pack
    (fun
     [ SKuncurse -> True
     | _ -> False ])
;
value scroll_of_scare_monsters_in_pack =
  find_scroll_in_pack
    (fun
     [ SKscare_mon -> True
     | _ -> False ])
;
value scroll_of_teleport_in_pack =
  find_scroll_in_pack
    (fun
     [ SKteleport -> True
     | _ -> False ])
;

value number_of_potions_of_detect_monsters_in_pack pack =
  match potion_of_detect_monsters_in_pack pack with
  [ Some (_, (nb, _)) -> nb
  | None -> 0 ]
;

value number_of_scrolls_of_scare_monsters_in_pack pack =
  match scroll_of_scare_monsters_in_pack pack with
  [ Some (_, (nb, _)) -> nb
  | None -> 0 ]
;

value number_of_scrolls_of_magic_mapping_in_pack pack =
  match scroll_of_magic_mapping_in_pack pack with
  [ Some (_, (nb, _)) -> nb
  | None -> 0 ]
;

value arrows_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Pweapon we -> we.we_kind = WKarrows
       | _ -> False ])
;
value short_bow_in_pack =
  pack_list_find
    (fun (_, (nb, obj)) ->
       match obj with
       [ Pweapon {we_kind = WKshort_bow; we_value = Some _} -> True
       | _ -> False ])
;
value two_handed_swords_in_pack g =
  List.filter
    (fun (_, (_, obj)) ->
       match obj with
       [ Pweapon {we_kind = WKtwo_handed_sword} -> True
       | _ -> False ])
    g.pack
;

value food_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Pfood -> True
       | _ -> False ])
;

value wand_of_magic_missile_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Pwand (Iwand (AKmagic_miss) (Some n)) -> n > 0
       | _ -> False ])
;

value anti_flamer_wand_list_in_pack : pack -> _ =
  loop [] where rec loop list =
    fun
    [ [(ch, (nb, obj)) :: rest] ->
        match obj with
        [ Pwand (Iwand (AKcancel | AKmagic_miss) (Some n)) ->
            let list =
              if n > 0 then [(n, (ch, (nb, obj))) :: list] else list
            in
            loop list rest
        | _ -> loop list rest ]
    | [] ->
        List.sort compare list ]
;

value usable_anti_flamer_wand_in_pack pack =
  match anti_flamer_wand_list_in_pack pack with
  [ [(_, x) :: _] -> Some x
  | [] -> None ]
;

value enough_anti_flamer_wand_in_pack pack =
  List.length (anti_flamer_wand_list_in_pack pack) >= 2
;

value check_armor g pred =
  let best_values =
    loop [] g.pack where rec loop list =
      fun
      [ [(ch, (nb, obj)) :: rest] ->
          let list =
            match obj with
            [ Parmor arm ->
                if_match arm.ar_value with_some v ->
                  [(v, arm.ar_protected, (ch, arm)) :: list]
                else list
            | _ -> list ]
          in
          loop list rest
      | [] ->
          List.sort (fun a b -> compare b a) list ]
  in
  match best_values with
  [ [(v1, True, (ch1, arm1)) :: _] -> Some (ch1, arm1)
  | [(v1, False, (ch1, arm1)) :: _] ->
      if_match pred best_values with_some (v1, (ch1, arm1)) ->
        let ch_arm =
          if_match g.worn_armor with_some (ch_worn, ar_worn) ->
            if_match ar_worn.ar_value with_some v ->
              if v = v1 then (ch_worn, ar_worn) else (ch1, arm1)
            else assert False
          else (ch1, arm1)
        in
        Some ch_arm
      else Some (ch1, arm1)
  | [] -> None ]
;
value best_armor g =
  check_armor g
     (fun
       [ [(v1, _, ch1) :: _] -> Some (v1, ch1)
       | [] -> None ])
;
(* good armor has value of the second in the values list *)
value good_armor g =
  check_armor g
     (fun
       [ [(v1, _, ch1); (v2, _, ch2) :: _] ->
           if (*v2 < 4 ||*) g.level < 3 then Some (v1, ch1) else
           Some (v2, ch2)
       | [_] | [] ->
           None ])
;

value is_best_armor g ch =
  if_match best_armor g with_some (ch1, _) -> ch = ch1 else False
;

value wearing_best_armor g =
  match g.worn_armor with
  [ Some (ch, _) -> is_best_armor g ch
  | None -> False ]
;

value is_good_armor g ch ar =
  if_match good_armor g with_some (ch1, ar1) ->
    ch = ch1 || ar.ar_value = ar1.ar_value
  else False
;

value wearing_good_armor g =
  match g.worn_armor with
  [ Some (ch, ar) -> is_good_armor g ch ar
  | None -> False ]
;

value armor_of_ch g ch =
  match try Some (List.assoc ch g.pack) with [ Not_found -> None ] with
  [ Some (_, Parmor ar) -> Some (ch, ar)
  | Some _ | None -> None ]
;

value unidentified_armor_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Parmor ar -> ar.ar_value = None
       | _ -> False ])
;
value unidentified_scroll_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Pscroll (Uscroll _) -> True
       | _ -> False ])
;
value unidentified_potion_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
       [ Ppotion (Upotion _) -> True
       | _ -> False ])
;
value unidentified_ring_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
       [ Pring (Uring _) -> True
       | _ -> False ])
;
value unidentified_wand_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
       [ Pwand (Uwand _) -> True
       | _ -> False ])
;
value unidentified_two_handed_sword_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
       [ Pweapon we -> we.we_kind = WKtwo_handed_sword && we.we_value = None
       | _ -> False ])
;
value is_wearing_armor g ch =
  if_match g.worn_armor with_some (ch1, _) -> ch = ch1 else False
;
value unidentified_object_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
       [ Parmor ar -> ar.ar_value = None
       | Pscroll (Uscroll _) -> True
       | Ppotion (Upotion _) -> True
       | Pring (Uring _) -> True
       | Pwand (Uwand _ | Iwand (AKcancel | AKmagic_miss) None) -> True
       | Pweapon we -> we.we_kind = WKtwo_handed_sword && we.we_value = None
       | _ -> False ])
;
value unidentified_object_not_used_in_pack g =
  pack_list_find
    (fun (ch, (_, obj)) ->
       match obj with
       [ Parmor ar -> ar.ar_value = None && is_wearing_armor g ch
       | Pscroll (Uscroll _) -> True
       | Ppotion (Upotion _) -> True
       | Pring (Uring _) -> Some ch <> g.ring_of_slow_digestion_on_hand
       | Pwand (Uwand _) -> True
       | Pweapon we ->
           we.we_kind = WKtwo_handed_sword && we.we_value = None &&
           ch <> g.main_sword
       | _ -> False ])
    g.pack
;

(* *)

value wielding_a_two_handed_sword g =
  match
    try Some (List.assoc g.main_sword g.pack) with [ Not_found -> None ]
  with
  [ Some (_, obj) -> do {
      match obj with
      [ Pweapon {we_kind = WKtwo_handed_sword} -> True
      | _ -> False ]
    }
  | None -> do {
      (* likely stolen by a nymph *)
      False
    } ]
;

value worn_armor_value g =
  if_match g.worn_armor with_some (ch, ar) ->
    if_match ar.ar_value with_some v -> v else assert False
  else 0
;

value worn_armor_protected g =
  if_match g.worn_armor with_some (ch, ar) -> ar.ar_protected else False
;

value interesting_objects g =
  let list = ['*'; '!'; '?'; '/'] in
  let list =
    if g.ring_of_slow_digestion_on_hand <> None then list
    else [':'; '=' :: list]
  in
  let list =
    if g.ring_of_slow_digestion_on_hand <> None &&
       number_of_scrolls_of_scare_monsters_in_pack g.pack > 0 &&
       wielding_a_two_handed_sword g
    then list
    else [')' :: list]
  in
  let list =
    if worn_armor_value g >= 10 && worn_armor_protected g then list
    else [']' :: list]
  in
  list
;

value will_take_not_interesting_object g ch =
  if g.hallucinated || g.was_hallucinated then False
  else if List.mem ch list_obj_ch then
    if List.mem ch (interesting_objects g) then False
    else True
  else False
;

value move_command3 g pos1 pos2 na = do {
  let mov = move_between pos1 pos2 in
  if (List.mem pos2 g.garbage || g.confused ||
      will_take_not_interesting_object g (dung_char g.dung pos2)) &&
     mov <> no_move
  then
    (Coth 'm', NAmove mov na, Some mov)
  else if mov = no_move then
    (Coth 's', na, Some mov)
  else
    (Cmov mov, na, Some mov)
};

type ars_return =
  [ ARSkont of char and read_state
  | ARSput_ring of char
  | ARSend ]
;

value enchant_armor g = do {
  match g.worn_armor with
  [ Some (ch, ar) -> do {
      let ar =
        match ar.ar_value with
        [ Some v -> {(ar) with ar_value = Some (min 99 (v + 1))}
        | None -> ar ]
      in
      g.worn_armor := Some (ch, ar);
      g.armor_cursed := False;
      redefine_in_pack g ch (Parmor ar);
    }
  | None -> assert False ]
};

value enchant_weapon g = do {
  let ch = g.main_sword in
  let we =
    let (_, obj) = List.assoc ch g.pack in
    match obj with
    [ Pweapon we -> we
    | _ -> assert False ]
  in
  let we =
    match we.we_value with
    [ Some v -> {(we) with we_value = Some (v + 1)}
    | None -> we ]
  in
  g.weapon_cursed := False;
  redefine_in_pack g ch (Pweapon we);
};

value hold_monsters g t = do {
  match t.t_prev_game with
  [ Some old_g -> do {
      let pos = rogue_pos g in
      loop (-2) (-2) where rec loop di dj = do {
        if di = 3 then ()
        else if dj = 3 then loop (di + 1) (-2)
        else do {
          let pos1 = add_mov pos {di = di; dj = dj} in
          if in_dung old_g pos1 then do {
            let ch = dung_char old_g.dung pos1 in
            if is_monster ch then do {
              g.frozen_monsters := [(pos1, ch) :: g.frozen_monsters]
            }
            else ()
          }
          else ();
          loop di (dj + 1)
        }
      };
    }
  | None -> () ]
};

value protect_armor g = do {
  match g.worn_armor with
  [ Some (ch, ar) -> do {
      let ar = {(ar) with ar_protected = True} in
      g.worn_armor := Some (ch, ar);
      g.armor_cursed := False;
      redefine_in_pack g ch (Parmor ar);
    }
  | None -> assert False ]
};

value uncurse_all g = do {
  g.armor_cursed := False;
  g.weapon_cursed := False;
};

value apply_scroll g t =
  fun
  [ SKaggr_mon -> g.frozen_monsters := []
  | SKench_ar -> enchant_armor g
  | SKench_wea -> enchant_weapon g
  | SKhold_mon -> hold_monsters g t
  | SKident -> ()
  | SKmagic_map -> g.map_showed_since := g.time
  | SKprotect -> protect_armor g
  | SKscare_mon -> ()
  | SKteleport -> ()
  | SKuncurse -> uncurse_all g
  | SKother -> () ]
;

value identified_object g message =
  let transl = transl g in
  if transl.is_potion message then
    let pk = identified_potion_kind_of_message g message in
    Some (Ppotion (Ipotion pk))
  else if transl.is_scroll message then do {
    let sk = identified_scroll_kind_of_message g message in
    Some (Pscroll (Iscroll sk))
  }
  else if transl.is_weapon message then
    let wk = weapon_kind_of_message g message in
    let we = {we_kind = wk; we_value = weapon_value message} in
    Some (Pweapon we)
  else if transl.is_armor message then
    let ar = {ar_value = armor_value message; ar_protected = False} in
    Some (Parmor ar)
  else if transl.is_ring message then
    let rk =
      if transl.is_ring_of_slow_digestion message then RKslow_digestion
      else RKother
    in
    Some (Pring (Iring rk))
  else if transl.is_wand message then
    let ak =
      if transl.is_wand_of_cancellation message then AKcancel
      else if transl.is_wand_of_magic_missile message then AKmagic_miss
      else if transl.is_identified_wand_kind message then AKother
      else failwith "is_wand"
    in
    let nbi = wand_value message in
    Some (Pwand (Iwand ak nbi))
  else None
;

value apply_read_scroll g t message ch_scr rs =
  let transl = transl g in
  match rs with
  [ RSread_what -> do {
      if transl.is_message_nothing_appropriate message then do {
        (* likely stoken by a nymph *)
        tempo g 1.0;
        let (nb, obj) = List.assoc ch_scr g.pack in
        remove_from_pack g ch_scr nb obj;
        ARSkont ' ' RScontinue
      }
      else do {
        ARSkont ch_scr RSscroll_read
      }
    }
  | RSscroll_read -> do {
      tempo g 1.0;
      let scr = read_scroll_kind_of_message g message in
      apply_scroll g t scr;
      let (nb, obj) = List.assoc ch_scr g.pack in
      remove_from_pack g ch_scr nb (Pscroll (Iscroll scr));
      let rs = if scr = SKident then RSidentify_what else RScontinue in
      ARSkont ' ' rs
    }
  | RSidentify_what ->
      loop
        [unidentified_ring_in_pack; unidentified_potion_in_pack;
         unidentified_two_handed_sword_in_pack; unidentified_wand_in_pack]
      where rec loop =
        fun
        [ [unidentified :: rest] ->
            if_match unidentified g.pack with_some (ch, _) -> do {
              ARSkont ch (RSidentify ch)
            }
            else loop rest
        | [] ->
            if_match unidentified_object_in_pack g.pack
            with_some (ch, _) -> do {
              ARSkont ch (RSidentify ch)
            }
            else do {
              ARSkont '\027' RScontinue
            } ]
  | RSidentify ch -> do {
      tempo g 1.0;
      if transl.is_message_there_is_no message then do {
        (* probably stolen by a nymph *)
        g.pack := List.remove_assoc ch g.pack;
        ARSkont ' ' RSidentify_what
      }
      else do {
        if_match identified_object g message with_some obj ->
          redefine_in_pack g ch obj
        else ();
        let rs =
          if transl.is_ring message &&
             transl.is_ring_of_slow_digestion message
          then
            RSput_ring ch
          else
            RScontinue
        in
        ARSkont ' ' rs
      }
    }
  | RScontinue ->
      if message <> "" then do {
        tempo g 1.0;
        ARSkont ' ' RScontinue
      }
      else ARSend
  | RSput_ring ch ->
      if message <> "" then do {
        tempo g 1.0;
        ARSkont ' ' (RSput_ring ch)
      }
      else do {
        ARSput_ring ch
      } ]
;

value find_object_to_be_used_when_full_pack g =
  if not g.blind && unidentified_scroll_in_pack g.pack <> None then
    unidentified_scroll_in_pack g.pack
  else if unidentified_potion_in_pack g.pack <> None then
    unidentified_potion_in_pack g.pack
  else
    list_find
      (fun (ch, (nb, obj)) ->
         match obj with
         [ Ppotion (Ipotion pk) ->
             match pk with
             [ PKacceler | PKdetect_obj | PKhealing | PKextra_heal |
               PKincr_stren | PKraise_level | PKrestore_str ->
                 True
             | PKdetect_mon -> not g.mon_detected
             | PKblindness | PKhallucination | PKsee_invis | PKother ->
                 False ]
         | Pscroll (Iscroll sk) ->
             if g.blind then False
             else
               match sk with
               [ SKaggr_mon -> False
               | SKench_ar | SKench_wea -> True
               | SKident -> unidentified_object_in_pack g.pack <> None
               | SKmagic_map -> g.map_showed_since = 0
               | SKprotect -> not (worn_armor_protected g)
               | SKhold_mon | SKscare_mon | SKteleport -> False
               | SKuncurse -> g.armor_cursed || g.weapon_cursed
               | SKother -> False ]
         | Pfood -> g.ring_of_slow_digestion_on_hand <> None
         | _ -> False ])
    g.pack
;

value find_unuseful_object_when_full_pack g =
  list_find
    (fun (ch, (nb, obj)) ->
       if is_wearing_armor g ch || ch = g.main_sword ||
         Some ch = g.ring_of_slow_digestion_on_hand
       then False
       else
         match obj with
         [ Parmor ar ->
             if worn_armor_protected g then
               let wav = worn_armor_value g in
               if wav >= 10 then True
               else if ar.ar_value = None then False
               else ar.ar_value <= Some wav
             else False
         | Pweapon we ->
             match we.we_kind with
             [ WKtwo_handed_sword ->
                 if wielding_a_two_handed_sword g then True else False
             | WKlong_sword -> True
             | WKmace | WKshort_bow -> we.we_value = None
             | WKarrows | WKother -> False ]
         | Pring (Uring _) -> g.ring_of_slow_digestion_on_hand <> None
         | Pring (Iring RKother) -> True
         | Pfood -> do {
(*
             if g.ring_of_slow_digestion_on_hand <> None then
               (status_line_values g).sl_hunger = ""
             else False
*)
             False
(**)
           }
         | Ppotion (Ipotion pk) ->
             match pk with
             [ PKacceler -> False
             | PKdetect_mon -> nb > 3
             | PKdetect_obj | PKhealing | PKextra_heal | PKincr_stren |
               PKraise_level | PKsee_invis | PKrestore_str -> False
             | PKblindness | PKhallucination | PKother -> True ]
         | Pscroll (Iscroll sk) ->
             match sk with
             [ SKaggr_mon -> True
             | SKench_ar | SKench_wea -> False
             | SKhold_mon -> nb > 3
             | SKident -> nb > 3
             | SKmagic_map -> nb > 3
             | SKprotect -> False
             | SKscare_mon -> nb > 6
             | SKteleport -> True
             | SKuncurse -> nb > 1
             | SKother -> True ]
         | Pwand (Iwand wk n_opt) ->
             match n_opt with
             [ Some 0 -> True
             | Some _ | None ->
                 match wk with
                 [ AKcancel | AKmagic_miss -> False
                 | AKother -> True ] ]
         | Ppotion (Upotion _) | Pscroll (Uscroll _) |
           Pring (Iring _) | Pwand (Uwand _) | Pamulet ->
             False ])
    g.pack
;

value object_to_be_used_in_pack_when_scaring g =
  list_find
    (fun (ch, (nb, obj)) ->
       match obj with
       [ Ppotion (Ipotion pk) ->
           match pk with
           [ PKacceler | PKdetect_obj | PKhealing | PKincr_stren |
             PKraise_level | PKrestore_str | PKsee_invis ->
               True
           | PKextra_heal ->
               if g.hallucination_discovered then
                 if g.blindness_discovered then True
                 else if
                   potion_of_healing_in_pack g.pack <> None ||
                   potion_of_see_invisible_in_pack g.pack <> None
                 then True
                 else nb > 1
               else nb > 1
           | PKdetect_mon -> not g.mon_detected
           | PKblindness | PKhallucination | PKother ->
               False ]
       | Ppotion (Upotion _) -> True
       | Pscroll (Iscroll sk) ->
           if g.blind then False
           else do {
             match sk with
             [ SKaggr_mon -> g.frozen_monsters = []
             | SKench_ar | SKench_wea -> True
             | SKident -> unidentified_object_in_pack g.pack <> None
             | SKmagic_map -> g.map_showed_since = 0
             | SKprotect -> True
             | SKscare_mon -> nb > 6
             | SKhold_mon | SKuncurse | SKteleport | SKother -> False ]
           }
       | Pscroll (Uscroll _) -> not g.blind && g.teleport_discovered
       | Pweapon {we_kind = WKtwo_handed_sword; we_value = None} ->
           not g.weapon_cursed && not (wielding_a_two_handed_sword g)
       | Pweapon {we_kind = WKtwo_handed_sword; we_value = Some v} ->
           v >= 0 && not g.weapon_cursed &&
           not (wielding_a_two_handed_sword g)
       | Pfood -> True
       | _ -> False ])
    g.pack
;

value unuseful_object_in_pack_when_scaring g =
  loop 0 g.pack where rec loop n_mm =
    fun
    [ [item :: rest] ->
         let (ch, (nb, obj)) = item in
         if is_wearing_armor g ch || is_best_armor g ch ||
           ch = g.main_sword || Some ch = g.ring_of_slow_digestion_on_hand
         then loop n_mm rest
         else
           match obj with
           [ Parmor _ | Pring _ -> Some item
           | Pfood ->
(*
               if (status_line_values g).sl_hunger <> "" then loop n_mm rest
               else Some item
*)
               loop n_mm rest
(**)
           | Pweapon we ->
               match we.we_kind with
               [ WKtwo_handed_sword ->
                   if wielding_a_two_handed_sword g then Some item
                   else loop n_mm rest
               | _ -> Some item ]
           | Ppotion (Ipotion pk) ->
               match pk with
               [ PKacceler -> loop n_mm rest
               | PKdetect_mon -> if nb > 3 then Some item else loop n_mm rest
               | PKdetect_obj | PKhealing | PKextra_heal | PKincr_stren |
                 PKraise_level | PKrestore_str ->
                   loop n_mm rest
               | PKblindness | PKhallucination | PKsee_invis | PKother ->
                   Some item ]
           | Pscroll (Iscroll sk) ->
               match sk with
               [ SKaggr_mon ->
                   if g.frozen_monsters (*=*)<> [] then Some item
                   else loop n_mm rest
               | SKench_ar | SKench_wea -> loop n_mm rest
               | SKhold_mon -> if nb > 3 then Some item else loop n_mm rest
               | SKident ->
                   if unidentified_object_in_pack g.pack = None then
                     Some item
                   else
                     loop n_mm rest
               | SKmagic_map -> if nb > 3 then Some item else loop n_mm rest
               | SKprotect -> loop n_mm rest
(*
               | SKscare_mon -> if nb > 6 then Some item else loop n_mm rest
*)
               | SKscare_mon -> loop n_mm rest
(**)
               | SKteleport | SKuncurse | SKother -> Some item ]
           | Pwand (Iwand wk n_opt) ->
               match n_opt with
               [ Some 0 -> Some item
               | Some _ | None ->
                   match wk with
                   [ AKcancel | AKmagic_miss ->
                       if n_mm >= 4 then Some item else loop (n_mm + 1) rest
                   | AKother ->
                       Some item ] ]
           | Pamulet -> Some item
           | Ppotion (Upotion _) | Pscroll (Uscroll _)
           | Pwand (Uwand _) ->
               loop n_mm rest ]
    | [] -> None ]
;

value use_object g t message na uo =
  let transl = transl g in
  match uo with
  [ UOeat_food ch step ->
      match step with
      [ "eat what" -> do {
          if transl.is_message_nothing_appropriate message then do {
            (* likely stolen by a nymph *)
            let (nb, obj) = List.assoc ch g.pack in
            remove_from_pack g ch nb obj;
            None
          }
          else do {
            let uo = UOeat_food ch "eaten" in
            let na = NAuse_object uo na in
            Some (ch, na)
          }
        }
      | "eaten" -> do {
          tempo g 1.0;
          let (nb, obj) = List.assoc ch g.pack in
          remove_from_pack g ch nb obj;
          None
        }
      | step ->
          failwith (sprintf "UOeat_food '%c' '%s'" ch step) ]
  | UOquaff_potion ch step ->
      match step with
      [ QSquaff_what -> do {
          let uo = UOquaff_potion ch QSquaffed in
          let na = NAuse_object uo na in
          Some (ch, na)
        }
      | QSquaffed -> do {
          tempo g 1.0;
          let (nb, obj) = List.assoc ch g.pack in
          let obj = Ppotion (quaffed_potion_of_message g message) in
          remove_from_pack g ch nb obj;
          None
        } ]
  | UOread_scroll ch rs -> do {
      match apply_read_scroll g t message ch rs with
      [ ARSkont ch rs -> do {
          let uo = UOread_scroll ch rs in
          let na = NAuse_object uo na in
          Some (ch, na)
        }
      | ARSput_ring ch -> do {
          let na = NAput_ring ch na 1 in
          Some ('P', na)
        }
      | ARSend -> do {
          None
        } ]
    }
  | UOthrow_unuseful_objects ch_junk step ->
      match step with
      [ 1 ->
          let move =
            match g.rogue_room_and_door with
            [ Some (_, Some dd) -> one_step_to_exit_room dd
            | Some (room, None) -> do {
                let pos = rogue_pos g in
                let (rmin, cmin, rmax, cmax) = room in
                let d1 = (pos.row - rmin, {di = -1; dj = 0}) in
                let d2 = (rmax - pos.row, {di = 1; dj = 0}) in
                let d3 = (pos.col - cmin, {di = 0; dj = -1}) in
                let d4 = (cmax - pos.col, {di = 0; dj = 1}) in
                let (_, mov) =
                  List.hd (List.sort (fun x y -> compare y x) [d1; d2; d3; d4])
                in
                loop 1 (add_mov pos mov) where rec loop dist pos =
                  if inside_room room pos && dist < 25 then
                    loop (dist + 1) (add_mov pos mov)
                  else if is_at_door g pos then ()
                  else do {
                    let garbage pos =
                      if List.mem pos g.garbage then ()
                      else if in_dung g pos && dung_char g.dung pos = '*' then
                        ()
                      else g.garbage := [pos :: g.garbage]
                    in
                    let pos = add_mov pos (opposite_move mov) in
                    garbage pos;
                    loop run_around_list where rec loop =
                      fun
                      [ [k :: kl] -> do {
                          let mov = mov_of_k k in
                          garbage (add_mov pos mov);
                          loop kl
                        }
                      | [] -> () ];
                  };
                mov
              }
            | None -> {di = 1; dj = 0} ]
          in
          let ch_dir = basic_command_of_move move in
          let uo = UOthrow_unuseful_objects ch_junk 2 in
          let na = NAuse_object uo na in
          Some (ch_dir, na)
      | 2 -> do {
          let (nb, obj) = List.assoc ch_junk g.pack in
          remove_from_pack g ch_junk nb obj;
          let uo = UOthrow_unuseful_objects ch_junk 3 in
          let na = NAuse_object uo na in
          Some (ch_junk, na)
        }
      | 3 ->
          if message <> "" then do {
            tempo g 0.5;
            let uo = UOthrow_unuseful_objects ch_junk 3 in
            let na = NAuse_object uo na in
            Some (' ', na)
          }
          else if g.attacked > 0 then None
          else do {
            tempo g 0.1;
            match unuseful_object_in_pack_when_scaring g with
            [ Some (ch, _) ->
                let uo = UOthrow_unuseful_objects ch 1 in
                let na = NAuse_object uo na in
                Some ('t', na)
            | None ->
                None ]
          }
      | _ ->
          failwith
            (sprintf "UOthrow_unuseful_objects %c %d" ch_junk step) ]
  | UOwield_sword ch step -> do {
      match step with
      [ "wield what" -> do {
          let uo = UOwield_sword ch "wielded" in
          let na = NAuse_object uo na in
          Some (ch, na)
        }
      | "wielded" -> do {
          if message <> "" then do {
            tempo g 1.0;
            g.main_sword := ch;
            let uo = UOwield_sword ch "wielded" in
            let na = NAuse_object uo na in
            Some (' ', na)
          }
          else None
        }
      | step -> failwith (sprintf "UOwield_sword %c '%s'" ch step) ]
    } ]
;

value wear_armor_and_read g t ch =
  match g.worn_armor with
  [ Some (ch_worn, ar) ->
      let ch_scr = ch in
      match best_armor g with
      [ Some (ch_arm, _) ->
          if not g.armor_cursed &&
             (ch_worn <> ch_arm || not ar.ar_protected)
          then
            let ws = WStoken_off in
            let na = NAwear_armor_and_test_scrolls ch_arm ch_scr ws in
            (Coth 'T', na, t.t_prev_mov)
          else do {
            let ws = WSscroll_read in
            let na = NAwear_armor_and_test_scrolls ch_arm ch_scr ws in
            let uo = UOread_scroll ch_scr RSread_what in
            let na = NAuse_object uo na in
            (Coth 'r', na, t.t_prev_mov)
          }
      | None -> assert False ]
  | None -> failwith "worn armor 4132" ]
;
