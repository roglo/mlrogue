(* $Id: rob_object.ml,v 1.18 2010/07/02 00:16:09 deraugla Exp $ *)

(* #load "pa_if_match.cmo" *)

open Rob_def
open Rob_misc
open Rob_position
open Printf

let list_all_obj_ch = ['*'; '!'; '?'; '/'; ']'; ')'; '='; ':'; ',']

let is_object ch = List.mem ch list_all_obj_ch

(* pack *)

type pack_item = char * (int * pack_obj)
type pack = pack_item list

let remove_from_pack g ch nb obj =
  g.pack <- List.remove_assoc ch g.pack;
  if nb > 1 then g.pack <- (ch, (nb - 1, obj)) :: g.pack;
  match obj with
    Pweapon _ -> if nb = 1 then g.pack_full <- false
  | _ -> g.pack_full <- false

let redefine_in_pack g ch obj =
  let (nb, _) = List.assoc ch g.pack in
  g.pack <- List.remove_assoc ch g.pack; g.pack <- (ch, (nb, obj)) :: g.pack

let pack_list_find : _ -> pack -> _ = list_find

let find_potion_in_pack f =
  pack_list_find
    (fun (_, (nb, obj)) ->
       match obj with
         Ppotion (Ipotion s) -> f nb s
       | _ -> false)
let potion_of_increase_strength_in_pack =
  find_potion_in_pack
    (fun _ ->
       function
         PKincr_stren -> true
       | _ -> false)
let potion_of_detect_monsters_in_pack =
  find_potion_in_pack
    (fun _ ->
       function
         PKdetect_mon -> true
       | _ -> false)
let potion_of_healing_in_pack =
  find_potion_in_pack
    (fun _ ->
       function
         PKhealing -> true
       | _ -> false)
let several_potions_of_healing_in_pack =
  find_potion_in_pack
    (fun nb ->
       function
         PKhealing -> nb > 1
       | _ -> false)
let potion_of_extra_healing_in_pack =
  find_potion_in_pack
    (fun _ ->
       function
         PKextra_heal -> true
       | _ -> false)
let several_potions_of_extra_healing_in_pack =
  find_potion_in_pack
    (fun nb ->
       function
         PKextra_heal -> nb > 1
       | _ -> false)
let potion_of_see_invisible_in_pack =
  find_potion_in_pack
    (fun _ ->
       function
         PKsee_invis -> true
       | _ -> false)

let find_scroll_in_pack f =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Pscroll (Iscroll s) -> f s
       | _ -> false)

let scroll_of_enchant_armor_in_pack =
  find_scroll_in_pack
    (function
       SKench_ar -> true
     | _ -> false)
let scroll_of_enchant_weapon_in_pack =
  find_scroll_in_pack
    (function
       SKench_wea -> true
     | _ -> false)
let scroll_of_hold_monsters_in_pack =
  find_scroll_in_pack
    (function
       SKhold_mon -> true
     | _ -> false)
let scroll_of_identification_in_pack =
  find_scroll_in_pack
    (function
       SKident -> true
     | _ -> false)
let scroll_of_magic_mapping_in_pack =
  find_scroll_in_pack
    (function
       SKmagic_map -> true
     | _ -> false)
let scroll_of_protect_armor_in_pack =
  find_scroll_in_pack
    (function
       SKprotect -> true
     | _ -> false)
let scroll_of_remove_curse_in_pack =
  find_scroll_in_pack
    (function
       SKuncurse -> true
     | _ -> false)
let scroll_of_scare_monsters_in_pack =
  find_scroll_in_pack
    (function
       SKscare_mon -> true
     | _ -> false)
let scroll_of_teleport_in_pack =
  find_scroll_in_pack
    (function
       SKteleport -> true
     | _ -> false)

let number_of_potions_of_detect_monsters_in_pack pack =
  match potion_of_detect_monsters_in_pack pack with
    Some (_, (nb, _)) -> nb
  | None -> 0

let number_of_scrolls_of_scare_monsters_in_pack pack =
  match scroll_of_scare_monsters_in_pack pack with
    Some (_, (nb, _)) -> nb
  | None -> 0

let number_of_scrolls_of_magic_mapping_in_pack pack =
  match scroll_of_magic_mapping_in_pack pack with
    Some (_, (nb, _)) -> nb
  | None -> 0

let arrows_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Pweapon we -> we.we_kind = WKarrows
       | _ -> false)
let short_bow_in_pack =
  pack_list_find
    (fun (_, (nb, obj)) ->
       match obj with
         Pweapon {we_kind = WKshort_bow; we_value = Some _} -> true
       | _ -> false)
let two_handed_swords_in_pack g =
  List.filter
    (fun (_, (_, obj)) ->
       match obj with
         Pweapon {we_kind = WKtwo_handed_sword} -> true
       | _ -> false)
    g.pack

let food_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Pfood -> true
       | _ -> false)

let wand_of_magic_missile_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Pwand (Iwand (AKmagic_miss, Some n)) -> n > 0
       | _ -> false)

let anti_flamer_wand_list_in_pack : pack -> _ =
  let rec loop list =
    function
      (ch, (nb, obj)) :: rest ->
        begin match obj with
          Pwand (Iwand ((AKcancel | AKmagic_miss), Some n)) ->
            let list = if n > 0 then (n, (ch, (nb, obj))) :: list else list in
            loop list rest
        | _ -> loop list rest
        end
    | [] -> List.sort compare list
  in
  loop []

let usable_anti_flamer_wand_in_pack pack =
  match anti_flamer_wand_list_in_pack pack with
    (_, x) :: _ -> Some x
  | [] -> None

let enough_anti_flamer_wand_in_pack pack =
  List.length (anti_flamer_wand_list_in_pack pack) >= 2

let check_armor g pred =
  let best_values =
    let rec loop list =
      function
        (ch, (nb, obj)) :: rest ->
          let list =
            match obj with
              Parmor arm ->
                begin match arm.ar_value with
                  Some v -> (v, arm.ar_protected, (ch, arm)) :: list
                | None -> list
                end
            | _ -> list
          in
          loop list rest
      | [] -> List.sort (fun a b -> compare b a) list
    in
    loop [] g.pack
  in
  match best_values with
    (v1, true, (ch1, arm1)) :: _ -> Some (ch1, arm1)
  | (v1, false, (ch1, arm1)) :: _ ->
      begin match pred best_values with
        Some (v1, (ch1, arm1)) ->
          let ch_arm =
            match g.worn_armor with
              Some (ch_worn, ar_worn) ->
                begin match ar_worn.ar_value with
                  Some v -> if v = v1 then ch_worn, ar_worn else ch1, arm1
                | None -> assert false
                end
            | None -> ch1, arm1
          in
          Some ch_arm
      | None -> Some (ch1, arm1)
      end
  | [] -> None
let best_armor g =
  check_armor g
    (function
       (v1, _, ch1) :: _ -> Some (v1, ch1)
     | [] -> None)
(* good armor has value of the second in the values list *)
let good_armor g =
  check_armor g
    (function
       (v1, _, ch1) :: (v2, _, ch2) :: _ ->
         if g.level < 3 then Some (v1, ch1) else Some (v2, ch2)
     | [_] | [] -> None)

let is_best_armor g ch =
  match best_armor g with
    Some (ch1, _) -> ch = ch1
  | None -> false

let wearing_best_armor g =
  match g.worn_armor with
    Some (ch, _) -> is_best_armor g ch
  | None -> false

let is_good_armor g ch ar =
  match good_armor g with
    Some (ch1, ar1) -> ch = ch1 || ar.ar_value = ar1.ar_value
  | None -> false

let wearing_good_armor g =
  match g.worn_armor with
    Some (ch, ar) -> is_good_armor g ch ar
  | None -> false

let armor_of_ch g ch =
  match try Some (List.assoc ch g.pack) with Not_found -> None with
    Some (_, Parmor ar) -> Some (ch, ar)
  | Some _ | None -> None

let unidentified_armor_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Parmor ar -> ar.ar_value = None
       | _ -> false)
let unidentified_scroll_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Pscroll (Uscroll _) -> true
       | _ -> false)
let unidentified_potion_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
         Ppotion (Upotion _) -> true
       | _ -> false)
let unidentified_ring_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
         Pring (Uring _) -> true
       | _ -> false)
let unidentified_wand_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
         Pwand (Uwand _) -> true
       | _ -> false)
let unidentified_two_handed_sword_in_pack =
  pack_list_find
    (fun (ch, (nb, obj)) ->
       match obj with
         Pweapon we -> we.we_kind = WKtwo_handed_sword && we.we_value = None
       | _ -> false)
let is_wearing_armor g ch =
  match g.worn_armor with
    Some (ch1, _) -> ch = ch1
  | None -> false
let unidentified_object_in_pack =
  pack_list_find
    (fun (_, (_, obj)) ->
       match obj with
         Parmor ar -> ar.ar_value = None
       | Pscroll (Uscroll _) -> true
       | Ppotion (Upotion _) -> true
       | Pring (Uring _) -> true
       | Pwand (Uwand _ | Iwand ((AKcancel | AKmagic_miss), None)) -> true
       | Pweapon we -> we.we_kind = WKtwo_handed_sword && we.we_value = None
       | _ -> false)
let unidentified_object_not_used_in_pack g =
  pack_list_find
    (fun (ch, (_, obj)) ->
       match obj with
         Parmor ar -> ar.ar_value = None && is_wearing_armor g ch
       | Pscroll (Uscroll _) -> true
       | Ppotion (Upotion _) -> true
       | Pring (Uring _) -> Some ch <> g.ring_of_slow_digestion_on_hand
       | Pwand (Uwand _) -> true
       | Pweapon we ->
           we.we_kind = WKtwo_handed_sword && we.we_value = None &&
           ch <> g.main_sword
       | _ -> false)
    g.pack

(* *)

let wielding_a_two_handed_sword g =
  match try Some (List.assoc g.main_sword g.pack) with Not_found -> None with
    Some (_, obj) ->
      begin match obj with
        Pweapon {we_kind = WKtwo_handed_sword} -> true
      | _ -> false
      end
  | None ->
      (* likely stolen by a nymph *)
      false

let worn_armor_value g =
  match g.worn_armor with
    Some (ch, ar) ->
      begin match ar.ar_value with
        Some v -> v
      | None -> assert false
      end
  | None -> 0

let worn_armor_protected g =
  match g.worn_armor with
    Some (ch, ar) -> ar.ar_protected
  | None -> false

let interesting_objects g =
  let list = ['*'; '!'; '?'; '/'] in
  let list =
    if g.ring_of_slow_digestion_on_hand <> None then list
    else ':' :: '=' :: list
  in
  let list =
    if g.ring_of_slow_digestion_on_hand <> None &&
       number_of_scrolls_of_scare_monsters_in_pack g.pack > 0 &&
       wielding_a_two_handed_sword g
    then
      list
    else ')' :: list
  in
  let list =
    if worn_armor_value g >= 10 && worn_armor_protected g then list
    else ']' :: list
  in
  list

let will_take_not_interesting_object g ch =
  if g.hallucinated || g.was_hallucinated then false
  else if List.mem ch list_obj_ch then
    if List.mem ch (interesting_objects g) then false else true
  else false

let move_command3 g pos1 pos2 na =
  let mov = move_between pos1 pos2 in
  if (List.mem pos2 g.garbage || g.confused ||
      will_take_not_interesting_object g (dung_char g.dung pos2)) &&
     mov <> no_move
  then
    Coth 'm', NAmove (mov, na), Some mov
  else if mov = no_move then Coth 's', na, Some mov
  else Cmov mov, na, Some mov

type ars_return =
    ARSkont of char * read_state
  | ARSput_ring of char
  | ARSend

let enchant_armor g =
  match g.worn_armor with
    Some (ch, ar) ->
      let ar =
        match ar.ar_value with
          Some v -> {ar with ar_value = Some (min 99 (v + 1))}
        | None -> ar
      in
      g.worn_armor <- Some (ch, ar);
      g.armor_cursed <- false;
      redefine_in_pack g ch (Parmor ar)
  | None -> assert false

let enchant_weapon g =
  let ch = g.main_sword in
  let we =
    let (_, obj) = List.assoc ch g.pack in
    match obj with
      Pweapon we -> we
    | _ -> assert false
  in
  let we =
    match we.we_value with
      Some v -> {we with we_value = Some (v + 1)}
    | None -> we
  in
  g.weapon_cursed <- false; redefine_in_pack g ch (Pweapon we)

let hold_monsters g t =
  match t.t_prev_game with
    Some old_g ->
      let pos = rogue_pos g in
      let rec loop di dj =
        if di = 3 then ()
        else if dj = 3 then loop (di + 1) (-2)
        else
          let pos1 = add_mov pos {di = di; dj = dj} in
          if in_dung old_g pos1 then
            begin let ch = dung_char old_g.dung pos1 in
              if is_monster ch then
                g.frozen_monsters <- (pos1, ch) :: g.frozen_monsters
            end;
          loop di (dj + 1)
      in
      loop (-2) (-2)
  | None -> ()

let protect_armor g =
  match g.worn_armor with
    Some (ch, ar) ->
      let ar = {ar with ar_protected = true} in
      g.worn_armor <- Some (ch, ar);
      g.armor_cursed <- false;
      redefine_in_pack g ch (Parmor ar)
  | None -> assert false

let uncurse_all g = g.armor_cursed <- false; g.weapon_cursed <- false

let apply_scroll g t =
  function
    SKaggr_mon -> g.frozen_monsters <- []
  | SKench_ar -> enchant_armor g
  | SKench_wea -> enchant_weapon g
  | SKhold_mon -> hold_monsters g t
  | SKident -> ()
  | SKmagic_map -> g.map_showed_since <- g.time
  | SKprotect -> protect_armor g
  | SKscare_mon -> ()
  | SKteleport -> ()
  | SKuncurse -> uncurse_all g
  | SKother -> ()

let identified_object g message =
  let transl = transl g in
  if transl.is_potion message then
    let pk = identified_potion_kind_of_message g message in
    Some (Ppotion (Ipotion pk))
  else if transl.is_scroll message then
    let sk = identified_scroll_kind_of_message g message in
    Some (Pscroll (Iscroll sk))
  else if transl.is_weapon message then
    let wk = weapon_kind_of_message g message in
    let we = {we_kind = wk; we_value = weapon_value message} in
    Some (Pweapon we)
  else if transl.is_armor message then
    let ar = {ar_value = armor_value message; ar_protected = false} in
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
    let nbi = wand_value message in Some (Pwand (Iwand (ak, nbi)))
  else None

let apply_read_scroll g t message ch_scr rs =
  let transl = transl g in
  match rs with
    RSread_what ->
      if transl.is_message_nothing_appropriate message then
        begin
          (* likely stoken by a nymph *)
          tempo g 1.0;
          let (nb, obj) = List.assoc ch_scr g.pack in
          remove_from_pack g ch_scr nb obj; ARSkont (' ', RScontinue)
        end
      else ARSkont (ch_scr, RSscroll_read)
  | RSscroll_read ->
      tempo g 1.0;
      let scr = read_scroll_kind_of_message g message in
      apply_scroll g t scr;
      let (nb, obj) = List.assoc ch_scr g.pack in
      remove_from_pack g ch_scr nb (Pscroll (Iscroll scr));
      let rs = if scr = SKident then RSidentify_what else RScontinue in
      ARSkont (' ', rs)
  | RSidentify_what ->
      let rec loop =
        function
          unidentified :: rest ->
            begin match unidentified g.pack with
              Some (ch, _) -> ARSkont (ch, RSidentify ch)
            | None -> loop rest
            end
        | [] ->
            match unidentified_object_in_pack g.pack with
              Some (ch, _) -> ARSkont (ch, RSidentify ch)
            | None -> ARSkont ('\027', RScontinue)
      in
      loop
        [unidentified_ring_in_pack; unidentified_potion_in_pack;
         unidentified_two_handed_sword_in_pack; unidentified_wand_in_pack]
  | RSidentify ch ->
      tempo g 1.0;
      if transl.is_message_there_is_no message then
        begin
          (* probably stolen by a nymph *)
          g.pack <- List.remove_assoc ch g.pack;
          ARSkont (' ', RSidentify_what)
        end
      else
        begin
          begin match identified_object g message with
            Some obj -> redefine_in_pack g ch obj
          | None -> ()
          end;
          let rs =
            if transl.is_ring message &&
               transl.is_ring_of_slow_digestion message
            then
              RSput_ring ch
            else RScontinue
          in
          ARSkont (' ', rs)
        end
  | RScontinue ->
      if message <> "" then begin tempo g 1.0; ARSkont (' ', RScontinue) end
      else ARSend
  | RSput_ring ch ->
      if message <> "" then
        begin tempo g 1.0; ARSkont (' ', RSput_ring ch) end
      else ARSput_ring ch

let find_object_to_be_used_when_full_pack g =
  if not g.blind && unidentified_scroll_in_pack g.pack <> None then
    unidentified_scroll_in_pack g.pack
  else if unidentified_potion_in_pack g.pack <> None then
    unidentified_potion_in_pack g.pack
  else
    list_find
      (fun (ch, (nb, obj)) ->
         match obj with
           Ppotion (Ipotion pk) ->
             begin match pk with
               PKacceler | PKdetect_obj | PKhealing | PKextra_heal |
               PKincr_stren | PKraise_level | PKrestore_str ->
                 true
             | PKdetect_mon -> not g.mon_detected
             | PKblindness | PKhallucination | PKsee_invis | PKother -> false
             end
         | Pscroll (Iscroll sk) ->
             if g.blind then false
             else
               begin match sk with
                 SKaggr_mon -> false
               | SKench_ar | SKench_wea -> true
               | SKident -> unidentified_object_in_pack g.pack <> None
               | SKmagic_map -> g.map_showed_since = 0
               | SKprotect -> not (worn_armor_protected g)
               | SKhold_mon | SKscare_mon | SKteleport -> false
               | SKuncurse -> g.armor_cursed || g.weapon_cursed
               | SKother -> false
               end
         | Pfood -> g.ring_of_slow_digestion_on_hand <> None
         | _ -> false)
      g.pack

let find_unuseful_object_when_full_pack g =
  list_find
    (fun (ch, (nb, obj)) ->
       if is_wearing_armor g ch || ch = g.main_sword ||
          Some ch = g.ring_of_slow_digestion_on_hand
       then
         false
       else
         match obj with
           Parmor ar ->
             if worn_armor_protected g then
               let wav = worn_armor_value g in
               if wav >= 10 then true
               else if ar.ar_value = None then false
               else ar.ar_value <= Some wav
             else false
         | Pweapon we ->
             begin match we.we_kind with
               WKtwo_handed_sword ->
                 if wielding_a_two_handed_sword g then true else false
             | WKlong_sword -> true
             | WKmace | WKshort_bow -> we.we_value = None
             | WKarrows | WKother -> false
             end
         | Pring (Uring _) -> g.ring_of_slow_digestion_on_hand <> None
         | Pring (Iring RKother) -> true
         | Pfood ->
             (*
                          if g.ring_of_slow_digestion_on_hand <> None then
                            (status_line_values g).sl_hunger = ""
                          else False
             *)
             false
         | Ppotion (Ipotion pk) ->
             begin match pk with
               PKacceler -> false
             | PKdetect_mon -> nb > 3
             | PKdetect_obj | PKhealing | PKextra_heal | PKincr_stren |
               PKraise_level | PKsee_invis | PKrestore_str ->
                 false
             | PKblindness | PKhallucination | PKother -> true
             end
         | Pscroll (Iscroll sk) ->
             begin match sk with
               SKaggr_mon -> true
             | SKench_ar | SKench_wea -> false
             | SKhold_mon -> nb > 3
             | SKident -> nb > 3
             | SKmagic_map -> nb > 3
             | SKprotect -> false
             | SKscare_mon -> nb > 6
             | SKteleport -> true
             | SKuncurse -> nb > 1
             | SKother -> true
             end
         | Pwand (Iwand (wk, n_opt)) ->
             begin match n_opt with
               Some 0 -> true
             | Some _ | None ->
                 match wk with
                   AKcancel | AKmagic_miss -> false
                 | AKother -> true
             end
         | Ppotion (Upotion _) | Pscroll (Uscroll _) | Pring (Iring _) |
           Pwand (Uwand _) | Pamulet ->
             false)
    g.pack

let object_to_be_used_in_pack_when_scaring g =
  list_find
    (fun (ch, (nb, obj)) ->
       match obj with
         Ppotion (Ipotion pk) ->
           begin match pk with
             PKacceler | PKdetect_obj | PKhealing | PKincr_stren |
             PKraise_level | PKrestore_str | PKsee_invis ->
               true
           | PKextra_heal ->
               if g.hallucination_discovered then
                 if g.blindness_discovered then true
                 else if
                   potion_of_healing_in_pack g.pack <> None ||
                   potion_of_see_invisible_in_pack g.pack <> None
                 then
                   true
                 else nb > 1
               else nb > 1
           | PKdetect_mon -> not g.mon_detected
           | PKblindness | PKhallucination | PKother -> false
           end
       | Ppotion (Upotion _) -> true
       | Pscroll (Iscroll sk) ->
           if g.blind then false
           else
             begin match sk with
               SKaggr_mon -> g.frozen_monsters = []
             | SKench_ar | SKench_wea -> true
             | SKident -> unidentified_object_in_pack g.pack <> None
             | SKmagic_map -> g.map_showed_since = 0
             | SKprotect -> true
             | SKscare_mon -> nb > 6
             | SKhold_mon | SKuncurse | SKteleport | SKother -> false
             end
       | Pscroll (Uscroll _) -> not g.blind && g.teleport_discovered
       | Pweapon {we_kind = WKtwo_handed_sword; we_value = None} ->
           not g.weapon_cursed && not (wielding_a_two_handed_sword g)
       | Pweapon {we_kind = WKtwo_handed_sword; we_value = Some v} ->
           v >= 0 && not g.weapon_cursed &&
           not (wielding_a_two_handed_sword g)
       | Pfood -> true
       | _ -> false)
    g.pack

let unuseful_object_in_pack_when_scaring g =
  let rec loop n_mm =
    function
      item :: rest ->
        let (ch, (nb, obj)) = item in
        if is_wearing_armor g ch || is_best_armor g ch || ch = g.main_sword ||
           Some ch = g.ring_of_slow_digestion_on_hand
        then
          loop n_mm rest
        else
          begin match obj with
            Parmor _ | Pring _ -> Some item
          | Pfood ->
              (*
                             if (status_line_values g).sl_hunger <> "" then loop n_mm rest
                             else Some item
              *)
              loop n_mm rest
          | Pweapon we ->
              begin match we.we_kind with
                WKtwo_handed_sword ->
                  if wielding_a_two_handed_sword g then Some item
                  else loop n_mm rest
              | _ -> Some item
              end
          | Ppotion (Ipotion pk) ->
              begin match pk with
                PKacceler -> loop n_mm rest
              | PKdetect_mon -> if nb > 3 then Some item else loop n_mm rest
              | PKdetect_obj | PKhealing | PKextra_heal | PKincr_stren |
                PKraise_level | PKrestore_str ->
                  loop n_mm rest
              | PKblindness | PKhallucination | PKsee_invis | PKother ->
                  Some item
              end
          | Pscroll (Iscroll sk) ->
              begin match sk with
                SKaggr_mon ->
                  if g.frozen_monsters <> [] then Some item
                  else loop n_mm rest
              | SKench_ar | SKench_wea -> loop n_mm rest
              | SKhold_mon -> if nb > 3 then Some item else loop n_mm rest
              | SKident ->
                  if unidentified_object_in_pack g.pack = None then Some item
                  else loop n_mm rest
              | SKmagic_map -> if nb > 3 then Some item else loop n_mm rest
              | SKprotect -> loop n_mm rest
              | SKscare_mon -> loop n_mm rest
              | SKteleport | SKuncurse | SKother -> Some item
              end
          | Pwand (Iwand (wk, n_opt)) ->
              begin match n_opt with
                Some 0 -> Some item
              | Some _ | None ->
                  match wk with
                    AKcancel | AKmagic_miss ->
                      if n_mm >= 4 then Some item else loop (n_mm + 1) rest
                  | AKother -> Some item
              end
          | Pamulet -> Some item
          | Ppotion (Upotion _) | Pscroll (Uscroll _) | Pwand (Uwand _) ->
              loop n_mm rest
          end
    | [] -> None
  in
  loop 0 g.pack

let use_object g t message na uo =
  let transl = transl g in
  match uo with
    UOeat_food (ch, step) ->
      begin match step with
        "eat what" ->
          if transl.is_message_nothing_appropriate message then
            let (nb, obj) = List.assoc ch g.pack in
            remove_from_pack g ch nb obj; None
          else
            let uo = UOeat_food (ch, "eaten") in
            let na = NAuse_object (uo, na) in Some (ch, na)
      | "eaten" ->
          tempo g 1.0;
          let (nb, obj) = List.assoc ch g.pack in
          remove_from_pack g ch nb obj; None
      | step -> failwith (sprintf "UOeat_food '%c' '%s'" ch step)
      end
  | UOquaff_potion (ch, step) ->
      begin match step with
        QSquaff_what ->
          let uo = UOquaff_potion (ch, QSquaffed) in
          let na = NAuse_object (uo, na) in Some (ch, na)
      | QSquaffed ->
          tempo g 1.0;
          let (nb, obj) = List.assoc ch g.pack in
          let obj = Ppotion (quaffed_potion_of_message g message) in
          remove_from_pack g ch nb obj; None
      end
  | UOread_scroll (ch, rs) ->
      begin match apply_read_scroll g t message ch rs with
        ARSkont (ch, rs) ->
          let uo = UOread_scroll (ch, rs) in
          let na = NAuse_object (uo, na) in Some (ch, na)
      | ARSput_ring ch -> let na = NAput_ring (ch, na, 1) in Some ('P', na)
      | ARSend -> None
      end
  | UOthrow_unuseful_objects (ch_junk, step) ->
      begin match step with
        1 ->
          let move =
            match g.rogue_room_and_door with
              Some (_, Some dd) -> one_step_to_exit_room dd
            | Some (room, None) ->
                let pos = rogue_pos g in
                let (rmin, cmin, rmax, cmax) = room in
                let d1 = pos.row - rmin, {di = -1; dj = 0} in
                let d2 = rmax - pos.row, {di = 1; dj = 0} in
                let d3 = pos.col - cmin, {di = 0; dj = -1} in
                let d4 = cmax - pos.col, {di = 0; dj = 1} in
                let (_, mov) =
                  List.hd
                    (List.sort (fun x y -> compare y x) [d1; d2; d3; d4])
                in
                begin let rec loop dist pos =
                  if inside_room room pos && dist < 25 then
                    loop (dist + 1) (add_mov pos mov)
                  else if is_at_door g pos then ()
                  else
                    let garbage pos =
                      if List.mem pos g.garbage then ()
                      else if in_dung g pos && dung_char g.dung pos = '*' then
                        ()
                      else g.garbage <- pos :: g.garbage
                    in
                    let pos = add_mov pos (opposite_move mov) in
                    garbage pos;
                    let rec loop =
                      function
                        k :: kl ->
                          let mov = mov_of_k k in
                          garbage (add_mov pos mov); loop kl
                      | [] -> ()
                    in
                    loop run_around_list
                in
                  loop 1 (add_mov pos mov)
                end;
                mov
            | None -> {di = 1; dj = 0}
          in
          let ch_dir = basic_command_of_move move in
          let uo = UOthrow_unuseful_objects (ch_junk, 2) in
          let na = NAuse_object (uo, na) in Some (ch_dir, na)
      | 2 ->
          let (nb, obj) = List.assoc ch_junk g.pack in
          remove_from_pack g ch_junk nb obj;
          let uo = UOthrow_unuseful_objects (ch_junk, 3) in
          let na = NAuse_object (uo, na) in Some (ch_junk, na)
      | 3 ->
          if message <> "" then
            begin
              tempo g 0.5;
              let uo = UOthrow_unuseful_objects (ch_junk, 3) in
              let na = NAuse_object (uo, na) in Some (' ', na)
            end
          else if g.attacked > 0 then None
          else
            begin
              tempo g 0.1;
              match unuseful_object_in_pack_when_scaring g with
                Some (ch, _) ->
                  let uo = UOthrow_unuseful_objects (ch, 1) in
                  let na = NAuse_object (uo, na) in Some ('t', na)
              | None -> None
            end
      | _ -> failwith (sprintf "UOthrow_unuseful_objects %c %d" ch_junk step)
      end
  | UOwield_sword (ch, step) ->
      match step with
        "wield what" ->
          let uo = UOwield_sword (ch, "wielded") in
          let na = NAuse_object (uo, na) in Some (ch, na)
      | "wielded" ->
          if message <> "" then
            begin
              tempo g 1.0;
              g.main_sword <- ch;
              let uo = UOwield_sword (ch, "wielded") in
              let na = NAuse_object (uo, na) in Some (' ', na)
            end
          else None
      | step -> failwith (sprintf "UOwield_sword %c '%s'" ch step)

let wear_armor_and_read g t ch =
  match g.worn_armor with
    Some (ch_worn, ar) ->
      let ch_scr = ch in
      begin match best_armor g with
        Some (ch_arm, _) ->
          if not g.armor_cursed && (ch_worn <> ch_arm || not ar.ar_protected)
          then
            let ws = WStoken_off in
            let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
            Coth 'T', na, t.t_prev_mov
          else
            let ws = WSscroll_read in
            let na = NAwear_armor_and_test_scrolls (ch_arm, ch_scr, ws) in
            let uo = UOread_scroll (ch_scr, RSread_what) in
            let na = NAuse_object (uo, na) in Coth 'r', na, t.t_prev_mov
      | None -> assert false
      end
  | None -> failwith "worn armor 4132"
