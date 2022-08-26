(* $Id: robot.ml,v 1.1007 2015/11/12 08:57:19 deraugla Exp $ *)

#load "pa_if_match.cmo";

open Printf;
open Scanf;
open Rob_def;
open Rob_misc;
open Rob_object;
open Rob_path;
open Rob_position;

type t = Rob_def.t;

value char_escaped ch =
  if Char.code ch >= 1 && Char.code ch <= 26 then
    sprintf "ctrl-%c" (Char.chr (Char.code ch - 1 + Char.code 'a'))
  else Char.escaped ch
;

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value string_of_option f =
  fun
  [ Some x -> sprintf "Some %s" (f x)
  | None -> "None" ]
;

value rec list_uniq_c eq =
  fun
  [ [x :: l] ->
      let l = list_uniq_c eq l in
      match l with
      [ [(y, c) :: r] -> if eq x y then [(y, c+1) :: r] else [(x, 1) :: l]
      | [] -> [(x, 1)] ]
  | [] -> [] ]
;

value rec string_of_next_action g t =
  fun
  [ NAfind_another_room_and_return fa ->
      let gp = fa.fa_gp in
      let base = fa.fa_base in
      sprintf
        "NAfind_another_room_and_return '%s' e=(%d,%d) t=(%d,%d) b (%d,%d)"
        fa.fa_state gp.epos.row gp.epos.col gp.tpos.row gp.tpos.col base.row
        base.col
  | NAgo_and_kill gp ch ->
      sprintf "NAgo_and_kill e=(%d,%d) t=(%d,%d) '%c'" gp.epos.row gp.epos.col
        gp.tpos.row gp.tpos.col ch
  | NAgo_identify_trap gp step na ->
      sprintf "(NAgo_identify_trap e (%d,%d) t (%d,%d) '%s' <%s>"
        gp.epos.row gp.epos.col gp.tpos.row gp.tpos.col step
        (string_of_next_action g t na)
  | NAgo_in_corridor_and_hit ce ->
      sprintf "NAgo_in_corridor_and_hit b (%d,%d) e (%d,%d) t (%d,%d) \"%s\""
        ce.ce_base.row ce.ce_base.col ce.ce_gp.epos.row ce.ce_gp.epos.col
        ce.ce_gp.tpos.row ce.ce_gp.tpos.col ce.ce_state
  | NAgo_unblock_monster um ->
      sprintf "NAgo_unblock_monster e (%d,%d) t (%d,%d) m (%d,%d) <%s>"
        um.um_gp.epos.row um.um_gp.epos.col um.um_gp.tpos.row
        um.um_gp.tpos.col um.um_mpos.row um.um_mpos.col
        (string_of_next_action g t um.um_kont)
  | NAread_scroll_for_best_armor ch na s ->
      sprintf "NAread_scroll_for_best_armor '%c' (%s) (%s)" ch
        (string_of_next_action g t na) (string_of_wear_read_state s)
  | NArestore_health na ->
      sprintf "NArestore_health <%s>" (string_of_next_action g t na)
  | NAreturn_to_base gp na ->
      sprintf "NAreturn_to_base e (%d,%d) t (%d,%d) <%s>" gp.epos.row
        gp.epos.col gp.tpos.row gp.tpos.col (string_of_next_action g t na)
  | NAtest_monster tml na ->
      let tmsl =
        List.map
          (fun (tm, mpos, mch) ->
             sprintf "(%s,(%d,%d),%c)" (string_of_test_monster tm)
               mpos.row mpos.col mch)
          tml
      in
      let tmls = String.concat ";" tmsl in
      sprintf "NAtest_monster [%s] <%s>" tmls (string_of_next_action g t na)
  | NAtest_potions ch step -> sprintf "NAtest_potions %c %d" ch step
  | NAuse_object uo na ->
      sprintf "(NAuse_object %s %s)" (string_of_use_object uo)
        (string_of_next_action g t na)
  | NAwear ch step na ->
      sprintf "NAwear %c %d %s" ch step (string_of_next_action g t na)
  | NAwear_armor_and_test_scrolls ch_arm ch_scr s ->
      sprintf "NAwear_armor_and_test_scrolls '%c' '%c' %s" ch_arm ch_scr
        (string_of_wear_read_state s)

  | NAput_ring ch na step ->
      sprintf "NAput_ring %c (%s) %d" ch (string_of_next_action g t na) step
  | NAthrow_in_the_garbage ch gp na step ->
      sprintf "NAthrow_in_the_garbage %c (%d,%d) (%s) '%s'" ch gp.tpos.row
        gp.tpos.col (string_of_next_action g t na) step
  | NAthrow_away ch step -> sprintf "NAthrow_away '%c' \"%s\"" ch step
  | NAstring s skip_mess na ->
      sprintf "(NAstring \"%s\" %b <%s>)" (String.escaped s) skip_mess
        (string_of_next_action g t na)
  | NAmove mov na ->
      sprintf "NAmove (%d,%d)' (%s)" mov.di mov.dj
        (string_of_next_action g t na)
  | NAseek_object gp ->
      sprintf "NAseek_object e=(%d,%d) t=(%d,%d)" gp.epos.row gp.epos.col
        gp.tpos.row gp.tpos.col
  | NAgo_to gp ->
      sprintf "NAgo_to e=(%d,%d) t=(%d,%d)" gp.epos.row gp.epos.col
        gp.tpos.row gp.tpos.col
  | NAgo_to_door gp ->
      sprintf "NAgo_to_door e=(%d,%d) t=(%d,%d)" gp.epos.row gp.epos.col
        gp.tpos.row gp.tpos.col
  | NAgo_to_shelter_and_test_scrolls gp ->
      sprintf "NAgo_to_shelter_and_test_scrolls e (%d,%d) t (%d,%d)"
        gp.epos.row gp.epos.col gp.tpos.row gp.tpos.col
  | NAmove_in_corridor ipos gp trail ->
      sprintf "NAmove_in_corridor i=(%d,%d) e=(%d,%d) t=(%d,%d)"
        ipos.row ipos.col gp.epos.row gp.epos.col gp.tpos.row
        gp.tpos.col
  | NAseek_gold_or_monster gp gold ->
      sprintf "NAseek_gold_or_monster e (%d,%d) t (%d,%d) g %b" gp.epos.row
        gp.epos.col gp.tpos.row gp.tpos.col gold
  | NAgo_to_stairs gp strict ->
      sprintf "NAgo_to_stairs e (%d,%d) t (%d,%d) %b" gp.epos.row gp.epos.col
        gp.tpos.row gp.tpos.col strict
  | NAlet_come ch move ->
      let ch = if t.t_no_lang_dep then '.' else ch in
      sprintf "NAlet_come '%c' (%d,%d)" ch move.di move.dj
  | NAfight mch answ na ->
      let mch = if t.t_no_lang_dep then '.' else mch in
      sprintf "NAfight '%c' answ %b (%s)" mch answ
        (string_of_next_action g t na)
  | NArun_away mch _ na ->
      let mch = if t.t_no_lang_dep then '.' else mch in
      sprintf "NArun_away '%c' (%s)" mch (string_of_next_action g t na)
  | NAzap mov ch na step ->
      sprintf "NAzap (%d,%d) '%c' (%s) %d" mov.di mov.dj ch
        (string_of_next_action g t na) step
  | NAcheck_no_trap pos pos1 ->
      sprintf "NAcheck_no_trap pos (%d,%d) pos1 (%d,%d)" pos.row pos.col
        pos1.row pos1.col
  | NAdrop_scare chopt na ->
      sprintf "NAdrop_scare %s (%s)" (string_of_option (String.make 1) chopt)
        (string_of_next_action g t na)
  | NAdrop_scare_and_kill ds ->
      sprintf "NAdrop_scare_and_kill (%d,%d) %s" ds.ds_base.row ds.ds_base.col
        (string_of_drop_scare_state ds.ds_state)
  | NAalone_in_room ar ->
      sprintf "NAalone_in_room %s" (string_of_alone_room_state ar.ar_state)
  | NAwield_bow_test_moving mpos mch s ->
      let mch = if t.t_no_lang_dep then '.' else mch in
      sprintf "NAwield_bow_test_moving m (%d,%d) %c %d" mpos.row mpos.col
        mch s
  | NAmove_throw1 _ mpos ch ->
      let ch = if t.t_no_lang_dep then '.' else ch in
      sprintf "NAmove_throw1 (%d,%d) %c" mpos.row mpos.col ch
  | NAmove_throw2 mpos ch step ->
      let ch = if t.t_no_lang_dep then '.' else ch in
      sprintf "NAmove_throw2 (%d,%d) %c \"%s\"" mpos.row mpos.col ch step
  | NAglobal_search1 gp around ->
      let s = if t.t_no_lang_dep then "..." else around.ar in
      sprintf "NAglobal_search1 (%d,%d) (%d,%d) \"%s\"" gp.epos.row
        gp.epos.col gp.tpos.row gp.tpos.col s
  | NAglobal_search2 _ _ ntimes ->
      sprintf "NAglobal_search2 %d" ntimes
  | NAsearch_and_back from around ntimes ->
      let s = if t.t_no_lang_dep then "..." else around.ar in
      sprintf "NAsearch_and_back (%d,%d) %d \"%s\"" from.di from.dj ntimes s
  | NAnone -> "NAnone" ]
and string_of_read_state =
  fun
  [ RSread_what -> "RSread_what"
  | RSscroll_read -> "RSscroll_read"
  | RSidentify_what -> "RSidentify_what"
  | RSidentify ch -> sprintf "(RSidentify '%c')" ch
  | RScontinue -> "RScontinue"
  | RSput_ring ch -> sprintf "(RSput_ring '%c')" ch ]
and string_of_quaff_state =
  fun
  [ QSquaff_what -> "QSquaff_what"
  | QSquaffed -> "QSquaffed" ]
and string_of_wear_read_state =
  fun
  [ WStoken_off -> "WStoken_off"
  | WSwear_what -> "WSwear_what"
  | WSarmor_worn ch -> sprintf "WSarmor_worn '%c'" ch
  | WSscroll_read -> "WSscroll_read" ]
and string_of_drop_scare_state =
  fun
  [ DSdrop ch -> sprintf "(DSdrop %c)" ch
  | DSdropped ntest -> sprintf "(DSdropped %d)" ntest
  | DSfree_monster gp mov wallmov ntimes ->
      sprintf "(DSfree_monster e (%d,%d) t (%d,%d) n %d)"
        gp.epos.row gp.epos.col gp.tpos.row gp.tpos.col ntimes
  | DSforce_kill ch -> sprintf "(DSforce_kill %c)" ch
  | DScheck_monsters -> "DScheck_monsters"
  | DSgive_them_chance s step ->
      sprintf "(DSgive_them_chance \"%s\" %d)" s step
  | DSgo_and_hit gp step ->
      sprintf "(DSgo_and_hit e (%d,%d) t (%d,%d) %d)" gp.epos.row
        gp.epos.col gp.tpos.row gp.tpos.col step
  | DSseek_object gp ->
      sprintf "(DSseek_object e (%d,%d) t (%d,%d))" gp.epos.row
        gp.epos.col gp.tpos.row gp.tpos.col
  | DStest_move ntest rc ->
      sprintf "(DStest_move %d)" ntest
  | DStest_out_in move _ step ->
      sprintf "(DStest_out_in (%d,%d) %d)" move.di move.dj step ]
and string_of_test_monster =
  fun
  [ TMstay -> "TMstay"
  | TMforward -> "TMforward"
  | TMbackward -> "TMbackward" ]
and string_of_use_object =
  fun
  [ UOeat_food ch step ->
      sprintf "(UOeat '%c' \"%s\")" ch step
  | UOquaff_potion ch qs ->
      sprintf "(UOquaff_potion '%c' \"%s\")" ch (string_of_quaff_state qs)
  | UOread_scroll ch rs ->
      sprintf "(UOread_scroll '%c' %s)" ch (string_of_read_state rs)
  | UOthrow_unuseful_objects ch step ->
      sprintf "(UOthrow_unuseful_objects '%c' %d)" ch step
  | UOwield_sword ch step ->
      sprintf "(UOwield_sword '%c' \"%s\")" ch step ]
and string_of_alone_room_state =
  fun
  [ ARgo_and_put_scare dpos ->
      sprintf "(ARgo_and_put_scare (%d,%d))" dpos.row dpos.col
  | ARseek_object gp ->
      sprintf "(ARseek_object at (%d,%d))" gp.tpos.row gp.tpos.col
  | ARgo_and_kill gp ->
      sprintf "(ARgo_and_kill at (%d,%d))" gp.tpos.row gp.tpos.col
  | ARgo_to_door first_door gp ->
      sprintf "(ARgo_to_door f %b e (%d,%d) t (%d,%d))" first_door
        gp.epos.row gp.epos.col gp.tpos.row gp.tpos.col
  | ARforce_kill mov ars ->
      sprintf "(ARforce_kill (%d,%d) %s)" mov.di mov.dj
        (string_of_alone_room_state ars)
  | ARcommand s ->
      sprintf "(ARcommand \"%s\")" s ]
;

(* language dependent *)

value is_amulet s = contains s "amulet";
value score_txt = "Roguist";

(* end (language dependent) *)

value is_hungry g =
  match g.status_line with
  [ Some sl -> sl.sl_hunger <> ""
  | None -> False ]
;
value is_very_hungry g =
  match g.status_line with
  [ Some sl -> (transl g).is_very_hungry sl.sl_hunger
  | None -> False ]
;

value is_identified_wand s = contains s "[";

value remove_message_more g mess =
  let transl = transl g in
  let m = transl.message_more mess in
  if m <> "" then String.sub mess 0 (String.length mess - String.length m)
  else mess
;

(* *)

value eq_dung g dung1 dung2 =
  loop 0 0 where rec loop row col =
    if row = Array.length dung1 then True
    else if col = String.length dung1.(0) then loop (row + 1) 0
    else
      let ch1 = dung1.(row).[col] in
      let ch2 = dung2.(row).[col] in
      if ch1 = ch2 then loop row (col + 1)
      else if is_monster ch1 || is_monster ch2 then loop row (col + 1)
      else False
;

value repetition_threshold = 100;
value repetition_old_state g =
  match g.hist_dung with
  [ [cdung :: hist_dung] ->
      loop 0 hist_dung where rec loop n =
        fun
        [ [dung :: hist_dung] ->
            if n > repetition_threshold && eq_dung g dung cdung then True
            else loop (n + 1) hist_dung
        | [] -> False ]
  | [] -> False ]
;

value rec string_of_pack_obj t =
  fun
  [ Parmor ar -> sprintf "Parmor %s" (string_of_armor_obj ar)
  | Ppotion pot -> sprintf "Ppotion %s" (string_of_potion_obj t pot)
  | Pscroll scr -> sprintf "Pscroll %s" (string_of_scroll_obj t scr)
  | Pweapon we -> sprintf "Pweapon %s" (string_of_weapon_obj we)
  | Pring rn -> sprintf "Pring %s" (string_of_ring_obj t rn)
  | Pwand wa -> sprintf "Pwand %s" (string_of_wand_obj t wa)
  | Pfood -> "Pfood"
  | Pamulet -> "Pamulet" ]
and string_of_armor_obj ar =
  sprintf "{ar_value = %s; ar_protected = %b}"
    (string_of_option string_of_int ar.ar_value) ar.ar_protected
and string_of_potion_obj t =
  fun
  [ Ipotion sk -> sprintf "(Ipotion %s)" (string_of_potion_kind sk)
  | Upotion s ->
      let s = if t.t_no_lang_dep then "..." else s in
      sprintf "(Upotion \"%s\")" s ]
and string_of_potion_kind =
  fun
  [ PKacceler -> "PKacceler"
  | PKblindness -> "PKblindness"
  | PKdetect_mon -> "PKdetect_mon"
  | PKdetect_obj -> "PKdetect_obj"
  | PKhallucination -> "PKhallucination"
  | PKhealing -> "PKhealing"
  | PKextra_heal -> "PKextra_heal"
  | PKincr_stren -> "PKincr_stren"
  | PKraise_level -> "PKraise_level"
  | PKrestore_str -> "PKrestore_str"
  | PKsee_invis -> "PKsee_invis"
  | PKother -> "PKother" ]
and string_of_scroll_obj t =
  fun
  [ Iscroll sk -> sprintf "(Iscroll %s)" (string_of_scroll_kind sk)
  | Uscroll s ->
      let s = if t.t_no_lang_dep then "..." else s in
      sprintf "(Uscroll \"%s\")" s ]
and string_of_scroll_kind =
  fun
  [ SKaggr_mon -> "SKaggr_mon"
  | SKench_ar -> "SKench_ar"
  | SKench_wea -> "SKench_wea"
  | SKhold_mon -> "SKhold_mon"
  | SKident -> "SKident"
  | SKmagic_map -> "SKmagic_map"
  | SKprotect -> "SKprotect"
  | SKscare_mon -> "SKscare_mon"
  | SKteleport -> "SKteleport"
  | SKuncurse -> "SKuncurse"
  | SKother -> "SKother" ]
and string_of_weapon_obj we =
  sprintf "{we_kind = %s; we_value = %s}" (string_of_weapon_kind we.we_kind)
    (string_of_option string_of_int we.we_value)
and string_of_weapon_kind =
  fun
  [ WKtwo_handed_sword -> "WKtwo_handed_sword"
  | WKlong_sword -> "WKlong_sword"
  | WKmace -> "WKmace"
  | WKshort_bow -> "WKshort_bow"
  | WKarrows -> "WKarrows"
  | WKother -> "WKother" ]
and string_of_ring_obj t =
  fun
  [ Iring rk -> sprintf "(Iring %s)" (string_of_ring_kind rk)
  | Uring s ->
      let s = if t.t_no_lang_dep then "..." else s in
      sprintf "(Uring \"%s\")" s ]
and string_of_ring_kind =
  fun
  [ RKslow_digestion -> "RKslow_digestion"
  | RKother -> "RKother" ]
and string_of_wand_obj t =
  fun
  [ Iwand ak nbi ->
      sprintf "(Iwand %s (%s))" (string_of_wand_kind ak)
        (string_of_option string_of_int nbi)
  | Uwand s ->
      let s = if t.t_no_lang_dep then "..." else s in
      sprintf "(Uwand \"%s\")" s ]
and string_of_wand_kind =
  fun
  [ AKcancel -> "AKcancel"
  | AKmagic_miss -> "AKmagic_miss"
  | AKother -> "AKother" ]
;

value string_of_door_dir =
  fun
  [ DoorUp -> "DoorUp"
  | DoorDown -> "DoorDown"
  | DoorLeft -> "DoorLeft"
  | DoorRight -> "DoorRight" ]
;

value home oc = fprintf oc "\027[0H";
value move oc x y = fprintf oc "\027[%d;%dH" x y;

value display_trail g = do {
  let oc = stdout in
  home oc;
  fprintf oc "\n";
  for row = 1 to g.dung.nrow - 1 do {
    for col = 0 to g.dung.ncol - 1 do {
      let pos = {row = row; col = col} in
      let v = try PosMap.find pos g.trail with [ Not_found -> 0 ] in
      let v = min v 9 in
      let ch = dung_char g.dung pos in
      fprintf oc "%c" (if v = 0 then ch else (Char.chr (Char.code '0' + v)));
    };
    if row <> g.dung.nrow - 1 then fprintf oc "\n" else ();
  };
  let pos = rogue_pos g in
  move oc (pos.row + 1) (pos.col + 1);
  flush oc;
};

value trace_pack g t = do {
  eprintf "pack:\n";
  List.iter
    (fun (ref, (nb, obj)) ->
       eprintf "-%c: %s%s%s\n" ref (string_of_pack_obj t obj)
         (if nb = 1 then "" else sprintf " (%d items)" nb)
         (if ref = g.main_sword then
            " in hand" ^ if g.weapon_cursed then " (cursed)" else ""
          else if Some ref = g.ring_of_slow_digestion_on_hand then
            " on hand"
          else
            match g.worn_armor with
            [ Some (ch, _) ->
               if ref = ch then
                 " being worn" ^ if g.armor_cursed then " (cursed)" else ""
               else ""
            | None -> "" ]))
    (List.sort compare g.pack);
  flush stderr;
};

(*
value trace_graph_global_search g graph pos path tpos = do {
(**)
  let oc = stdout in
  tempo g 1.0;
  home ();
(*
let oc = stderr in
*)
  fprintf oc "  pos (%d,%d) target (%d,%d) %d: " pos.row pos.col tpos.row
    tpos.col (List.length path);
  flush oc;
  List.iter (fun mov -> fprintf oc "%c" (basic_command_of_move mov)) path;
  fprintf oc "   \n";
  for row = 1 to g.dung.nrow - 1 do {
    for col = 0 to g.dung.ncol - 1 do {
      let node = graph.(row).(col) in
      if node.search = ToSearch then fprintf oc "\027[34m"
      else if node.search = SearchFailed then fprintf oc "\027[31m"
      else ();
      let n =
        List.fold_left
          (fun cnt connected -> if connected then cnt + 1 else cnt)
          0 (Array.to_list node.connection)
      in
      if n > 0 && (pos.row <> row || pos.col <> col) then fprintf oc "%d" n
      else fprintf oc "%c" g.dung.tab.(row).[col];
      fprintf oc "\027[m"
    };
    if row < g.dung.nrow - 1 then fprintf oc "\n" else ();
  };
  flush oc;
};
*)

value is_entering_a_monsters_room g t =
  match t.t_next_action with
  [ NAdrop_scare_and_kill _ -> False
  | NAalone_in_room _ -> False
  | NAreturn_to_base _ (NAdrop_scare_and_kill _) -> False
  | NAreturn_to_base _ (NAalone_in_room _) -> False
  | NAgo_unblock_monster _ -> False
  | _ ->
      match t.t_prev_mov with
      [ Some mov -> do {
          match g.rogue_room_and_door with
          [ Some ((rmin, cmin, rmax, cmax), Some dir) -> do {
              if mov = one_step_to_enter_room dir then
                let count_monsters =
                  loop 0 rmin cmin where rec loop cnt row col =
                    if row > rmax then cnt
                    else if col > cmax then loop cnt (row + 1) cmin
                    else
                      let cnt =
                        if is_monster g.dung.tab.(row).[col] then cnt + 1
                        else cnt
                      in
                      loop cnt row (col + 1)
                in
                count_monsters > 6
              else False
            }
          | Some (_, None) | None -> False ]
        }
      | None ->
          False ] ]
;

value trace_path t path = do {
  List.iter (fun pos -> trace t (sprintf "(%d,%d)" pos.row pos.col)) path
};

value nb_diff g =
  fun
  [ Some old_g ->
      loop 0 0 0 where rec loop n row col =
        if row = g.dung.nrow then n
        else if col = g.dung.ncol then loop n (row + 1) 0
        else
          let od = old_g.dung.tab.(row).[col] in
          let nd = g.dung.tab.(row).[col] in
          if nd = od || nd = '.' || od = '.' then
            loop n row (col + 1)
          else
            loop (n + 1) row (col + 1)
  | None -> 0 ]
;

value is_score_display g =
  loop_row 0 where rec loop_row row =
    if row = g.dung.nrow then False
    else
      loop 0 0 where rec loop col i =
        if i = String.length score_txt then True
        else if col = g.dung.ncol then loop_row (row + 1)
        else if g.dung.tab.(row).[col] = score_txt.[i] then
          loop (col + 1) (i + 1)
        else loop (col - i + 1) 0
;

value can_be_called_while_going_to_stairs =
  fun
  [ NAfight _ _ _ | NAglobal_search1 _ _ | NAglobal_search2 _ _ _ |
    NAgo_to_shelter_and_test_scrolls _ | NAgo_to_stairs _ _ |
    NAseek_gold_or_monster _ _ -> True
  | _ -> False ]
;

value non_interruptable_action =
  fun
  [ NAdrop_scare_and_kill
      {ds_state = DSforce_kill _ (*| DSgo_in_corridor_and_hit _ "start"*)} |
    NAfight _ _ _ | NAgo_in_corridor_and_hit _ | NAmove _ _ |
    NAmove_throw2 _ _ "direction" |
    NAput_ring _ _ _ | NAread_scroll_for_best_armor _ _ _ |
    NAtest_potions _ 1 | NAuse_object _ _ | NAwear _ _ _ |
    NAwear_armor_and_test_scrolls _ _ _ | NAzap _ _ _ _ -> True
  | _ -> False ]
;

value all_visited g = do {
  let n = number_of_visited_rooms g in
  n = 9 && g.regrets = [] && nothing_interesting_in_current_room g
};

value trace_trail t trail = do {
  let trail = List.rev trail in
  trace t "*** trail: ";
  List.iter (fun pos -> trace t (sprintf "(%d,%d)" pos.row pos.col)) trail;
  trace t "\n";
  let room_trail =
    List.map
      (fun pos ->
         let r = (pos.row, pos.col, pos.row, pos.col) in
         match (gen_room_row r, gen_room_col r) with
         [ (Some rr, Some rc) -> Some (rr, rc)
         | _ -> None ])
      trail
  in
  let room_trail = list_uniq_c \= room_trail in
  trace t "*** room trail:";
  List.iter
    (fun (rt, c) -> do {
       trace t " ";
       match rt with
       [ Some (rr, rc) -> trace t (sprintf "(%d,%d)" rr rc)
       | None -> trace t "(..)" ];
       if c > 1 then trace t (sprintf "x%d" c) else ();
     })
    room_trail;
  trace t "\n";
};

value add_object_in_pack g ch s =
  let transl = transl g in
  let (nb, obj) =
    let n =
      if s.[0] >= '1' && s.[0] <= '9' then
        let len =
          loop 0 where rec loop i =
            if s.[i] >= '0' && s.[i] <= '9' then loop (i + 1)
            else i
        in
        int_of_string (String.sub s 0 len)
      else 1
    in
    if transl.is_armor s then
      let v = armor_value s in
      let prot = transl.is_leather_armor s in
      let ar = {ar_value = v; ar_protected = prot} in
      (1, Parmor ar)
    else if transl.is_scroll s then
      let scr =
        if transl.is_identified_scroll s then do {
          let sk = identified_scroll_kind_of_message g s in
          Iscroll sk
        }
        else Uscroll s
      in
      (n, Pscroll scr)
    else if transl.is_potion s then
      let pot =
        if transl.is_identified_potion s then
          Ipotion (identified_potion_kind_of_message g s)
        else Upotion s
      in
      (n, Ppotion pot)
    else if transl.is_weapon s then
      let wk = weapon_kind_of_message g s in
      let v = weapon_value s in
      let we = {we_kind = wk; we_value = v} in
      (n, Pweapon we)
    else if transl.is_ring s then
      let rg =
        if transl.is_identified_ring s then
          let rk =
            if transl.is_ring_of_slow_digestion s then RKslow_digestion
            else RKother
          in
          Iring rk
        else Uring s
      in
      (1, Pring rg)
    else if transl.is_wand s then
      let aw =
        let ak () =
          if transl.is_wand_of_cancellation s then AKcancel
          else if transl.is_wand_of_magic_missile s then AKmagic_miss
          else AKother
        in
        if is_identified_wand s then Iwand (ak ()) (wand_value s)
        else if transl.is_identified_wand_kind s then Iwand (ak ()) None
        else Uwand s
      in
      (1, Pwand aw)
    else if transl.is_food s then (n, Pfood)
    else if is_amulet s then (1, Pamulet)
    else failwith (sprintf "what: '%s'" (String.escaped s))
  in
  g.pack := [(ch, (nb, obj)) :: List.remove_assoc ch g.pack]
;

value is_low_alpha c = c >= 'a' && c <= 'z';
value is_alpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';

exception Breakpoint of int;

value play tab nrow ncol t = do {
  let message =
    let first_line_len =
      loop (String.length tab.(0) - 1) where rec loop i =
        if i < 0 then 0
        else if tab.(0).[i] = ' ' then loop (i - 1)
        else i + 1
    in
    String.sub tab.(0) 0 first_line_len
  in
  let g =
    let trail =
      match t.t_prev_game with
      [ Some g -> g.trail
      | None -> PosMap.empty ]
    in
    let (rogue_pos, trail) =
      loop 1 0 where rec loop row col =
(*
        if row = Array.length tab - 1 then (None, trail)
*)
        if row = Array.length tab - 1 then do {
          let rogue_pos =
            match t.t_prev_game with
            [ Some old_g -> old_g.rogue_pos
            | None -> None ]
          in
          (rogue_pos, trail)
        }
(**)
        else if col = String.length tab.(row) then loop (row + 1) 0
        else if tab.(row).[col] = '@' then do {
          let pos = {row = row; col = col} in
          let inc =
            match t.t_prev_game with
            [ Some old_g -> if old_g.rogue_pos = Some pos then 0 else 1
            | None -> 1 ]
          in
          let n = try PosMap.find pos trail + inc with [ Not_found -> 1 ] in
          (Some pos, PosMap.add pos n trail)
        }
        else loop row (col + 1)
    in
    let rogue_pos =
      match t.t_prev_game with
      [ Some g -> if g.dead then None else rogue_pos
      | None -> rogue_pos ]
    in
    let dung = {tab = tab; nrow = nrow; ncol = ncol} in
    let rogue_room_and_door =
      match rogue_pos with
      [ Some pos -> pos_room_and_door dung pos
      | None -> None ]
    in
    let move_result =
      match t.t_prev_pos with
      [ Some old_g ->
          match (old_g.rogue_pos, t.t_prev_comm, rogue_pos) with
          [ (Some prev_pos, Some (Cmov {di = di; dj = dj}), Some pos) ->
              if pos = {row = prev_pos.row + di; col = prev_pos.col + dj} then
                MRok
              else if pos = prev_pos then MRcaught
              else MRteleported
          | _ -> MRok ]
      | None -> MRok ]
    in
    let is_message_more =
      match t.t_prev_game with
      [ Some g -> (transl g).message_more message <> ""
      | None -> False ]
    in
    match t.t_prev_game with
    [ Some g -> do {
        let prev_level = g.level in
        let status_line_opt =
          try Some ((transl g).scan_status_line tab.(g.dung.nrow-1)) with
          [ Scan_failure _ -> None ]
        in
        let level =
          match status_line_opt with
          [ Some sl -> sl.sl_level
          | None -> g.level ]
        in
        let current_dung = Array.sub tab 1 (nrow - 2) in
        let rogtime =
          if Array.sub tab 1 (nrow - 1) <> Array.sub g.dung.tab 1 (nrow - 1)
          then g.rogtime + 1
          else g.rogtime
        in
        let g =
          {(g) with dung = dung; level = level; rogtime = rogtime;
           time = g.time + 1; status_line = status_line_opt;
           time_in_level = g.time_in_level + 1;
           is_message_more = is_message_more; trail = trail;
           rogue_pos = rogue_pos; rogue_room_and_door = rogue_room_and_door;
           move_result = move_result;
           hist_dung = [current_dung :: g.hist_dung]}
        in
        if level <> prev_level ||
           prev_level = 99 &&
              (t.t_prev_comm = Some (Coth '>') ||
               (transl g).is_fallen_down message)
        then do {
          for i = 0 to 2 do {
            for j = 0 to 2 do {
              g.visited.(i).(j) := False;
            }
          };
          g.speed := t.t_speed;
          g.time_in_level := 0;
          g.trail := PosMap.empty;
          g.sure_stairs_pos := None;
          g.garbage := [];
          g.scare_pos := [];
          g.graph := None;
          g.map_showed_since := 0;
          g.mon_detected := False;
          g.was_hallucinated := g.hallucinated;
          g.attacked := 0;
          g.attacked_by_invisible := False;
          g.attacked_by_flame := 0;
          g.frozen_monsters := [];
          g.regrets := [];
          g.nb_of_reinit_search := 0;
          Hashtbl.clear g.traps;
          g.hist_dung := [];
          g.paradise := False;
          if t.t_move_trace then trace_pack g t else ();
        }
        else ();
        g
      }
    | None ->
        let ar = {ar_value = Some 4; ar_protected = False} in
        {dung = dung; rogtime = 0; time = 0; status_line = None;
         is_message_more = is_message_more; move_result = move_result;
         random_state = Random.get_state (); level = 1; lang = "en";
         speed = t.t_speed; time_in_level = 0; trail = trail;
         rogue_pos = rogue_pos; sure_stairs_pos = None;
         rogue_room_and_door = rogue_room_and_door; on_something_at = None;
         pack =
           [('a', (1, Pfood)); ('b', (1, Parmor ar));
            ('c', (1, Pweapon {we_kind = WKmace; we_value = Some 2}));
            ('d', (1, Pweapon {we_kind = WKshort_bow; we_value = Some 1}));
            ('e', (15, Pweapon {we_kind = WKarrows; we_value = Some 0}))];
         pack_full = False; worn_armor = Some ('b', ar); main_sword = 'c';
         armor_cursed = False; weapon_cursed = False;
         ring_of_slow_digestion_on_hand = None; garbage = []; scare_pos = [];
         graph = None; map_showed_since = 0; mon_detected = False;
         confused = False; hallucinated = False; was_hallucinated = False;
         blind = False; held = False; attacked = 0;
         attacked_by_invisible = False; attacked_by_flame = 0;
         frozen_monsters = []; regrets = []; blindness_discovered = False;
         hallucination_discovered = False; teleport_discovered = False;
         after_first_pack_full = False;
         visited = Array.init 3 (fun _ -> Array.make 3 False);
         nb_of_reinit_search = 0; traps = Hashtbl.create 1;
         paradise = False; hist_dung = []; dead = False} ]
  in

  let frozen_monsters =
    List.filter (fun (pos, ch) -> dung_char g.dung pos = ch) g.frozen_monsters
  in
  g.frozen_monsters := frozen_monsters;

  match g.rogue_pos with
  [ Some pos -> g.regrets := List.filter (fun pos1 -> pos <> pos1) g.regrets
  | None -> () ];

  match t.t_slow_at_level with
  [ Some lev -> if g.level = lev then g.speed := 1.0 else ()
  | None -> () ];

  match t.t_slow_at_time with
  [ Some time -> if g.time = time then g.speed := 1.0 else ()
  | None -> () ];

  let transl = transl g in
  if t.t_move_trace then do {
    match t.t_prev_comm with
    [ Some comm ->
        trace t
          (sprintf "%d (lev %d): %s '%s'%s%s%s%s%s%s%s%s%s%s%s%s <%s>\n"
             g.time g.level
             (match comm with
              [ Cmov mov -> sprintf "(Cmov (%d,%d))" mov.di mov.dj
              | Cansw_left -> "(Cansw_left)"
              | Cansw_yes -> "(Cansw_yes)"
              | Coth ch -> sprintf "(Coth '%c')" ch ])
             (let ch =
                match comm with
                [ Cmov mov -> basic_command_of_move mov
                | Cansw_left ->
                    if t.t_no_lang_dep then '.' else transl.answer_left_hand
                | Cansw_yes ->
                    if t.t_no_lang_dep then '.' else transl.answer_yes
                | Coth ch -> ch ]
              in
              char_escaped ch)
             (match g.move_result with
              [ MRcaught -> " (caught)"
              | MRteleported -> " (teleported)"
              | MRok -> "" ])
             (match g.rogue_pos with
              [ Some pos -> sprintf " pos (%d,%d)" pos.row pos.col
              | None -> "" ])
             (match if g.time = 1 then None else g.status_line with
              [ Some sl -> sprintf " hp %d(%d)" sl.sl_hp sl.sl_max_hp
              | None -> "" ])
             (if message <> "" then
                sprintf " \"%s\"" (if t.t_no_lang_dep then "..." else message)
              else "")
             (if g.pack_full then " <full>" else "")
             (if g.armor_cursed then " <acursed>" else "")
             (if g.weapon_cursed then " <wcursed>" else "")
             (if g.hallucinated then " <hallu>" else "")
             (if g.blind then " <blind>" else "")
             (if g.confused then " <confus>" else "")
             (if g.attacked_by_flame > 0 then " <flame>" else "")
             (if g.held then " <held>" else "")
             (string_of_next_action g t t.t_next_action))
    | None -> () ];
    match t.t_prev_game with
    [ Some old_g ->
        if g.pack_full && not old_g.pack_full then trace_pack g t else ()
    | None -> () ]
  }
  else ();

  let s = remove_message_more g message in
  let i = String.length s in

  if i > 3 && s.[i-3] = '(' && s.[i-1] = ')' && is_low_alpha s.[i-2] then do {
    let ch = s.[i-2] in
    let s = String.sub s 0 (i-4) in
    g.on_something_at := None;
    add_object_in_pack g ch s
  }
  else if i > 3 && is_low_alpha s.[0] && s.[1] = ')' && s.[2] = ' ' then do {
    let ch = s.[0] in
    let s = String.sub s 3 (i - 3) in
    add_object_in_pack g ch s;
    if t.t_move_trace then trace_pack g t else ();
  }
  else ();

  if message <> "" then do {
    if transl.is_message_moved_onto message then do {
      let pos = rogue_pos g in
      let on_scare = transl.is_scroll_of_scare_monsters message in
      g.on_something_at := Some (pos, on_scare);
    }
    else do {
      g.hist_dung := [];
      if transl.is_message_weaken_armor message then
        match g.worn_armor with
        [ Some (ch, ar) -> do {
            let v =
              match ar.ar_value with
              [ Some v -> Some (v - 1)
              | None -> None ]
            in
            let ar = {(ar) with ar_value = v} in
            g.worn_armor := Some (ch, ar);
            redefine_in_pack g ch (Parmor ar);
          }
        | None -> assert False ]
      else if transl.is_message_has_been_confused message then
        g.confused := True
      else if transl.is_message_less_confused message then
        g.confused := False
      else if transl.is_message_cosmic message then do {
        g.hallucinated := True;
        g.was_hallucinated := True
      }
      else if transl.is_message_boring message then g.hallucinated := False
      else if transl.is_message_darkness message then do {
        g.blind := True;
        g.blindness_discovered := True
      }
      else if transl.is_message_no_darkness message then g.blind := False
      else if transl.is_message_pack_full message then g.pack_full := True
      else if transl.is_message_something_attacked message then
        g.attacked_by_invisible := True
      else if transl.is_message_shows_a_map message then
        g.map_showed_since := g.time
      else ();
    };
    g.attacked_by_flame :=
      if transl.is_message_attacked_by_flame message then 5 else 0;
    g.held := transl.is_message_held message;
    let prev_was_message_more =
      match t.t_prev_game with
      [ Some g -> g.is_message_more
      | None -> False ]
    in
    if transl.is_message_aggressive_monster message then do {
      if prev_was_message_more then g.attacked := g.attacked + 1
      else g.attacked := 1
    }
    else if prev_was_message_more then ()
    else g.attacked := 0;
  }
  else do {
    let pos = rogue_pos g in
    match t.t_prev_pos with
    [ Some {rogue_pos = Some old_pos} ->
(*
        if pos <> old_pos then do {
          g.attacked := 0;
          if is_at_door g old_pos && g.attacked_by_flame then
            g.attacked_by_flame := False
          else ();
        }
        else ()
*)
(*
        if pos <> old_pos && is_at_door g old_pos && g.attacked_by_flame > 0
        then do {
          g.attacked_by_flame := 0;
          g.attacked := 0;
        }
        else ()
*)
        if pos <> old_pos && g.attacked_by_flame > 0 then do {
          if is_at_door g old_pos then do {
            g.attacked_by_flame := 0;
            g.attacked := 0;
          }
          else do {
            g.attacked_by_flame := g.attacked_by_flame - 1;
            if g.attacked_by_flame = 0 then g.attacked := 0 else ()
          }
        }
        else ()
(**)
    | Some _ | None -> () ];
  };

  if transl.is_message_aggressive_monster message &&
     not g.hallucinated && g.attacked_by_flame = 0 &&
     not g.attacked_by_invisible
  then do {
    let old_hp =
      match t.t_prev_game with
      [ Some old_g -> health_points old_g
      | None -> assert False ]
    in
    let new_hp = health_points g in
    let wound = old_hp - new_hp in
    if wound > 0 then do {
      let pos = rogue_pos g in
      let monl = Rob_monster.monsters_around g pos in
      match monl with
      [ [mov] -> do {
          let mch = dung_char g.dung (add_mov pos mov) in
          if transl.is_message_about_fliting_monster message &&
             not (Rob_monster.is_fliting_monster g mch)
          then ()
          else do {
            let mp = Rob_monster.basic_monster_power g t mch (fun _ -> []) in
            if wound > mp then
(*
if not (try let _ = String.index "BCDEFIJKLMOPQRSTUVWXYZ" mch in True with _ -> False) then failwith (sprintf "mch '%c' pow %d" mch wound) else
*)
              Rob_monster.set_monster_power g t mch wound
            else ();
          }
        }
      | [] | [_ :: _] -> () ];
    }
    else ();
  }
  else ();

  if t.t_move_trace && g.attacked > 0 then
    trace t (sprintf "*** g.attacked %d\n" g.attacked)
  else ();

  if g.time = t.t_breakpoint then do {
    if t.t_move_trace then trace_pack g t else ();
(*
    display_trail g;
*)
    raise (Breakpoint g.time)
  }
  else ();

  let (comm, next_action, mov) =

    let i = String.length message in
    let s = message in
    if_match
      if i > 3 && s.[i-3] <> '(' && s.[i-1] = ')' then do {
        let j = String.index s '(' in
        let lang = String.sub s (j + 1) 2 in
        if is_alpha lang.[0] && is_alpha lang.[1] then Some lang
        else None
      }
      else None
    with_some lang -> do {
      g.lang := lang;
      (Coth '\n', t.t_next_action, None)
    }

    else if g.dead || transl.is_message_dead message then do {
      if not (is_score_display g) then
        let d = nb_diff g t.t_prev_game in
        let wait = max 1 (d / 30) in
        tempo g (float wait)
      else ();
      g.dead := True;
      (Coth ' ', NAnone, None)
    }

    else if transl.is_fallen_down message then do {
      tempo g 1.0;
      (Coth ' ', NAnone, None)
    }

    else if transl.is_message_really_pick message then do {
      (Cansw_yes, t.t_next_action, t.t_prev_mov)
    }

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if
        not (is_very_hungry g) &&
          (not (is_hungry g) || g.ring_of_slow_digestion_on_hand = None)
      then None
      else food_in_pack g.pack
    with_some (ch, _) -> do {
      let uo = UOeat_food ch "eat what" in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'e', na, t.t_prev_mov)
    }

    else if
      message = "" && not g.confused &&
      match t.t_prev_comm with
      [ Some (Cmov mov) -> do {
          match t.t_prev_game with
          [ Some old_g -> do {
              let old_pos = rogue_pos old_g in
              let supposed_pos = add_mov old_pos mov in
              assert (in_dung g supposed_pos);
              let ch = dung_char g.dung supposed_pos in
              if rogue_pos g = old_pos && (is_monster ch || is_object ch)
              then True
              else False
            }
          | None -> False ]
        }
      | Some _ | None -> False ]
    then do {
      (* moved onto a monster or an object and resulting of no message
         nor effective move: likely hidden place *)
      let na = t.t_next_action in
      (Coth 's', na, t.t_prev_mov)
    }

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if not g.blind then None
      else potion_of_see_invisible_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo NAnone in
      (Coth 'q', na, None)

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if not g.blind then None
      else potion_of_healing_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, None)

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if not g.blind then None
      else potion_of_extra_healing_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, None)

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if not g.hallucinated then None
      else potion_of_extra_healing_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, None)

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else if
        not g.blindness_discovered &&
        potion_of_see_invisible_in_pack g.pack = None
      then None
      else potion_of_healing_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, t.t_prev_mov)

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else several_potions_of_healing_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, t.t_prev_mov)

    else if
      message = "" && not (non_interruptable_action t.t_next_action) &&
        (g.hallucination_discovered &&
         potion_of_extra_healing_in_pack g.pack <> None &&
         (g.blindness_discovered ||
          potion_of_healing_in_pack g.pack <> None ||
          potion_of_see_invisible_in_pack g.pack <> None) ||
         several_potions_of_extra_healing_in_pack g.pack <> None)
    then do {
      match potion_of_extra_healing_in_pack g.pack with
      [ Some (ch, _) ->
          let uo = UOquaff_potion ch QSquaff_what in
          let na = NAuse_object uo t.t_next_action in
          (Coth 'q', na, t.t_prev_mov)
      | None -> assert False ]
    }

    else if_match
      if message <> "" || non_interruptable_action t.t_next_action then None
      else potion_of_increase_strength_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOquaff_potion ch QSquaff_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'q', na, t.t_prev_mov)

    else if_match
      if message <> "" || g.blind || non_interruptable_action t.t_next_action
      then None
      else scroll_of_enchant_weapon_in_pack g.pack
    with_some (ch, _) ->
      let uo = UOread_scroll ch RSread_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'r', na, t.t_prev_mov)

    else if
      message = "" && not (non_interruptable_action t.t_next_action) &&
      not g.armor_cursed && not (wearing_good_armor g) &&
      not (worn_armor_protected g && wearing_best_armor g) &&
      not (Rob_monster.aquator_around g)
    then do {
      match good_armor g with
      [ Some (ch, _) ->
          let na = t.t_next_action in
          match g.worn_armor with
          [ Some _ -> (Coth 'T', NAwear ch 1 na, t.t_prev_mov)
          | None -> (Coth 'W', NAwear ch 2 na, t.t_prev_mov) ]
      | None -> failwith "no armor at all" ]
    }

    else if
      message = "" && g.armor_cursed && not g.blind &&
      not (non_interruptable_action t.t_next_action) &&
      (scroll_of_remove_curse_in_pack g.pack <> None ||
       scroll_of_enchant_armor_in_pack g.pack <> None ||
       scroll_of_protect_armor_in_pack g.pack <> None)
    then
      let ch =
        match scroll_of_remove_curse_in_pack g.pack with
        [ Some (ch, _) -> ch
        | None ->
            match scroll_of_enchant_armor_in_pack g.pack with
            [ Some (ch, _) -> ch
            | None ->
                match scroll_of_protect_armor_in_pack g.pack with
                [ Some (ch, _) -> ch
                | None -> assert False ] ] ]
      in
      let uo = UOread_scroll ch RSread_what in
      let na = NAuse_object uo t.t_next_action in
      (Coth 'r', na, None)

    else if
      not (non_interruptable_action t.t_next_action) &&
      is_entering_a_monsters_room g t
    then do {
      if message <> "" then do {
        tempo g 1.0;
        let na =
          match t.t_next_action with
          [ NAfight mch _ na -> NAfight mch True na
          | na -> na ]
        in
        (Coth ' ', na, t.t_prev_mov)
      }
      else do {
        match scroll_of_scare_monsters_in_pack g.pack with
        [ Some (ch, _) -> do {
            let pos = rogue_pos g in
            if t.t_stop_at_paradise &&
               g.ring_of_slow_digestion_on_hand <> None
            then
              Rob_action.stop_paradise t
            else ();
            let ds = Rob_action.start_drop_scare pos ch in
            let na = NAdrop_scare_and_kill ds in
            (Coth 'd', na, None)
          }
        | None -> do {
            match scroll_of_hold_monsters_in_pack g.pack with
            [ Some (ch, _) -> do {
                let na =
                  match g.rogue_room_and_door with
                  [ Some (room, Some dir) ->
                      let move = one_step_to_exit_room dir in
                      let mmov = {di = -move.di; dj = -move.dj} in
                      let movl = [move] in
                      let mov =
                        match
                          Rob_action.run_away_if_possible g False movl mmov
                        with
                        [ Some (mov, _) -> mov
                        | None -> mmov ]
                      in
                      let ch = basic_command_of_move mov in
                      let ch = Char.chr (Char.code ch - Char.code 'a' + 1) in
                      NAstring (String.make 1 ch) False NAnone
                  | Some (_, None) | None -> NAnone ]
                in
                let uo = UOread_scroll ch RSread_what in
                let na = NAuse_object uo na in
                (Coth 'r', na, None)
              }
            | None -> do {
                Rob_action.apply g t message
              } ]
          } ]
      }
    }

    else if
      not (non_interruptable_action t.t_next_action) && not g.blind &&
      g.attacked = 0 &&
      not (can_be_called_while_going_to_stairs t.t_next_action) &&
      (repetition_old_state g || not g.paradise && all_visited g) &&
      message = "" && g.sure_stairs_pos <> Some (rogue_pos g)
    then do {
      g.hist_dung := [];
      let graph = make_graph g False in
      let sp = stairs_pos g in
      if sp <> [] then do {
        let pos = rogue_pos g in
        let spos = List.nth sp (random_int g (List.length sp)) in
        Rob_action.go_to_stairs g t graph pos spos False
      }
(*
      else if not g.map_showed_since then do {
        Rob_action.start_search g t graph
      }
      else do {
        (* perhaps stairs hidden under a non-moving monster *)
        let pos = rogue_pos g in
        match Rob_monster.path_to_closest_static_monster g t pos with
        [ Some gp -> do {
            let na = NAseek_gold_or_monster gp False in
            slow_down g;
            (Coth ' ', na, None)
          }
        | None -> do {
            Rob_action.random_move g pos NAnone
          } ]
      }
*)
      else do {
        (* perhaps stairs hidden under a non-moving monster *)
        let pos = rogue_pos g in
        match path_to_closest_static_monster g t pos with
        [ Some gp -> do {
            let na = NAseek_gold_or_monster gp False in
            Rob_action.slow_down g t;
            (Coth ' ', na, None)
          }
        | None -> do {
            if g.map_showed_since = 0 then do {
              Rob_action.start_search g t graph
            }
            else do {
              Rob_action.random_move g pos NAnone
            }
          } ]
      }
(**)
    }

    else if
      message = "" && not (non_interruptable_action t.t_next_action) &&
      g.level >= level_of_very_mean_monsters && g.map_showed_since = 0 &&
      number_of_scrolls_of_magic_mapping_in_pack g.pack > 1
    then do {
      match scroll_of_magic_mapping_in_pack g.pack with
      [ Some (ch, _) -> do {
          let na = t.t_next_action in
          let uo = UOread_scroll ch RSread_what in
          let na = NAuse_object uo na in
          (Coth 'r', na, None)
        }
      | None -> assert False ]
    }

    else if
      message = "" && not (non_interruptable_action t.t_next_action) &&
      g.level >= level_of_very_mean_monsters && not g.mon_detected &&
      number_of_potions_of_detect_monsters_in_pack g.pack > 1
    then do {
      match potion_of_detect_monsters_in_pack g.pack with
      [ Some (ch, _) -> do {
          let na = t.t_next_action in
          let uo = UOquaff_potion ch QSquaff_what in
          let na = NAuse_object uo na in
          (Coth 'q', na, None)
        }
      | None -> assert False ]
    }

    else do {
      Rob_action.apply g t message
    }
  in
  let on_stairs =
    match comm with
    [ Cmov mov ->
        let pos = add_mov (rogue_pos g) mov in
        in_dung g pos && dung_char g.dung pos = '%'
    | Cansw_left | Cansw_yes | Coth _ -> False ]
  in

(*
  let mov =
    (* variable superseded by 'rmov'; should be removed one day *)
    match mov with
    [ Some (0, 0) -> t.t_prev_mov
    | Some _ | None -> mov ]
  in
*)
  let ch =
    match comm with
    [ Cmov mov -> basic_command_of_move mov
    | Cansw_left -> transl.answer_left_hand
    | Cansw_yes -> transl.answer_yes
    | Coth ch -> ch ]
  in

  let neutral_move = [Coth ' '; Coth ''; Coth ''] in
  let t =
    {(t) with
     t_prev_pos = if List.mem comm neutral_move then t.t_prev_pos else Some g;
     t_prev_game = Some g; t_prev_comm = Some comm; t_prev_mov = mov;
     t_on_stairs = on_stairs; t_next_action = next_action}
  in
  (ch, t)
};

value start_wizard = NAstring "\023password\n" False NAnone;
value start_no_wizard = NAstring "@Ie" False NAnone;
value start_nothing_special = NAnone;

value start_action = start_no_wizard;

value arg_list_of_string str =
  loop [] 0 where rec loop rev_arg_list i =
    if i >= String.length str then List.rev rev_arg_list
    else
      let j =
        match
          try Some (String.index_from str i ',') with [ Not_found -> None ]
        with
        [ Some j -> j
        | None -> String.length str ]
      in
      let arg = String.sub str i (j - i) in
      let rev_arg_list =
        match arg.[0] with
        [ 'a'..'z' ->
            let arg1 = sprintf "-%c" arg.[0] in
            if String.length arg = 1 then [arg1 :: rev_arg_list]
            else
              let arg2 = String.sub arg 1 (String.length arg - 1) in
              [arg2; arg1 :: rev_arg_list]
        | _ -> [arg :: rev_arg_list] ]
      in
      loop rev_arg_list (j + 1)
;

value default_monpow_fname = ".robot.monpow.txt";

value arg_breakpoint = ref (-1);
value arg_slow_at_time = ref 0;
value arg_monpow_fname = ref default_monpow_fname;
value arg_move_trace = ref False;
value arg_no_lang_dep = ref False;
value arg_slow_at_level = ref 0;
value arg_stop_at_paradise = ref False;
value arg_speed = ref 1.0;

value speclist =
  Arg.align
    [("-b", Arg.Set_int arg_breakpoint, " breakpoint at time");
     ("-l", Arg.Set_int arg_slow_at_time, "<int> slow at time");
     ("-m", Arg.Set_string arg_monpow_fname, "<file> use that monpow file");
     ("-n", Arg.Set arg_no_lang_dep, " no lang dependent in traces");
     ("-p", Arg.Set arg_stop_at_paradise, " stop at paradise");
     ("-s", Arg.Set_int arg_slow_at_level, "<int> slow at level");
     ("-t", Arg.Set arg_move_trace, " move trace")]
;
value anon_fun s = arg_speed.val := float_of_string s;
value usage_msg = "";

value make str = do {
  let args = Array.of_list ["" :: arg_list_of_string str] in
  Arg.current.val := 0;
  Arg.parse_argv args speclist anon_fun usage_msg;
  if arg_monpow_fname.val <> default_monpow_fname &&
     not (Sys.file_exists arg_monpow_fname.val)
  then failwith (sprintf "File '%s' does not exist" arg_monpow_fname.val)
  else ();
  let na = start_action in
  let arg_slow_at_level =
    if arg_slow_at_level.val = 0 then None else Some arg_slow_at_level.val
  in
  let arg_slow_at_time =
    if arg_slow_at_time.val = 0 then None else Some arg_slow_at_time.val
  in
  {t_prev_pos = None; t_prev_game = None; t_speed = arg_speed.val;
   t_move_trace = arg_move_trace.val; t_monpow_fname = arg_monpow_fname.val;
   t_no_lang_dep = arg_no_lang_dep.val; t_slow_at_level = arg_slow_at_level;
   t_slow_at_time = arg_slow_at_time;
   t_stop_at_paradise = arg_stop_at_paradise.val;
   t_breakpoint = arg_breakpoint.val; t_prev_comm = None;
   t_prev_mov = None; t_on_stairs = False; t_next_action = na}
};

value reinit after_fail arg_t t = do {
  let prev_game =
    map_option
      (fun g ->
        {(g) with
         speed = arg_t.t_speed;
         time = if after_fail then g.time - 1 else g.time})
      t.t_prev_game
  in
  {(t) with
   t_prev_game = prev_game;
   t_speed = arg_t.t_speed;
   t_move_trace = arg_t.t_move_trace;
   t_slow_at_level = arg_t.t_slow_at_level;
   t_slow_at_time = arg_t.t_slow_at_time;
   t_stop_at_paradise = arg_t.t_stop_at_paradise;
   t_breakpoint = arg_t.t_breakpoint}
};
