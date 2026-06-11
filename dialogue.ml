(* $Id: dialogue.ml,v 1.61 2018/04/26 09:52:36 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

(* #use "rogue.def" *)


(* #use "keyboard.def" *)


#open "rogue";;
#open "rfield";;
#open "object";;
#open "printf";;
#open "translate";;

let string_make = string__make_string;;
let string_create = string__create_string;;
let string_length = string__string_length;;
let string_get = string__nth_char;;
let string_set = string__set_nth_char;;
let string_of_bytes (s : string) = s;;

let armor_desc g a =
  let i = int_of_armor a.ar_kind in
  let t = transl g.lang armor_tab.(i).o_title in
  let t =
    if a.ar_identified || g.wizard then
      sprintf "%s%d %s [%d]" (if a.ar_enchant >= 0 then "+" else "")
        a.ar_enchant t (imisc__get_armor_class (Some (` `, a)))
    else t
  in
  if a.ar_in_use then sprintf "%s %s" t (transl g.lang "being worn") else t
;;

let name_of g obj =
  match obj.ob_kind with
    Scroll _ ->
      if obj.ob_quantity > 1 then transl g.lang "scrolls"
      else transl g.lang "scroll"
  | Potion _ ->
      if obj.ob_quantity > 1 then transl g.lang "potions"
      else transl g.lang "potion"
  | Food Ration -> transl g.lang "food"
  | Food Fruit ->
      if g.fruit <> default_fruit then g.fruit else transl g.lang g.fruit
  | Wand w ->
      let i = int_of_wand w.wa_kind in
      if g.is_wood.(i) then transl g.lang "staff" else transl g.lang "wand"
  | Weapon w ->
      let i = int_of_weapon w.we_kind in
      let t = weapon_tab.(i).o_title in
      transl g.lang (imisc__nth_field t (if obj.ob_quantity = 1 then 0 else 1))
  | Armor _ -> transl g.lang "armor"
  | Ring _ -> transl g.lang "ring"
  | Amulet -> transl g.lang "amulet"
  | _ -> "not impl, name_of"
;;

let ring_desc g r cap =
  let i = int_of_ring r.rg_kind in
  let name = transl g.lang "ring" in
  sprintf "%s %s%s"
    (if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)")
    (match g.id_rings.(i), g.wizard with
       Unidentified s, false -> transl g.lang s ^ " " ^ name
     | Called s, false -> name ^ " " ^ transl g.lang "called" ^ " " ^ s
     | Identified, false | _, true ->
         sprintf "%s%s %s"
           (match r.rg_identified || g.wizard, r.rg_kind with
              true, (Dexterity | AddStrength) ->
                sprintf "%s%d " (if r.rg_class > 0 then "+" else "")
                  r.rg_class
            | _ -> "")
           name (transl g.lang ring_tab.(i).o_title))
    (match r.rg_in_use with
       Some LeftHand -> " " ^ transl g.lang "on left hand"
     | Some RightHand -> " " ^ transl g.lang "on right hand"
     | None -> "")
;;

let weapon_desc g obj w cap =
  let i = int_of_weapon w.we_kind in
  let id =
    if w.we_identified || g.wizard then
      sprintf "%s%d,%s%d " (if w.we_hit_enchant >= 0 then "+" else "")
        w.we_hit_enchant (if w.we_d_enchant >= 0 then "+" else "")
        w.we_d_enchant
    else ""
  in
  let t = weapon_tab.(i).o_title in
  let t =
    if obj.ob_quantity = 1 then
      sprintf "%s %s%s"
        (if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)") id
        (transl g.lang (imisc__nth_field t 0))
    else
      sprintf "%d %s%s" obj.ob_quantity id
        (transl g.lang (imisc__nth_field t 1))
  in
  if w.we_in_use then t ^ " " ^ transl g.lang "in hand" else t
;;

let get_desc g obj cap =
  match obj.ob_kind with
    Amulet -> transl g.lang "The amulet of Yendor"
  | Gold -> sprintf (ftransl g.lang "%d pieces of gold.") obj.ob_quantity
  | Armor a -> armor_desc g a
  | Food Fruit ->
      let art =
        if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)"
      in
      art ^ " " ^ name_of g obj
  | Food Ration ->
      let s =
        if obj.ob_quantity > 1 then
          sprintf (ftransl g.lang "%d rations of") obj.ob_quantity
        else if cap then transl g.lang "Some"
        else transl g.lang "some"
      in
      s ^ " " ^ transl g.lang "food"
  | Potion p ->
      let i = int_of_potion p in
      let name =
        if obj.ob_quantity = 1 then transl g.lang "potion"
        else transl g.lang "potions"
      in
      (if obj.ob_quantity = 1 then
         if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)"
       else string_of_int obj.ob_quantity) ^
      " " ^
      (match g.id_potions.(i), g.wizard with
         Unidentified s, false -> transl g.lang s ^ " " ^ name
       | Called s, false -> name ^ " " ^ transl g.lang "called" ^ " " ^ s
       | Identified, false | _, true ->
           name ^ " " ^ transl g.lang potion_tab.(i).o_title)
  | Ring r -> ring_desc g r cap
  | Scroll s ->
      let i = int_of_scroll s in
      let name =
        if obj.ob_quantity = 1 then transl g.lang "scroll"
        else transl g.lang "scrolls"
      in
      (if obj.ob_quantity = 1 then
         if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)"
       else string_of_int obj.ob_quantity) ^
      " " ^ name ^ " " ^
      (match g.id_scrolls.(i), g.wizard with
         Unidentified s, false -> transl g.lang "entitled:" ^ " " ^ s
       | Called s, false -> transl g.lang "called" ^ " " ^ s
       | Identified, false | _, true ->
           sprintf "%s" (transl g.lang scroll_tab.(i).o_title))
  | Wand w ->
      let i = int_of_wand w.wa_kind in
      let name =
        if g.is_wood.(i) then transl g.lang "staff" else transl g.lang "wand"
      in
      sprintf "%s %s"
        (if cap then transl g.lang "A@(n?n)" else transl g.lang "a@(n?n)")
        (match g.id_wands.(i), g.wizard with
           Unidentified s, false -> transl g.lang s ^ " " ^ name
         | Called s, false -> name ^ " " ^ transl g.lang "called" ^ " " ^ s
         | Identified, false | _, true ->
             sprintf "%s %s%s" name (transl g.lang wand_tab.(i).o_title)
               (if w.wa_identified || g.wizard then sprintf " [%d]" w.wa_hits
                else ""))
  | Weapon w -> weapon_desc g obj w cap
;;

let save_screen () =
  match try Some (open_out "rogue.screen") with sys__Sys_error _ -> None with
    Some oc ->
      for i = 0 to 24 - 1 do
        let spaces = ref 0 in
        for j = 0 to 80 - 1 do
          match curses__mvinch i j with
            ` ` -> incr spaces
          | ch ->
              for i = 1 to !spaces do output_char oc ` ` done;
              output_char oc ch;
              spaces := 0
        done;
        output_char oc `\n`
      done;
      close_out oc
  | None -> ()
;;

let rec rgetchar_stdin =
  function
    `\018` -> curses__wrefresh_curscr (); rgetchar_stdin (curses__getch ())
  | `X` -> save_screen (); rgetchar_stdin (curses__getch ())
  | ch -> ch
;;
let rgetchar_human () = rgetchar_stdin (curses__getch ());;

let dungeon_string g =
  vect__init_vect (vect__vect_length g.dungeon)
    (fun i ->
       let line = string_create (vect__vect_length g.dungeon.(i)) in
       for j = 0 to string_length line - 1 do
         string_set line j (curses__mvinch i j)
       done;
       line)
;;

let display_pause line =
  let txt = " (pause)" in
  let something =
    let rec loop len i =
      if len = 0 then false
      else if line.[i] = ` ` then loop (len - 1) (i - 1)
      else true
    in
    loop (string__string_length txt) 79
  in
  if something then ()
  else
    let (row, col) = curses__pos_get () in
    curses__move 0 (79 - string__string_length txt);
    curses__refresh ();
    printf "%s" txt;
    flush stdout;
    flush stderr;
    curses__move row col;
    curses__refresh ()
;;

let pause_char = `\003`;;

(*
let rgetchar_local_robot g rob =
  let (row, col) = curses__pos_get () in
  let dung = dungeon_string g in
  curses__move row col;
  let char_on_input =
    if f_bool.efield__get g.env "break" false then
      begin
        display_pause (string_of_bytes dung.(0));
        let ch = curses__getch () in
        if ch = pause_char then
          begin
            curses__wrefresh_curscr ();
            f_bool.efield__set g.env "break" false;
            Some `\027`
          end
        else Some ch
      end
    else
      let (fd_in, _, _) = unix__select [unix__stdin] [] [] 0.0 in
      if list__list_length fd_in <> 0 then
        let ch = curses__getch () in
        if ch = pause_char then
          begin
            display_pause (string_of_bytes dung.(0));
            let ch = curses__getch () in
            if ch = pause_char then begin curses__wrefresh_curscr (); None end
            else begin f_bool.efield__set g.env "break" true; Some ch end
          end
        else None
      else None
  in
  match char_on_input with
    Some ch -> rgetchar_stdin ch
  | None ->
      let nrow = vect__vect_length dung in
      let ncol = string_length dung.(0) in
      match
        let sdung = vect__init_vect nrow (fun i -> string_of_bytes dung.(i)) in
        try Some (robot__play sdung nrow ncol rob) with
          exc ->
            if f_bool.efield__get g.env "no handle robot" false then raise exc
            else
              begin
                curses__home ();
                printf "\n";
                printf "%s" (Printexc.to_string exc);
                curses__home ();
                flush stdout;
                flush stderr;
                None
              end
      with
        Some (ch, rob) ->
          f_player_species.efield__set g.env "player_species" (PSrobot rob); ch
      | None ->
          f_bool.efield__set g.env "failed" true;
          f_player_species.efield__set g.env "player_species" (PSrobot rob);
          rgetchar_human ()
;;
*)

let rgetchar g =
  if f_bool.efield__get g.env "failed" false then rgetchar_human ()
  else
    match f_player_species.efield__get g.env "player_species" PShuman with
      PSsocket s -> rogbotio__getchar 24 80 s
(*
    | PSrobot rob -> rgetchar_local_robot g rob
*)
    | PShuman -> rgetchar_human ()
;;

(* message *)

let sound_bell () = print_char `\007`; flush stdout;;

let wait_for_ack g = while rgetchar g <> ` ` do () done;;

let check_message g =
  if g.msg_cleared then ()
  else
    begin
      curses__move (1 - 1) 0;
      curses__clrtoeol ();
      curses__refresh ();
      g.msg_cleared <- true
    end
;;

let message g msg intrpt =
  if intrpt then g.interrupted <- true;
  g.can_int <- true;
  if not g.msg_cleared then
    begin
      curses__mvaddstr (1 - 1) g.msg_col (transl g.lang " -- More --");
      curses__refresh ();
      wait_for_ack g;
      check_message g;
      if msg <> "" && msg = g.msg_line then g.same_msg <- g.same_msg + 1
      else g.same_msg <- 0
    end
  else g.same_msg <- 0;
  g.msg_line <- msg;
  curses__mvaddstr (1 - 1) 0 msg;
  if g.same_msg > 0 then
    let buf = sprintf " (%d)" (g.same_msg + 1) in
    curses__addstr buf; g.msg_col <- string__string_length buf
  else g.msg_col <- 0;
  curses__addch ` `;
  curses__refresh ();
  g.msg_cleared <- false;
  g.msg_col <- g.msg_col + string__string_length msg;
  g.can_int <- false
;;

let remessage g = if g.msg_line <> "" then message g g.msg_line false;;

let inv_sel g pack mask prompt term =
  if pack = [] then
    begin message g (transl g.lang "Your pack is empty.") true; None end
  else
    let list = misc__list_filter (fun (_, obj) -> mask obj.ob_kind) pack in
    let list = sort__sort (fun x y -> x <= y) list in
    let (list, maxlen) =
      list__list_it
        (fun (c, obj) (list, maxlen) ->
           let p =
             match obj with
               {ob_kind = Armor {ar_is_protected = true}} -> `}`
             | {ob_kind = Weapon w} ->
                 if w.we_has_been_uncursed &&
                    (not w.we_identified || w.we_hit_enchant < 0 ||
                     w.we_d_enchant < 0)
                 then
                   `|`
                 else `)`
             | _ -> `)`
           in
           let s = sprintf " %c%c %s" c p (etransl (get_desc g obj true)) in
           let s = ustring__of_string s in
           s :: list, max maxlen (ustring__length s))
        list ([], 0)
    in
    let len = list__list_length list in
    let maxlen = max maxlen (string__string_length prompt) in
    let col = 80 - (maxlen + 2) in
    let saved =
      vect__init_vect (len + 1) (fun _ -> string_make (maxlen + 2) ` `)
    in
    for i = 0 to len do
      for j = 0 to maxlen + 1 do
        string_set saved.(i) j (curses__mvinch i (j + col))
      done
    done;
    let saved_col =
      vect__init_vect (len + 1)
        (fun _ -> vect__init_vect (maxlen + 2) (fun _ -> -1, -1))
    in
    for i = 0 to len do
      for j = 0 to maxlen + 1 do
        saved_col.(i).(j) <- curses__color_get i (j + col)
      done
    done;
    let rec loop row =
      function
        s :: sl ->
          if row > 0 then ();
          curses__mvaddstr row col (ustring__to_string s);
          curses__clrtoeol ();
          loop (row + 1) sl
      | [] -> ()
    in
    loop 0 list;
    curses__mvaddstr len col prompt;
    curses__clrtoeol ();
    curses__refresh ();
    let retc =
      let rec loop () =
        let retc = rgetchar g in
        try let _ = string__index_char term retc in retc with Not_found -> loop ()
      in
      loop ()
    in
    for i = 0 to len do
      for j = 0 to maxlen + 1 do
        let (fg, bg) = saved_col.(i).(j) in
        curses__color_set fg bg;
        curses__mvaddch i (j + col) (string_get saved.(i) j)
      done
    done;
    curses__color_set (-1) (-1);
    Some retc
;;

let inventory g pack mask =
  let (_ : char option) =
    inv_sel g pack mask (transl g.lang " -- Press space to continue --")
      " \027"
  in
  ()
;;

(* statistics *)

let rec scanbrd brd i n =
  let pp1 = i in
  let pp2 = try string__index_char_from brd i `:` with Not_found -> -1 in
  if pp2 >= 0 then
    let pp2 = pp2 + 2 in
    if n > 0 then
      let pp2 = try string__index_char_from brd pp2 ` ` with Not_found -> -1 in
      if pp2 >= 0 then scanbrd brd (pp2 + 1) (n - 1) else None
    else Some (pp1, pp2)
  else None
;;

let pad s n = for i = string__string_length s to n - 1 do curses__addch ` ` done;;

let print_stats g stat_mask =
  let brd =
    transl g.lang
      "Level: 99 Gold: 999999 Hp: 999(999) Str: 99(99) Arm: 99 Exp: 21/10000000"
  in
  let row = 24 - 1 in
  if stat_mask = 0o377 then
    begin curses__mvaddstr row 0 ""; curses__clrtoeol () end;
  let label = stat_mask land 0o200 <> 0 in
  let pr mask1 start1 b1 pad1 =
    if stat_mask land mask1 <> 0 then
      match scanbrd brd 0 start1 with
        Some (p1, p2) ->
          if label then curses__mvaddnstr row p1 brd p1 (p2 - p1);
          curses__mvaddstr row p2 b1;
          pad b1 pad1
      | None -> ()
  in
  pr 0x1 0 (string_of_int g.cur_level) 2;
  pr 0o2 1 (string_of_int g.rogue.gold) 6;
  pr 0o4 2 (sprintf "%d(%d)" g.rogue.hp_current g.rogue.hp_max) 8;
  pr 0o10 3
    (sprintf "%d(%d)" (g.rogue.str_current + g.rogue.add_strength)
       g.rogue.str_max)
    6;
  pr 0o20 4 (string_of_int (imisc__get_armor_class g.rogue.armor)) 2;
  pr 0o40 5 (sprintf "%d/%d" g.rogue.exp g.rogue.exp_points) 11;
  if stat_mask land 0o100 <> 0 then
    begin
      curses__mvaddstr row 73
        (if g.hunger_str <> "" then transl g.lang g.hunger_str else "");
      curses__clrtoeol ()
    end;
  curses__refresh ()
;;

(* pack select *)

let mask_pack pack mask = list__exists (fun (_, x) -> mask x.ob_kind) pack;;

let in_use =
  function
    Armor a -> a.ar_in_use
  | Ring r -> r.rg_in_use <> None
  | Weapon w -> w.we_in_use
  | _ -> false
;;

let is_pack_letter g ch mask =
  match ch with
    `a`..`z` | `\027` | `*` -> Some (ch, mask)
  | `?` ->
      Some
        (`*`,
         (function
            Scroll _ -> true
          | _ -> false))
  | `!` ->
      Some
        (`*`,
         (function
            Potion _ -> true
          | _ -> false))
  | `:` ->
      Some
        (`*`,
         (function
            Food _ -> true
          | _ -> false))
  | `)` ->
      Some
        (`*`,
         (function
            Weapon _ -> true
          | _ -> false))
  | `]` ->
      Some
        (`*`,
         (function
            Armor _ -> true
          | _ -> false))
  | `/` ->
      Some
        (`*`,
         (function
            Wand _ -> true
          | _ -> false))
  | `=` ->
      Some
        (`*`,
         (function
            Ring _ -> true
          | _ -> false))
  | `,` ->
      Some
        (`*`,
         (function
            Amulet -> true
          | _ -> false))
  | `.` -> Some (`*`, in_use)
  | _ -> None
;;

let pack_letter g prompt mask =
  let tmask = mask in
  if not (mask_pack g.rogue.pack mask) then
    begin message g (transl g.lang "Nothing appropriate.") false; `\027` end
  else
    let ch =
      let rec loop () =
        message g prompt false;
        let (ch, mask) =
          let rec loop1 () =
            let ch = rgetchar g in
            match is_pack_letter g ch mask with
              None -> sound_bell (); loop1 ()
            | Some ch_mask -> ch_mask
          in
          loop1 ()
        in
        if ch = `*` then
          let ch =
            let rec loop mask =
              check_message g;
              let cho =
                inv_sel g g.rogue.pack mask
                  (transl g.lang " -- Press space or letter --")
                  " !)]*=?:.abcdefghijklmnopqrstuvwxyz\027"
              in
              match cho with
                Some ch ->
                  begin match ch with
                    `\027` | ` ` | `a`..`z` -> ch
                  | _ ->
                      match is_pack_letter g ch tmask with
                        Some (_, mask) -> loop mask
                      | None -> ch
                  end
              | None -> loop tmask
            in
            loop mask
          in
          match ch with
            `a`..`z` -> ch
          | _ -> loop ()
        else ch
      in
      loop ()
    in
    check_message g; ch
;;

(* wizard *)

let wizard_sel create list =
  let (_, list) =
    list__it_list
      (fun (ch, list) t ->
         let list = (ch, create (Some t)) :: list in
         char__char_of_int (char__int_of_char ch + 1), list)
      (`a`, []) list
  in
  list
;;

let new_object_for_wizard g =
  if imisc__pack_count g None >= 24 then
    message g (transl g.lang "Pack full.") false
  else
    let obj_sel = "!?:)]=/," in
    let rec loop () =
      message g (transl g.lang "Which object kind" ^ " " ^ obj_sel) false;
      let ch =
        let rec loop () =
          let ch = rgetchar g in
          try let (_ : int) = string__index_char (obj_sel ^ " \027") ch in ch with
            Not_found -> loop ()
        in
        loop ()
      in
      check_message g;
      let rec lloop ch =
        let obj_list =
          match ch with
            `!` ->
              wizard_sel gr_potion
                [IncreaseStrength; RestoreStrength; Healing; ExtraHealing;
                 Poison; RaiseLevel; Blindness; Hallucination; DetectMonsters;
                 DetectObjects; Confusion; Levitation; HasteSelf;
                 SeeInvisible]
          | `?` ->
              wizard_sel gr_scroll
                [ProtectArmor; HoldMonster; EnchantWeapon; EnchantArmor;
                 Identify; Teleport; Sleep; ScareMonster; RemoveCurse;
                 CreateMonster; AggravateMonster; MagicMapping]
          | `:` -> wizard_sel get_food [Ration; Fruit]
          | `)` ->
              wizard_sel gr_weapon
                [Bow; Dart; Arrow; Dagger; Shuriken; Mace; LongSword;
                 TwoHandedSword]
          | `]` ->
              wizard_sel gr_armor
                [Leather; Ringmail; Scale; Chain; Banded; Splint; Plate]
          | `=` ->
              wizard_sel gr_ring
                [Stealth; RTeleport; Regeneration; SlowDigest; AddStrength;
                 SustainStrength; Dexterity; Adornment; RSeeInvisible;
                 MaintainArmor; Searching]
          | `/` ->
              wizard_sel gr_wand
                [TeleportAway; SlowMonster; ConfuseMonster; Invisibility;
                 Polymorph; HasteMonster; PutToSleep; MagicMissile;
                 Cancellation; DoNothing]
          | `,` -> wizard_sel get_amulet [()]
          | ` ` | `\027` -> []
          | _ -> []
        in
        if obj_list = [] then ()
        else
          let sel =
            string__concat ""
              (list__map (string__make_string 1) (list__map fst obj_list))
          in
          let sel = sel ^ obj_sel ^ " \027" in
          let retc =
            inv_sel g obj_list (fun _ -> true)
              (" " ^ transl g.lang "Choose:" ^ " ") sel
          in
          match retc with
            None | Some (` ` | `\027`) -> loop ()
          | Some ch ->
              try
                let obj = list__assoc ch obj_list in
                let (c, obj) = imisc__add_to_pack g obj in
                let desc = etransl (get_desc g obj true) in
                message g (sprintf "%s (%c)" desc c) false
              with Not_found -> lloop ch
      in
      lloop ch
    in
    loop ()
;;
