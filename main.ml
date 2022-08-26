(* $Id: main.ml,v 1.197 2020/06/11 00:07:40 deraugla Exp $ *)

#load "pa_more.cmo";

#use "rogue.def";
#use "keyboard.def";

open Rogue;
open Rfield;
open Dialogue;
open Imisc;
open Misc;
open Object;
open Printf;
open Translate;

value version = "1.05-exp";

value string_make = Bytes.make;
value string_get = Bytes.get;
value string_set = Bytes.set;
value string_copy = Bytes.copy;
value string_of_bytes = Bytes.to_string;
value string_to_bytes = Bytes.of_string;

value init_display g = do {
  let row = g.rogue.row in
  let col = g.rogue.col in
  relight g;
  match get_room_number g row col with
  [ Some rn -> Move.wake_room g rn True row col
  | None -> () ];
  if g.new_level_message <> "" then do {
    message g g.new_level_message False;
    g.new_level_message := ""
  }
  else ()
};

value drop_check g =
  if g.wizard then True
  else if g.dungeon.(g.rogue.row).(g.rogue.col) land STAIRS <> 0 then
    if g.rogue.levitate > 0 then do {
      message g (transl g.lang "You're floating in the air!") False;
      False
    }
    else True
  else do { message g (transl g.lang "I see no way down.") False; False }
;

value check_up g =
  if not g.wizard && g.dungeon.(g.rogue.row).(g.rogue.col) land STAIRS = 0
  then do {
    message g (transl g.lang "I see no way up.") False;
    False
  }
  else if not g.wizard && not (has_amulet g) then do {
    message g (transl g.lang "Your way is magically blocked.") False;
    False
  }
  else if g.cur_level = 1 then Finish.win g
  else do {
    g.new_level_message :=
      transl g.lang "You feel a wrenching sensation in your gut.";
    g.cur_level sub_eq 2;
    True
  }
;

value drop g =
  if g.dungeon.(g.rogue.row).(g.rogue.col) land
     (OBJECT lor STAIRS lor TRAP) <>
       0
  then
    message g (transl g.lang "There's already something there.") False
  else if g.rogue.pack = [] then
    message g (transl g.lang "You have nothing to drop.") False
  else
    let ch = pack_letter g (transl g.lang "Drop what?") (fun _ -> True) in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch False with
      [ None -> ()
      | Some {ob_kind = Weapon {we_is_cursed = True; we_in_use = True}} |
        Some {ob_kind = Armor {ar_is_cursed = True; ar_in_use = True}} |
        Some {ob_kind = Ring {rg_is_cursed = True; rg_in_use = Some _}} ->
          message g (transl g.lang "You can't, it appears to be cursed.")
            False
      | Some obj -> do {
          let obj =
            match obj.ob_kind with
            [ Weapon w -> do {
                if w.we_in_use then unwield g else ();
                take_from_pack g ch;
                obj
              }
            | Armor a -> do {
                if a.ar_in_use then do { Monster.mv_aquators g; unwear g }
                else ();
                print_stats g STAT_ARMOR;
                take_from_pack g ch;
                obj
              }
            | Ring r -> do { un_put_on g r; take_from_pack g ch; obj }
            | _ ->
                if obj.ob_quantity > 1 then do {
                  obj.ob_quantity --;
                  {(obj) with ob_quantity = 1}
                }
                else do { take_from_pack g ch; obj } ]
          in
          Level.place_at g obj g.rogue.row g.rogue.col;
          let msg = transl g.lang "Dropped" ^ " " ^ get_desc g obj False in
          message g (etransl msg ^ ".") False;
          Move.reg_move g
        } ]
;

value show_traps g =
  for i = 0 to DROWS - 1 do {
    for j = 0 to DCOLS - 1 do {
      if g.dungeon.(i).(j) land TRAP <> 0 then Curses.mvaddch i j '^' else ();
    };
  }
;

value get_input_line g prompt insert if_cancelled do_echo = do {
  message g prompt False;
  let n = Ustring.length (Ustring.of_string prompt) in
  let (i, buf) =
    if insert <> "" then do {
      Curses.mvaddstr 0 (n + 1) insert;
      Curses.refresh ();
      let i = Ustring.length (Ustring.of_string insert) in
      (i, string_of_bytes (string_copy (string_to_bytes insert)))
    }
    else (0, "")
  in
  let buf = Ustring.of_string buf in
  let (buf, ch) =
    loop_i buf i where rec loop_i buf i =
      let ch = rgetchar g in
      if ch <> '\r' && ch <> '\n' && ch <> ROGUE_KEY_CANCEL then do {
        let (buf, i) =
          if ch = '\b' || ch = '\127' then do {
            if i > 0 then do {
              Curses.mvaddch 0 (i + n) ' ';
              Curses.move (MIN_ROW - 1) (i + n);
              let buf = Ustring.but_last buf in
              (buf, Ustring.length buf)
            }
            else (buf, i)
          }
          else if ch = CTRL 'u' then do {
            for i = 1 to i do { Curses.mvaddch 0 (n + i) ' ' };
            Curses.move (MIN_ROW - 1) (n + 1);
            (Ustring.of_string "", 0)
          }
          else if i < MAX_TITLE_LENGTH - 2 then
            if ch <> ' ' || i > 0 then do {
	      let buf = Ustring.append_char buf ch in
              if do_echo then Curses.addch ch else Curses.addch '.';
              (buf, Ustring.length buf)
            }
            else (buf, i)
          else (buf, i)
        in
        Curses.refresh ();
        loop_i buf i
      }
      else (buf, ch)
  in
  check_message g;
  let buf =
    loop buf where rec loop buf =
      if Ustring.is_empty buf then buf
      else if Ustring.last_char buf = ' ' then loop (Ustring.but_last buf)
      else buf
  in
  if ch = ROGUE_KEY_CANCEL || Ustring.is_empty buf then do {
    if if_cancelled <> "" then message g if_cancelled False else ();
    ""
  }
  else Ustring.to_string buf
};

value call_it g =
  let ch =
    pack_letter g (transl g.lang "Call what?")
      (fun
       [ Scroll _ | Potion _ | Wand _ | Ring _ -> True
       | _ -> False ])
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch False with
    [ None -> ()
    | Some obj ->
        let id_i =
          match obj.ob_kind with
          [ Scroll s -> Some (g.id_scrolls, int_of_scroll s)
          | Potion p -> Some (g.id_potions, int_of_potion p)
          | Wand w -> Some (g.id_wands, int_of_wand w.wa_kind)
          | Ring r -> Some (g.id_rings, int_of_ring r.rg_kind)
          | _ -> None ]
        in
        match id_i with
        [ Some (id, i) ->
            let s =
              match id.(i) with
              [ Unidentified s ->
                  match obj.ob_kind with
                  [ Potion _ | Ring _ | Wand _ ->
                      etransl ("@(s)" ^ transl g.lang s)
                  | _ -> s ]
              | Called s -> s
              | Identified -> "" ]
            in
            let buf = get_input_line g (transl g.lang "Call it:") s "" True in
            if buf <> "" then id.(i) := Called buf else ()
        | None ->
            message g
              (transl g.lang "Surely you already know what that's called!")
              False ] ]
;

value single_inv g ichar =
  let ch =
    match ichar with
    [ Some ichar -> ichar
    | None ->
        pack_letter g (transl g.lang "Inventory what?") (fun _ -> True) ]
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch False with
    [ None -> ()
    | Some obj ->
        let p =
          match obj with
          [ {ob_kind = Armor {ar_is_protected = True}} -> '}'
          | {ob_kind =
               Weapon {we_has_been_uncursed = True; we_identified = False}} ->
              '|'
          | _ -> ')' ]
        in
        let s = sprintf "%c%c %s" ch p (etransl (get_desc g obj True)) in
        message g s False ]
;

value inv_armor g =
  match g.rogue.armor with
  [ Some (c, _) -> single_inv g (Some c)
  | None -> message g (transl g.lang "Not wearing anything.") False ]
;

value inv_weapon g =
  match g.rogue.weapon with
  [ Some (c, _) -> single_inv g (Some c)
  | None -> message g (transl g.lang "Not wielding anything.") False ]
;

value discovered_kind g title name id tab = do {
  let prompt = transl g.lang " -- Press space to continue --" in
  let list =
    loop_i [] 0 where rec loop_i list i =
      if i = Array.length id then List.rev list
      else
        match id.(i) with
        [ Identified ->
            let s = transl g.lang tab.(i).o_title in
            loop_i [(tab.(i).o_interest, name i, s) :: list] (i + 1)
        | Called s ->
            let s = transl g.lang "called" ^ " " ^ s in
            loop_i [(Neutral, name i, s) :: list] (i + 1)
        | _ -> loop_i list (i + 1) ]
  in
  let list = List.sort compare list in
  let title = " *** " ^ etransl title ^ " ***" in
  let (list, _) =
    List.fold_right
      (fun (interest, name, tit) (list, prev_interest) ->
         let s = sprintf " %s" (etransl (transl g.lang name ^ " " ^ tit)) in
         if list <> [] && interest <> prev_interest then
           ([s; "" :: list], interest)
         else ([s :: list], interest))
      list ([], Harmful)
  in
  let list = [title; "" :: list @ [""; prompt]] in
  let maxlen = List.fold_left max 0 (List.map String.length list) in
  let len = List.length list in
  let col = DCOLS - (maxlen + 2) in
  let saved = Array.make (len + 1) (string_to_bytes "") in
  for i = 0 to len do {
    let a = string_make (maxlen + 2) ' ' in
    saved.(i) := a;
    for j = 0 to maxlen + 1 do { string_set a j (Curses.mvinch i (j + col)) };
  };
  Array.iteri
    (fun i str -> do { Curses.mvaddstr i col str; Curses.clrtoeol () })
    (Array.of_list list);
  Curses.refresh ();
  let ch =
    loop () where rec loop () =
      let ch = rgetchar g in
      if String.contains "!?=/ \027" ch then ch else loop ()
  in
  for i = 0 to len do {
    for j = 0 to maxlen + 1 do {
      Curses.mvaddch i (j + col) (string_get saved.(i) j)
    };
  };
  ch
};

value discovered g =
  let obj_sel = "!?=/" in
  loop_ok () where rec loop_ok () = do {
    message g (transl g.lang "Which object kind" ^ " " ^ obj_sel) False;
    let ch =
      loop () where rec loop () =
        let ch = rgetchar g in
        if String.contains (obj_sel ^ " \027") ch then ch else loop ()
    in
    check_message g;
    if ch = '\027' || ch = ' ' then ()
    else
      let rec loop ch =
        let ch =
          match ch with
          [ '!' ->
              let name _ = "potion" in
              discovered_kind g (transl g.lang "potions") name g.id_potions
                potion_tab
          | '?' ->
              let name _ = "scroll" in
              discovered_kind g (transl g.lang "scrolls") name g.id_scrolls
                scroll_tab
          | '=' ->
              let name _ = "ring" in
              discovered_kind g (transl g.lang "rings") name g.id_rings
                ring_tab
          | '/' ->
              let name i = if g.is_wood.(i) then "staff" else "wand" in
              discovered_kind g (transl g.lang "staffs and wands") name
                g.id_wands wand_tab
          | _ -> ch ]
        in
        if ch = '\027' || ch = ' ' then loop_ok () else loop ch
      in
      loop ch
  }
;

value rest g count = do {
  g.interrupted := False;
  let rec loop i =
    if i < count then
      if g.interrupted then () else do { Move.reg_move g; loop (i + 1) }
    else ()
  in
  loop 0
};

value throw g count = do {
  let dir =
    loop True where rec loop first_miss =
      let ch = rgetchar g in
      if not (is_direction ch) then do {
        sound_bell ();
        if first_miss then message g (transl g.lang "Direction?") False
        else ();
        loop False
      }
      else ch
  in
  check_message g;
  if dir = ROGUE_KEY_CANCEL then ()
  else
    let ch =
      pack_letter g
        (transl g.lang "Throw what?" ^
         (if count > 1 then
            " (" ^ sprintf (ftransl g.lang "%d times") count ^ ")"
          else ""))
        (fun
         [ Weapon _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else do {
      check_message g;
      match get_letter_object g ch False with
      [ None -> ()
      | Some {ob_kind = Armor {ar_is_cursed = True; ar_in_use = True}} |
        Some {ob_kind = Ring {rg_is_cursed = True; rg_in_use = Some _}} |
        Some {ob_kind = Weapon {we_is_cursed = True; we_in_use = True}} ->
          message g (transl g.lang "You can't, it appears to be cursed.")
            False
      | Some {ob_kind = Armor {ar_in_use = True; ar_is_protected = True}} ->
	  (* prevent to throw by mistake protected used armors *)
	  ()
      | Some obj -> do {
          let count = min count obj.ob_quantity in
          g.interrupted := False;
          let rec loop count =
            if count = 0 then ()
            else do {
              let just_once = Attack.one_throw g dir (ch, obj) in
              Move.reg_move g;
              if just_once || g.interrupted then () else loop (count - 1)
            }
          in
          loop count
        } ]
    }
};

value quit g from_intrpt =
  let yes =
    if g.ask_quit then do {
      check_message g;
      message g (transl g.lang "Really quit? (y/n)") True;
      if rgetchar g <> translc g.lang 'y' then do {
        (* ... *)
        check_message g;
        (* ... *)
        False
      }
      else True
    }
    else True
  in
  if yes then do {
    (* ... *)
    check_message g;
    Finish.killed_by g Quit
  }
  else ()
;

value unknown_command g ch =
  message g
    (sprintf "%s '%s'" (transl g.lang "Unknown command")
       (if Char.code ch <= 26 then
          sprintf "ctrl-%c" (Char.chr (Char.code ch + Char.code 'a' - 1))
        else if ch = '\027' then "esc"
        else if ch = '\127' then "del"
        else if ch = '\\' then "\\"
        else Char.escaped ch))
    False
;

value wizardize g =
  if g.wizard then do {
    g.wizard := False;
    message g (transl g.lang "Not wizard anymore.") False
  }
  else
    let buf =
      get_input_line g (transl g.lang "Wizard's password:") "" "" False
    in
    if buf = "password" then do {
      g.wizard := True;
      g.score_only := True;
      message g (transl g.lang "Welcome, mighty wizard!") False
    }
    else message g (transl g.lang "Sorry" ^ ".") False
;

value msg_is g ch s =
  let msg = sprintf (ftransl g.lang "<%c> is %s") ch s in
  message g (etransl msg ^ ".") False
;

value whatisit g = do {
  message g (transl g.lang "What character would you like to know?") False;
  let ch = Curses.getch () in
  check_message g;
  match ch with
  [ 'A'..'Z' ->
      let tch = itgmc g ch in
      let i = Char.code tch - Char.code 'A' in
      let s = transl g.lang (Imonster.visible_mon_name g i) in
      let art = transl g.lang "a@(n?n)" in
      msg_is g ch (art ^ " " ^ s)
  | ROGUE_KEY_CANCEL -> ()
  | '|' | '-' -> msg_is g ch (transl g.lang "the wall")
  | '+' -> msg_is g ch (transl g.lang "a door")
  | '#' -> msg_is g ch (transl g.lang "a tunnel")
  | '.' -> msg_is g ch (transl g.lang "a floor tile")
  | '!' ->
      msg_is g ch (transl g.lang "a@(n?n)" ^ " " ^ transl g.lang "potion")
  | '?' ->
      msg_is g ch (transl g.lang "a@(n?n)" ^ " " ^ transl g.lang "scroll")
  | ')' -> msg_is g ch (transl g.lang "a weapon")
  | ']' -> msg_is g ch (transl g.lang "a suit of armour")
  | '*' -> msg_is g ch (transl g.lang "some gold")
  | ':' -> msg_is g ch (transl g.lang "some food")
  | '/' -> msg_is g ch (transl g.lang "a wand or staff")
  | '=' -> msg_is g ch (transl g.lang "a@(n?n)" ^ " " ^ transl g.lang "ring")
  | ',' -> msg_is g ch (transl g.lang "The Amulet of Yendor")
  | '^' -> msg_is g ch (transl g.lang "a trap")
  | '%' -> msg_is g ch (transl g.lang "stairs")
  | '@' -> msg_is g ch (transl g.lang "you")
  | _ ->
      message g
        (sprintf (ftransl g.lang "I don't know what <%c> is either") ch ^ ".")
        False ]
};

value instructions_file = "rogue.instr";

value conv_instr s =
  let b = Buffer.create 80 in
  loop 0 where rec loop i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then do {
        let c =
          match s.[i+1] with
          [ 'y' -> ROGUE_KEY_NORTHWEST
          | 'k' -> ROGUE_KEY_NORTH
          | 'u' -> ROGUE_KEY_NORTHEAST
          | 'h' -> ROGUE_KEY_WEST
          | 'l' -> ROGUE_KEY_EAST
          | 'b' -> ROGUE_KEY_SOUTHWEST
          | 'j' -> ROGUE_KEY_SOUTH
          | 'n' -> ROGUE_KEY_SOUTHEAST
          | '?' -> ROGUE_KEY_INSTRUCTIONS
          | '.' -> ROGUE_KEY_REST
          | 's' -> ROGUE_KEY_SEARCH
          | 'i' -> ROGUE_KEY_INVENTORY
          | 'f' -> ROGUE_KEY_FIGHT
          | 'F' -> ROGUE_KEY_FIGHT_TO_DEATH
          | 'e' -> ROGUE_KEY_EAT
          | 'q' -> ROGUE_KEY_QUAFF
          | 'r' -> ROGUE_KEY_READ
          | 'm' -> ROGUE_KEY_MOVE
          | 'd' -> ROGUE_KEY_DROP
          | 'P' -> ROGUE_KEY_PUT_ON_RING
          | 'R' -> ROGUE_KEY_REMOVE_RING
          | '>' -> ROGUE_KEY_DROP_CHECK
          | '<' -> ROGUE_KEY_CHECK_UP
          | ')' -> ROGUE_KEY_INV_WEAPON
          | ']' -> ROGUE_KEY_INV_ARMOR
          | '=' -> ROGUE_KEY_INV_RINGS
          | '^' -> ROGUE_KEY_ID_TRAP
          | 'I' -> ROGUE_KEY_SINGLE_INV
          | 'D' -> ROGUE_KEY_DISCOVERED
          | 'T' -> ROGUE_KEY_TAKE_OFF
          | 'W' -> ROGUE_KEY_WEAR
          | 'w' -> ROGUE_KEY_WIELD
          | 'c' -> ROGUE_KEY_CALL
          | 'z' -> ROGUE_KEY_ZAPP
          | 't' -> ROGUE_KEY_THROW
          | ',' -> ROGUE_KEY_PICK_UP
          | '@' -> ROGUE_KEY_CHANGE_LANG
          | 'v' -> ROGUE_KEY_VERSION
          | 'Q' -> ROGUE_KEY_QUIT
          | 'S' -> ROGUE_KEY_SAVE_GAME
          | '/' -> ROGUE_KEY_WHATISIT
          | 'X' -> ROGUE_KEY_SAVE_SCREEN
          | x -> '?' ]
        in
        Buffer.add_char b c;
        Buffer.add_char b ' ';
        loop (i + 2)
      }
      else do { Buffer.add_char b s.[i]; loop (i + 1) }
    else Buffer.contents b
;

value instructions g =
  match try Some (open_in instructions_file) with [ Sys_error _ -> None ] with
  [ Some ic -> do {
      if g.lang <> "" then
        try
          loop () where rec loop () =
            let line = input_line ic in
            try
              let i = String.index line ':' in
              if string_eq g.lang 0 line 0 i then () else raise Not_found
            with
            [ Not_found -> loop () ]
        with
        [ End_of_file -> seek_in ic 0 ]
      else ();
      let buffer = Array.init DROWS (fun _ -> Array.make DCOLS ' ') in
      for row = 0 to DROWS - 1 do {
        for col = 0 to DCOLS - 1 do {
          buffer.(row).(col) := Curses.mvinch row col;
        };
      };
      Curses.clear ();
      try
        loop 0 where rec loop i = do {
          if i < DROWS then do {
            let line = conv_instr (input_line ic) in
            Curses.mvaddstr i 0 line;
            loop (i + 1)
          }
          else ();
        }
      with
      [ End_of_file -> () ];
      close_in ic;
      Curses.refresh ();
      let _ : char = rgetchar g in
      display_dungeon g buffer
    }
  | None -> message g (transl g.lang "Help file not on line.") False ]
;

value change_lang g =
  let q = transl g.lang "Language:" in
  let q = if g.lang = "" then q else q ^ " (" ^ g.lang ^ ")" in
  let new_lang = get_input_line g q "" "" True in
  if new_lang <> "" && new_lang <> g.lang then do {
    g.lang := new_lang;
    clear_lexicon g.lang;
    print_stats g STAT_ALL;
    List.iter
      (fun mon ->
         if mon.mn_flags land IMITATES <> 0 then ()
         else if
           g.rogue.blind = 0 && g.rogue.detect_monster ||
           rogue_can_see g mon.mn_row mon.mn_col
         then
           show_monster g mon.mn_row mon.mn_col mon (tgmc g mon.mn_char)
         else ())
      g.level_monsters
  }
  else ()
;

value save_game g =
  let fname =
    get_input_line g (transl g.lang "File name?") g.save_file
      (transl g.lang "Game not saved.") True
  in
  if fname <> "" then do {
    check_message g;
    message g fname False;
    save_into_file g fname;
    Finish.clean_up ""
  }
  else ()
;

value show_average_hp g =
  let rogue = g.rogue in
  let real_average =
    if g.rogue.exp = 1 then 0
    else
      let den = rogue.hp_max - rogue.extra_hp - INIT_HP + rogue.less_hp in
      let num = rogue.exp - 1 in
      (2 * den + num) / (2 * num)
  in
  let effective_average =
    if rogue.exp = 1 then 0
    else
      let den = rogue.hp_max - INIT_HP in
      let num = rogue.exp - 1 in
      (2 * den + num) / (2 * num)
  in
  let mbuf =
    sprintf (ftransl g.lang "R-Hp: %.2d, E-Hp: %.2d (!: %d, V: %d)")
      real_average effective_average rogue.extra_hp rogue.less_hp
  in
  message g mbuf False
;

value backup_if_required g =
  match f_backup.Efield.get g.env "backup" None with
  [ Some (fname, time) -> do {
      if time mod 1000 = 0 then do {
        let fname = sprintf "%s.%d" fname (time / 1000 mod 5) in
        Misc.save_into_file g fname
      }
      else ();
      f_backup.Efield.set g.env "backup" (Some (fname, time + 1))
    }
  | None -> () ]
;

value rec play_level g = do {
  g.interrupted := False;
  if g.hit_message <> "" then do {
    message g g.hit_message True;
    g.hit_message := ""
  }
  else ();
  if g.trap_door then g.trap_door := False
  else do {
    Curses.move g.rogue.row g.rogue.col;
    Curses.refresh ();
    backup_if_required g;
    let ch = rgetchar g in
    check_message g;
    let (count, ch) =
      loop 0 ch where rec loop count ch =
        match ch with
        [ '0'..'9' -> do {
            Curses.move g.rogue.row g.rogue.col;
            Curses.refresh ();
            let count =
              if count < 1000 then 10 * count + Char.code ch - Char.code '0'
              else count
            in
            let ch = rgetchar g in
            loop (if ch = ROGUE_KEY_CANCEL then 0 else count) ch
          }
        | ch -> (count, ch) ]
    in
    let cont =
      match ch with
      [ ROGUE_KEY_DROP_CHECK -> not (drop_check g)
      | ROGUE_KEY_CHECK_UP -> not (check_up g)
      | _ -> do {
          match ch with
          [ ROGUE_KEY_INSTRUCTIONS -> instructions g
          | ROGUE_KEY_REST -> rest g (max 1 count)
          | ROGUE_KEY_SEARCH -> Move.search g (max 1 count) False
          | ROGUE_KEY_INVENTORY -> inventory g g.rogue.pack (fun _ -> True)
          | ROGUE_KEY_FIGHT -> Move.fight g False
          | ROGUE_KEY_FIGHT_TO_DEATH -> Move.fight g True
          | ROGUE_KEY_NORTH | ROGUE_KEY_SOUTH | ROGUE_KEY_EAST |
            ROGUE_KEY_WEST | ROGUE_KEY_NORTHEAST | ROGUE_KEY_NORTHWEST |
            ROGUE_KEY_SOUTHEAST | ROGUE_KEY_SOUTHWEST ->
              Move.one_move_rogue g ch True
          | ROGUE_KEY_WEST_SHIFT | ROGUE_KEY_SOUTH_SHIFT |
            ROGUE_KEY_NORTH_SHIFT | ROGUE_KEY_EAST_SHIFT |
            ROGUE_KEY_NORTHWEST_SHIFT | ROGUE_KEY_NORTHEAST_SHIFT |
            ROGUE_KEY_SOUTHEAST_SHIFT | ROGUE_KEY_SOUTHWEST_SHIFT |
            ROGUE_KEY_WEST_CTRL | ROGUE_KEY_SOUTH_CTRL |
            ROGUE_KEY_NORTH_CTRL | ROGUE_KEY_EAST_CTRL |
            ROGUE_KEY_NORTHWEST_CTRL | ROGUE_KEY_NORTHEAST_CTRL |
            ROGUE_KEY_SOUTHEAST_CTRL | ROGUE_KEY_SOUTHWEST_CTRL ->
              Move.multiple_move_rogue g ch
          | ROGUE_KEY_EAT -> Use.eat g
          | ROGUE_KEY_QUAFF -> Use.quaff g
          | ROGUE_KEY_READ -> Use.read_scroll g
          | ROGUE_KEY_MOVE -> Move.move_onto g
          | ROGUE_KEY_DROP -> drop g
          | ROGUE_KEY_PUT_ON_RING -> Use.put_on_ring g
          | ROGUE_KEY_REMOVE_RING -> Use.remove_ring g
          | ROGUE_KEY_REMESSAGE -> remessage g
          | ROGUE_KEY_WIZARDIZE -> wizardize g
          | ROGUE_KEY_INV_ARMOR -> inv_armor g
          | ROGUE_KEY_INV_WEAPON -> inv_weapon g
          | ROGUE_KEY_INV_RINGS -> Use.inv_rings g
          | ROGUE_KEY_ID_TRAP -> Move.id_trap g
          | ROGUE_KEY_SINGLE_INV -> single_inv g None
          | ROGUE_KEY_DISCOVERED -> discovered g
          | ROGUE_KEY_CHANGE_LANG -> change_lang g
          | ROGUE_KEY_TAKE_OFF -> Use.take_off g
          | ROGUE_KEY_WEAR -> Use.wear g
          | ROGUE_KEY_WIELD -> Use.wield g
          | ROGUE_KEY_CALL -> call_it g
          | ROGUE_KEY_ZAPP -> if Attack.zap g then Move.reg_move g else ()
          | ROGUE_KEY_THROW -> throw g (max 1 count)
          | ROGUE_KEY_VERSION ->
              message g
                (sprintf "mlrogue %s (%s %s)" version g.nick_name
                   g.login_name)
                False
          | ROGUE_KEY_QUIT -> quit g False
          | ROGUE_KEY_NOP | ROGUE_KEY_CANCEL -> ()
          | ROGUE_KEY_WIZ_INVENTORY ->
              if g.wizard then
                inventory g (List.map (fun obj -> ('.', obj)) g.level_objects)
                  (fun _ -> True)
              else unknown_command g ch
          | ROGUE_KEY_WIZ_MAGIC_MAP ->
              if g.wizard then Use.draw_magic_map g True
              else unknown_command g ch
          | ROGUE_KEY_WIZ_SHOW_TRAPS ->
              if g.wizard then show_traps g else unknown_command g ch
          | ROGUE_KEY_WIZ_SHOW_OBJS ->
              if g.wizard then Use.show_objects g else unknown_command g ch
          | ROGUE_KEY_SHOW_AV_HP -> show_average_hp g
          | ROGUE_KEY_WIZ_NEW_OBJ ->
              if g.wizard then new_object_for_wizard g
              else unknown_command g ch
          | ROGUE_KEY_WIZ_SHOW_MONST ->
              if g.wizard then show_monsters g else unknown_command g ch
          | ROGUE_KEY_SAVE_GAME -> save_game g
          | ROGUE_KEY_PICK_UP -> Move.kick_into_pack g
          | ROGUE_KEY_WHATISIT -> whatisit g
          | _ -> unknown_command g ch ];
          True
        } ]
    in
    if cont then play_level g else ()
  }
};

value rec game_loop g = do {
  print_stats g STAT_ALL;
  play_level g;
  Curses.clear ();
  Level.create g;
  init_display g;
  game_loop g
};

type alternative 'a 'b =
  [ Left of 'a
  | Right of 'b ]
;

value main () = do {
  let (lang, init, rob_opt, backup_opt, fast, batch, no_record_score) =
    Init.f Sys.argv
  in
  let (player_spec, nhr) =
    match rob_opt with
    [ Some (ps, ehr) -> (ps, ehr)
    | None -> (PShuman, False) ]
  in
  if batch then Curses.no_output () else ();
  Curses.initscr ();
  if Curses.lines () < DROWS || Curses.cols () < DCOLS then
    Finish.clean_up
      (sprintf (ftransl lang "Must be played on a %d x %d or better screen")
         DROWS DCOLS)
  else ();
  match init with
  [ Init.NewGame g -> do {
      f_player_species.Efield.set g.env "player_species" player_spec;
      f_backup.Efield.set g.env "backup" backup_opt;
      f_bool.Efield.set g.env "no handle robot" nhr;
      f_bool.Efield.set g.env "fast" fast;
      f_bool.Efield.set g.env "batch" batch;
      Level.create g;
      init_display g;
      if no_record_score then g.score_only := True else ();
      game_loop g
    }
  | Init.RestoreGame rest_file ->
      match try Left (Misc.restore rest_file) with exc -> Right exc with
      [ Left g -> do {
          if no_record_score then g.score_only := True else ();
          if g.score_only then
            match f_random.Efield.get g.env "random" None with
            [ Some r -> Random.set_state r
            | None -> () ]
          else ();
          let player_spec =
            match player_spec with
            [ PSrobot arg_rob ->
                let rob =
                  match
                    f_player_species.Efield.get g.env "player_species" PShuman
                  with
                  [ PSrobot rob -> do {
                      let after_fail =
                        f_bool.Efield.get g.env "failed" False
                      in
                      Robot.reinit after_fail arg_rob rob
                    }
                  | PSsocket _ | PShuman -> arg_rob ]
                in
                PSrobot rob
            | PSsocket _ | PShuman -> player_spec ]
          in
          f_player_species.Efield.set g.env "player_species" player_spec;
          match backup_opt with
          [ Some (arg_back, _) ->
              let time =
                match f_backup.Efield.get g.env "backup" None with
                [ Some (_, time) -> time
                | None -> 0 ]
              in
              f_backup.Efield.set g.env "backup" (Some (arg_back, time))
          | None -> f_backup.Efield.set g.env "backup" None ];
          f_bool.Efield.set g.env "fast" fast;
          f_bool.Efield.set g.env "no handle robot" nhr;
          f_bool.Efield.set g.env "batch" batch;
          f_bool.Efield.set g.env "break" False;
          f_bool.Efield.set g.env "failed" False;
          g.msg_cleared := False;
          ring_stats g;
          game_loop g
        }
      | Right (Sys_error _ | Failure _) ->
          Finish.clean_up
            (sprintf "%s: %s" rest_file (transl lang "cannot open file"))
      | Right e -> raise e ]
  | Init.ScoreOnly -> do {
      Finish.put_scores lang True None;
      Finish.clean_up ""
    } ]
};

try main () with e -> do { Curses.endwin (); raise e };
