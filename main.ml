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

value init_display gg = do {
  let g = gg.game_v in
  let row = g.rogue.row in
  let col = g.rogue.col in
  relight gg;
  match get_room_number gg row col with
  [ Some rn -> Move.wake_room gg rn True row col
  | None -> () ];
  if g.new_level_message <> "" then do {
    message gg (fun _ → g.new_level_message) False;
    g.new_level_message := ""
  }
  else ()
};

value drop_check gg =
  let g = gg.game_v in
  if g.wizard then True
  else if g.dungeon.(g.rogue.row).(g.rogue.col) land STAIRS <> 0 then
    if g.rogue.levitate > 0 then do {
      message gg (fun lang → transl lang "You're floating in the air!") False;
      False
    }
    else True
  else do {
    message gg (fun lang → transl lang "I see no way down.") False;
    False
  }
;

value check_up gg =
  let g = gg.game_v in
  if not g.wizard && g.dungeon.(g.rogue.row).(g.rogue.col) land STAIRS = 0
  then do {
    message gg (fun lang → transl lang "I see no way up.") False;
    False
  }
  else if not g.wizard && not (has_amulet gg) then do {
    message gg (fun lang → transl lang "Your way is magically blocked.") False;
    False
  }
  else if g.cur_level = 1 then Finish.win gg
  else do {
    g.new_level_message :=
      transl g.lang "You feel a wrenching sensation in your gut.";
    g.cur_level sub_eq 2;
    True
  }
;

value drop gg =
  let g = gg.game_v in
  if g.dungeon.(g.rogue.row).(g.rogue.col) land
     (OBJECT lor STAIRS lor TRAP) <>
       0
  then
    message gg
      (fun lang → transl lang "There's already something there.") False
  else if g.rogue.pack = [] then
    message gg (fun lang → transl lang "You have nothing to drop.") False
  else
    let ch =
      pack_letter gg (fun lang → transl lang "Drop what?") (fun _ -> True)
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object gg ch False with
      [ None -> ()
      | Some {ob_kind = Weapon {we_is_cursed = True; we_in_use = True}} |
        Some {ob_kind = Armor {ar_is_cursed = True; ar_in_use = True}} |
        Some {ob_kind = Ring {rg_is_cursed = True; rg_in_use = Some _}} ->
          message gg
            (fun lang → transl lang "You can't, it appears to be cursed.")
            False
      | Some obj -> do {
          let obj =
            match obj.ob_kind with
            [ Weapon w -> do {
                if w.we_in_use then unwield gg else ();
                take_from_pack gg ch;
                obj
              }
            | Armor a -> do {
                if a.ar_in_use then do { Monster.mv_aquators gg; unwear gg }
                else ();
                print_stats gg STAT_ARMOR;
                take_from_pack gg ch;
                obj
              }
            | Ring r -> do { un_put_on gg r; take_from_pack gg ch; obj }
            | _ ->
                if obj.ob_quantity > 1 then do {
                  obj.ob_quantity --;
                  {(obj) with ob_quantity = 1}
                }
                else do { take_from_pack gg ch; obj } ]
          in
          Level.place_at gg obj g.rogue.row g.rogue.col;
          message gg
            (fun lang →
               let msg =
                 transl lang "Dropped" ^ " " ^ get_desc gg lang obj False
               in
               etransl msg ^ ".") False;
          Move.reg_move gg
        } ]
;

value show_traps g =
  for i = 0 to DROWS - 1 do {
    for j = 0 to DCOLS - 1 do {
      if g.dungeon.(i).(j) land TRAP <> 0 then Curses.mvaddch i j '^' else ();
    };
  }
;

value get_input_line gg prompt insert if_cancelled do_echo = do {
  let g = gg.game_v in
  message_norec gg prompt False;
  let n = Ustring.width (Ustring.of_string (prompt g.lang)) in
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
      let ch = rgetchar gg in
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
  check_message gg;
  let buf =
    loop buf where rec loop buf =
      if Ustring.is_empty buf then buf
      else if Ustring.last_char buf = ' ' then loop (Ustring.but_last buf)
      else buf
  in
  if ch = ROGUE_KEY_CANCEL || Ustring.is_empty buf then do {
    if if_cancelled g.lang <> "" then message gg if_cancelled False else ();
    ""
  }
  else Ustring.to_string buf
};

value call_it gg =
  let g = gg.game_v in
  let ch =
    pack_letter gg (fun lang → transl lang "Call what?")
      (fun
       [ Scroll _ | Potion _ | Wand _ | Ring _ -> True
       | _ -> False ])
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object gg ch False with
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
            let buf =
              get_input_line gg (fun lang → transl lang "Call it:")
                (s) (fun _ → "") True
            in
            if buf <> "" then id.(i) := Called buf else ()
        | None ->
            message gg
              (fun lang →
                 transl lang "Surely you already know what that's called!")
              False ] ]
;

value single_inv gg ichar =
  let ch =
    match ichar with
    [ Some ichar -> ichar
    | None ->
        pack_letter gg (fun lang → transl lang "Inventory what?")
          (fun _ -> True) ]
  in
  if ch = ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object gg ch False with
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
        message gg
          (fun lang →
             sprintf "%c%c %s" ch p (etransl (get_desc gg lang obj True)))
          False ]
;

value inv_armor gg =
  let g = gg.game_v in
  match g.rogue.armor with
  [ Some (c, _) -> single_inv gg (Some c)
  | None -> message gg (fun lang → transl lang "Not wearing anything.") False ]
;

value inv_weapon gg =
  let g = gg.game_v in
  match g.rogue.weapon with
  [ Some (c, _) -> single_inv gg (Some c)
  | None ->
      message gg (fun lang → transl lang "Not wielding anything.") False ]
;

value discovered_kind gg title name id tab = do {
  let g = gg.game_v in
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
  let list =
    List.sort (fun (i1, _, _) (i2, _, _) → compare i1 i2) list
  in
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
      let ch = rgetchar gg in
      if ch = ROGUE_KEY_REMESSAGE then ch
      else if String.contains "!?=/ \027" ch then ch else loop ()
  in
  for i = 0 to len do {
    for j = 0 to maxlen + 1 do {
      Curses.mvaddch i (j + col) (string_get saved.(i) j);
      Curses.clrtoeol ()
    };
  };
  ch
};

value discovered gg =
  let g = gg.game_v in
  let obj_sel = "!?=/" in
  loop_ok () where rec loop_ok () = do {
    message_norec gg
      (fun lang → transl lang "Which object kind" ^ " " ^ obj_sel) False;
    let ch =
      loop () where rec loop () =
        let ch = rgetchar gg in
        if String.contains (obj_sel ^ " \027") ch then ch else loop ()
    in
    check_message gg;
    if ch = '\027' || ch = ' ' then ()
    else
      let rec loop ch =
        let ch2 =
          match ch with
          [ '!' ->
              let name _ = "potion@(p?s:)" in
              discovered_kind gg ("@(p)" ^ transl g.lang "potion@(p?s:)") name
                g.id_potions potion_tab
          | '?' ->
              let name _ = "scroll@(p?s:)" in
              discovered_kind gg ("@(p)" ^ transl g.lang "scroll@(p?s:)") name
                g.id_scrolls scroll_tab
          | '=' ->
              let name _ = "ring" in
              discovered_kind gg (transl g.lang "rings") name g.id_rings
                ring_tab
          | '/' ->
              let name i = if g.is_wood.(i) then "staff" else "wand" in
              discovered_kind gg (transl g.lang "staffs and wands") name
                g.id_wands wand_tab
          | _ -> ch ]
        in
        if ch2 = ROGUE_KEY_REMESSAGE then do {
          switch_lang gg;
          print_monsters_and_stats gg;
          loop ch;
        }
        else if ch2 = '\027' || ch2 = ' ' then loop_ok () else loop ch2
      in
      loop ch
  }
;

value rest gg count = do {
  let g = gg.game_v in
  g.interrupted := False;
  let rec loop i =
    if i < count then
      if g.interrupted then () else do { Move.reg_move gg; loop (i + 1) }
    else ()
  in
  loop 0
};

value throw gg count = do {
  let g = gg.game_v in
  let dir =
    loop True where rec loop first_miss =
      let ch = rgetchar gg in
      if not (is_direction ch) then do {
        sound_bell ();
        if first_miss then
          message gg (fun lang → transl lang "Direction?") False
        else ();
        loop False
      }
      else ch
  in
  check_message gg;
  if dir = ROGUE_KEY_CANCEL then ()
  else
    let ch =
      pack_letter gg
        (fun lang → transl lang "Throw what?" ^
           (if count > 1 then
              " (" ^ sprintf (ftransl lang "%d times") count ^ ")"
            else ""))
        (fun
         [ Weapon _ -> True
         | _ -> False ])
    in
    if ch = ROGUE_KEY_CANCEL then ()
    else do {
      check_message gg;
      match get_letter_object gg ch False with
      [ None -> ()
      | Some {ob_kind = Armor {ar_is_cursed = True; ar_in_use = True}} |
        Some {ob_kind = Ring {rg_is_cursed = True; rg_in_use = Some _}} |
        Some {ob_kind = Weapon {we_is_cursed = True; we_in_use = True}} ->
          message gg
            (fun lang → transl lang "You can't, it appears to be cursed.")
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
              let just_once = Attack.one_throw gg dir (ch, obj) in
              Move.reg_move gg;
              if just_once || g.interrupted then () else loop (count - 1)
            }
          in
          loop count
        } ]
    }
};

value quit gg from_intrpt =
  let g = gg.game_v in
  let yes =
    if g.ask_quit then do {
      check_message gg;
      message_norec gg (fun lang → transl lang "Really quit? (y/n)") True;
      if rgetchar gg <> translc g.lang 'y' then do {
        check_message gg;
        False
      }
      else True
    }
    else True
  in
  if yes then do {
    check_message gg;
    Finish.killed_by gg Quit
  }
  else ()
;

value unknown_command gg ch =
  let s =
    if Char.code ch <= 26 then
      sprintf "ctrl-%c" (Char.chr (Char.code ch + Char.code 'a' - 1))
    else if ch = '\027' then "esc"
    else if ch = '\127' then "del"
    else if ch = '\\' then "\\"
    else Char.escaped ch
  in
  message gg (fun lang → sprintf "%s '%s'" (transl lang "Unknown command") s)
    False
;

value wizardize gg =
  let g = gg.game_v in
  if g.wizard then do {
    g.wizard := False;
    message gg (fun lang → transl lang "Not wizard anymore.") False
  }
  else
    let buf =
      get_input_line gg (fun lang → transl lang "Wizard's password:") ""
        (fun _ → "") False
    in
    if buf = "password" then do {
      g.wizard := True;
      g.score_only := True;
      message gg (fun lang → transl lang "Welcome, mighty wizard!") False
    }
    else message gg (fun lang → transl lang "Sorry" ^ ".") False
;

value msg_is gg ch s =
  message gg
    (fun lang →
       etransl (sprintf (ftransl lang "<%c> is %s") ch (s lang)) ^ ".")
    False
;

value rec whatisit gg = do {
  message gg
    (fun lang → transl lang "What character would you like to know?") False;
  let ch = Curses.getch () in
  check_message gg;
  match ch with
  [ 'A'..'Z' ->
      let tch = itgmc gg ch in
      let i = Char.code tch - Char.code 'A' in
      msg_is gg ch
        (fun lang →
           let s = transl lang (Imonster.visible_mon_name gg i) in
           let art = transl lang "a@(n?n)" in
           art ^ " " ^ s)
  | ROGUE_KEY_CANCEL -> ()
  | ROGUE_KEY_REMESSAGE → do {
      switch_lang gg;
      print_monsters_and_stats gg;
      whatisit gg;
    }
  | '|' | '-' -> msg_is gg ch (fun lang → transl lang "the wall")
  | '+' -> msg_is gg ch (fun lang → transl lang "a door")
  | '#' -> msg_is gg ch (fun lang → transl lang "a tunnel")
  | '.' -> msg_is gg ch (fun lang → transl lang "a floor tile")
  | '!' ->
      msg_is gg ch
        (fun lang →
           let an = transl lang "a@(n?n)" in
           (if an = "" then "" else an ^ " ") ^ transl lang "potion@(p?s:)")
  | '?' ->
      msg_is gg ch
        (fun lang →
           transl lang "a@(n?n)" ^ " " ^ transl lang "scroll@(p?s:)")
  | ')' -> msg_is gg ch (fun lang → transl lang "a weapon")
  | ']' -> msg_is gg ch (fun lang → transl lang "a suit of armour")
  | '*' -> msg_is gg ch (fun lang → transl lang "some gold")
  | ':' -> msg_is gg ch (fun lang → transl lang "some food")
  | '/' -> msg_is gg ch (fun lang → transl lang "a wand or staff")
  | '=' ->
      msg_is gg ch
        (fun lang → transl lang "a@(n?n)" ^ " " ^ transl lang "ring")
  | ',' -> msg_is gg ch (fun lang → transl lang "The Amulet of Yendor")
  | '^' -> msg_is gg ch (fun lang → transl lang "a trap")
  | '%' -> msg_is gg ch (fun lang → transl lang "stairs")
  | '@' -> msg_is gg ch (fun lang → transl lang "you")
  | _ ->
      message gg
        (fun lang →
           sprintf (ftransl lang "I don't know what <%s> is either")
             (Char.escaped ch) ^ ".")
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

value rec instructions_loop gg =
  let g = gg.game_v in
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
      let c : char = rgetchar gg in
      if c = ROGUE_KEY_REMESSAGE then do {
        switch_lang gg;
        instructions_loop gg;
      }
      else True;
    }
  | None -> False ]
;

value instructions gg = do {
  let buffer = Array.init DROWS (fun _ -> Array.make DCOLS ' ') in
  for row = 0 to DROWS - 1 do {
    for col = 0 to DCOLS - 1 do {
      buffer.(row).(col) := Curses.mvinch row col;
    };
  };
  let r = instructions_loop gg in
  display_dungeon gg buffer;
  print_monsters_and_stats gg;
  if r then ()
  else message gg (fun lang → transl lang "Help file not on line.") False;
};

value change_lang gg =
  let g = gg.game_v in
  let new_lang =
    get_input_line gg
      (fun lang →
         let q = transl lang "Language:" in
         if lang = "" then q else q ^ " (" ^ lang ^ ")")
      "" (fun _ → "") True
  in
  if new_lang <> "" && new_lang <> g.lang then do {
    g.lang := new_lang;
    clear_lexicon ();
    print_monsters_and_stats gg;
  }
  else ()
;

value save_game gg =
  let g = gg.game_v in
  let fname =
    get_input_line gg (fun lang → transl lang "File name?") g.save_file
      (fun lang → transl lang "Game not saved.") True
  in
  if fname <> "" then do {
    check_message gg;
    message gg (fun _ → fname) False;
    save_into_file gg fname;
    Finish.clean_up ""
  }
  else ()
;

value show_average_hp gg =
  let g = gg.game_v in
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
  message gg
    (fun lang →
       sprintf (ftransl lang "R-Hp: %.2d, E-Hp: %.2d (!: %d, V: %d)")
         real_average effective_average rogue.extra_hp rogue.less_hp)
    False
;

value backup_if_required gg =
  let g = gg.game_v in
  match f_backup.Efield.get g.env "backup" None with
  [ Some (fname, time) -> do {
      if time mod 1000 = 0 then do {
        let fname = sprintf "%s.%d" fname (time / 1000 mod 5) in
        Misc.save_into_file gg fname
      }
      else ();
      f_backup.Efield.set g.env "backup" (Some (fname, time + 1))
    }
  | None -> () ]
;

value rec play_level gg = do {
  let g = gg.game_v in
  g.interrupted := False;
  if gg.hit_message g.lang <> "" then do {
    message gg gg.hit_message True;
    gg.hit_message := fun _ → ""
  }
  else ();
  if g.trap_door then g.trap_door := False
  else do {
    Curses.move g.rogue.row g.rogue.col;
    Curses.refresh ();
    backup_if_required gg;
    let ch = rgetchar gg in
    check_message gg;
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
            let ch = rgetchar gg in
            loop (if ch = ROGUE_KEY_CANCEL then 0 else count) ch
          }
        | ch -> (count, ch) ]
    in
    let cont =
      match ch with
      [ ROGUE_KEY_DROP_CHECK -> not (drop_check gg)
      | ROGUE_KEY_CHECK_UP -> not (check_up gg)
      | _ -> do {
          match ch with
          [ ROGUE_KEY_INSTRUCTIONS -> instructions gg
          | ROGUE_KEY_REST -> rest gg (max 1 count)
          | ROGUE_KEY_SEARCH -> Move.search gg (max 1 count) False
          | ROGUE_KEY_INVENTORY -> inventory gg g.rogue.pack (fun _ -> True)
          | ROGUE_KEY_FIGHT -> Move.fight gg False
          | ROGUE_KEY_FIGHT_TO_DEATH -> Move.fight gg True
          | ROGUE_KEY_NORTH | ROGUE_KEY_SOUTH | ROGUE_KEY_EAST |
            ROGUE_KEY_WEST | ROGUE_KEY_NORTHEAST | ROGUE_KEY_NORTHWEST |
            ROGUE_KEY_SOUTHEAST | ROGUE_KEY_SOUTHWEST ->
              Move.one_move_rogue gg ch True
          | ROGUE_KEY_WEST_SHIFT | ROGUE_KEY_SOUTH_SHIFT |
            ROGUE_KEY_NORTH_SHIFT | ROGUE_KEY_EAST_SHIFT |
            ROGUE_KEY_NORTHWEST_SHIFT | ROGUE_KEY_NORTHEAST_SHIFT |
            ROGUE_KEY_SOUTHEAST_SHIFT | ROGUE_KEY_SOUTHWEST_SHIFT |
            ROGUE_KEY_WEST_CTRL | ROGUE_KEY_SOUTH_CTRL |
            ROGUE_KEY_NORTH_CTRL | ROGUE_KEY_EAST_CTRL |
            ROGUE_KEY_NORTHWEST_CTRL | ROGUE_KEY_NORTHEAST_CTRL |
            ROGUE_KEY_SOUTHEAST_CTRL | ROGUE_KEY_SOUTHWEST_CTRL ->
              Move.multiple_move_rogue gg ch
          | ROGUE_KEY_EAT -> Use.eat gg
          | ROGUE_KEY_QUAFF -> Use.quaff gg
          | ROGUE_KEY_READ -> Use.read_scroll gg
          | ROGUE_KEY_MOVE -> Move.move_onto gg
          | ROGUE_KEY_DROP -> drop gg
          | ROGUE_KEY_PUT_ON_RING -> Use.put_on_ring gg
          | ROGUE_KEY_REMOVE_RING -> Use.remove_ring gg
          | ROGUE_KEY_REMESSAGE -> remessage gg
          | ROGUE_KEY_WIZARDIZE -> wizardize gg
          | ROGUE_KEY_INV_ARMOR -> inv_armor gg
          | ROGUE_KEY_INV_WEAPON -> inv_weapon gg
          | ROGUE_KEY_INV_RINGS -> Use.inv_rings gg
          | ROGUE_KEY_ID_TRAP -> Move.id_trap gg
          | ROGUE_KEY_SINGLE_INV -> single_inv gg None
          | ROGUE_KEY_DISCOVERED -> discovered gg
          | ROGUE_KEY_CHANGE_LANG -> change_lang gg
          | ROGUE_KEY_TAKE_OFF -> Use.take_off gg
          | ROGUE_KEY_WEAR -> Use.wear gg
          | ROGUE_KEY_WIELD -> Use.wield gg
          | ROGUE_KEY_CALL -> call_it gg
          | ROGUE_KEY_ZAPP -> if Attack.zap gg then Move.reg_move gg else ()
          | ROGUE_KEY_THROW -> throw gg (max 1 count)
          | ROGUE_KEY_VERSION ->
              message gg
                (fun _ →
                   sprintf "mlrogue %s (%s %s)" version g.nick_name
                     g.login_name)
                False
          | ROGUE_KEY_QUIT -> quit gg False
          | ROGUE_KEY_NOP | ROGUE_KEY_CANCEL -> ()
          | ROGUE_KEY_WIZ_INVENTORY ->
              if g.wizard then
                inventory gg (List.map (fun obj -> ('.', obj)) g.level_objects)
                  (fun _ -> True)
              else unknown_command gg ch
          | ROGUE_KEY_WIZ_MAGIC_MAP ->
              if g.wizard then Use.draw_magic_map gg True
              else unknown_command gg ch
          | ROGUE_KEY_WIZ_SHOW_TRAPS ->
              if g.wizard then show_traps g else unknown_command gg ch
          | ROGUE_KEY_WIZ_SHOW_OBJS ->
              if g.wizard then Use.show_objects gg else unknown_command gg ch
          | ROGUE_KEY_SHOW_AV_HP -> show_average_hp gg
          | ROGUE_KEY_WIZ_NEW_OBJ ->
              if g.wizard then new_object_for_wizard gg
              else unknown_command gg ch
          | ROGUE_KEY_WIZ_SHOW_MONST ->
              if g.wizard then show_monsters gg else unknown_command gg ch
          | ROGUE_KEY_SAVE_GAME -> save_game gg
          | ROGUE_KEY_PICK_UP -> Move.kick_into_pack gg
          | ROGUE_KEY_WHATISIT -> whatisit gg
          | _ -> unknown_command gg ch ];
          True
        } ]
    in
    if cont then play_level gg else ()
  }
};

value rec game_loop gg = do {
  print_stats gg STAT_ALL;
  play_level gg;
  Curses.clear ();
  Level.create gg;
  init_display gg;
  game_loop gg
};

value handle_game g =
  Sys.Signal_handle
    (fun s → do { save_into_file g ".rogue.saved"; Finish.clean_up "" })
;

value game g = do {
  Sys.set_signal Sys.sigterm (handle_game g);
  Sys.set_signal Sys.sigint (handle_game g);
  Sys.set_signal Sys.sighup (handle_game g);
  Sys.set_signal Sys.sigquit (handle_game g);
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
  [ Init.NewGame gg -> do {
      let g = gg.game_v in
      f_player_species.Efield.set g.env "player_species" player_spec;
      f_backup.Efield.set g.env "backup" backup_opt;
      f_bool.Efield.set g.env "no handle robot" nhr;
      f_bool.Efield.set g.env "fast" fast;
      f_bool.Efield.set g.env "batch" batch;
      Level.create gg;
      init_display gg;
      if no_record_score then g.score_only := True else ();
      game gg
    }
  | Init.RestoreGame rest_file ->
      match try Left (Misc.restore rest_file) with exc -> Right exc with
      [ Left gg -> do {
          let g = gg.game_v in
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
          ring_stats gg;
          game gg
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
