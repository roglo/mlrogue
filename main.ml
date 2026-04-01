(* $Id: main.ml,v 1.197 2020/06/11 00:07:40 deraugla Exp $ *)

(* #load "pa_more.cmo" *)

open Rogue_def
open Keyboard_def

open Rogue
open Rfield
open Dialogue
open Imisc
open Misc
open Object
open Printf
open Translate

let version = "1.05-exp"

let string_make = Bytes.make
let string_get = Bytes.get
let string_set = Bytes.set
let string_copy = Bytes.copy
let string_of_bytes = Bytes.to_string
let string_to_bytes = Bytes.of_string

let init_display g =
  let row = g.rogue.row in
  let col = g.rogue.col in
  relight g;
  begin match get_room_number g row col with
    Some rn -> Move.wake_room g rn true row col
  | None -> ()
  end;
  if g.new_level_message <> "" then
    begin message g g.new_level_message false; g.new_level_message <- "" end

let drop_check g =
  if g.wizard then true
  else if g.dungeon.(g.rogue.row).(g.rogue.col) land _STAIRS <> 0 then
    if g.rogue.levitate > 0 then
      begin
        message g (transl g.lang "You're floating in the air!") false;
        false
      end
    else true
  else begin message g (transl g.lang "I see no way down.") false; false end

let check_up g =
  if not g.wizard && g.dungeon.(g.rogue.row).(g.rogue.col) land _STAIRS = 0
  then
    begin message g (transl g.lang "I see no way up.") false; false end
  else if not g.wizard && not (has_amulet g) then
    begin
      message g (transl g.lang "Your way is magically blocked.") false;
      false
    end
  else if g.cur_level = 1 then Finish.win g
  else
    begin
      g.new_level_message <-
        transl g.lang "You feel a wrenching sensation in your gut.";
      g.cur_level <- g.cur_level - 2;
      true
    end

let drop g =
  if g.dungeon.(g.rogue.row).(g.rogue.col) land
     (_OBJECT lor _STAIRS lor _TRAP) <>
       0
  then
    message g (transl g.lang "There's already something there.") false
  else if g.rogue.pack = [] then
    message g (transl g.lang "You have nothing to drop.") false
  else
    let ch = pack_letter g (transl g.lang "Drop what?") (fun _ -> true) in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      match get_letter_object g ch false with
        None -> ()
      | Some {ob_kind = Weapon {we_is_cursed = true; we_in_use = true}} |
        Some {ob_kind = Armor {ar_is_cursed = true; ar_in_use = true}} |
        Some {ob_kind = Ring {rg_is_cursed = true; rg_in_use = Some _}} ->
          message g (transl g.lang "You can't, it appears to be cursed.")
            false
      | Some obj ->
          let obj =
            match obj.ob_kind with
              Weapon w ->
                if w.we_in_use then unwield g; take_from_pack g ch; obj
            | Armor a ->
                if a.ar_in_use then begin Monster.mv_aquators g; unwear g end;
                print_stats g _STAT_ARMOR;
                take_from_pack g ch;
                obj
            | Ring r -> un_put_on g r; take_from_pack g ch; obj
            | _ ->
                if obj.ob_quantity > 1 then
                  begin
                    obj.ob_quantity <- obj.ob_quantity - 1;
                    {obj with ob_quantity = 1}
                  end
                else begin take_from_pack g ch; obj end
          in
          Level.place_at g obj g.rogue.row g.rogue.col;
          let msg = transl g.lang "Dropped" ^ " " ^ get_desc g obj false in
          message g (etransl msg ^ ".") false; Move.reg_move g

let show_traps g =
  for i = 0 to _DROWS - 1 do
    for j = 0 to _DCOLS - 1 do
      if g.dungeon.(i).(j) land _TRAP <> 0 then Curses.mvaddch i j '^'
    done
  done

let get_input_line g prompt insert if_cancelled do_echo =
  message g prompt false;
  let n = Ustring.length (Ustring.of_string prompt) in
  let (i, buf) =
    if insert <> "" then
      begin
        Curses.mvaddstr 0 (n + 1) insert;
        Curses.refresh ();
        let i = Ustring.length (Ustring.of_string insert) in
        i, string_of_bytes (string_copy (string_to_bytes insert))
      end
    else 0, ""
  in
  let buf = Ustring.of_string buf in
  let (buf, ch) =
    let rec loop_i buf i =
      let ch = rgetchar g in
      if ch <> '\r' && ch <> '\n' && ch <> _ROGUE_KEY_CANCEL then
        let (buf, i) =
          if ch = '\b' || ch = '\127' then
            if i > 0 then
              begin
                Curses.mvaddch 0 (i + n) ' ';
                Curses.move (_MIN_ROW - 1) (i + n);
                let buf = Ustring.but_last buf in buf, Ustring.length buf
              end
            else buf, i
          else if ch = _CTRL 'u' then
            begin
              for i = 1 to i do Curses.mvaddch 0 (n + i) ' ' done;
              Curses.move (_MIN_ROW - 1) (n + 1);
              Ustring.of_string "", 0
            end
          else if i < _MAX_TITLE_LENGTH - 2 then
            if ch <> ' ' || i > 0 then
              let buf = Ustring.append_char buf ch in
              if do_echo then Curses.addch ch else Curses.addch '.';
              buf, Ustring.length buf
            else buf, i
          else buf, i
        in
        Curses.refresh (); loop_i buf i
      else buf, ch
    in
    loop_i buf i
  in
  check_message g;
  let buf =
    let rec loop buf =
      if Ustring.is_empty buf then buf
      else if Ustring.last_char buf = ' ' then loop (Ustring.but_last buf)
      else buf
    in
    loop buf
  in
  if ch = _ROGUE_KEY_CANCEL || Ustring.is_empty buf then
    begin if if_cancelled <> "" then message g if_cancelled false; "" end
  else Ustring.to_string buf

let call_it g =
  let ch =
    pack_letter g (transl g.lang "Call what?")
      (function
         Scroll _ | Potion _ | Wand _ | Ring _ -> true
       | _ -> false)
  in
  if ch = _ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch false with
      None -> ()
    | Some obj ->
        let id_i =
          match obj.ob_kind with
            Scroll s -> Some (g.id_scrolls, int_of_scroll s)
          | Potion p -> Some (g.id_potions, int_of_potion p)
          | Wand w -> Some (g.id_wands, int_of_wand w.wa_kind)
          | Ring r -> Some (g.id_rings, int_of_ring r.rg_kind)
          | _ -> None
        in
        match id_i with
          Some (id, i) ->
            let s =
              match id.(i) with
                Unidentified s ->
                  begin match obj.ob_kind with
                    Potion _ | Ring _ | Wand _ ->
                      etransl ("@(s)" ^ transl g.lang s)
                  | _ -> s
                  end
              | Called s -> s
              | Identified -> ""
            in
            let buf = get_input_line g (transl g.lang "Call it:") s "" true in
            if buf <> "" then id.(i) <- Called buf
        | None ->
            message g
              (transl g.lang "Surely you already know what that's called!")
              false

let single_inv g ichar =
  let ch =
    match ichar with
      Some ichar -> ichar
    | None -> pack_letter g (transl g.lang "Inventory what?") (fun _ -> true)
  in
  if ch = _ROGUE_KEY_CANCEL then ()
  else
    match get_letter_object g ch false with
      None -> ()
    | Some obj ->
        let p =
          match obj with
            {ob_kind = Armor {ar_is_protected = true}} -> '}'
          | {ob_kind =
               Weapon {we_has_been_uncursed = true; we_identified = false}} ->
              '|'
          | _ -> ')'
        in
        let s = sprintf "%c%c %s" ch p (etransl (get_desc g obj true)) in
        message g s false

let inv_armor g =
  match g.rogue.armor with
    Some (c, _) -> single_inv g (Some c)
  | None -> message g (transl g.lang "Not wearing anything.") false

let inv_weapon g =
  match g.rogue.weapon with
    Some (c, _) -> single_inv g (Some c)
  | None -> message g (transl g.lang "Not wielding anything.") false

let discovered_kind g title name id tab =
  let prompt = transl g.lang " -- Press space to continue --" in
  let list =
    let rec loop_i list i =
      if i = Array.length id then List.rev list
      else
        match id.(i) with
          Identified ->
            let s = transl g.lang tab.(i).o_title in
            loop_i ((tab.(i).o_interest, name i, s) :: list) (i + 1)
        | Called s ->
            let s = transl g.lang "called" ^ " " ^ s in
            loop_i ((Neutral, name i, s) :: list) (i + 1)
        | _ -> loop_i list (i + 1)
    in
    loop_i [] 0
  in
  let list = List.sort compare list in
  let title = " *** " ^ etransl title ^ " ***" in
  let (list, _) =
    List.fold_right
      (fun (interest, name, tit) (list, prev_interest) ->
         let s = sprintf " %s" (etransl (transl g.lang name ^ " " ^ tit)) in
         if list <> [] && interest <> prev_interest then
           s :: "" :: list, interest
         else s :: list, interest)
      list ([], Harmful)
  in
  let list = title :: "" :: (list @ [""; prompt]) in
  let maxlen = List.fold_left max 0 (List.map String.length list) in
  let len = List.length list in
  let col = _DCOLS - (maxlen + 2) in
  let saved = Array.make (len + 1) (string_to_bytes "") in
  for i = 0 to len do
    let a = string_make (maxlen + 2) ' ' in
    saved.(i) <- a;
    for j = 0 to maxlen + 1 do string_set a j (Curses.mvinch i (j + col)) done
  done;
  Array.iteri (fun i str -> Curses.mvaddstr i col str; Curses.clrtoeol ())
    (Array.of_list list);
  Curses.refresh ();
  let ch =
    let rec loop () =
      let ch = rgetchar g in
      if String.contains "!?=/ \027" ch then ch else loop ()
    in
    loop ()
  in
  for i = 0 to len do
    for j = 0 to maxlen + 1 do
      Curses.mvaddch i (j + col) (string_get saved.(i) j)
    done
  done;
  ch

let discovered g =
  let obj_sel = "!?=/" in
  let rec loop_ok () =
    message g (transl g.lang "Which object kind" ^ " " ^ obj_sel) false;
    let ch =
      let rec loop () =
        let ch = rgetchar g in
        if String.contains (obj_sel ^ " \027") ch then ch else loop ()
      in
      loop ()
    in
    check_message g;
    if ch = '\027' || ch = ' ' then ()
    else
      let rec loop ch =
        let ch =
          match ch with
            '!' ->
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
          | _ -> ch
        in
        if ch = '\027' || ch = ' ' then loop_ok () else loop ch
      in
      loop ch
  in
  loop_ok ()

let rest g count =
  g.interrupted <- false;
  let rec loop i =
    if i < count then
      if g.interrupted then () else begin Move.reg_move g; loop (i + 1) end
  in
  loop 0

let throw g count =
  let dir =
    let rec loop first_miss =
      let ch = rgetchar g in
      if not (is_direction ch) then
        begin
          sound_bell ();
          if first_miss then message g (transl g.lang "Direction?") false;
          loop false
        end
      else ch
    in
    loop true
  in
  check_message g;
  if dir = _ROGUE_KEY_CANCEL then ()
  else
    let ch =
      pack_letter g
        (transl g.lang "Throw what?" ^
         (if count > 1 then
            " (" ^ sprintf (ftransl g.lang "%d times") count ^ ")"
          else ""))
        (function
           Weapon _ -> true
         | _ -> false)
    in
    if ch = _ROGUE_KEY_CANCEL then ()
    else
      begin
        check_message g;
        match get_letter_object g ch false with
          None -> ()
        | Some {ob_kind = Armor {ar_is_cursed = true; ar_in_use = true}} |
          Some {ob_kind = Ring {rg_is_cursed = true; rg_in_use = Some _}} |
          Some {ob_kind = Weapon {we_is_cursed = true; we_in_use = true}} ->
            message g (transl g.lang "You can't, it appears to be cursed.")
              false
        | Some {ob_kind = Armor {ar_in_use = true; ar_is_protected = true}} ->
            (* prevent to throw by mistake protected used armors *)
            ()
        | Some obj ->
            let count = min count obj.ob_quantity in
            g.interrupted <- false;
            let rec loop count =
              if count = 0 then ()
              else
                let just_once = Attack.one_throw g dir (ch, obj) in
                Move.reg_move g;
                if just_once || g.interrupted then () else loop (count - 1)
            in
            loop count
      end

let quit g from_intrpt =
  let yes =
    if g.ask_quit then
      begin
        check_message g;
        message g (transl g.lang "Really quit? (y/n)") true;
        if rgetchar g <> translc g.lang 'y' then
          begin
            (* ... *)
            check_message g;
            (* ... *)
            false
          end
        else true
      end
    else true
  in
  if yes then
    begin
      (* ... *)
      check_message g;
      Finish.killed_by g Quit
    end

let unknown_command g ch =
  message g
    (sprintf "%s '%s'" (transl g.lang "Unknown command")
       (if Char.code ch <= 26 then
          sprintf "ctrl-%c" (Char.chr (Char.code ch + Char.code 'a' - 1))
        else if ch = '\027' then "esc"
        else if ch = '\127' then "del"
        else if ch = '\\' then "\\"
        else Char.escaped ch))
    false

let wizardize g =
  if g.wizard then
    begin
      g.wizard <- false;
      message g (transl g.lang "Not wizard anymore.") false
    end
  else
    let buf =
      get_input_line g (transl g.lang "Wizard's password:") "" "" false
    in
    if buf = "password" then
      begin
        g.wizard <- true;
        g.score_only <- true;
        message g (transl g.lang "Welcome, mighty wizard!") false
      end
    else message g (transl g.lang "Sorry" ^ ".") false

let msg_is g ch s =
  let msg = sprintf (ftransl g.lang "<%c> is %s") ch s in
  message g (etransl msg ^ ".") false

let whatisit g =
  message g (transl g.lang "What character would you like to know?") false;
  let ch = Curses.getch () in
  check_message g;
  match ch with
    'A'..'Z' ->
      let tch = itgmc g ch in
      let i = Char.code tch - Char.code 'A' in
      let s = transl g.lang (Imonster.visible_mon_name g i) in
      let art = transl g.lang "a@(n?n)" in msg_is g ch (art ^ " " ^ s)
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
      if ch = _ROGUE_KEY_CANCEL then ()
      else
        message g
          (sprintf (ftransl g.lang "I don't know what <%c> is either") ch ^
           ".")
          false

let instructions_file = "rogue.instr"

let conv_instr s =
  let b = Buffer.create 80 in
  let rec loop i =
    if i < String.length s then
      if i + 1 < String.length s && s.[i] = '%' then
        let c =
          match s.[i+1] with
            'y' -> _ROGUE_KEY_NORTHWEST
          | 'k' -> _ROGUE_KEY_NORTH
          | 'u' -> _ROGUE_KEY_NORTHEAST
          | 'h' -> _ROGUE_KEY_WEST
          | 'l' -> _ROGUE_KEY_EAST
          | 'b' -> _ROGUE_KEY_SOUTHWEST
          | 'j' -> _ROGUE_KEY_SOUTH
          | 'n' -> _ROGUE_KEY_SOUTHEAST
          | '?' -> _ROGUE_KEY_INSTRUCTIONS
          | '.' -> _ROGUE_KEY_REST
          | 's' -> _ROGUE_KEY_SEARCH
          | 'i' -> _ROGUE_KEY_INVENTORY
          | 'f' -> _ROGUE_KEY_FIGHT
          | 'F' -> _ROGUE_KEY_FIGHT_TO_DEATH
          | 'e' -> _ROGUE_KEY_EAT
          | 'q' -> _ROGUE_KEY_QUAFF
          | 'r' -> _ROGUE_KEY_READ
          | 'm' -> _ROGUE_KEY_MOVE
          | 'd' -> _ROGUE_KEY_DROP
          | 'P' -> _ROGUE_KEY_PUT_ON_RING
          | 'R' -> _ROGUE_KEY_REMOVE_RING
          | '>' -> _ROGUE_KEY_DROP_CHECK
          | '<' -> _ROGUE_KEY_CHECK_UP
          | ')' -> _ROGUE_KEY_INV_WEAPON
          | ']' -> _ROGUE_KEY_INV_ARMOR
          | '=' -> _ROGUE_KEY_INV_RINGS
          | '^' -> _ROGUE_KEY_ID_TRAP
          | 'I' -> _ROGUE_KEY_SINGLE_INV
          | 'D' -> _ROGUE_KEY_DISCOVERED
          | 'T' -> _ROGUE_KEY_TAKE_OFF
          | 'W' -> _ROGUE_KEY_WEAR
          | 'w' -> _ROGUE_KEY_WIELD
          | 'c' -> _ROGUE_KEY_CALL
          | 'z' -> _ROGUE_KEY_ZAPP
          | 't' -> _ROGUE_KEY_THROW
          | ',' -> _ROGUE_KEY_PICK_UP
          | '@' -> _ROGUE_KEY_CHANGE_LANG
          | 'v' -> _ROGUE_KEY_VERSION
          | 'Q' -> _ROGUE_KEY_QUIT
          | 'S' -> _ROGUE_KEY_SAVE_GAME
          | '/' -> _ROGUE_KEY_WHATISIT
          | 'X' -> _ROGUE_KEY_SAVE_SCREEN
          | x -> '?'
        in
        Buffer.add_char b c; Buffer.add_char b ' '; loop (i + 2)
      else begin Buffer.add_char b s.[i]; loop (i + 1) end
    else Buffer.contents b
  in
  loop 0

let instructions g =
  match try Some (open_in instructions_file) with Sys_error _ -> None with
    Some ic ->
      if g.lang <> "" then
        begin try
          let rec loop () =
            let line = input_line ic in
            try
              let i = String.index line ':' in
              if string_eq g.lang 0 line 0 i then () else raise Not_found
            with Not_found -> loop ()
          in
          loop ()
        with End_of_file -> seek_in ic 0
        end;
      let buffer = Array.init _DROWS (fun _ -> Array.make _DCOLS ' ') in
      for row = 0 to _DROWS - 1 do
        for col = 0 to _DCOLS - 1 do
          buffer.(row).(col) <- Curses.mvinch row col
        done
      done;
      Curses.clear ();
      begin try
        let rec loop i =
          if i < _DROWS then
            let line = conv_instr (input_line ic) in
            Curses.mvaddstr i 0 line; loop (i + 1)
        in
        loop 0
      with End_of_file -> ()
      end;
      close_in ic;
      Curses.refresh ();
      let _ = (rgetchar g : char) in display_dungeon g buffer
  | None -> message g (transl g.lang "Help file not on line.") false

let change_lang g =
  let q = transl g.lang "Language:" in
  let q = if g.lang = "" then q else q ^ " (" ^ g.lang ^ ")" in
  let new_lang = get_input_line g q "" "" true in
  if new_lang <> "" && new_lang <> g.lang then
    begin
      g.lang <- new_lang;
      clear_lexicon g.lang;
      print_stats g _STAT_ALL;
      List.iter
        (fun mon ->
           if mon.mn_flags land _IMITATES <> 0 then ()
           else if
             g.rogue.blind = 0 && g.rogue.detect_monster ||
             rogue_can_see g mon.mn_row mon.mn_col
           then
             show_monster g mon.mn_row mon.mn_col mon (tgmc g mon.mn_char))
        g.level_monsters
    end

let save_game g =
  let fname =
    get_input_line g (transl g.lang "File name?") g.save_file
      (transl g.lang "Game not saved.") true
  in
  if fname <> "" then
    begin
      check_message g;
      message g fname false;
      save_into_file g fname;
      Finish.clean_up ""
    end

let show_average_hp g =
  let rogue = g.rogue in
  let real_average =
    if g.rogue.exp = 1 then 0
    else
      let den = rogue.hp_max - rogue.extra_hp - _INIT_HP + rogue.less_hp in
      let num = rogue.exp - 1 in (2 * den + num) / (2 * num)
  in
  let effective_average =
    if rogue.exp = 1 then 0
    else
      let den = rogue.hp_max - _INIT_HP in
      let num = rogue.exp - 1 in (2 * den + num) / (2 * num)
  in
  let mbuf =
    sprintf (ftransl g.lang "R-Hp: %.2d, E-Hp: %.2d (!: %d, V: %d)")
      real_average effective_average rogue.extra_hp rogue.less_hp
  in
  message g mbuf false

let backup_if_required g =
  match f_backup.Efield.get g.env "backup" None with
    Some (fname, time) ->
      if time mod 1000 = 0 then
        begin let fname = sprintf "%s.%d" fname (time / 1000 mod 5) in
          Misc.save_into_file g fname
        end;
      f_backup.Efield.set g.env "backup" (Some (fname, time + 1))
  | None -> ()

let rec play_level g =
  g.interrupted <- false;
  if g.hit_message <> "" then
    begin message g g.hit_message true; g.hit_message <- "" end;
  if g.trap_door then g.trap_door <- false
  else
    begin
      Curses.move g.rogue.row g.rogue.col;
      Curses.refresh ();
      backup_if_required g;
      let ch = rgetchar g in
      check_message g;
      let (count, ch) =
        let rec loop count ch =
          match ch with
            '0'..'9' ->
              Curses.move g.rogue.row g.rogue.col;
              Curses.refresh ();
              let count =
                if count < 1000 then 10 * count + Char.code ch - Char.code '0'
                else count
              in
              let ch = rgetchar g in
              loop (if ch = _ROGUE_KEY_CANCEL then 0 else count) ch
          | ch -> count, ch
        in
        loop 0 ch
      in
      let cont =
        if ch = _ROGUE_KEY_DROP_CHECK then not (drop_check g)
        else if ch = _ROGUE_KEY_CHECK_UP then not (check_up g)
        else
          begin
            if ch = _ROGUE_KEY_INSTRUCTIONS then instructions g
            else if ch = _ROGUE_KEY_REST then rest g (max 1 count)
            else if ch = _ROGUE_KEY_SEARCH then
              Move.search g (max 1 count) false
            else if ch = _ROGUE_KEY_INVENTORY then
              inventory g g.rogue.pack (fun _ -> true)
            else if ch = _ROGUE_KEY_FIGHT then Move.fight g false
            else if ch = _ROGUE_KEY_FIGHT_TO_DEATH then Move.fight g true
            else if is_direction ch then Move.one_move_rogue g ch true
            else if is_direction_shift ch then Move.multiple_move_rogue g ch
            else if is_direction_ctrl ch then Move.multiple_move_rogue g ch
            else if ch = _ROGUE_KEY_EAT then Use.eat g
            else if ch = _ROGUE_KEY_QUAFF then Use.quaff g
            else if ch = _ROGUE_KEY_READ then Use.read_scroll g
            else if ch = _ROGUE_KEY_MOVE then Move.move_onto g
            else if ch = _ROGUE_KEY_DROP then drop g
            else if ch = _ROGUE_KEY_PUT_ON_RING then Use.put_on_ring g
            else if ch = _ROGUE_KEY_REMOVE_RING then Use.remove_ring g
            else if ch = _ROGUE_KEY_REMESSAGE then remessage g
            else if ch = _ROGUE_KEY_WIZARDIZE then wizardize g
            else if ch = _ROGUE_KEY_INV_ARMOR then inv_armor g
            else if ch = _ROGUE_KEY_INV_WEAPON then inv_weapon g
            else if ch = _ROGUE_KEY_INV_RINGS then Use.inv_rings g
            else if ch = _ROGUE_KEY_ID_TRAP then Move.id_trap g
            else if ch = _ROGUE_KEY_SINGLE_INV then single_inv g None
            else if ch = _ROGUE_KEY_DISCOVERED then discovered g
            else if ch = _ROGUE_KEY_CHANGE_LANG then change_lang g
            else if ch = _ROGUE_KEY_TAKE_OFF then Use.take_off g
            else if ch = _ROGUE_KEY_WEAR then Use.wear g
            else if ch = _ROGUE_KEY_WIELD then Use.wield g
            else if ch = _ROGUE_KEY_CALL then call_it g
            else if ch = _ROGUE_KEY_ZAPP then
              if Attack.zap g then Move.reg_move g else ()
            else if ch = _ROGUE_KEY_THROW then throw g (max 1 count)
            else if ch = _ROGUE_KEY_VERSION then
              message g
                (sprintf "mlrogue %s (%s %s)" version g.nick_name
                   g.login_name)
                false
            else if ch = _ROGUE_KEY_QUIT then quit g false
            else if ch = _ROGUE_KEY_NOP then ()
            else if ch = _ROGUE_KEY_CANCEL then ()
            else if ch = _ROGUE_KEY_WIZ_INVENTORY then
              if g.wizard then
                inventory g (List.map (fun obj -> '.', obj) g.level_objects)
                  (fun _ -> true)
              else unknown_command g ch
            else if ch = _ROGUE_KEY_WIZ_MAGIC_MAP then
              if g.wizard then Use.draw_magic_map g true
              else unknown_command g ch
            else if ch = _ROGUE_KEY_WIZ_SHOW_TRAPS then
              if g.wizard then show_traps g else unknown_command g ch
            else if ch = _ROGUE_KEY_WIZ_SHOW_OBJS then
              if g.wizard then Use.show_objects g else unknown_command g ch
            else if ch = _ROGUE_KEY_SHOW_AV_HP then show_average_hp g
            else if ch = _ROGUE_KEY_WIZ_NEW_OBJ then
              if g.wizard then new_object_for_wizard g
              else unknown_command g ch
            else if ch = _ROGUE_KEY_WIZ_SHOW_MONST then
              if g.wizard then show_monsters g else unknown_command g ch
            else if ch = _ROGUE_KEY_SAVE_GAME then save_game g
            else if ch = _ROGUE_KEY_PICK_UP then Move.kick_into_pack g
            else if ch = _ROGUE_KEY_WHATISIT then whatisit g
            else unknown_command g ch;
            true
          end
      in
      if cont then play_level g
    end

let rec game_loop g =
  print_stats g _STAT_ALL;
  play_level g;
  Curses.clear ();
  Level.create g;
  init_display g;
  game_loop g

type ('a, 'b) alternative =
    Left of 'a
  | Right of 'b

let main () =
  let (lang, init, rob_opt, backup_opt, fast, batch, no_record_score) =
    Init.f Sys.argv
  in
  let (player_spec, nhr) =
    match rob_opt with
      Some (ps, ehr) -> ps, ehr
    | None -> PShuman, false
  in
  if batch then Curses.no_output ();
  Curses.initscr ();
  if Curses.lines () < _DROWS || Curses.cols () < _DCOLS then
    Finish.clean_up
      (sprintf (ftransl lang "Must be played on a %d x %d or better screen")
         _DROWS _DCOLS);
  match init with
    Init.NewGame g ->
      f_player_species.Efield.set g.env "player_species" player_spec;
      f_backup.Efield.set g.env "backup" backup_opt;
      f_bool.Efield.set g.env "no handle robot" nhr;
      f_bool.Efield.set g.env "fast" fast;
      f_bool.Efield.set g.env "batch" batch;
      Level.create g;
      init_display g;
      if no_record_score then g.score_only <- true;
      game_loop g
  | Init.RestoreGame rest_file ->
      begin match
        (try Left (Misc.restore rest_file) with exc -> Right exc)
      with
        Left g ->
          if no_record_score then g.score_only <- true;
          if g.score_only then
            begin match f_random.Efield.get g.env "random" None with
              Some r -> Random.set_state r
            | None -> ()
            end;
          let player_spec =
            match player_spec with
              PSrobot arg_rob ->
                let rob =
                  match
                    f_player_species.Efield.get g.env "player_species" PShuman
                  with
                    PSrobot rob ->
                      let after_fail =
                        f_bool.Efield.get g.env "failed" false
                      in
                      Robot.reinit after_fail arg_rob rob
                  | PSsocket _ | PShuman -> arg_rob
                in
                PSrobot rob
            | PSsocket _ | PShuman -> player_spec
          in
          f_player_species.Efield.set g.env "player_species" player_spec;
          begin match backup_opt with
            Some (arg_back, _) ->
              let time =
                match f_backup.Efield.get g.env "backup" None with
                  Some (_, time) -> time
                | None -> 0
              in
              f_backup.Efield.set g.env "backup" (Some (arg_back, time))
          | None -> f_backup.Efield.set g.env "backup" None
          end;
          f_bool.Efield.set g.env "fast" fast;
          f_bool.Efield.set g.env "no handle robot" nhr;
          f_bool.Efield.set g.env "batch" batch;
          f_bool.Efield.set g.env "break" false;
          f_bool.Efield.set g.env "failed" false;
          g.msg_cleared <- false;
          ring_stats g;
          game_loop g
      | Right (Sys_error _ | Failure _) ->
          Finish.clean_up
            (sprintf "%s: %s" rest_file (transl lang "cannot open file"))
      | Right e -> raise e
      end
  | Init.ScoreOnly -> Finish.put_scores lang true None; Finish.clean_up ""

let _ = try main () with e -> Curses.endwin (); raise e
