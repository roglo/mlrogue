(* $Id: finish.ml,v 1.43 2018/04/26 09:52:36 deraugla Exp $ *)

#load "pa_more.cmo";
#use "rogue.def";

open Rogue;
open Rfield;
open Dialogue;
open Imisc;
open Object;
open Printf;
open Translate;

value string_create = Bytes.create;
value string_length = Bytes.length;
value string_of_bytes = Bytes.to_string;

value win_message g =
  let f = Curses.mvaddstr in
  match try Some (open_in "rogue.win") with [ Sys_error _ -> None ] with
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
        loop 7 where rec loop i =
          let line = input_line ic in
          if String.contains line ':' then ()
          else do { f i 11 line; loop (i + 1) }
      with
      [ End_of_file -> () ];
      close_in ic;
      message g "" True;
      message g "" True
    }
  | None -> do {
      Curses.clear ();
      f 10 11 "@   @  @@@   @   @      @  @  @   @@@   @   @   @";
      f 11 11 " @ @  @   @  @   @      @  @  @  @   @  @@  @   @";
      f 12 11 "  @   @   @  @   @      @  @  @  @   @  @ @ @   @";
      f 13 11 "  @   @   @  @   @      @  @  @  @   @  @  @@    ";
      f 14 11 "  @    @@@    @@@        @@ @@    @@@   @   @   @";
      f 17 11 "Congratulations,  you have  been admitted  to  the";
      f 18 11 "Fighters' Guild.   You return home,  sell all your";
      f 19 11 "treasures at great profit and retire into comfort.";
      message g "" False;
      message g "" False
    } ]
;

value id_all g pack = do {
  Array.fill g.id_potions 0 (Array.length g.id_potions) Identified;
  Array.fill g.id_rings 0 (Array.length g.id_rings) Identified;
  Array.fill g.id_scrolls 0 (Array.length g.id_scrolls) Identified;
  Array.fill g.id_wands 0 (Array.length g.id_wands) Identified;
  List.iter
    (fun obj ->
       match obj.ob_kind with
       [ Armor a -> a.ar_identified := True
       | Ring r -> r.rg_identified := True
       | Wand w -> w.wa_identified := True
       | Weapon w -> w.we_identified := True
       | Scroll _ | Potion _ | Food _ | Gold | Amulet -> () ])
    pack
};

value get_value g obj =
  match obj.ob_kind with
  [ Weapon w ->
      let v = weapon_tab.(int_of_weapon w.we_kind).o_value in
      let v =
        match w.we_kind with
        [ Arrow | Dagger | Shuriken | Dart -> v * obj.ob_quantity
        | _ -> v ]
      in
      let v = v + w.we_d_enchant * 85 in
      let v = v + w.we_hit_enchant * 85 in
      v
  | Armor a ->
      let v = armor_tab.(int_of_armor a.ar_kind).o_value in
      let v = v + a.ar_enchant * 75 in
      if a.ar_is_protected then v + 200 else v
  | Wand w -> wand_tab.(int_of_wand w.wa_kind).o_value * (w.wa_hits + 1)
  | Scroll s -> scroll_tab.(int_of_scroll s).o_value * obj.ob_quantity
  | Potion p -> potion_tab.(int_of_potion p).o_value * obj.ob_quantity
  | Amulet -> 5000
  | Ring r -> ring_tab.(int_of_ring r.rg_kind).o_value * (r.rg_class + 1)
  | _ -> 0 ]
;

value sell_pack g = do {
  Curses.clear ();
  Curses.mvaddstr 1 0 (transl g.lang "Value      Item");
  let _ =
    List.fold_left
      (fun row (_, obj) ->
         match obj.ob_kind with
         [ Food _ -> row
         | _ -> do {
             let v = max 10 (get_value g obj) in
             g.rogue.gold add_eq v;
             if row < DROWS then
               let d = get_desc g obj True in
               let line = sprintf "%5d      %s" v (etransl d) in
               Curses.mvaddstr row 0 line
             else ();
             row + 1
           } ])
      2 (List.sort (fun (a, _) (b, _) -> compare a b) g.rogue.pack)
  in
  Curses.refresh ();
  message g "" False
};

DEFINE MAXRANK = 15;

value score_magic = "RGSC0001";
value score_file = ".rogue.scores";

type score_type =
  { sc_score : int;
    sc_name : string;
    sc_ending : ending;
    sc_level : int;
    sc_with_amulet : bool }
;

value read_scores () =
  let ic =
    match try Some (open_in_bin score_file) with [ Sys_error _ -> None ] with
    [ Some ic -> do {
        let b = string_create (String.length score_magic) in
        really_input ic b 0 (string_length b);
	let b = string_of_bytes b in
        if b <> score_magic then do { close_in ic; None } else Some ic
      }
    | None -> None ]
  in
  match ic with
  [ Some ic -> do {
      let v : list score_type = input_value ic in
      close_in ic;
      v
    }
  | None ->
      [{sc_score = 100; sc_name = "john"; sc_ending = Monster "hobgoblin";
        sc_level = 3; sc_with_amulet = False};
       {sc_score = 25; sc_name = "bob"; sc_ending = Starvation; sc_level = 4;
        sc_with_amulet = False}] ]
;

value write_scores scores =
  match try Some (open_out_bin score_file) with [ Sys_error _ -> None ] with
  [ Some oc -> do {
      let scores =
        if List.length scores > MAXRANK then
          List.rev (List.tl (List.rev scores))
        else scores
      in
      output_string oc score_magic;
      output_value oc (scores : list score_type);
      close_out oc
    }
  | None -> () ]
;

value insert_score sc =
  loop False 1 where rec loop inserted rank scl =
    if rank = MAXRANK + 1 then
      if not inserted then ([sc], rank)
      else
        match scl with
        [ [sc1 :: _] -> ([sc1], -1)
        | [] -> ([], -1) ]
    else
      match scl with
      [ [sc1 :: scl] ->
          if not inserted then
            if sc1.sc_score < sc.sc_score then
              let (scl, _) = loop True (rank + 1) [sc1 :: scl] in
              ([sc :: scl], rank)
            else
              let (scl, rank) = loop False (rank + 1) scl in
              ([sc1 :: scl], rank)
          else
            let (scl, _) = loop True (rank + 1) scl in
            ([sc1 :: scl], -1)
      | [] -> if not inserted then ([sc], rank) else ([], -1) ]
;

value text_of_ending lang =
  fun
  [ Monster name ->
      let art = transl lang "a@(n?n)" in
      transl lang "killed by" ^ " " ^ art ^ " " ^ transl lang name
  | Hypothermia -> transl lang "died of hypothermia"
  | Starvation -> transl lang "died of starvation"
  | PoisonDart -> transl lang "killed by a dart"
  | Quit -> transl lang "quit"
  | Win -> transl lang "a total winner" ]
;

value clean_up estr = do {
  Curses.mvaddstr (Curses.lines () - 1) 0 estr;
  Curses.refresh ();
  Curses.endwin ();
  printf "\n";
  flush stdout;
  exit 0
};

value ending_reason_line lang score =
  let s =
    text_of_ending lang score.sc_ending ^ " " ^
    sprintf (ftransl lang "on level %d") score.sc_level ^
    (if score.sc_ending <> Win && score.sc_with_amulet then
       " " ^ transl lang "with amulet"
     else "")
  in
  etransl s
;

value put_scores lang score_only g_ending = do {
  let scores = read_scores () in
  let (scores, n) =
    match g_ending with
    [ Some (g, ending) -> do {
        (* ... *)
        Curses.refresh ();
        let score =
          {sc_score = g.rogue.gold;
           sc_name = if g.nick_name <> "" then g.nick_name else g.login_name;
           sc_ending = ending; sc_level = g.max_level;
           sc_with_amulet = has_amulet g}
        in
        if f_bool.Efield.get g.env "batch" False then do {
          let s = ending_reason_line lang score in
          printf "%s" s;
        }
        else ();
        insert_score score scores
      }
    | _ -> (scores, MAXRANK+1) ]
  in
  if score_only then () else write_scores scores;
  Curses.clear ();
  Curses.mvaddstr 3 30 (transl lang "Top  Rogueists");
  Curses.mvaddstr 5 0 (transl lang "Rank   Score   Name");
  let _ =
    List.fold_left
      (fun rank score ->
         if rank > MAXRANK && rank <> n then rank + 1
         else do {
           let buf = ending_reason_line lang score in
           let txt =
             sprintf "%s    %6d   %s: %s"
               (if rank > MAXRANK || score_only && rank = n then ".."
                else
                  let rank =
                    if score_only && rank > n then rank - 1
                    else rank
                  in
                  sprintf "%2d" rank)
               score.sc_score score.sc_name buf
           in
           let txt =
             if rank = n then
               let len = DCOLS - String.length txt - 2 in
               txt ^ String.make (max 0 len) ' '
             else txt
           in
           Curses.move (6 + rank) 0;
           if not score_only && rank = n then Curses.standout () else ();
           Curses.addstr txt;
           if not score_only && rank = n then Curses.standend () else ();
           rank + 1
         })
      1 scores
  in
  ();
  Curses.refresh ()
};

value win g = do {
  (* ... *)
  win_message g;
  id_all g (List.map snd g.rogue.pack);
  sell_pack g;
  put_scores g.lang g.score_only (Some (g, Win));
  message g "" False;
  check_message g;
  clean_up ""
};

value has_unidentifed_objects g pack =
  List.exists
    (fun (_, obj) ->
       match obj.ob_kind with
       [ Armor a -> not a.ar_identified
       | Weapon w -> not w.we_identified
       | Potion p -> g.id_potions.(int_of_potion p) <> Identified
       | Scroll s -> g.id_scrolls.(int_of_scroll s) <> Identified
       | Ring r ->
           not r.rg_identified ||
           g.id_rings.(int_of_ring r.rg_kind) <> Identified
       | Wand w ->
           not w.wa_identified ||
           g.id_wands.(int_of_wand w.wa_kind) <> Identified
       | _ -> False ])
    pack
;

value select_unidentified g (_, obj) =
  match obj.ob_kind with
  [ Armor a ->
      if a.ar_identified then False else do { a.ar_identified := True; True }
  | Weapon w ->
      if w.we_identified then False else do { w.we_identified := True; True }
  | Potion p ->
      if g.id_potions.(int_of_potion p) = Identified then False
      else do { g.id_potions.(int_of_potion p) := Identified; True }
  | Scroll s ->
      if g.id_scrolls.(int_of_scroll s) = Identified then False
      else do { g.id_scrolls.(int_of_scroll s) := Identified; True }
  | Ring r ->
      if r.rg_identified && g.id_rings.(int_of_ring r.rg_kind) = Identified
      then
        False
      else do {
        r.rg_identified := True;
        g.id_rings.(int_of_ring r.rg_kind) := Identified;
        True
      }
  | Wand w ->
      if w.wa_identified && g.id_wands.(int_of_wand w.wa_kind) = Identified
      then
        False
      else do {
        w.wa_identified := True;
        g.id_wands.(int_of_wand w.wa_kind) := Identified;
        True
      }
  | Food _ | Gold | Amulet -> False ]
;

value killed_by g death = do {
  if death <> Quit then g.rogue.gold := g.rogue.gold * 9 / 10 else ();
  let buf =
    match death with
    [ Monster mon_name ->
        let art = transl g.lang "a@(n?n)" in
        transl g.lang "Killed by" ^ " " ^ art ^ " " ^ transl g.lang mon_name
    | Hypothermia -> transl g.lang "Died of hypothermia"
    | Starvation -> transl g.lang "Died of starvation"
    | PoisonDart -> transl g.lang "Killed by a dart"
    | Quit -> transl g.lang "Quit"
    | Win -> "win?" ]
  in
  let buf =
    buf ^ " " ^ sprintf (ftransl g.lang "with %d gold") g.rogue.gold
  in
  let buf = etransl buf in
  match death with
  [ Monster _ | Hypothermia | Starvation | PoisonDart
    when g.show_skull -> do {
      let center row buf =
        let margin = (DCOLS - String.length buf) / 2 in
        Curses.mvaddstr row margin buf
      in
      Curses.clear ();
      Curses.mvaddstr 04 27 "     __---------__";
      Curses.mvaddstr 05 27 "   _~             ~_";
      Curses.mvaddstr 06 27 "  /                 \\";
      Curses.mvaddstr 07 27 " ~                   ~";
      Curses.mvaddstr 08 27 "/                     \\";
      Curses.mvaddstr 09 27 "|    XXXX     XXXX    |";
      Curses.mvaddstr 10 27 "|    XXXX     XXXX    |";
      Curses.mvaddstr 11 27 "|    XXX       XXX    |";
      Curses.mvaddstr 12 27 " \\         @         /";
      Curses.mvaddstr 13 27 "  --\\     @@@     /--";
      Curses.mvaddstr 14 27 "   | |    @@@    | |";
      Curses.mvaddstr 15 27 "   | |           | |";
      Curses.mvaddstr 16 27 "   | vvVvvvvvvvVvv |";
      Curses.mvaddstr 17 27 "   |  ^^^^^^^^^^^  |";
      Curses.mvaddstr 18 27 "    \\_           _/";
      Curses.mvaddstr 19 27 "      ~---------~";
      center 21 (if g.nick_name <> "" then g.nick_name else g.login_name);
      center 22 buf;
      check_message g;
      message g "" False
    }
  | _ -> message g (buf ^ ".") False ];
  message g "" False;
  let pack_opt =
    if g.rogue.pack <> [] && has_unidentifed_objects g g.rogue.pack then
      let pack = List.filter (select_unidentified g) g.rogue.pack in
      Some pack
    else None
  in
  id_all g g.level_objects;
  let prompt2 =
    match pack_opt with
    [ Some _ -> transl g.lang " -- Press space or backspace --"
    | None -> transl g.lang " -- Press space to continue --" ]
  in
  let term2 =
    match pack_opt with
    [ Some _ -> " \027\b\127"
    | None -> " \027" ]
  in
  loop () where rec loop () = do {
    match pack_opt with
    [ Some pack -> inventory g pack (fun _ -> True)
    | None -> () ];
    let retc =
      inv_sel g (List.map (fun obj -> ('.', obj)) g.level_objects)
        (fun _ -> True) prompt2 term2
    in
    match retc with
    [ Some ('\b' | '\127') -> loop ()
    | Some _ | None -> () ]
  };
  put_scores g.lang g.score_only (Some (g, death));
  message g "" False;
  check_message g;
  clean_up ""
};
