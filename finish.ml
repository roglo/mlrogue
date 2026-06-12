(* $Id: finish.ml,v 1.43 2018/04/26 09:52:36 deraugla Exp $ *)

(* #load "pa_more.cmo" *)
(* #use "rogue.def" *)


#open "rogue";;
#open "rfield";;
#open "dialogue";;
#open "imisc";;
#open "object";;
#open "printf";;
#open "translate";;

let string_create = string__create_string;;
let string_contains s c =
  try let _ = string__index_char s c in true with Not_found -> false
;;
let string_length = string__string_length;;
let string_of_bytes (s : string) = s;;

let win_message g =
  let f = curses__mvaddstr in
  match try Some (open_in "rogue.win") with sys__Sys_error _ -> None with
    Some ic ->
      if g.lang <> "" then
        begin try
          let rec loop () =
            let line = input_line ic in
            try
              let i = string__index_char line `:` in
              if string_eq g.lang 0 line 0 i then () else raise Not_found
            with Not_found -> loop ()
          in
          loop ()
        with End_of_file -> seek_in ic 0
        end;
      curses__clear ();
      begin try
        let rec loop i =
          let line = input_line ic in
          if string_contains line `:` then ()
          else begin f i 11 line; loop (i + 1) end
        in
        loop 7
      with End_of_file -> ()
      end;
      close_in ic;
      message g "" true;
      message g "" true
  | None ->
      curses__clear ();
      f 10 11 "@   @  @@@   @   @      @  @  @   @@@   @   @   @";
      f 11 11 " @ @  @   @  @   @      @  @  @  @   @  @@  @   @";
      f 12 11 "  @   @   @  @   @      @  @  @  @   @  @ @ @   @";
      f 13 11 "  @   @   @  @   @      @  @  @  @   @  @  @@    ";
      f 14 11 "  @    @@@    @@@        @@ @@    @@@   @   @   @";
      f 17 11 "Congratulations,  you have  been admitted  to  the";
      f 18 11 "Fighters' Guild.   You return home,  sell all your";
      f 19 11 "treasures at great profit and retire into comfort.";
      message g "" false;
      message g "" false
;;

let id_all g pack =
  vect__fill_vect g.id_potions 0 (vect__vect_length g.id_potions) Identified;
  vect__fill_vect g.id_rings 0 (vect__vect_length g.id_rings) Identified;
  vect__fill_vect g.id_scrolls 0 (vect__vect_length g.id_scrolls) Identified;
  vect__fill_vect g.id_wands 0 (vect__vect_length g.id_wands) Identified;
  list__do_list
    (fun obj ->
       match obj.ob_kind with
         Armor a -> a.ar_identified <- true
       | Ring r -> r.rg_identified <- true
       | Wand w -> w.wa_identified <- true
       | Weapon w -> w.we_identified <- true
       | Scroll _ | Potion _ | Food _ | Gold | Amulet -> ())
    pack
;;

let get_value g obj =
  match obj.ob_kind with
    Weapon w ->
      let v = weapon_tab.(int_of_weapon w.we_kind).o_value in
      let v =
        match w.we_kind with
          Arrow | Dagger | Shuriken | Dart -> v * obj.ob_quantity
        | _ -> v
      in
      let v = v + w.we_d_enchant * 85 in
      let v = v + w.we_hit_enchant * 85 in v
  | Armor a ->
      let v = armor_tab.(int_of_armor a.ar_kind).o_value in
      let v = v + a.ar_enchant * 75 in
      if a.ar_is_protected then v + 200 else v
  | Wand w -> wand_tab.(int_of_wand w.wa_kind).o_value * (w.wa_hits + 1)
  | Scroll s -> scroll_tab.(int_of_scroll s).o_value * obj.ob_quantity
  | Potion p -> potion_tab.(int_of_potion p).o_value * obj.ob_quantity
  | Amulet -> 5000
  | Ring r -> ring_tab.(int_of_ring r.rg_kind).o_value * (r.rg_class + 1)
  | _ -> 0
;;

let sell_pack g =
  curses__clear ();
  curses__mvaddstr 1 0 (transl g.lang "Value      Item");
  let _ =
    list__it_list
      (fun row (_, obj) ->
         match obj.ob_kind with
           Food _ -> row
         | _ ->
             let v = max 10 (get_value g obj) in
             g.rogue.gold <- g.rogue.gold + v;
             if row < 24 then
               begin let d = get_desc g obj true in
                 let line = sprintf "%5d      %s" v (etransl d) in
                 curses__mvaddstr row 0 line
               end;
             row + 1)
      2 (sort__sort (fun (a, _) (b, _) -> a <= b) g.rogue.pack)
  in
  curses__refresh (); message g "" false
;;

(* *)

let score_magic = "RGSC0001";;
let score_file = ".rogue.scores";;

type score_type =
  { sc_score : int;
    sc_name : string;
    sc_ending : ending;
    sc_level : int;
    sc_with_amulet : bool }
;;

let read_scores () =
  let ic =
    match try Some (open_in_bin score_file) with sys__Sys_error _ -> None with
      Some ic ->
        let b = string_create (string__string_length score_magic) in
        really_input ic b 0 (string_length b);
        let b = string_of_bytes b in
        if b <> score_magic then begin close_in ic; None end else Some ic
    | None -> None
  in
  match ic with
    Some ic -> let (v : score_type list) = input_value ic in close_in ic; v
  | None ->
      [{sc_score = 100; sc_name = "john"; sc_ending = Monster "hobgoblin";
        sc_level = 3; sc_with_amulet = false};
       {sc_score = 25; sc_name = "bob"; sc_ending = Starvation; sc_level = 4;
        sc_with_amulet = false}]
;;

let write_scores scores =
  match try Some (open_out_bin score_file) with sys__Sys_error _ -> None with
    Some oc ->
      let scores =
        if list__list_length scores > 15 then list__rev (list__tl (list__rev scores))
        else scores
      in
      output_string oc score_magic;
      output_value oc (scores : score_type list);
      close_out oc
  | None -> ()
;;

let insert_score sc =
  let rec loop inserted rank scl =
    if rank = 15 + 1 then
      if not inserted then [sc], rank
      else
        match scl with
          sc1 :: _ -> [sc1], -1
        | [] -> [], -1
    else
      match scl with
        sc1 :: scl ->
          if not inserted then
            if sc1.sc_score < sc.sc_score then
              let (scl, _) = loop true (rank + 1) (sc1 :: scl) in
              sc :: scl, rank
            else
              let (scl, rank) = loop false (rank + 1) scl in sc1 :: scl, rank
          else let (scl, _) = loop true (rank + 1) scl in sc1 :: scl, -1
      | [] -> if not inserted then [sc], rank else [], -1
  in
  loop false 1
;;

let text_of_ending lang =
  function
    Monster name ->
      let art = transl lang "a@(n?n)" in
      transl lang "killed by" ^ " " ^ art ^ " " ^ transl lang name
  | Hypothermia -> transl lang "died of hypothermia"
  | Starvation -> transl lang "died of starvation"
  | PoisonDart -> transl lang "killed by a dart"
  | Quit -> transl lang "quit"
  | Win -> transl lang "a total winner"
;;

let clean_up estr =
  curses__mvaddstr (curses__lines () - 1) 0 estr;
  curses__refresh ();
  curses__endwin ();
  printf "\n";
  flush stdout;
  exit 0
;;

let ending_reason_line lang score =
  let s =
    text_of_ending lang score.sc_ending ^ " " ^
    sprintf (ftransl lang "on level %d") score.sc_level ^
    (if score.sc_ending <> Win && score.sc_with_amulet then
       " " ^ transl lang "with amulet"
     else "")
  in
  etransl s
;;

let put_scores lang score_only g_ending =
  let scores = read_scores () in
  let (scores, n) =
    match g_ending with
      Some (g, ending) ->
        (* ... *)
        curses__refresh ();
        let score =
          {sc_score = g.rogue.gold;
           sc_name = if g.nick_name <> "" then g.nick_name else g.login_name;
           sc_ending = ending; sc_level = g.max_level;
           sc_with_amulet = has_amulet g}
        in
        if f_bool.efield__get g.env "batch" false then
          (let s = ending_reason_line lang score in printf "%s" s);
        insert_score score scores
    | _ -> scores, 15 + 1
  in
  if score_only then () else write_scores scores;
  curses__clear ();
  curses__mvaddstr 3 30 (transl lang "Top  Rogueists");
  curses__mvaddstr 5 0 (transl lang "Rank   Score   Name");
  let _ =
    list__it_list
      (fun rank score ->
         if rank > 15 && rank <> n then rank + 1
         else
           let buf = ending_reason_line lang score in
           let txt =
             sprintf "%s    %6d   %s: %s"
               (if rank > 15 || score_only && rank = n then ".."
                else
                  let rank =
                    if score_only && rank > n then rank - 1 else rank
                  in
                  sprintf "%2d" rank)
               score.sc_score score.sc_name buf
           in
           let txt =
             if rank = n then
               let len = 80 - string__string_length txt - 2 in
               txt ^ string__make_string (max 0 len) ` `
             else txt
           in
           curses__move (6 + rank) 0;
           if not score_only && rank = n then curses__standout ();
           curses__addstr txt;
           if not score_only && rank = n then curses__standend ();
           rank + 1)
      1 scores
  in
  (); curses__refresh ()
;;

let win g =
  (* ... *)
  win_message g;
  id_all g (list__map snd g.rogue.pack);
  sell_pack g;
  put_scores g.lang g.score_only (Some (g, Win));
  message g "" false;
  check_message g;
  clean_up ""
;;

let has_unidentifed_objects g pack =
  list__exists
    (fun (_, obj) ->
       match obj.ob_kind with
         Armor a -> not a.ar_identified
       | Weapon w -> not w.we_identified
       | Potion p -> g.id_potions.(int_of_potion p) <> Identified
       | Scroll s -> g.id_scrolls.(int_of_scroll s) <> Identified
       | Ring r ->
           not r.rg_identified ||
           g.id_rings.(int_of_ring r.rg_kind) <> Identified
       | Wand w ->
           not w.wa_identified ||
           g.id_wands.(int_of_wand w.wa_kind) <> Identified
       | _ -> false)
    pack
;;

let select_unidentified g (_, obj) =
  match obj.ob_kind with
    Armor a ->
      if a.ar_identified then false
      else begin a.ar_identified <- true; true end
  | Weapon w ->
      if w.we_identified then false
      else begin w.we_identified <- true; true end
  | Potion p ->
      if g.id_potions.(int_of_potion p) = Identified then false
      else begin g.id_potions.(int_of_potion p) <- Identified; true end
  | Scroll s ->
      if g.id_scrolls.(int_of_scroll s) = Identified then false
      else begin g.id_scrolls.(int_of_scroll s) <- Identified; true end
  | Ring r ->
      if r.rg_identified && g.id_rings.(int_of_ring r.rg_kind) = Identified
      then
        false
      else
        begin
          r.rg_identified <- true;
          g.id_rings.(int_of_ring r.rg_kind) <- Identified;
          true
        end
  | Wand w ->
      if w.wa_identified && g.id_wands.(int_of_wand w.wa_kind) = Identified
      then
        false
      else
        begin
          w.wa_identified <- true;
          g.id_wands.(int_of_wand w.wa_kind) <- Identified;
          true
        end
  | Food _ | Gold | Amulet -> false
;;

let killed_by g death =
  if death <> Quit then g.rogue.gold <- g.rogue.gold * 9 / 10;
  let buf =
    match death with
      Monster mon_name ->
        let art = transl g.lang "a@(n?n)" in
        transl g.lang "Killed by" ^ " " ^ art ^ " " ^ transl g.lang mon_name
    | Hypothermia -> transl g.lang "Died of hypothermia"
    | Starvation -> transl g.lang "Died of starvation"
    | PoisonDart -> transl g.lang "Killed by a dart"
    | Quit -> transl g.lang "Quit"
    | Win -> "win?"
  in
  let buf =
    buf ^ " " ^ sprintf (ftransl g.lang "with %d gold") g.rogue.gold
  in
  let buf = etransl buf in
  begin match death with
    Monster _ | Hypothermia | Starvation | PoisonDart when g.show_skull ->
      let center row buf =
        let margin = (80 - string__string_length buf) / 2 in
        curses__mvaddstr row margin buf
      in
      curses__clear ();
      curses__mvaddstr 04 27 "     __---------__";
      curses__mvaddstr 05 27 "   _~             ~_";
      curses__mvaddstr 06 27 "  /                 \\";
      curses__mvaddstr 07 27 " ~                   ~";
      curses__mvaddstr 08 27 "/                     \\";
      curses__mvaddstr 09 27 "|    XXXX     XXXX    |";
      curses__mvaddstr 10 27 "|    XXXX     XXXX    |";
      curses__mvaddstr 11 27 "|    XXX       XXX    |";
      curses__mvaddstr 12 27 " \\         @         /";
      curses__mvaddstr 13 27 "  --\\     @@@     /--";
      curses__mvaddstr 14 27 "   | |    @@@    | |";
      curses__mvaddstr 15 27 "   | |           | |";
      curses__mvaddstr 16 27 "   | vvVvvvvvvvVvv |";
      curses__mvaddstr 17 27 "   |  ^^^^^^^^^^^  |";
      curses__mvaddstr 18 27 "    \\_           _/";
      curses__mvaddstr 19 27 "      ~---------~";
      center 21 (if g.nick_name <> "" then g.nick_name else g.login_name);
      center 22 buf;
      check_message g;
      message g "" false
  | _ -> message g (buf ^ ".") false
  end;
  message g "" false;
  let pack_opt =
    if g.rogue.pack <> [] && has_unidentifed_objects g g.rogue.pack then
      let pack = list_filter (select_unidentified g) g.rogue.pack in Some pack
    else None
  in
  id_all g g.level_objects;
  let prompt2 =
    match pack_opt with
      Some _ -> transl g.lang " -- Press space or backspace --"
    | None -> transl g.lang " -- Press space to continue --"
  in
  let term2 =
    match pack_opt with
      Some _ -> " \027\b\127"
    | None -> " \027"
  in
  begin let rec loop () =
    begin match pack_opt with
      Some pack -> inventory g pack (fun _ -> true)
    | None -> ()
    end;
    let retc =
      inv_sel g (list__map (fun obj -> `.`, obj) g.level_objects)
        (fun _ -> true) prompt2 term2
    in
    match retc with
      Some (`\b` | `\127`) -> loop ()
    | Some _ | None -> ()
  in
    loop ()
  end;
  put_scores g.lang g.score_only (Some (g, death));
  message g "" false;
  check_message g;
  clean_up ""
;;
