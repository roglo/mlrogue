(* $Id: init.ml,v 1.64 2013/01/29 14:00:23 deraugla Exp $ *)

open Rogue_def

open Rfield
open Rogue
open Printf
open Translate

let syllabes =
  [| "blech"; "foo"; "barf"; "rech"; "bar"; "quo"; "bloto"; "woh"; "caca";
     "blorp"; "erp"; "festr"; "rot"; "slie"; "snorf"; "iky"; "yuky"; "ooze";
     "ah"; "bahl"; "zep"; "druhl"; "flem"; "behil"; "arek"; "mep"; "zihr";
     "grit"; "kona"; "kini"; "ichi"; "niah"; "ogr"; "ooh"; "ighr"; "coph";
     "swerr"; "mihln"; "poxi" |]

let max_syllabes = Array.length syllabes

let gems =
  [| "diamond"; "stibotantalite"; "lapi-lazuli"; "ruby"; "emerald";
     "sapphire"; "amethyst"; "quartz"; "tiger-eye"; "opal"; "agate";
     "turquoise"; "pearl"; "garnet" |]

let wand_materials =
  [| "steel"; "bronze"; "gold"; "silver"; "copper"; "nickel"; "cobalt"; "tin";
     "iron"; "magnesium"; "chrome"; "carbon"; "platinum"; "silicon";
     "titanium"; "teak"; "oak"; "cherry"; "birch"; "pine"; "cedar"; "redwood";
     "balsa"; "ivory"; "walnut"; "maple"; "mahogany"; "elm"; "palm";
     "wooden" |]

let max_metal =
  let rec loop_i i =
    if wand_materials.(i) = "titanium" then i else loop_i (i + 1)
  in
  loop_i 0

let my_int_of_string s =
  let (i, sign) =
    if 0 < String.length s && s.[0] = '-' then 1, -1 else 0, 1
  in
  let rec loop_i n i =
    if i = String.length s then sign * n
    else
      match s.[i] with
        '0'..'9' -> loop_i (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
      | _ -> failwith "int_of_string"
  in
  loop_i 0 i

type args =
  { mutable arg_lang : string option;
    mutable arg_seed : int option;
    mutable arg_robot_player : string option;
    mutable arg_no_handle_robot : bool;
    mutable arg_batch : bool;
    mutable arg_score_only : bool;
    mutable arg_backup : string option;
    mutable arg_rest_file : string }

let do_args argv =
  let args =
    {arg_lang = None; arg_seed = None; arg_robot_player = None;
     arg_no_handle_robot = false; arg_batch = false; arg_score_only = false;
     arg_backup = None; arg_rest_file = ""}
  in
  let rec loop_i i =
    if i < Array.length argv then
      if argv.(i) = "-lang" && i + 1 < Array.length argv then
        begin args.arg_lang <- Some argv.(i+1); loop_i (i + 2) end
      else if argv.(i) = "-seed" && i + 1 < Array.length argv then
        begin
          args.arg_seed <- Some (my_int_of_string argv.(i+1));
          loop_i (i + 2)
        end
      else if argv.(i) = "-backup" && i + 1 < Array.length argv then
        begin args.arg_backup <- Some argv.(i+1); loop_i (i + 2) end
      else if argv.(i) = "-r" && i + 1 < Array.length argv then
        begin args.arg_robot_player <- Some argv.(i+1); loop_i (i + 2) end
      else if argv.(i) = "-nhr" then
        begin args.arg_no_handle_robot <- true; loop_i (i + 1) end
      else if argv.(i) = "-b" then
        begin args.arg_batch <- true; loop_i (i + 1) end
      else if argv.(i) = "-s" then
        begin args.arg_score_only <- true; loop_i (i + 1) end
      else if argv.(i).[0] = '-' then
        begin
          eprintf "Unknown option %s\n" argv.(i);
          eprintf "\
Usage: %s [args] [restore_file]
  -r <param>    Robot player (<param> is either socket file or speed)
  -nhr          No handle robot (in case of robot exception)
  -s            Score only
  -lang <lang>  Displaying language\n"
            argv.(0);
          exit 2
        end
      else begin args.arg_rest_file <- argv.(i); loop_i (i + 1) end
    else args
  in
  loop_i 1

let start_with s i t = Imisc.string_eq s i t 0 (String.length t)

let env_get_value s i =
  let rec loop_j j =
    if j = String.length s then String.sub s i (j - i), j
    else if s.[j] = ',' then String.sub s i (j - i), j
    else loop_j (j + 1)
  in
  loop_j i

type opts =
  { mutable opt_fruit : string;
    mutable opt_save_file : string;
    mutable opt_jump : bool;
    mutable opt_nick_name : string;
    mutable opt_ask_quit : bool;
    mutable opt_show_skull : bool;
    mutable opt_fast : bool }

let do_opts () =
  let opts =
    {opt_fruit = ""; opt_save_file = ""; opt_jump = true; opt_nick_name = "";
     opt_ask_quit = true; opt_show_skull = false; opt_fast = false}
  in
  match try Some (Sys.getenv "ROGUEOPTS") with Not_found -> None with
    Some eptr ->
      let rec loop_i i =
        if i < String.length eptr then
          if eptr.[i] = ' ' then loop_i (i + 1)
          else
            let i =
              if start_with eptr i "fruit=" then
                let i = i + String.length "fruit=" in
                let (v, i) = env_get_value eptr i in opts.opt_fruit <- v; i
              else if start_with eptr i "file=" then
                let i = i + String.length "file=" in
                let (v, i) = env_get_value eptr i in
                opts.opt_save_file <- v; i
              else if start_with eptr i "nojump" then
                let i = i + String.length "nojump" in
                opts.opt_jump <- false; i
              else if start_with eptr i "name=" then
                let i = i + String.length "name=" in
                let (v, i) = env_get_value eptr i in
                opts.opt_nick_name <- v; i
              else if start_with eptr i "noaskquit" then
                let i = i + String.length "noaskquit" in
                opts.opt_ask_quit <- false; i
              else if start_with eptr i "noskull" then
                let i = i + String.length "noskull" in
                opts.opt_show_skull <- false; i
              else if start_with eptr i "notomb" then
                let i = i + String.length "notomb" in
                opts.opt_show_skull <- false; i
              else if start_with eptr i "fast" then
                let i = i + String.length "fast" in opts.opt_fast <- true; i
              else i + 1
            in
            let i =
              if i < String.length eptr && eptr.[i] = ',' then i + 1 else i
            in
            loop_i i
        else opts
      in
      loop_i 0
  | None -> opts

let mix_colours g =
  let len = Array.length Object.colours in
  let mix = Array.init len (fun i -> i) in
  for i = 0 to 31 do
    let j = Imisc.get_rand 0 (len - 1) in
    let k = Imisc.get_rand 0 (len - 1) in
    let t = mix.(j) in mix.(j) <- mix.(k); mix.(k) <- t
  done;
  for i = 0 to Array.length Object.potion_tab - 1 do
    g.id_potions.(i) <- Unidentified Object.colours.(mix.(i))
  done

let get_wand_and_ring_materials g =
  let used = Array.make (Array.length wand_materials) false in
  for i = 0 to Array.length Object.wand_tab - 1 do
    let j =
      let rec loop () =
        let j = Imisc.get_rand 0 (Array.length wand_materials - 1) in
        if used.(j) then loop () else j
      in
      loop ()
    in
    used.(j) <- true;
    g.id_wands.(i) <- Unidentified wand_materials.(j);
    g.is_wood.(i) <- j > max_metal
  done;
  let used = Array.make (Array.length gems) false in
  for i = 0 to Array.length Object.ring_tab - 1 do
    let j =
      let rec loop () =
        let j = Imisc.get_rand 0 (Array.length gems - 1) in
        if used.(j) then loop () else j
      in
      loop ()
    in
    used.(j) <- true; g.id_rings.(i) <- Unidentified gems.(j)
  done

let make_scroll_titles g =
  for i = 0 to Array.length Object.scroll_tab - 1 do
    let sylls = Imisc.get_rand 2 5 in
    let title =
      let rec loop t j =
        if j = sylls then t
        else
          let s = Imisc.get_rand 1 max_syllabes - 1 in
          let syll = syllabes.(s) in
          loop (if t = "" then syll else t ^ " " ^ syll) (j + 1)
      in
      loop "" 0
    in
    g.id_scrolls.(i) <- Unidentified (sprintf "'%s'" title)
  done

let player_init g =
  g.rogue.pack <- [];
  let _ =
    (let ob = Object.get_food (Some Ration) in Imisc.add_to_pack g ob :
     char * objet)
  in
  let a =
    {ar_kind = Ringmail; ar_class = 3; ar_is_cursed = false;
     ar_is_protected = false; ar_enchant = 1; ar_in_use = false;
     ar_identified = false}
  in
  let (c, _) =
    let ob = Object.create_obj (Armor a) 1 in Imisc.add_to_pack g ob
  in
  Imisc.do_wear g c a;
  let w =
    {we_kind = Mace; we_damage = 2, 3, None; we_quiver = 0;
     we_is_cursed = false; we_has_been_uncursed = false; we_hit_enchant = 1;
     we_d_enchant = 1; we_in_use = false; we_identified = true}
  in
  let (c, _) =
    let ob = Object.create_obj (Weapon w) 1 in Imisc.add_to_pack g ob
  in
  Imisc.do_wield g c w;
  let w =
    {we_kind = Bow; we_damage = 1, 2, None; we_quiver = 0;
     we_is_cursed = false; we_has_been_uncursed = false; we_hit_enchant = 1;
     we_d_enchant = 0; we_in_use = false; we_identified = true}
  in
  let _ =
    (let ob = Object.create_obj (Weapon w) 1 in Imisc.add_to_pack g ob :
     char * objet)
  in
  let w =
    {we_kind = Arrow; we_damage = 1, 2, None; we_quiver = 1;
     we_is_cursed = false; we_has_been_uncursed = false; we_hit_enchant = 0;
     we_d_enchant = 0; we_in_use = false; we_identified = true}
  in
  let _ =
    (let q = Imisc.get_rand 25 35 in
     let ob = Object.create_obj (Weapon w) q in Imisc.add_to_pack g ob :
     char * objet)
  in
  ()

let create_g saved_uid true_uid login_name args opts lang =
  let empty_room _ =
    {bottom_row = 0; right_col = 0; left_col = 0; top_row = 0;
     doors = Array.make 4 None; is_room = 0}
  in
  let rogue =
    {armor = None; weapon = None; gold = 0; hp_current = _INIT_HP;
     hp_max = _INIT_HP; extra_hp = 0; less_hp = 0; str_current = 16;
     str_max = 16; exp = 1; exp_points = 0; pack = []; row = 0; col = 0;
     fight_monster = None; moves_left = 1250; confused = 0; blind = 0;
     halluc = 0; see_invisible = false; detect_monster = false; levitate = 0;
     haste_self = 0; bear_trap = 0; being_held = false; stealthy = 0;
     left_ring = None; right_ring = None; e_rings = 0; r_rings = 0;
     r_teleport = false; r_see_invisible = false; maintain_armor = false;
     sustain_strength = false; add_strength = 0; regeneration = 0;
     ring_exp = 0; auto_search = 0; fchar = '@'}
  in
  let fruit =
    if opts.opt_fruit <> "" then opts.opt_fruit else Object.default_fruit
  in
  let env = Efield.make () in
  {saved_uid = saved_uid; true_uid = true_uid; cur_level = 0; max_level = 1;
   cur_room = None; lang = lang; score_only = args.arg_score_only;
   save_file = opts.opt_save_file; nick_name = opts.opt_nick_name;
   login_name = login_name; fruit = fruit; ask_quit = opts.opt_ask_quit;
   show_skull = opts.opt_show_skull; jump = opts.opt_jump; party_counter = 0;
   party_room = None; foods = 0; r_de = None; trap_door = false;
   interrupted = false; can_int = false; reg_search = false;
   level_objects = []; level_monsters = []; new_level_message = "";
   monsters_count = 0; mon_disappeared = false; hunger_str = "";
   hit_message = ""; msg_line = ""; msg_col = 0; msg_cleared = true;
   same_msg = 0; m_moves = 0; wizard = false;
   experimented_pick_up_scare_monster = false; rogue = rogue;
   random_rooms = [| 3; 7; 5; 2; 0; 6; 1; 4; 8 |];
   id_potions = Array.make (Array.length Object.potion_tab) Identified;
   id_rings = Array.make (Array.length Object.ring_tab) Identified;
   id_scrolls = Array.make (Array.length Object.scroll_tab) Identified;
   id_wands = Array.make (Array.length Object.wand_tab) Identified;
   is_wood = Array.make (Array.length Object.wand_tab) false;
   rooms = Array.init _MAXROOMS empty_room;
   traps = Array.make _MAX_TRAPS None;
   dungeon = Array.make_matrix _DROWS _DCOLS 0; env = env}

type init =
    NewGame of game
  | RestoreGame of string
  | ScoreOnly

let robot_env nhr =
  function
    Some str ->
      let locrob =
        if str = "" then Some ""
        else
          match str.[0] with
            '0'..'9' -> Some str
          | _ -> None
      in
      begin match locrob with
        Some str -> Some (PSrobot (Robot.make str), nhr)
      | None -> Some (PSsocket (Rogbotio.socket str), nhr)
      end
  | None -> None

let backup_env =
  function
    Some str -> Some (str, 0)
  | None -> None

let f argv =
  let saved_uid = Unix.geteuid () in
  let true_uid = Unix.getuid () in
  Unix.setuid true_uid;
  let login_name = (Unix.getpwuid (Unix.getuid ())).Unix.pw_name in
  let args = do_args argv in
  begin match args.arg_seed with
    Some seed -> Random.init seed
  | None -> Random.self_init ()
  end;
  let opts = do_opts () in
  let lang =
    match args.arg_lang with
      Some lang -> lang
    | None -> try Sys.getenv "LANG" with Not_found -> ""
  in
  let r =
    if args.arg_score_only then ScoreOnly
    else if args.arg_rest_file <> "" then RestoreGame args.arg_rest_file
    else
      begin
        if not opts.opt_fast then
          begin
            printf
              (ftransl lang
                 "Hello %s, just a moment while I dig the dungeon...")
              (if opts.opt_nick_name <> "" then opts.opt_nick_name
               else login_name);
            flush stdout;
            Unix.sleep 2
          end;
        let g = create_g saved_uid true_uid login_name args opts lang in
        mix_colours g;
        get_wand_and_ring_materials g;
        make_scroll_titles g;
        player_init g;
        g.party_counter <- Imisc.get_rand 1 _PARTY_TIME;
        NewGame g
      end
  in
  let no_handle_robot = args.arg_no_handle_robot || args.arg_batch in
  let robenv = robot_env no_handle_robot args.arg_robot_player in
  let backupenv = backup_env args.arg_backup in
  let fast = opts.opt_fast || args.arg_batch in
  let batch = args.arg_batch in
  let no_record_score = args.arg_seed <> None || args.arg_backup <> None in
  lang, r, robenv, backupenv, fast, batch, no_record_score
