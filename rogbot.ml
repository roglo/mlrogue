(* $Id: rogbot.ml,v 1.13 2018/04/26 09:52:37 deraugla Exp $ *)

#open "printf";;

let string_make = string__make_string;;
let string_create = string__create_string;;
let string_get = string__nth_char;;
let string_length = string__string_length;;
let string_copy = misc__string_copy;;
let string_of_bytes (s : string) = s;;
let string_to_bytes (s : string) = s;;

let home () = printf "\027[H";;
let clear_scr () = printf "\027[J";;

let single_unix_read s b ofs len =
  let rec loop nrd ofs len =
    match
      try Some (unix__read s b ofs len) with
        unix__Unix_error (unix__ECONNRESET, _, _) -> None
    with
      Some 0 | None -> 0
    | Some n ->
        if n = len then nrd + n else loop (nrd + n) (ofs + n) (len - n)
  in
  loop 0 ofs len
;;

let rogbot_magic = "RGBT0001";;

let check_magic s =
  let buff = string_make (string__length rogbot_magic) ' ' in
  let len = single_unix_read s buff 0 (string_length buff) in
  if len = 0 then false
  else
    let buff = string_of_bytes buff in
    if buff = rogbot_magic then true
    else if string__sub buff 0 4 = string__sub rogbot_magic 0 4 then
      failwith
        (sprintf "rogbot magic incompatible version (%s instead of %s)" buff
           rogbot_magic)
    else failwith "bad rogbot magic"
;;

let skip_newline s =
  let buff = string_to_bytes " " in
  let _ : int = single_unix_read s buff 0 1 in
  if string_of_bytes buff = "\n" then ()
  else failwith "newline expected in protocol"
;;

let input_int s =
  let buff = string_to_bytes " " in
  let rec loop n =
    let _ : int = single_unix_read s buff 0 1 in
    match string_get buff 0 with
      '0'..'9' ->
        loop (10 * n + Char.code (string_get buff 0) - Char.code '0')
    | '\n' -> n
    | c ->
        failwith (sprintf "unexpected char '%s' in protocol" (Char.escaped c))
  in
  loop 0
;;

let input_dungeon s =
  if check_magic s then
    begin
      skip_newline s;
      let nrows = input_int s in
      let ncols = input_int s in
      let buff = string_create ncols in
      let a =
        Array.init nrows
          (fun row ->
             let len = single_unix_read s buff 0 ncols in
             if len <> ncols then failwith "bad dungeon line in protocol"
             else begin skip_newline s; string_copy buff end)
      in
      Some a
    end
  else None
;;

let rec play_loop info s =
  match input_dungeon s with
    Some tab ->
      home ();
      let nrow = Array.length tab in
      let ncol = string_length tab.(0) in
      for row = 0 to nrow - 1 do
        printf "%s" (string_of_bytes tab.(row));
        if row <> nrow - 1 then printf "\n"
      done;
      flush stdout;
      let stab = Array.init nrow (fun i -> string_of_bytes tab.(i)) in
      let (ch, info) = Robot.play stab nrow ncol info in
      let _ : int = unix__write s (string_make 1 ch) 0 1 in play_loop info s
  | None -> ()
;;

let arg_addr = ref None;;
let arg_speed = ref "";;

let speclist =
  Arg.align ["-speed", Arg.Set_string arg_speed, "<num> Speed (default: 1.0)"]
;;
let anonfun s = arg_addr := Some s;;
let usage = "Usage: " ^ Sys.argv.(0) ^ " [option]... <addr>\n\nOptions:";;

let main () =
  Arg.parse speclist anonfun usage;
  let addr =
    match !arg_addr with
      Some str -> unix__ADDR_UNIX str
    | None ->
        eprintf "missing addr; type -help for usage\n"; flush stderr; exit 2
  in
  let s = unix__socket (unix__domain_of_sockaddr addr) unix__SOCK_STREAM 0 in
  begin let rec loop () =
    try unix__connect s addr with
      unix__Unix_error ((unix__ECONNREFUSED | unix__ENOENT), _, _) ->
        unix__sleep 1; loop ()
  in
    loop ()
  end;
  home ();
  clear_scr ();
  let rob = Robot.make !arg_speed in
  (try play_loop rob s with e -> unix__close s; raise e); unix__close s
;;

main ();;
