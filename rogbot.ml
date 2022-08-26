(* $Id: rogbot.ml,v 1.13 2018/04/26 09:52:37 deraugla Exp $ *)

open Printf;

value string_make = Bytes.make;
value string_create = Bytes.create;
value string_get = Bytes.get;
value string_length = Bytes.length;
value string_copy = Bytes.copy;
value string_of_bytes = Bytes.to_string;
value string_to_bytes = Bytes.of_string;

value home () = printf "\027[H";
value clear_scr () = printf "\027[J";

value single_unix_read s b ofs len =
  loop 0 ofs len where rec loop nrd ofs len =
    match
      try Some (Unix.read s b ofs len) with
      [ Unix.Unix_error Unix.ECONNRESET _ _ -> None ]
    with
    [ Some 0 | None -> 0
    | Some n ->
        if n = len then nrd + n
        else loop (nrd + n) (ofs + n) (len - n) ]
;

value rogbot_magic = "RGBT0001";

value check_magic s = do {
  let buff = string_make (String.length rogbot_magic) ' ' in
  let len = single_unix_read s buff 0 (string_length buff) in
  if len = 0 then False
  else do {
    let buff = string_of_bytes buff in
    if buff = rogbot_magic then True
    else if String.sub buff 0 4 = String.sub rogbot_magic 0 4 then
      failwith
        (sprintf "rogbot magic incompatible version (%s instead of %s)"
           buff rogbot_magic)
    else failwith "bad rogbot magic"
  }
};

value skip_newline s = do {
  let buff = string_to_bytes " " in
  let _ : int = single_unix_read s buff 0 1 in
  if string_of_bytes buff = "\n" then ()
  else failwith "newline expected in protocol"
};

value input_int s = do {
  let buff = string_to_bytes " " in
  loop 0 where rec loop n =
    let _ : int = single_unix_read s buff 0 1 in
    match string_get buff 0 with
    [ '0'..'9' -> loop (10 * n + Char.code (string_get buff 0) - Char.code '0')
    | '\n' -> n
    | c ->
        failwith
          (sprintf "unexpected char '%s' in protocol" (Char.escaped c)) ]
};

value input_dungeon s = do {
  if check_magic s then do {
    skip_newline s;
    let nrows = input_int s in
    let ncols = input_int s in
    let buff = string_create ncols in
    let a =
      Array.init nrows
        (fun row ->
           let len = single_unix_read s buff 0 ncols in
           if len <> ncols then failwith "bad dungeon line in protocol"
           else do {
             skip_newline s;
             string_copy buff
           })
    in
    Some a
  }
  else
    None
};

value rec play_loop info s = do {
  match input_dungeon s with
  [ Some tab -> do {
      home ();
      let nrow = Array.length tab in
      let ncol = string_length tab.(0) in
      for row = 0 to nrow - 1 do {
        printf "%s" (string_of_bytes tab.(row));
        if row <> nrow - 1 then printf "\n" else ();
      };
      flush stdout;
      let stab = Array.init nrow (fun i -> string_of_bytes tab.(i)) in
      let (ch, info) = Robot.play stab nrow ncol info in
      let _ : int = Unix.write s (string_make 1 ch) 0 1 in
      play_loop info s
    }
  | None -> () ]
};

value arg_addr = ref None;
value arg_speed = ref "";

value speclist =
  Arg.align
    [("-speed", Arg.Set_string arg_speed, "<num> Speed (default: 1.0)")]
;
value anonfun s = arg_addr.val := Some s;
value usage = "Usage: " ^ Sys.argv.(0) ^ " [option]... <addr>\n\nOptions:";

value main () = do {
  Arg.parse speclist anonfun usage;
  let addr =
    match arg_addr.val with
    [ Some str -> Unix.ADDR_UNIX str
    | None -> do {
        eprintf "missing addr; type -help for usage\n";
        flush stderr;
        exit 2
      } ]
  in
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  loop () where rec loop () =
     try Unix.connect s addr with
     [ Unix.Unix_error (Unix.ECONNREFUSED | Unix.ENOENT) _ _ -> do {
         Unix.sleep 1;
         loop ()
       } ];
  home ();
  clear_scr ();
  let rob = Robot.make arg_speed.val in
  try play_loop rob s with
  [ e -> do {
      Unix.close s;
      raise e;
    } ]
  ;
  Unix.close s
};

main ();
