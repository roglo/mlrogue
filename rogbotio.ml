(* $Id: rogbotio.ml,v 1.6 2018/04/26 09:52:37 deraugla Exp $ *)

#open "printf";;

let string_create = string__create_string;;
let string_set = string__set_nth_char;;
let string_of_bytes (s : string) = s;;
let string_to_bytes (s : string) = s;;

let is_socket_file name =
  let stats = Unix.lstat name in stats.Unix.st_kind = Unix.S_SOCK
;;

let socket str =
  let addr =
    try Unix.ADDR_INET (Unix.inet_addr_any, int_of_string str) with
      Failure _ ->
        if Sys.file_exists str then
          if is_socket_file str then Unix.unlink str
          else failwith (sprintf "error: file \'%s\' exists." str);
        Unix.ADDR_UNIX str
  in
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  begin try
    Unix.setsockopt s Unix.SO_REUSEADDR true;
    Unix.bind s addr;
    Unix.listen s 1
  with e -> Unix.close s; raise e
  end;
  eprintf "Waiting for socket connection...\n";
  flush stderr;
  let (s2, addr) = Unix.accept s in Unix.close s; s2
;;

let rogbot_magic = "RGBT0001";;

let getchar nrow ncol s =
  let txt = sprintf "%s\n" rogbot_magic in
  let _ : int = Unix.write s (string_to_bytes txt) 0 (String.length txt) in
  let txt = sprintf "%d\n" nrow in
  let _ : int = Unix.write s (string_to_bytes txt) 0 (String.length txt) in
  let txt = sprintf "%d\n" ncol in
  let _ : int = Unix.write s (string_to_bytes txt) 0 (String.length txt) in
  let line = string_create ncol in
  for row = 0 to nrow - 1 do
    for col = 0 to ncol - 1 do
      string_set line col (Curses.mvinch row col)
    done;
    let txt = sprintf "%s\n" (string_of_bytes line) in
    let _ : int = Unix.write s (string_to_bytes txt) 0 (String.length txt) in
    ()
  done;
  let b = " " in let _ : int = Unix.read s (string_to_bytes b) 0 1 in b.[0]
;;

