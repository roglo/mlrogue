(* $Id: rogbotio.ml,v 1.6 2018/04/26 09:52:37 deraugla Exp $ *)

#open "printf";;

let string_create = string__create_string;;
let string_set = string__set_nth_char;;
let string_of_bytes (s : string) = s;;
let string_to_bytes (s : string) = s;;

let is_socket_file name =
  let stats = unix__lstat name in stats.unix__st_kind = unix__S_SOCK
;;

let socket str =
  let addr =
    try unix__ADDR_INET (unix__inet_addr_any, int_of_string str) with
      Failure _ ->
        if Sys.file_exists str then
          if is_socket_file str then unix__unlink str
          else failwith (sprintf "error: file \'%s\' exists." str);
        unix__ADDR_UNIX str
  in
  let s = unix__socket (unix__domain_of_sockaddr addr) unix__SOCK_STREAM 0 in
  begin try
    unix__setsockopt s unix__SO_REUSEADDR true;
    unix__bind s addr;
    unix__listen s 1
  with e -> unix__close s; raise e
  end;
  eprintf "Waiting for socket connection...\n";
  flush stderr;
  let (s2, addr) = unix__accept s in unix__close s; s2
;;

let rogbot_magic = "RGBT0001";;

let getchar nrow ncol s =
  let txt = sprintf "%s\n" rogbot_magic in
  let _ : int = unix__write s (string_to_bytes txt) 0 (String.length txt) in
  let txt = sprintf "%d\n" nrow in
  let _ : int = unix__write s (string_to_bytes txt) 0 (String.length txt) in
  let txt = sprintf "%d\n" ncol in
  let _ : int = unix__write s (string_to_bytes txt) 0 (String.length txt) in
  let line = string_create ncol in
  for row = 0 to nrow - 1 do
    for col = 0 to ncol - 1 do
      string_set line col (Curses.mvinch row col)
    done;
    let txt = sprintf "%s\n" (string_of_bytes line) in
    let _ : int = unix__write s (string_to_bytes txt) 0 (String.length txt) in
    ()
  done;
  let b = " " in let _ : int = unix__read s (string_to_bytes b) 0 1 in b.[0]
;;

