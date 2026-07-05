let random_get_state () = 1;;

let char_code = char__int_of_char;;
let char_chr = char__char_of_int;;
let char_escaped = char__char_for_read;;

let string_escaped = string__string_for_read;;

let rec string_concat sep sl =
  match sl with
    [] -> ""
  | [s] -> s
  | s :: sl -> s ^ sep ^ string_concat sep sl
;;

let rec list_remove_assoc a l =
  match l with
    [] -> []
  | (b, c) :: l -> if a = b then l else (b, c) :: list_remove_assoc a l
;;

type buffer == (string * int) ref;;

let buffer_create len = ref (string__create_string len, 0);;

let buffer_add_char b c =
  let (s, i) = !b in
  let s =
    if i < string__string_length s then s
    else s ^ string__create_string (string__string_length s)
  in
  s.[i] <- c; b := (s, i + 1)
;;

let buffer_add_string b s =
  for i = 0 to string__string_length s - 1 do
    buffer_add_char b (string__nth_char s i)
  done
;;

let buffer_contents b =
  let (s, i) = !b in
  string__sub_string s 0 i
;;
