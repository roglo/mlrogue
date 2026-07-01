let random_self_init () = random__init (unix__time ());;
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
