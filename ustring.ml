type t == string;;

let of_string s = s;;
let to_string s = s;;
let is_empty s = s = "";;

let last_char s = if s = "" then `\000` else s.[string__string_length s - 1];;

let length s =
  let rec loop len i =
    if i = string__string_length s then len
    else if char__int_of_char s.[i] < 128 then loop (len + 1) (i + 1)
    else if char__int_of_char s.[i] land 64 <> 0 then loop (len + 1) (i + 1)
    else loop len (i + 1)
  in
  loop 0 0
;;

let but_last buf =
  let rec loop i =
    if i < 0 then ""
    else if char__int_of_char buf.[i] < 128 then string__sub_string buf 0 i
    else if char__int_of_char buf.[i] land 64 <> 0 then string__sub_string buf 0 i
    else loop (i - 1)
  in
  loop (string__string_length buf - 1)
;;

let append_char buf c = buf ^ string__make_string 1 c;;
