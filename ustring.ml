type t = string

let of_string s = s
let to_string s = s
let is_empty s = s = ""

let last_char s = if s = "" then '\000' else s.[String.length s - 1]

let length s =
  let rec loop len i =
    if i = String.length s then len
    else if Char.code s.[i] < 128 then loop (len + 1) (i + 1)
    else if Char.code s.[i] land 64 <> 0 then loop (len + 1) (i + 1)
    else loop len (i + 1)
  in
  loop 0 0

let but_last buf =
  let rec loop i =
    if i < 0 then ""
    else if Char.code buf.[i] < 128 then String.sub buf 0 i
    else if Char.code buf.[i] land 64 <> 0 then String.sub buf 0 i
    else loop (i - 1)
  in
  loop (String.length buf - 1)

let append_char buf c = buf ^ String.make 1 c
