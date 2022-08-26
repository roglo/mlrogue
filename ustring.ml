type t = string;

value of_string s = s;
value to_string s = s;
value is_empty s = s = "";

value last_char s = if s = "" then '\000' else s.[String.length s - 1];

value length s =
  loop 0 0 where rec loop len i =
    if i = String.length s then len
    else if Char.code s.[i] < 128 then loop (len + 1) (i + 1)
    else if Char.code s.[i] land 64 <> 0 then loop (len + 1) (i + 1)
    else loop len (i + 1)
;

value but_last buf =
  loop (String.length buf - 1) where rec loop i =
    if i < 0 then ""
    else if Char.code buf.[i] < 128 then String.sub buf 0 i
    else if Char.code buf.[i] land 64 <> 0 then String.sub buf 0 i
    else loop (i - 1)
;

value append_char buf c = buf ^ String.make 1 c;
