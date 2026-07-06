let int_of_digit c = char__int_of_char c - char__int_of_char `0`;;

let rec parse_uint_loop n =
  function
    [< '`0`..`9` as c; (parse_uint_loop (10 * n + int_of_digit c)) m >] -> m
  | [< >] -> n
;;

let parse_uint =
  function
  | [< '`0`..`9` as c; (parse_uint_loop (int_of_digit c)) m >] -> m
;;

let parse_int =
  function
  | [< '`-`; parse_uint i >] -> -i
  | [< parse_uint i >] -> i
;;

let parse_id_char =
  function [< '`a`..`z` | `A`..`Z` as c >] -> c
;;

let rec parse_id_cont =
  function
    [< '`a`..`z` | `A`..`Z` | `_` | `0`..`9` as c; parse_id_cont s >] ->
      string__make_string 1 c ^ s
  | [< >] -> ""
;;

let parse_id =
  function [< parse_id_char c; parse_id_cont s >] -> string__make_string 1 c ^ s
;;

let parse_uid =
  function
    | [< '`A`..`Z` as c; parse_id_cont s >] -> string__make_string 1 c ^ s
;;

let rec parse_spaces =
  function
  | [< '` `; parse_spaces () >] -> ()
  | [< >] -> ()
;;

let rec parse_non_spaces =
  function
    [< stream__end_of_stream () >] -> ""
  | [< stream__stream_get (c, _); strm >] ->
      if c = ` ` then ""
      else begin
        let (_ : char) = stream__stream_next strm in
        string__make_string 1 c ^ parse_non_spaces strm
      end
;;
