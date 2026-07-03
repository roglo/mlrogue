type error =
  | Unknown of string
;;

exception Stop of error;;

let help_action () = raise (Stop (Unknown "-help"));;

let add_help speclist =
  speclist @
    [("--help", Unit help_action, " Display this list of options")]
;;

let rec second_word s =
  let len = string__string_length s in
  let rec loop n =
    if n >= len then len
    else if s.[n] = ` ` then loop (n+1)
    else n
  in
  try loop (string__index_char s ` `)
  with Not_found -> len
;;

let max_arg_len cur (kwd, _, doc) =
  max cur (string__string_length kwd + second_word doc)
;;

let add_padding len ksd =
  match ksd with
  | (kwd, spec, msg) ->
      let cutcol = second_word msg in
      let spaces =
        string__make_string (len - string__string_length kwd - cutcol) ` `
      in
      let prefix1 = string__sub_string msg 0 cutcol in
      let suffix =
        string__sub_string msg cutcol (string__string_length msg - cutcol)
      in
      (kwd, spec, prefix1 ^ spaces ^ suffix)
;;

let align speclist =
  let completed = add_help speclist in
  let len = list__it_list max_arg_len 0 completed in
  list__map (add_padding len) completed
;;
