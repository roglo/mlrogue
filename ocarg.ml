#open "printf";;
#open "util";;

type error =
  | Unknown of string
  | Wrong of string * string * string
  | Missing of string
  | Message of string
;;

exception Stop of error;;

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

let help_action () = raise (Stop (Unknown "-help"));;

let add_help speclist =
  speclist @
    [("--help", Unit help_action, " Display this list of options")]
;;

let print_spec buf (key, spec, doc) =
  buffer_add_string buf (sprintf "  %s %s\n" key doc)
;;

let current = ref 0;;

let usage_b buf speclist errmsg =
  buffer_add_string buf (sprintf "%s\n" errmsg);
  list__do_list (print_spec buf) (add_help speclist);
;;

let parse_argv argv speclist anonfun errmsg =
  let l = vect__vect_length argv in
  let b = buffer_create 200 in
  let initpos = !current in
  let stop error =
    let progname = if initpos < l then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          buffer_add_string b (sprintf "%s: unknown option `%s'.\n" progname s)
      | Missing s ->
          buffer_add_string b
            (sprintf "%s: option `%s' needs an argument.\n" progname s)
      | Wrong (opt, arg, expected) ->
          buffer_add_string b
            (sprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
                  progname arg opt expected)
      | Message s ->
          buffer_add_string b (sprintf "%s: %s.\n" progname s)
    end;
    usage_b b speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then raise (Help (buffer_contents b))
    else raise (Bad (buffer_contents b))
  in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    if string__string_length s >= 1 && string__nth_char s 0 = `-` then begin
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        let rec treat_action = function
        | Unit f -> f ();
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (int_of_string arg)
            with Failure "int_of_string" ->
                   raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | _ -> raise (Stop (Missing s))
        in
        treat_action action
      with Bad m -> stop (Message m);
         | Stop e -> stop e;
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;

let parse l f msg =
  try
    parse_argv sys__command_line l f msg;
  with
  | Bad msg -> eprintf "%s" msg; exit 2;
  | Help msg -> printf "%s" msg; exit 0;
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
