(* $Id: curses.ml,v 1.60 2018/04/26 09:52:36 deraugla Exp $ *)

#open "printf";;

type utf8 = { utf8_v : string };;

type data =
  { mutable max_row : int;
    mutable max_col : int;
    mutable crow : int;
    mutable ccol : int;
    mutable nrow : int;
    mutable ncol : int;
    mutable bcur : utf8 vect vect;
    mutable bnew : utf8 vect vect;
    mutable acur : attr vect vect;
    mutable anew : attr vect vect;
    mutable attr_set : attr;
    mutable cur_attr : attr;
    mutable no_output : bool }
and attr =
  { a_standout : bool; a_bold : bool; a_back_col : int; a_fore_col : int }
;;

let utf8_of_char c = {utf8_v = string__make_string 1 c};;
let utf8_to_char u =
  if string__string_length u.utf8_v = 1 then u.utf8_v.[0]
  else invalid_arg "utf8_to_char"
;;

let string_make = string__make_string;;
let string_get = vect__vect_item;;
let string_set = vect__vect_assign;;
let string_sub = string__sub_string;;
let string_length = vect__vect_length;;
let string_fill = string__fill_string;;
let string_contains s c =
  try let _ = string__index_char s c in true with Not_found -> false
;;
let string_of_bytes (s : string) = s;;
let string_to_bytes (s : string) = s;;

let no_attr =
  {a_standout = false; a_bold = false; a_back_col = -1; a_fore_col = -1}
;;

let d =
  {max_row = 0; max_col = 0; crow = 0; ccol = 0; nrow = 0; ncol = 0;
   bcur = [| |]; bnew = [| |]; acur = [| |]; anew = [| |]; attr_set = no_attr;
   cur_attr = no_attr; no_output = false}
;;

let no_output () = d.no_output <- true;;

let check row col =
  row >= 0 && row < d.max_row && col >= 0 && col < d.max_col
;;

let set_attr a =
  if a <> d.cur_attr then
    begin
      if d.no_output then ()
      else
        begin
          if d.cur_attr.a_bold && not a.a_bold ||
             (d.cur_attr.a_fore_col <> a.a_fore_col ||
              d.cur_attr.a_back_col <> a.a_back_col) &&
             (a.a_fore_col = -1 || a.a_back_col = -1)
          then
            printf "\027[m";
          if not d.cur_attr.a_standout && a.a_standout then printf "\027[7m"
          else if d.cur_attr.a_standout && not a.a_standout then
            printf "\027[27m";
          if d.cur_attr.a_fore_col <> a.a_fore_col ||
             d.cur_attr.a_back_col <> a.a_back_col
          then
            begin
              if a.a_fore_col <> -1 then printf "\027[3%dm" a.a_fore_col;
              if a.a_back_col <> -1 then printf "\027[4%dm" a.a_back_col
            end;
          if not d.cur_attr.a_bold && a.a_bold then printf "\027[1m"
        end;
      d.cur_attr <- a
    end
;;

let utf8_to_string u = u.utf8_v;;

let utf8_of_substring s i =
  if i >= string__string_length s then
    failwith (printf__sprintf "utf8_of_substring \"%s\" %d" s i)
  else if char__int_of_char s.[i] land 0x80 = 0 then utf8_of_char s.[i], i + 1
  else if char__int_of_char s.[i] land 0x40 = 0 then
    failwith (printf__sprintf "utf8_of_substring \"%s\" %d, bad utf8" s i)
  else if char__int_of_char s.[i] land 0x20 = 0 then
    if i + 1 >= string__string_length s then failwith "utf8_of_substring error"
    else {utf8_v = string__sub_string s i 2}, i + 2
  else if char__int_of_char s.[i] land 0x10 = 0 then
    if i + 2 >= string__string_length s then failwith "utf8_of_substring error"
    else {utf8_v = string__sub_string s i 3}, i + 3
  else if char__int_of_char s.[i] land 0x08 = 0 then
    if i + 3 >= string__string_length s then failwith "utf8_of_substring error"
    else {utf8_v = string__sub_string s i 4}, i + 4
  else
    failwith
      (printf__sprintf "utf8_of_substring case not impl 0x%0x"
         (char__int_of_char s.[i]))
;;

let print_encode_char c =
  if d.no_output then () else print_string (utf8_to_string c)
;;

let cprint_string s = if d.no_output then () else print_string s;;

let update (c : 'a vect) (n : 'b vect) ac an i jbeg j =
  if i = d.crow && jbeg = d.ccol then ()
  else if i = d.crow && jbeg = d.ccol - 1 then cprint_string "\b"
  else if i = d.crow && jbeg = d.ccol + 1 then
    begin set_attr an.(d.ccol); print_encode_char (string_get n d.ccol) end
  else if d.no_output then ()
  else printf "\027[%d;%dH" (i + 1) (jbeg + 1);
  if jbeg = j - 1 then
    begin
      set_attr an.(jbeg);
      print_encode_char (string_get n jbeg);
      string_set c jbeg (string_get n jbeg);
      ac.(jbeg) <- an.(jbeg)
    end
  else
    begin let same_attr =
      let rec loop k =
        if k = j then true
        else if an.(k) = an.(jbeg) then loop (k + 1)
        else false
      in
      loop jbeg
    in
      if same_attr then
        begin
          set_attr an.(jbeg);
          for k = jbeg to j - 1 do print_encode_char (string_get n k) done
        end
      else
        for k = jbeg to j - 1 do
          set_attr an.(k);
          print_encode_char (string_get n k)
        done;
      for k = jbeg to j - 1 do
        string_set c k (string_get n k);
        ac.(k) <- an.(k)
      done
    end;
  d.crow <- i;
  d.ccol <- j
;;

let rec gap_equal k c n j =
  if k = 0 || j >= string_length c then false
  else if string_get c j <> string_get n j then true
  else gap_equal (k - 1) c n (j + 1)
;;

let cflush () =
  for i = 0 to vect__vect_length d.bcur - 1 do
    let c = d.bcur.(i) in
    let n = d.bnew.(i) in
    let ac = d.acur.(i) in
    let an = d.anew.(i) in
    if c <> n || ac <> an then
      let len = string_length c in
      let rec loop_j jbeg j =
        if j = len then (if jbeg < j then update c n ac an i jbeg j)
        else if string_get c j <> string_get n j || ac.(j) <> an.(j) then
          loop_j jbeg (j + 1)
        else if jbeg < j then
          if j + 1 < len && gap_equal 8 c n (j + 1) then loop_j jbeg (j + 1)
          else begin update c n ac an i jbeg j; loop_j (j + 1) (j + 1) end
        else loop_j (j + 1) (j + 1)
      in
      loop_j 0 0
  done;
  if not (check d.nrow d.ncol) then ()
  else if d.crow <> d.nrow || d.ccol <> d.ncol then
    begin
      if d.crow = d.nrow && d.ccol < d.max_col then
        let n = d.bnew.(d.nrow) in
        if d.ccol = d.ncol + 1 then cprint_string "\b"
        else if d.ccol = d.ncol + 2 then cprint_string "\b\b"
        else if d.ccol = d.ncol + 3 then cprint_string "\b\b\b"
        else if d.ccol = d.ncol + 4 then cprint_string "\b\b\b\b"
        else if d.ccol = d.ncol + 5 then cprint_string "\b\b\b\b\b"
        else if d.ccol = d.ncol - 1 then
          begin
            set_attr d.anew.(d.crow).(d.ccol);
            print_encode_char (string_get n d.ccol)
          end
        else if d.ccol = d.ncol - 2 then
          begin
            set_attr d.anew.(d.crow).(d.ccol);
            print_encode_char (string_get n d.ccol);
            set_attr d.anew.(d.crow).(d.ccol + 1);
            print_encode_char (string_get n (d.ccol + 1))
          end
        else if d.no_output then ()
        else printf "\027[%d;%dH" (d.nrow + 1) (d.ncol + 1)
      else
        begin
          set_attr d.anew.(d.nrow).(d.ncol);
          if d.no_output then ()
          else printf "\027[%d;%dH" (d.nrow + 1) (d.ncol + 1)
        end;
      d.crow <- d.nrow;
      d.ccol <- d.ncol
    end
;;

(* *)

let adduch c =
  if check d.nrow d.ncol then
    begin
      string_set d.bnew.(d.nrow) d.ncol c;
      d.anew.(d.nrow).(d.ncol) <- d.attr_set
    end;
  d.ncol <- d.ncol + 1
;;

let addch c = adduch (utf8_of_char c);;

let addstr s =
  let rec loop i =
    if i = string__string_length s then ()
    else let (c, i) = utf8_of_substring s i in adduch c; loop i
  in
  loop 0
;;

let attroff al =
  list__do_list
    (function
       A_standout ->
         d.attr_set <-
           {a_standout = false; a_bold = d.attr_set.a_bold;
            a_back_col = d.attr_set.a_back_col;
            a_fore_col = d.attr_set.a_fore_col}
     | A_bold ->
         d.attr_set <-
           {a_bold = false; a_standout = d.attr_set.a_standout;
            a_back_col = d.attr_set.a_back_col;
            a_fore_col = d.attr_set.a_fore_col})
    al
;;

let attron al =
  list__do_list
    (function
       A_standout ->
         d.attr_set <-
           {a_standout = true; a_bold = d.attr_set.a_bold;
            a_back_col = d.attr_set.a_back_col;
            a_fore_col = d.attr_set.a_fore_col}
     | A_bold ->
         d.attr_set <-
           {a_standout = true; a_bold = d.attr_set.a_bold;
            a_back_col = d.attr_set.a_back_col;
            a_fore_col = d.attr_set.a_fore_col})
    al
;;

let vt_device_status_report = "\027[6n";;
let vt_erase_in_display = "\027[J";;
let vt_erase_line_from_cursor = "\027[K";;

let utf8_sp = utf8_of_char ` `;;

let clear () =
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to vect__vect_length d.bcur - 1 do
    vect__fill_vect d.bcur.(i) 0 (vect__vect_length d.bcur.(i)) utf8_sp;
    vect__fill_vect d.bnew.(i) 0 (vect__vect_length d.bnew.(i)) utf8_sp;
    vect__fill_vect d.acur.(i) 0 (vect__vect_length d.bcur.(i)) no_attr;
    vect__fill_vect d.anew.(i) 0 (vect__vect_length d.bnew.(i)) no_attr
  done;
  d.crow <- 0;
  d.ccol <- 0;
  d.nrow <- 0;
  d.ncol <- 0
;;

let clrtoeol () =
  cflush ();
  cprint_string vt_erase_line_from_cursor;
  if check d.crow d.ccol && check d.nrow d.ncol then
    let s = d.bcur.(d.crow) in
    vect__fill_vect s d.ccol (vect__vect_length s - d.ccol) utf8_sp;
    let s = d.bnew.(d.nrow) in
    vect__fill_vect s d.ccol (vect__vect_length s - d.ncol) utf8_sp;
    let s = d.acur.(d.nrow) in
    vect__fill_vect s d.ccol (vect__vect_length s - d.ncol) no_attr;
    let s = d.anew.(d.nrow) in
    vect__fill_vect s d.ccol (vect__vect_length s - d.ncol) no_attr
;;

let color_set fg bg =
  d.attr_set <-
    {a_fore_col = fg; a_back_col = bg;
     a_standout = d.attr_set.a_standout;
     a_bold = d.attr_set.a_bold}
;;

let color_get i j =
  if check i j then let ac = d.acur.(i).(j) in ac.a_fore_col, ac.a_back_col
  else -1, -1
;;

let home () =
  set_attr no_attr;
  cprint_string "\027[H";
  d.crow <- 0;
  d.ccol <- 0;
  d.nrow <- 0;
  d.ncol <- 0
;;

let lines () = d.max_row;;
let cols () = d.max_col;;

let pos_get () = d.nrow, d.ncol;;

let move row col = d.nrow <- row; d.ncol <- col;;

let mvaddch i j c =
  if check i j then
    begin
      string_set d.bnew.(i) j (utf8_of_char c);
      d.anew.(i).(j) <- d.attr_set
    end;
  d.nrow <- i;
  d.ncol <- j + 1
;;

let mvaddnstr row col s i len =
  d.nrow <- row;
  d.ncol <- col;
  let rec loop j =
    if j = len then ()
    else
      let (uc, k) = utf8_of_substring s (i + j) in
      string_set d.bnew.(d.nrow) d.ncol uc;
      d.anew.(d.nrow).(d.ncol) <- d.attr_set;
      d.ncol <- d.ncol + 1;
      loop (k - i)
  in
  loop 0
;;

let mvaddstr row col s =
  d.nrow <- row;
  d.ncol <- col;
  let rec loop j =
    if j = string__string_length s then ()
    else if check d.nrow d.ncol then
      let (uc, k) = utf8_of_substring s j in
      string_set d.bnew.(d.nrow) d.ncol uc;
      d.anew.(d.nrow).(d.ncol) <- d.attr_set;
      d.ncol <- d.ncol + 1;
      loop k
  in
  loop 0
;;

let mvinch row col =
  d.nrow <- row;
  d.ncol <- col;
  if check row col then
    try utf8_to_char (string_get d.bnew.(row) col) with
      Invalid_argument _ -> ` `
  else ` `
;;

let refresh () = cflush (); flush stdout;;

let standend () = d.attr_set <- no_attr;;
let standout () =
  d.attr_set <-
    {a_standout = true; a_bold = d.attr_set.a_bold;
     a_back_col = d.attr_set.a_back_col;
     a_fore_col = d.attr_set.a_fore_col}
;;

let wrefresh_curscr () =
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to vect__vect_length d.bcur - 1 do
    vect__fill_vect d.bcur.(i) 0 (string_length d.bcur.(i)) utf8_sp
  done;
  d.crow <- 0;
  d.ccol <- 0;
  cflush ();
  flush stdout
;;

let getch () = input_char stdin;;

let tty_fd_and_ini_attr = ref None;;
let tty_fd () =
  match !tty_fd_and_ini_attr with
    Some (fd, _) -> fd
  | None ->
      let fd = unix__open "/dev/tty" [unix__O_RDWR] 0o000 in
      let ini_attr = unix__tcgetattr fd in
      tty_fd_and_ini_attr := Some (fd, ini_attr); fd
;;

let edit_tcio = ref None;;

let set_edit () =
  let tcio =
    match !edit_tcio with
      Some e -> e
    | None ->
        let fd = tty_fd () in
        let tcio = unix__tcgetattr fd in
        tcio.unix__c_echo <- false;
        tcio.unix__c_icanon <- false;
        tcio.unix__c_vmin <- 1;
        tcio.unix__c_isig <- false;
        tcio.unix__c_ixon <- false;
        tcio.unix__c_inlcr <- false;
        tcio.unix__c_icrnl <- false;
        edit_tcio := Some tcio;
        tcio
  in
  let fd = tty_fd () in unix__tcsetattr fd unix__TCSADRAIN tcio
and unset_edit () =
  match !tty_fd_and_ini_attr with
    Some (fd, ini_attr) -> unix__tcsetattr fd unix__TCSADRAIN ini_attr
  | None -> ()
;;

let int_of_digit c = char__int_of_char c - char__int_of_char `0`;;

let rec parse_int_loop n =
  function
    [< '`0`..`9` as c; (parse_int_loop (10 * n + int_of_digit c)) m >] -> m
  | [< >] -> n
;;

let parse_int = parse_int_loop 0;;

let parse_screen_size =
  function
    [< '`\027`; '`[`; parse_int row; '`;`; parse_int col; '`R` >] ->
      (row, col)
;;

let initscr () =
  if d.no_output then begin d.max_row <- 24; d.max_col <- 80 end
  else
    begin let fd = tty_fd () in
      let s = string_to_bytes ("\027[99;99H" ^ vt_device_status_report) in
      let len = unix__write fd s 0 (string__string_length s) in
      if len <> string__string_length s then failwith "Curses.initscr";
      set_edit ();
      let line =
        let buff = string_make 20 ` ` in
        let rec loop_i i =
          let (icl, _, _) = unix__select [fd] [] [] 1.0 in
          if icl = [] then string_sub buff 0 i
          else
            let len = unix__read fd buff i (string__string_length buff - i) in
            if len = 0 || string_contains buff `R` then
              string_sub buff 0 (i + len)
            else loop_i (i + len)
        in
        loop_i 0
      in
      try
        let (row, col) =
          parse_screen_size (stream__stream_of_string (string_of_bytes line))
        in
        d.max_row <- row;
        d.max_col <- col
      with stream__Parse_failure | stream__Parse_error ->
        d.max_row <- 24; d.max_col <- 80
    end;
  d.bcur <- vect__init_vect d.max_row (fun _ -> vect__make_vect d.max_col utf8_sp);
  d.bnew <- vect__init_vect d.max_row (fun _ -> vect__make_vect d.max_col utf8_sp);
  d.acur <- vect__init_vect d.max_row (fun _ -> vect__make_vect d.max_col no_attr);
  d.anew <- vect__init_vect d.max_row (fun _ -> vect__make_vect d.max_col no_attr);
  d.attr_set <- no_attr;
  d.cur_attr <- no_attr;
  clear ()
;;

let endwin () = cflush (); unset_edit (); flush stdout;;
