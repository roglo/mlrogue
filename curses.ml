(* $Id: curses.ml,v 1.60 2018/04/26 09:52:36 deraugla Exp $ *)

open Printf

type data =
  { mutable max_row : int;
    mutable max_col : int;
    mutable crow : int;
    mutable ccol : int;
    mutable nrow : int;
    mutable ncol : int;
    mutable bcur : bytes array;
    mutable bnew : bytes array;
    mutable acur : attr array array;
    mutable anew : attr array array;
    mutable attr_set : attr;
    mutable cur_attr : attr;
    mutable no_output : bool }
and attr =
  { a_standout : bool; a_bold : bool; a_back_col : int; a_fore_col : int }

type attribute = A_standout | A_bold

let string_make = Bytes.make
let string_get = Bytes.get
let string_set = Bytes.set
let string_sub = Bytes.sub
let string_length = Bytes.length
let string_fill = Bytes.fill
let string_contains = Bytes.contains
let string_of_bytes = Bytes.to_string
let string_to_bytes = Bytes.of_string

let no_attr =
  {a_standout = false; a_bold = false; a_back_col = -1; a_fore_col = -1}

let d =
  {max_row = 0; max_col = 0; crow = 0; ccol = 0; nrow = 0; ncol = 0;
   bcur = [| |]; bnew = [| |]; acur = [| |]; anew = [| |]; attr_set = no_attr;
   cur_attr = no_attr; no_output = false}

let no_output () = d.no_output <- true

let check row col = row >= 0 && row < d.max_row && col >= 0 && col < d.max_col

let tty_fd_and_ini_attr = ref None
let tty_fd () =
  match !tty_fd_and_ini_attr with
    Some (fd, _) -> fd
  | None ->
      let fd = Unix.openfile "/dev/tty" [Unix.O_RDWR] 0o000 in
      let ini_attr = Unix.tcgetattr fd in
      tty_fd_and_ini_attr := Some (fd, ini_attr); fd

let edit_tcio = ref None

let set_edit () =
  let tcio =
    match !edit_tcio with
      Some e -> e
    | None ->
        let fd = tty_fd () in
        let tcio = Unix.tcgetattr fd in
        tcio.Unix.c_echo <- false;
        tcio.Unix.c_icanon <- false;
        tcio.Unix.c_vmin <- 1;
        tcio.Unix.c_isig <- false;
        tcio.Unix.c_ixon <- false;
        tcio.Unix.c_inlcr <- false;
        tcio.Unix.c_icrnl <- false;
        edit_tcio := Some tcio;
        tcio
  in
  let fd = tty_fd () in Unix.tcsetattr fd Unix.TCSADRAIN tcio
and unset_edit () =
  match !tty_fd_and_ini_attr with
    Some (fd, ini_attr) -> Unix.tcsetattr fd Unix.TCSADRAIN ini_attr
  | None -> ()

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

let print_encode_char c = if d.no_output then () else print_char c

let cprint_string s = if d.no_output then () else print_string s

let update c n ac an i jbeg j =
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

let rec gap_equal k c n j =
  if k = 0 || j >= string_length c then false
  else if string_get c j <> string_get n j then true
  else gap_equal (k - 1) c n (j + 1)

let cflush () =
  for i = 0 to Array.length d.bcur - 1 do
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

(* *)

let addch c =
  if check d.nrow d.ncol then
    begin
      string_set d.bnew.(d.nrow) d.ncol c;
      d.anew.(d.nrow).(d.ncol) <- d.attr_set
    end;
  d.ncol <- d.ncol + 1

let addstr s = for i = 0 to String.length s - 1 do addch s.[i] done

let attroff al =
  List.iter
    (function
       A_standout -> d.attr_set <- {d.attr_set with a_standout = false}
     | A_bold -> d.attr_set <- {d.attr_set with a_bold = false})
    al

let attron al =
  List.iter
    (function
       A_standout -> d.attr_set <- {d.attr_set with a_standout = true}
     | A_bold -> d.attr_set <- {d.attr_set with a_bold = true})
    al

let vt_device_status_report = "\027[6n"
let vt_erase_in_display = "\027[J"
let vt_erase_line_from_cursor = "\027[K"

let clear () =
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to Array.length d.bcur - 1 do
    string_fill d.bcur.(i) 0 (string_length d.bcur.(i)) ' ';
    string_fill d.bnew.(i) 0 (string_length d.bnew.(i)) ' ';
    Array.fill d.acur.(i) 0 (string_length d.bcur.(i)) no_attr;
    Array.fill d.anew.(i) 0 (string_length d.bnew.(i)) no_attr
  done;
  d.crow <- 0;
  d.ccol <- 0;
  d.nrow <- 0;
  d.ncol <- 0

let clrtoeol () =
  cflush ();
  cprint_string vt_erase_line_from_cursor;
  if check d.crow d.ccol && check d.nrow d.ncol then
    let s = d.bcur.(d.crow) in
    string_fill s d.ccol (string_length s - d.ccol) ' ';
    let s = d.bnew.(d.nrow) in
    string_fill s d.ccol (string_length s - d.ncol) ' ';
    let s = d.acur.(d.nrow) in
    Array.fill s d.ccol (Array.length s - d.ncol) no_attr;
    let s = d.anew.(d.nrow) in
    Array.fill s d.ccol (Array.length s - d.ncol) no_attr

let color_set fg bg =
  d.attr_set <- {d.attr_set with a_fore_col = fg; a_back_col = bg}

let color_get i j =
  if check i j then let ac = d.acur.(i).(j) in ac.a_fore_col, ac.a_back_col
  else -1, -1

let home () =
  set_attr no_attr;
  cprint_string "\027[H";
  d.crow <- 0;
  d.ccol <- 0;
  d.nrow <- 0;
  d.ncol <- 0

let initscr () =
  if d.no_output then begin d.max_row <- 24; d.max_col <- 80 end
  else
    begin let fd = tty_fd () in
      let s = string_to_bytes ("\027[99;99H" ^ vt_device_status_report) in
      let len = Unix.write fd s 0 (string_length s) in
      if len <> string_length s then failwith "Curses.initscr";
      set_edit ();
      let line =
        let buff = string_make 20 ' ' in
        let rec loop_i i =
          let (icl, _, _) = Unix.select [fd] [] [] 1.0 in
          if icl = [] then string_sub buff 0 i
          else
            let len = Unix.read fd buff i (string_length buff - i) in
            if len = 0 || string_contains buff 'R' then
              string_sub buff 0 (i + len)
            else loop_i (i + len)
        in
        loop_i 0
      in
      try
        Scanf.sscanf (string_of_bytes line) "\027[%d;%dR"
          (fun x y -> d.max_row <- x; d.max_col <- y)
      with Scanf.Scan_failure _ | End_of_file ->
        d.max_row <- 24; d.max_col <- 80
    end;
  d.bcur <- Array.init d.max_row (fun _ -> string_make d.max_col ' ');
  d.bnew <- Array.init d.max_row (fun _ -> string_make d.max_col ' ');
  d.acur <- Array.init d.max_row (fun _ -> Array.make d.max_col no_attr);
  d.anew <- Array.init d.max_row (fun _ -> Array.make d.max_col no_attr);
  d.attr_set <- no_attr;
  d.cur_attr <- no_attr;
  clear ()

let endwin () = cflush (); unset_edit (); flush stdout

let lines () = d.max_row
let cols () = d.max_col

let pos_get () = d.nrow, d.ncol

let move row col = d.nrow <- row; d.ncol <- col

let mvaddch i j c =
  if check i j then
    begin string_set d.bnew.(i) j c; d.anew.(i).(j) <- d.attr_set end;
  d.nrow <- i;
  d.ncol <- j + 1

let mvaddnstr row col s i len =
  d.nrow <- row;
  d.ncol <- col;
  for j = 0 to len - 1 do
    string_set d.bnew.(d.nrow) d.ncol s.[i+j];
    d.anew.(d.nrow).(d.ncol) <- d.attr_set;
    d.ncol <- d.ncol + 1
  done

let mvaddstr row col s =
  d.nrow <- row;
  d.ncol <- col;
  for j = 0 to String.length s - 1 do
    if check d.nrow d.ncol then
      begin
        string_set d.bnew.(d.nrow) d.ncol s.[j];
        d.anew.(d.nrow).(d.ncol) <- d.attr_set;
        d.ncol <- d.ncol + 1
      end
  done

let mvinch row col =
  d.nrow <- row;
  d.ncol <- col;
  if check row col then string_get d.bnew.(row) col else ' '

let refresh () = cflush (); flush stdout

let standend () = d.attr_set <- no_attr
let standout () = d.attr_set <- {d.attr_set with a_standout = true}

let wrefresh_curscr () =
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to Array.length d.bcur - 1 do
    string_fill d.bcur.(i) 0 (string_length d.bcur.(i)) ' '
  done;
  d.crow <- 0;
  d.ccol <- 0;
  cflush ();
  flush stdout

let getch () = input_char stdin
