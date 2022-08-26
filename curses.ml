(* $Id: curses.ml,v 1.60 2018/04/26 09:52:36 deraugla Exp $ *)

open Printf;

type data =
  { max_row : mutable int;
    max_col : mutable int;
    crow : mutable int;
    ccol : mutable int;
    nrow : mutable int;
    ncol : mutable int;
    bcur : mutable array bytes;
    bnew : mutable array bytes;
    acur : mutable array (array attr);
    anew : mutable array (array attr);
    attr_set : mutable attr;
    cur_attr : mutable attr;
    no_output : mutable bool }
and attr =
  { a_standout : bool;
    a_bold : bool;
    a_back_col : int;
    a_fore_col : int }
;

type attribute = [ A_standout | A_bold ];

value string_make = Bytes.make;
value string_get = Bytes.get;
value string_set = Bytes.set;
value string_sub = Bytes.sub;
value string_length = Bytes.length;
value string_fill = Bytes.fill;
value string_contains = Bytes.contains;
value string_of_bytes = Bytes.to_string;
value string_to_bytes = Bytes.of_string;

value no_attr =
  {a_standout = False; a_bold = False; a_back_col = -1; a_fore_col = -1}
;

value d =
  {max_row = 0; max_col = 0; crow = 0; ccol = 0; nrow = 0; ncol = 0;
   bcur = [| |]; bnew = [| |]; acur = [| |]; anew = [| |];
   attr_set = no_attr; cur_attr = no_attr; no_output = False}
;

value no_output () = d.no_output := True;

value check row col =
  row >= 0 && row < d.max_row && col >= 0 && col < d.max_col
;

value tty_fd_and_ini_attr = ref None;
value tty_fd () =
  match tty_fd_and_ini_attr.val with
  [ Some (fd, _) -> fd
  | None -> do {
      let fd = Unix.openfile "/dev/tty" [Unix.O_RDWR] 0o000 in
      let ini_attr = Unix.tcgetattr fd in
      tty_fd_and_ini_attr.val := Some (fd, ini_attr);
      fd
    } ]
;

value edit_tcio = ref None;

value set_edit () = do {
  let tcio =
    match edit_tcio.val with
    [ Some e -> e
    | None -> do {
        let fd = tty_fd () in
        let tcio = Unix.tcgetattr fd in
        tcio.Unix.c_echo := False;
        tcio.Unix.c_icanon := False;
        tcio.Unix.c_vmin := 1;
        tcio.Unix.c_isig := False;
        tcio.Unix.c_ixon := False;
        tcio.Unix.c_inlcr := False;
        tcio.Unix.c_icrnl := False;
        edit_tcio.val := Some tcio;
        tcio
      } ]
  in
  let fd = tty_fd () in
  Unix.tcsetattr fd Unix.TCSADRAIN tcio;
}
and unset_edit () = do {
  match tty_fd_and_ini_attr.val with
  [ Some (fd, ini_attr) -> Unix.tcsetattr fd Unix.TCSADRAIN ini_attr
  | None -> () ]
};

value set_attr a =
  if a <> d.cur_attr then do {
    if d.no_output then ()
    else do {
      if d.cur_attr.a_bold && not a.a_bold ||
         (d.cur_attr.a_fore_col <> a.a_fore_col ||
          d.cur_attr.a_back_col <> a.a_back_col) &&
         (a.a_fore_col = -1 || a.a_back_col = -1)
      then
        printf "\027[m"
      else ();
      if not d.cur_attr.a_standout && a.a_standout then
        printf "\027[7m"
      else if d.cur_attr.a_standout && not a.a_standout then
        printf "\027[27m"
      else ();
      if d.cur_attr.a_fore_col <> a.a_fore_col ||
         d.cur_attr.a_back_col <> a.a_back_col
      then do {
        if a.a_fore_col <> -1 then printf "\027[3%dm" a.a_fore_col else ();
        if a.a_back_col <> -1 then printf "\027[4%dm" a.a_back_col else ();
      }
      else ();
      if not d.cur_attr.a_bold && a.a_bold then printf "\027[1m" else ();
    };
    d.cur_attr := a;
  }
  else ()
;

value print_encode_char c =
  if d.no_output then () else print_char c
;

value cprint_string s = if d.no_output then () else print_string s;

value update c n ac an i jbeg j = do {
  if i = d.crow && jbeg = d.ccol then ()
  else if i = d.crow && jbeg = d.ccol - 1 then cprint_string "\b"
  else if i = d.crow && jbeg = d.ccol + 1 then do {
    set_attr an.(d.ccol);
    print_encode_char (string_get n d.ccol);
  }
  else if d.no_output then ()
  else printf "\027[%d;%dH" (i+1) (jbeg+1);
  if jbeg = j - 1 then do {
    set_attr an.(jbeg);
    print_encode_char (string_get n jbeg);
    string_set c jbeg (string_get n jbeg);
    ac.(jbeg) := an.(jbeg)
  }
  else do {
    let same_attr =
      loop jbeg where rec loop k =
        if k = j then True
        else if an.(k) = an.(jbeg) then loop (k + 1)
        else False
    in
    if same_attr then do {
      set_attr an.(jbeg);
      for k = jbeg to j - 1 do { print_encode_char (string_get n k) };
    }
    else do {
      for k = jbeg to j - 1 do {
        set_attr an.(k);
        print_encode_char (string_get n k);
      }
    };
    for k = jbeg to j - 1 do {
      string_set c k (string_get n k);
      ac.(k) := an.(k);
    }
  };
  d.crow := i;
  d.ccol := j
};

value rec gap_equal k c n j =
  if k = 0 || j >= string_length c then False
  else
    if string_get c j <> string_get n j then True
    else gap_equal (k - 1) c n (j + 1)
;

value cflush () = do {
  for i = 0 to Array.length d.bcur - 1 do {
    let c = d.bcur.(i) in
    let n = d.bnew.(i) in
    let ac = d.acur.(i) in
    let an = d.anew.(i) in
    if c <> n || ac <> an then
      let len = string_length c in
      loop_j 0 0 where rec loop_j jbeg j =
        if j = len then
          if jbeg < j then update c n ac an i jbeg j else ()
        else if string_get c j <> string_get n j || ac.(j) <> an.(j) then
	  loop_j jbeg (j + 1)
        else if jbeg < j then
          if j + 1 < len && gap_equal 8 c n (j + 1) then loop_j jbeg (j + 1)
          else do {
            update c n ac an i jbeg j;
            loop_j (j + 1) (j + 1)
          }
        else loop_j (j + 1) (j + 1)
    else ();
  };
  if not (check d.nrow d.ncol) then ()
  else if d.crow <> d.nrow || d.ccol <> d.ncol then do {
    if d.crow = d.nrow && d.ccol < d.max_col then
      let n = d.bnew.(d.nrow) in
      if d.ccol = d.ncol + 1 then cprint_string "\b"
      else if d.ccol = d.ncol + 2 then cprint_string "\b\b"
      else if d.ccol = d.ncol + 3 then cprint_string "\b\b\b"
      else if d.ccol = d.ncol + 4 then cprint_string "\b\b\b\b"
      else if d.ccol = d.ncol + 5 then cprint_string "\b\b\b\b\b"
      else if d.ccol = d.ncol - 1 then do {
        set_attr d.anew.(d.crow).(d.ccol);
        print_encode_char (string_get n d.ccol)
      }
      else if d.ccol = d.ncol - 2 then do {
        set_attr d.anew.(d.crow).(d.ccol);
        print_encode_char (string_get n d.ccol);
        set_attr d.anew.(d.crow).(d.ccol+1);
        print_encode_char (string_get n (d.ccol+1))
      }
      else if d.no_output then ()
      else printf "\027[%d;%dH" (d.nrow+1) (d.ncol+1)
    else do {
      set_attr d.anew.(d.nrow).(d.ncol);
      if d.no_output then ()
      else printf "\027[%d;%dH" (d.nrow+1) (d.ncol+1)
    };
    d.crow := d.nrow;
    d.ccol := d.ncol
  }
  else ()
};

(* *)

value addch c = do {
  if check d.nrow d.ncol then do {
    string_set d.bnew.(d.nrow) d.ncol c;
    d.anew.(d.nrow).(d.ncol) := d.attr_set;
  }
  else ();
  d.ncol := d.ncol + 1
};

value addstr s =
  for i = 0 to String.length s - 1 do {
    addch s.[i];
  }
;

value attroff al =
  List.iter
    (fun
     [ A_standout -> d.attr_set := {(d.attr_set) with a_standout = False}
     | A_bold -> d.attr_set := {(d.attr_set) with a_bold = False} ])
    al
;

value attron al =
  List.iter
    (fun
     [ A_standout -> d.attr_set := {(d.attr_set) with a_standout = True}
     | A_bold -> d.attr_set := {(d.attr_set) with a_bold = True} ])
    al
;

value vt_device_status_report = "\027[6n";
value vt_erase_in_display = "\027[J";
value vt_erase_line_from_cursor = "\027[K";

value clear () = do {
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to Array.length d.bcur - 1 do {
    string_fill d.bcur.(i) 0 (string_length d.bcur.(i)) ' ';
    string_fill d.bnew.(i) 0 (string_length d.bnew.(i)) ' ';
    Array.fill d.acur.(i) 0 (string_length d.bcur.(i)) no_attr;
    Array.fill d.anew.(i) 0 (string_length d.bnew.(i)) no_attr;
  };
  d.crow := 0;
  d.ccol := 0;
  d.nrow := 0;
  d.ncol := 0;
};

value clrtoeol () = do {
  cflush ();
  cprint_string vt_erase_line_from_cursor;
  if check d.crow d.ccol && check d.nrow d.ncol then do {
    let s = d.bcur.(d.crow) in
    string_fill s d.ccol (string_length s - d.ccol) ' ';
    let s = d.bnew.(d.nrow) in
    string_fill s d.ccol (string_length s - d.ncol) ' ';
    let s = d.acur.(d.nrow) in
    Array.fill s d.ccol (Array.length s - d.ncol) no_attr;
    let s = d.anew.(d.nrow) in
    Array.fill s d.ccol (Array.length s - d.ncol) no_attr;
  }
  else ();
};

value color_set fg bg =
  d.attr_set := {(d.attr_set) with a_fore_col = fg; a_back_col = bg}
;

value color_get i j =
  if check i j then
    let ac = d.acur.(i).(j) in
    (ac.a_fore_col, ac.a_back_col)
  else
    (-1, -1)
;

value home () = do {
  set_attr no_attr;
  cprint_string "\027[H";
  d.crow := 0;
  d.ccol := 0;
  d.nrow := 0;
  d.ncol := 0;
};

value initscr () = do {
  if d.no_output then do {
    d.max_row := 24;
    d.max_col := 80;
  }
  else do {
    let fd = tty_fd () in
    let s = string_to_bytes ("\027[99;99H" ^ vt_device_status_report) in
    let len = Unix.write fd s 0 (string_length s) in
    if len <> string_length s then failwith "Curses.initscr" else ();
    set_edit ();
    let line =
      let buff = string_make 20 ' ' in
      loop_i 0 where rec loop_i i =
        let (icl, _, _) = Unix.select [fd] [] [] 1.0 in
        if icl = [] then string_sub buff 0 i
        else
          let len = Unix.read fd buff i (string_length buff - i) in
          if len = 0 || string_contains buff 'R' then
            string_sub buff 0 (i + len)
          else loop_i (i + len)
    in
    try
      Scanf.sscanf (string_of_bytes line) "\027[%d;%dR"
        (fun x y -> do { d.max_row := x; d.max_col := y })
    with
    [ Scanf.Scan_failure _ | End_of_file -> do {
        d.max_row := 24;
        d.max_col := 80;
      } ];
  };
  d.bcur := Array.init d.max_row (fun _ -> string_make d.max_col ' ');
  d.bnew := Array.init d.max_row (fun _ -> string_make d.max_col ' ');
  d.acur := Array.init d.max_row (fun _ -> Array.make d.max_col no_attr);
  d.anew := Array.init d.max_row (fun _ -> Array.make d.max_col no_attr);
  d.attr_set := no_attr;
  d.cur_attr := no_attr;
  clear ();
};

value endwin () =
  do { cflush (); unset_edit (); flush stdout; }
;

value lines () = d.max_row;
value cols () = d.max_col;

value pos_get () = (d.nrow, d.ncol);

value move row col = do {
  d.nrow := row;
  d.ncol := col;
};

value mvaddch i j c = do {
  if check i j then do {
    string_set d.bnew.(i) j c;
    d.anew.(i).(j) := d.attr_set;
  }
  else ();
  d.nrow := i;
  d.ncol := j + 1;
};

value mvaddnstr row col s i len = do {
  d.nrow := row;
  d.ncol := col;
  for j = 0 to len - 1 do {
    string_set d.bnew.(d.nrow) d.ncol s.[i + j];
    d.anew.(d.nrow).(d.ncol) := d.attr_set;
    d.ncol := d.ncol + 1;
  };
};

value mvaddstr row col s = do {
  d.nrow := row;
  d.ncol := col;
  for j = 0 to String.length s - 1 do {
    if check d.nrow d.ncol then do {
      string_set d.bnew.(d.nrow) d.ncol s.[j];
      d.anew.(d.nrow).(d.ncol) := d.attr_set;
      d.ncol := d.ncol + 1;
    }
    else ();
  };
};

value mvinch row col = do {
  d.nrow := row;
  d.ncol := col;
  if check row col then string_get d.bnew.(row) col else ' ';
};

value refresh () = do {
  cflush ();
  flush stdout;
};

value standend () = d.attr_set := no_attr;
value standout () = d.attr_set := {(d.attr_set) with a_standout = True};

value wrefresh_curscr () = do {
  cprint_string "\027[H";
  cprint_string vt_erase_in_display;
  for i = 0 to Array.length d.bcur - 1 do {
    string_fill d.bcur.(i) 0 (string_length d.bcur.(i)) ' ';
  };
  d.crow := 0;
  d.ccol := 0;
  cflush ();
  flush stdout;
};

value getch () = input_char stdin;
