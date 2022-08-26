(* $Id: curses.mli,v 1.16 2013/01/29 14:00:23 deraugla Exp $ *)

type attribute = [ A_standout | A_bold ];

value addch : char -> unit;
value addstr : string -> unit;
value attron : list attribute -> unit;
value attroff : list attribute -> unit;
value clear : unit -> unit;
value clrtoeol : unit -> unit;
value color_get : int -> int -> (int * int);
value color_set : int -> int -> unit;
value cols : unit -> int;
value endwin : unit -> unit;
value getch : unit -> char;
value home : unit -> unit;
value initscr : unit -> unit;
value lines : unit -> int;
value move : int -> int -> unit;
value mvaddch : int -> int -> char -> unit;
value mvaddnstr : int -> int -> string -> int -> int -> unit;
value mvaddstr : int -> int -> string -> unit;
value mvinch : int -> int -> char;
value pos_get : unit -> (int * int);
value refresh : unit -> unit;
value standend : unit -> unit;
value standout : unit -> unit;
value wrefresh_curscr : unit -> unit;

value no_output : unit -> unit;
