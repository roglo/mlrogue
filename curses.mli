(* $Id: curses.mli,v 1.16 2013/01/29 14:00:23 deraugla Exp $ *)

type attribute = A_standout | A_bold

val addch : char -> unit
val addstr : string -> unit
val attron : attribute list -> unit
val attroff : attribute list -> unit
val clear : unit -> unit
val clrtoeol : unit -> unit
val color_get : int -> int -> int * int
val color_set : int -> int -> unit
val cols : unit -> int
val endwin : unit -> unit
val getch : unit -> char
val home : unit -> unit
val initscr : unit -> unit
val lines : unit -> int
val move : int -> int -> unit
val mvaddch : int -> int -> char -> unit
val mvaddnstr : int -> int -> string -> int -> int -> unit
val mvaddstr : int -> int -> string -> unit
val mvinch : int -> int -> char
val pos_get : unit -> int * int
val refresh : unit -> unit
val standend : unit -> unit
val standout : unit -> unit
val wrefresh_curscr : unit -> unit

val no_output : unit -> unit
