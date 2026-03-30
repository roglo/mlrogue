(* $Id: rogbotio.mli,v 1.3 2010/05/04 07:55:17 deraugla Exp $ *)

val socket : string -> Unix.file_descr
val getchar : int -> int -> Unix.file_descr -> char
