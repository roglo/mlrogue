(* $Id: rogbotio.mli,v 1.3 2010/05/04 07:55:17 deraugla Exp $ *)

value socket : string -> unix__file_descr;;
value getchar : int -> int -> unix__file_descr -> char;;
