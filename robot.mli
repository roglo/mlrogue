(* $Id: robot.mli,v 1.9 2010/04/26 14:57:01 deraugla Exp $ *)

type t

val make : string -> t
val reinit : bool -> t -> t -> t
val play : string array -> int -> int -> t -> char * t
