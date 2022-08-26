(* $Id: robot.mli,v 1.9 2010/04/26 14:57:01 deraugla Exp $ *)

type t = 'abstract;

value make : string -> t;
value reinit : bool -> t -> t -> t;
value play : array string -> int -> int -> t -> (char * t);
