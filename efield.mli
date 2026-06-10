(* $Id: efield.mli,v 1.4 2010/04/27 11:46:10 deraugla Exp $ *)

type 'a t;;

type ('a, 'b) field_fun =
  { get : 'a t -> string -> 'b -> 'b; set : 'a t -> string -> 'b -> unit }
;;

value make : unit -> 'a t;;
value make_fun :
  string -> ('a -> 'b option) * ('b -> 'a) -> ('a, 'b) field_fun;;
