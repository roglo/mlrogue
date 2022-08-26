(* $Id: efield.mli,v 1.4 2010/04/27 11:46:10 deraugla Exp $ *)

type t 'a = 'abstract;

type field_fun 'a 'b =
  { get : t 'a -> string -> 'b -> 'b;
    set : t 'a -> string -> 'b -> unit }
;

value make : unit -> t 'a;
value make_fun : string -> ('a -> option 'b * 'b -> 'a)  -> field_fun 'a 'b;
