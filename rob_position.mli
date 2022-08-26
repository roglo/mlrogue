(* $Id: rob_position.mli,v 1.2 2010/05/09 05:01:15 deraugla Exp $ *)

type position = { row : int; col : int };

module PosMap :
  sig
    type t 'a = 'abstract;
    value empty : t 'a;
    value add : position -> 'a -> t 'a -> t 'a;
    value mem : position -> t 'a -> bool;
    value find : position -> t 'a -> 'a;
  end
;

module PosSet :
  sig
    type t = 'abstract;
    value empty : t;
    value add : position -> t -> t;
    value mem : position -> t -> bool;
  end
;
