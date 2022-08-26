(* $Id: translate.mli,v 1.7 2010/03/31 12:23:39 deraugla Exp $ *)

open Rogue;

value transl : string -> string -> string;
value ftransl : string -> format 'a 'b 'c -> format 'a 'b 'c;
value translc : string -> char -> char;
value etransl : string -> string;
value clear_lexicon : string -> unit;

value fast_transl : string -> string -> string;
  (* like [transl] but don't check lexicon file change; interesting
     when called a very big number of times; but answers the same
     thing even if the lexicon file has changed. *)
