(* $Id: translate.mli,v 1.7 2010/03/31 12:23:39 deraugla Exp $ *)

open Rogue

val transl : string -> string -> string
val ftransl : string -> ('a, 'b, 'c) format -> ('a, 'b, 'c) format
val translc : string -> char -> char
val etransl : string -> string
val clear_lexicon : string -> unit

val fast_transl : string -> string -> string
  (* like [transl] but don't check lexicon file change; interesting
     when called a very big number of times; but answers the same
     thing even if the lexicon file has changed. *)
