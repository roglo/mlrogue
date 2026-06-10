(* $Id: translate.mli,v 1.7 2010/03/31 12:23:39 deraugla Exp $ *)

#open "rogue";;

value transl : string -> string -> string;;
value ftransl : string -> ('a, 'b, 'c) printf__format -> ('a, 'b, 'c) printf__format;;
value translc : string -> char -> char;;
value etransl : string -> string;;
value clear_lexicon : string -> unit;;

value fast_transl : string -> string -> string;;
  (* like [transl] but don't check lexicon file change; interesting
     when called a very big number of times; but answers the same
     thing even if the lexicon file has changed. *)
