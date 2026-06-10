(* $Id: init.mli,v 1.14 2010/04/27 10:15:30 deraugla Exp $ *)

#open "rogue";;
#open "rfield";;

type init =
    NewGame of game
  | RestoreGame of string
  | ScoreOnly
;;

value f :
  string vect ->
    string * init * (player_species * bool) option * (string * int) option *
      bool * bool * bool;;
