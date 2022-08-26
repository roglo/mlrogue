(* $Id: rfield.ml,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type player_species =
  [ PSsocket of Unix.file_descr
  | PSrobot of Robot.t
  | PShuman ]
;

type env =
  [ Earray of array int
  | Ebackup of option (string * int)
  | Ebool of bool
  | Eint of int
  | Eplayer of player_species
  | Erandom of option Random.State.t ]
;

value f_array =
  Efield.make_fun "an array"
    (fun [ Earray x -> Some x | _ -> None ], fun x -> Earray x)
;

value f_backup =
  Efield.make_fun "a backup"
    (fun [ Ebackup x -> Some x | _ -> None ], fun x -> Ebackup x)
;

value f_bool =
  Efield.make_fun "a bool"
    (fun [ Ebool x -> Some x | _ -> None ], fun x -> Ebool x)
;

value f_int =
  Efield.make_fun "an int"
    (fun [ Eint x -> Some x | _ -> None ], fun x -> Eint x)
;

value f_player_species =
  Efield.make_fun "a player species"
    (fun [ Eplayer x -> Some x | _ -> None ], fun x -> Eplayer x)
;

value f_random =
  Efield.make_fun "a random"
    (fun [ Erandom x -> Some x | _ -> None ], fun x -> Erandom x)
;
