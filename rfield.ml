(* $Id: rfield.ml,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type player_species =
    PSsocket of Unix.file_descr
  | PSrobot of Robot.t
  | PShuman

type env =
    Earray of int array
  | Ebackup of (string * int) option
  | Ebool of bool
  | Eint of int
  | Eplayer of player_species
  | Erandom of Random.State.t option

let f_array =
  Efield.make_fun "an array"
    ((function
        Earray x -> Some x
      | _ -> None),
     (fun x -> Earray x))

let f_backup =
  Efield.make_fun "a backup"
    ((function
        Ebackup x -> Some x
      | _ -> None),
     (fun x -> Ebackup x))

let f_bool =
  Efield.make_fun "a bool"
    ((function
        Ebool x -> Some x
      | _ -> None),
     (fun x -> Ebool x))

let f_int =
  Efield.make_fun "an int"
    ((function
        Eint x -> Some x
      | _ -> None),
     (fun x -> Eint x))

let f_player_species =
  Efield.make_fun "a player species"
    ((function
        Eplayer x -> Some x
      | _ -> None),
     (fun x -> Eplayer x))

let f_random =
  Efield.make_fun "a random"
    ((function
        Erandom x -> Some x
      | _ -> None),
     (fun x -> Erandom x))
