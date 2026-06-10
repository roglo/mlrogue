(* $Id: rfield.ml,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type env =
    Earray of int vect
  | Ebackup of (string * int) option
  | Ebool of bool
  | Eint of int
  | Eplayer of player_species
  | Erandom of int(*Random.State.t*) option
;;

let f_array =
  efield__make_fun "an array"
    ((function
        Earray x -> Some x
      | _ -> None),
     (fun x -> Earray x))
;;

let f_backup =
  efield__make_fun "a backup"
    ((function
        Ebackup x -> Some x
      | _ -> None),
     (fun x -> Ebackup x))
;;

let f_bool =
  efield__make_fun "a bool"
    ((function
        Ebool x -> Some x
      | _ -> None),
     (fun x -> Ebool x))
;;

let f_int =
  efield__make_fun "an int"
    ((function
        Eint x -> Some x
      | _ -> None),
     (fun x -> Eint x))
;;

let f_player_species =
  efield__make_fun "a player species"
    ((function
        Eplayer x -> Some x
      | _ -> None),
     (fun x -> Eplayer x))
;;

let f_random =
  efield__make_fun "a random"
    ((function
        Erandom x -> Some x
      | _ -> None),
     (fun x -> Erandom x))
;;
