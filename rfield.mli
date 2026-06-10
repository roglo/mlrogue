(* $Id: rfield.mli,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type env;;

type player_species =
    PSsocket of unix__file_descr
(*
  | PSrobot of robot__t
*)
  | PShuman
;;

value f_array : (env, int vect) efield__field_fun;;
value f_backup : (env, (string * int) option) efield__field_fun;;
value f_bool : (env, bool) efield__field_fun;;
value f_int : (env, int) efield__field_fun;;
value f_player_species : (env, player_species) efield__field_fun;;
value f_random : (env, int(*Random.State.t*) option) efield__field_fun;;
