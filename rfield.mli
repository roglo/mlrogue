(* $Id: rfield.mli,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type env

type player_species =
    PSsocket of Unix.file_descr
  | PSrobot of Robot.t
  | PShuman

val f_array : (env, int array) Efield.field_fun
val f_backup : (env, (string * int) option) Efield.field_fun
val f_bool : (env, bool) Efield.field_fun
val f_int : (env, int) Efield.field_fun
val f_player_species : (env, player_species) Efield.field_fun
val f_random : (env, Random.State.t option) Efield.field_fun
