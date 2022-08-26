(* $Id: rfield.mli,v 1.4 2010/05/03 17:12:57 deraugla Exp $ *)

type env = 'abstract;

type player_species =
  [ PSsocket of Unix.file_descr
  | PSrobot of Robot.t
  | PShuman ]
;

value f_array : Efield.field_fun env (array int);
value f_backup : Efield.field_fun env (option (string * int));
value f_bool : Efield.field_fun env bool;
value f_int : Efield.field_fun env int;
value f_player_species : Efield.field_fun env player_species;
value f_random : Efield.field_fun env (option Random.State.t);
