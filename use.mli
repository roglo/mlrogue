(* $Id: use.mli,v 1.5 2005/10/17 23:20:24 ddr Exp $ *)

open Rogue;

value show_objects : game -> unit;

value wield : game -> unit;
value take_off : game -> unit;
value put_on_ring : game -> unit;

value wear : game -> unit;
value remove_ring : game -> unit;

value inv_rings : game -> unit;

value quaff : game -> unit;

value draw_magic_map : game -> bool -> unit;

value read_scroll : game -> unit;
value eat : game -> unit;
