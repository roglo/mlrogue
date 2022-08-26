(* $Id: efield.ml,v 1.4 2010/04/27 11:46:10 deraugla Exp $ *)

open Printf;

type t 'a = Hashtbl.t string 'a;

type field_fun 'a 'b =
  { get : t 'a -> string -> 'b -> 'b;
    set : t 'a -> string -> 'b -> unit }
;

value make () = Hashtbl.create 1;

value get_env env var =
  try Some (Hashtbl.find env var) with [ Not_found -> None ]
;

value set_env env var v = Hashtbl.replace env var v;

value make_fun a_name (get, set) =
  {get env var def =
     match get_env env var with
     [ Some t -> do {
         match get t with
         [ Some x -> x
         | None -> failwith (sprintf "field %s not %s" var a_name) ];
       }
     | None -> def ];
   set env var x = set_env env var (set x)}
;
