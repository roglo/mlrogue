(* $Id: efield.ml,v 1.4 2010/04/27 11:46:10 deraugla Exp $ *)

open Printf

type 'a t = (string, 'a) Hashtbl.t

type ('a, 'b) field_fun =
  { get : 'a t -> string -> 'b -> 'b; set : 'a t -> string -> 'b -> unit }

let make () = Hashtbl.create 1

let get_env env var = try Some (Hashtbl.find env var) with Not_found -> None

let set_env env var v = Hashtbl.replace env var v

let make_fun a_name (get, set) =
  {get =
    (fun env var def ->
       match get_env env var with
         Some t ->
           begin match get t with
             Some x -> x
           | None -> failwith (sprintf "field %s not %s" var a_name)
           end
       | None -> def);
   set = fun env var x -> set_env env var (set x)}
