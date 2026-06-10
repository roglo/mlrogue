(* $Id: efield.ml,v 1.4 2010/04/27 11:46:10 deraugla Exp $ *)

#open "printf";;

type 'a t == (string, 'a) hashtbl__t;;

let make () = hashtbl__new 1;;

let get_env env var = try Some (hashtbl__find env var) with Not_found -> None;;

let set_env env var v =
  hashtbl__remove env var;
  hashtbl__add env var v
;;

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
;;
