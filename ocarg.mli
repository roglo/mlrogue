type spec =
  | Unit of (unit -> unit)
  | String of (string -> unit)
  | Int of (int -> unit)
;;

value align : (string * spec * string) list -> (string * spec * string) list;;
