exception Bad of string;;
exception Help of string;;

type spec =
  | Unit of (unit -> unit)
  | String of (string -> unit)
  | Int of (int -> unit)
;;

value current : int ref;;
value align :
  (string * spec * string) list -> (string * spec * string) list;;
value parse :
  (string * spec * string) list -> (string -> 'a) -> string -> unit;;
value parse_argv :
  string vect -> (string * spec * string) list -> (string -> 'a) -> string ->
    unit;;
