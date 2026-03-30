(* utf8 strings *)

type t

val of_string : string -> t
val to_string : t -> string
val length : t -> int
val is_empty : t -> bool
val last_char : t -> char
val but_last : t -> t
val append_char : t -> char -> t
