value random_get_state : unit -> int;;
value random_self_init : unit -> unit;;

value char_code : char -> int;;
value char_chr : int -> char;;
value char_escaped : char -> string;;

value string_escaped : string -> string;;
value string_concat : string -> string list -> string;;

value list_remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list;;

type buffer;;

value buffer_create : int -> buffer;;
value buffer_add_char : buffer -> char -> unit;;
value buffer_add_string : buffer -> string -> unit;;
value buffer_contents : buffer -> string;;
