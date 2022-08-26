(* utf8 strings *)

type t = 'abstract;

value of_string : string → t;
value to_string : t → string;
value length : t → int;
value is_empty : t → bool;
value last_char : t → char;
value but_last : t → t;
value append_char : t → char → t;
