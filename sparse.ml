let int_of_digit c = char__int_of_char c - char__int_of_char `0`;;

let rec parse_int_loop n =
  function
    [< '`0`..`9` as c; (parse_int_loop (10 * n + int_of_digit c)) m >] -> m
  | [< >] -> n
;;

let parse_int =
  function
  | [< '`0`..`9` as c; (parse_int_loop (int_of_digit c)) m >] -> m
;;
