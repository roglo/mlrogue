(* $Id: translate.ml,v 1.28 2018/04/26 09:52:37 deraugla Exp $ *)

value string_create = Bytes.create;
value string_set = Bytes.set;
value string_get = Bytes.get;
value string_sub = Bytes.sub;
value string_of_bytes = Bytes.to_string;

value lex = "rogue.lexicon";

value lexicon = Hashtbl.create 1;
value lexicon_mtime = ref 0.0;
value lang = ref "";

value add_lexicon word transl =
  let transl =
    match transl with
    [ Some transl -> transl
    | None -> "[" ^ word ^ "]" ]
  in
  Hashtbl.add lexicon word transl
;

value cut_trail_dot s =
  let len = String.length s in
  if (len >= 3 && s.[len-2] = ' ' || len = 1) && s.[len-1] = '.' then
    String.sub s 0 (len - 1)
  else s
;

value read_lexicon () = do {
  let ic = open_in lex in
  try
    while True do {
      let s =
        let s = input_line ic in
        cut_trail_dot s
      in
      let len = String.length s in
      if len >= 4 && String.sub s 0 4 = "    " then
        let s = String.sub s 4 (len - 4) in
        if lang.val <> "" then
          loop "" where rec loop default =
            let t = try Some (input_line ic) with [ End_of_file -> None ] in
            let ti =
              match t with
              [ Some t ->
                  try Some (t, String.index t ':') with
                  [ Not_found -> None ]
              | None -> None ]
            in
            match ti with
            [ Some (t, i) ->
                let line_lang = String.sub t 0 i in
                if line_lang = lang.val ||
                   String.length lang.val > String.length line_lang &&
                   String.sub lang.val 0 (String.length line_lang) =
                     line_lang
                then
                  let t =
                    if i + 2 < String.length t then
                      String.sub t (i + 2) (String.length t - i - 2)
                    else ""
                  in
                  let t = cut_trail_dot t in
                  if line_lang = lang.val then add_lexicon s (Some t)
                  else loop t
                else loop default
            | None ->
                add_lexicon s (if default = "" then None else Some default) ]
        else add_lexicon s (Some s)
      else ();
    }
  with
  [ End_of_file -> () ];
  close_in ic;
};

value gen_transl glang str = do {
  if Sys.file_exists lex then
    let stbuf = Unix.stat lex in
    if stbuf.Unix.st_mtime > lexicon_mtime.val then do {
      lang.val := glang;
      Hashtbl.clear lexicon;
      lexicon_mtime.val := stbuf.Unix.st_mtime;
      read_lexicon ();
    }
    else ()
  else ();
  Hashtbl.find lexicon str
};

value transl glang str =
  try gen_transl glang str with
  [ Not_found -> if lang.val = "" then str else "[" ^ str ^ "]" ]
;

value fast_transl glang str =
  try Hashtbl.find lexicon str with
  [ Not_found -> transl glang str ]
;

value check_format ini_fmt (r : string) =
  let s : string = string_of_format (ini_fmt : format 'a 'b 'c) in
  let rec loop i j =
    if i < String.length s - 1 && j < String.length r - 1 then
      match (s.[i], s.[i + 1], r.[j], r.[j + 1]) with
      [ ('%', x, '%', y) ->
          if x = y then loop (i + 2) (j + 2) else None
      | ('%', _, _, _) -> loop i (j + 1)
      | (_, _, '%', _) -> loop (i + 1) j
      | _ -> loop (i + 1) (j + 1) ]
    else if i < String.length s - 1 then
      if s.[i] = '%' then None else loop (i + 1) j
    else if j < String.length r - 1 then
      if r.[j] = '%' then None else loop i (j + 1)
    else
      Some (Scanf.format_from_string r ini_fmt : format 'a 'b 'c)
  in
  loop 0 0
;

value tnf s = "[" ^ s ^ "]";

value valid_format ini_fmt r =
  match check_format ini_fmt r with
  | Some fmt -> fmt
  | None -> Scanf.format_from_string (tnf (string_of_format ini_fmt)) ini_fmt
  end
;

value ftransl glang (fmt : format 'a 'b 'c) =
  let sfmt : string = string_of_format fmt in
  try valid_format fmt (gen_transl glang sfmt) with
  [ Not_found ->
      if lang.val = "" then fmt
      else
        (Scanf.format_from_string ("[" ^ sfmt ^ "]") fmt : format 'a 'b 'c) ]
;

value erase str i j =
  String.sub str 0 i ^ String.sub str j (String.length str - j)
;

(*
 * eval_set scans strings of the form @(x) where x is a list of characters
 * meaning a predicate to set for each character. Fills [set], the set of
 * predicates. Treats also the special case for @(&) = delete the next
 * character if any.
 *)

value eval_set str =
  loop [] str 0 where rec loop set str i =
    if i + 3 < String.length str then
      if str.[i] = '@' && str.[i+1] = '(' && str.[i+3] <> '?' &&
         str.[i+3] <> '-'
      then
        if str.[i+2] = '&' && str.[i+3] = ')' && i + 4 < String.length str
        then
          loop set (erase str i (i + 5)) i
        else
          let (set, j) =
            loop set (i + 2) where rec loop set i =
              if i < String.length str then
                if str.[i] <> ')' then loop [str.[i] :: set] (i + 1)
                else (set, i + 1)
              else (set, i)
          in
          loop set (erase str i j) i
      else loop set str (i + 1)
    else (set, str)
;

value rec apply_expr set str i =
  if i + 1 < String.length str && str.[i+1] = '?' then
    if List.mem str.[i] set then
      let str = erase str i (i + 2) in
      let (str, i) = apply_expr set str i in
      if i < String.length str && str.[i] = ':' then
        let (str, j) = apply_expr set str (i + 1) in
        (erase str i j, i)
      else (str, i)
    else
      let (str, j) = apply_expr set str (i + 2) in
      let str = erase str i j in
      if i < String.length str && str.[i] = ':' then
        let str = erase str i (i + 1) in
        apply_expr set str i
      else (str, i)
  else if i < String.length str && (str.[i] = ':' || str.[i] = ')') then
    (str, i)
  else apply_expr set str (i + 1)
;

(*
 * eval_app scans strings matching expressions between @( and ).
 *    an expression is:
 *     - a [character] followed by "?", an [expression] and possibly ":" and
 *       [another expression]
 *     - any [string] not holding ":"
 *    The [character] is a predicate. If defined, the first expression is
 *    evaluated, else it is the second one. The evaluation of a string is
 *    itself.
 *
 *  ex: p?e:m?A?en:er:w?e:n?es
 *    In this example, if m and A are only defined predicates:
 *      p not being defined, it is m?A?en:er:w?e:n?es
 *      m being defined, it is A?en:er
 *      A being defined, it is en
 *    This example shows how to display adjectives in German, where
 *    m is for masculine, w for feminine and n for neuter
 *)

value eval_app set str =
  loop str 0 where rec loop str i =
    if i + 3 < String.length str then
      if str.[i] = '@' && str.[i+1] = '(' && str.[i+3] <> '-' then (
        let str = erase str i (i + 2) in
        let (str, i) = apply_expr set str i in
        if i < String.length str then
          if str.[i] = ')' then loop (erase str i (i + 1)) i
          else loop str i
        else str
      )
      else loop str (i + 1)
    else str
;

(*
 * eval_shift scans strings matching:
 *   @(#-) shifting # words of the left after the next word.
 *   @(#--) shifting # words of the left to the end.
 * ex:
 *   before: "Une avec un diamant@(3-) bague"
 *    after: "Une bague avec un diamant"
 *   before: "Sie haben geworfen@(1--) einen kurzen Bogen"
 *    after: "Sie haben einen kurzen Bogen geworfen"
 *)

value rec eval_shift s =
  let t = string_create (String.length s) in
  loop False 0 0 where rec loop changed i j =
    if i + 4 < String.length s && s.[i] = '@' && s.[i+1] = '(' &&
       s.[i+3] = '-'
    then
      let nleft = Char.code s.[i+2] - Char.code '0' in
      let to_the_end = s.[i+4] = '-' in
      let k = if to_the_end then i + 5 else i + 4 in
      if k < String.length s && s.[k] = ')' then (
        let l =
          loop nleft (i - 1) where rec loop nleft l =
            if l > 0 then
              if s.[l] = ' ' then
                if nleft <= 1 then l + 1
                else loop (nleft - 1) (l - 1)
              else loop nleft (l - 1)
            else 0
        in
        let len = i - l in
        let j = j - len in
        let k = k + 1 in
        let i = if k < String.length s && s.[k] = ' ' then k + 1 else k in
        let (i, j) =
          if to_the_end then
            loop i j where rec loop i j =
              if i < String.length s then do {
                string_set t j s.[i];
                loop (i + 1) (j + 1)
              }
              else do {
                let j =
                  if string_get t (j-1) <> ' ' then do {
		    string_set t j ' '; j + 1
		  }
                  else j
                in
                String.blit s l t j len;
                (i, j + len)
              }
          else
            loop i j where rec loop i j =
              if i < String.length s then
                if s.[i] = ' ' then do {
                  string_set t j ' ';
                  String.blit s l t (j + 1) len;
                  (i, j + 1 + len)
                }
                else do {
                  string_set t j s.[i];
                  loop (i + 1) (j + 1)
                }
              else if k < String.length s && s.[k] = ' ' then do {
                string_set t j ' ';
                String.blit s l t (j + 1) len;
                (i, j + 1 + len)
              }
              else do {
                String.blit s l t j len;
                (i, j + len)
              }
        in
        loop True i j
      )
      else do {
        string_set t j s.[i];
        loop changed (i + 1) (j + 1)
      }
    else if i < String.length s then do {
      string_set t j s.[i];
      loop changed (i + 1) (j + 1)
    }
    else if changed then eval_shift (string_of_bytes (string_sub t 0 j))
    else string_of_bytes (string_sub t 0 j)
;

(* etransl make grammatical transformations indicated inside strings
   between "@(" and ")". This system allows e.g. to conjugate articles,
   nouns, adjectives, verbs, to put adjectives after nouns, to put verbs
   at the end of the sentence, things like that. Useful at least in
   French (fr:) and German (de:).
     The idea is simple: some parts of the sentence set predicates
   and other parts generate strings depending on these predicates.
   A predicate is synctactically a character and semantically what
   the user wants: "this noun is masculine", "this verb expects
   accusative", and so on.
     The syntax to set the predicate x is @(x).
     The syntax of evaluation is between @( and ) and looks like
   the C expression x?y:z which can be use recursively. It can be
   also indications to shift words.

     Example in German. The concatenation of phrases:
       Sie haben geworfen@(A)@(1--)      (You dropped)
       ein@(w?e:A?m?en)                  (a)
       +1,+0
       kurz@(A?en:er) Bogen@(m)          (short bow)
   gives the initial sentence:

 Sie haben geworfen@(A)@(1--) ein@(w?e:A?m?en) +1,+0 kurz@(A?en:er) Bogen@(m)

   1/ First step ("eval_set") extracts the predicates. Here, the set
      predicates are "A" and "m" (User semantics of "A" is "this
      verb expects accusative" and "m" is "this noun is masculine").

 Sie haben geworfen@(A)@(1--) ein@(w?e:A?m?en) +1,+0 kurz@(A?en:er) Bogen@(m)
                   ^^^^                                                  ^^^^
      resulting string (predicates deleted):
 Sie haben geworfen@(1--) ein@(w?e:A?m?en) +1,+0 kurz@(A?en:er) Bogen

    2/ Second step ("eval_app") evaluates the expressions of the form
       x?y:z:

 Sie haben geworfen@(1--) ein@(w?e:A?m?en) +1,+0 kurz@(A?en:er) Bogen
                             ^^^^^^^^^^^^^           ^^^^^^^^^^
     Expression @(w?e:A?m?en) is read: if predicate "w" is set then "e"
     else if predicate "A" is set then if predicate "m" is set then "en"
     (else nothing). Semantically, it means that if the noun is feminine
     (weiblich in German), the result is "e" (word is "eine") else if the
     noun is masculine and if the accusative form is expected, the result
     is "en" (word is "einen") else nothing (word is "ein").
       The second expression @(A?en:er) returns here "en" (word "kurzen").
     Resulting string:

       Sie haben geworfen@(1--) einen +1,+0 kurzen Bogen

    3/ Third step ("eval_shift") applies the shift predicates. Here,
       @(1--) means put "1" word on the left to the end of the
       sentence. Result:

       Sie haben einen +1,+0 kurzen Bogen geworfen
*)

value etransl str =
  let (set, str) = eval_set str in
  let str = eval_app set str in
  eval_shift str
;

value translc lang c =
  let s = transl lang (String.make 1 c) in
  if String.length s = 1 then s.[0] else c
;

value clear_lexicon lang = do {
  Hashtbl.clear lexicon;
  lexicon_mtime.val := 0.0;
};
