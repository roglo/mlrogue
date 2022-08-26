(* $Id: pa_if_match.ml,v 1.6 2010/05/12 23:25:36 deraugla Exp $ *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;

EXTEND
  expr: LEVEL "top"
    [ [ "if_match"; e = expr; "with_some"; p = ipatt;
        "->"; e1 = expr; "else"; e2 = expr ->
          <:expr< match $e$ with [ Some $p$ -> $e1$ | None -> $e2$ ] >> ] ]
  ;
END;
