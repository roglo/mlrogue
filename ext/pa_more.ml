(* $Id: pa_more.ml,v 1.2 2010/04/28 00:52:05 deraugla Exp $ *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;

EXTEND
  GLOBAL: expr;
  expr: LEVEL ":="
    [ [ e1 = SELF; "add_eq"; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ + $e2$ >>
      | e1 = SELF; "sub_eq"; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ - $e2$ >>
      | e1 = SELF; "*="; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ * $e2$ >>
      | e1 = SELF; "div_eq"; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ / $e2$ >>
      | e1 = SELF; "^="; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ ^ $e2$ >>
      | e1 = SELF; "or_eq"; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ lor $e2$ >>
      | e1 = SELF; "land_eq"; e2 = SELF; dummy ->
          <:expr< $e1$ := $e1$ land $e2$ >> ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; ".."; f = LIDENT; "("; t = LIDENT; d = SELF; ")" ->
        <:expr< $lid:"f_" ^ t$.Efield.get $e$.env $str:f$ $d$ >> ] ]
  ;
  expr: BEFORE "."
    [ [ e = SELF; "++" -> <:expr< $e$ := $e$ + 1 >>
      | e = SELF; "--" -> <:expr< $e$ := $e$ - 1 >> ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
END;
