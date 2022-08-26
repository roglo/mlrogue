(* $Id: pr_more.ml,v 1.3 2010/01/26 10:01:25 deraugla Exp $ *)

#load "pa_extprint.cmo";
#load "pa_pprintf.cmo";
#load "q_ast.cmo";

open Pcaml;

value expr = Eprinter.apply pr_expr;

EXTEND_PRINTER
  pr_expr: LEVEL "assign"
    [ [ <:expr< $x$ := $y$ lor $z$ >> when eq_expr x y ->
          pprintf pc "%p |=@;%p" next x expr z
      | <:expr< $x$ := $y$ land $z$ >> when eq_expr x y ->
          pprintf pc "%p &=@;%p" next x expr z
      | <:expr< $x$ := $y$ + 1 >> when eq_expr x y ->
          pprintf pc "%p ++" next x
      | <:expr< $x$ := $y$ - 1 >> when eq_expr x y ->
          pprintf pc "%p --" next x
      | <:expr< $x$ := $y$ + $z$ >> when eq_expr x y ->
          pprintf pc "%p +=@;%p" next x expr z
      | <:expr< $x$ := $y$ - $z$ >> when eq_expr x y ->
          pprintf pc "%p -=@;%p" next x expr z
      | <:expr< $x$ := $y$ / $z$ >> when eq_expr x y ->
          pprintf pc "%p /=@;%p" next x expr z ] ]
  ;
END;
