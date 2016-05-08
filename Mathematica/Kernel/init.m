(** User Mathematica initialization file **)
AppendTo[$Path, "~/Dropbox/Knowledge"];

PlusMinus[{x_, err_}] := 
 Module[{errE = Last@MantissaExponent[err], 
   xE = Last@MantissaExponent[x]}, 
  Row[{"(", 
    NumberForm[
     N@Round[x, 10^(errE - 1)]*10^(-xE + 1), {-errE, -errE - 1}], 
    " \[PlusMinus] ", 
    NumberForm[
     N@Round[err, 10^(errE - 1)]*10^(-xE + 1), {1, -errE - 1}, 
     ExponentFunction -> (Null &)], ")", " \[Times] ", 
    DisplayForm@SuperscriptBox["10", ToString[xE - 1]]}]];
PlusMinus[x_, err_] := PlusMinus[{x, err}];