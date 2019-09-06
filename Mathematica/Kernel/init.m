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

EvaluatedAt[expr_, Automatic, min_, max_] := EvaluatedAt[expr, Replace[Reduce`FreeVariables[expr], {{v_,___}->v, _->None}], min, max];
EvaluatedAt[expr_, x_, min_, max_] := (expr /. x->max) - (expr /. x->min);
DiffOp[expr_] := Replace[Reduce`FreeVariables[expr],
    {
    {} -> expr,
    {x_} :> D[expr, x],
    x_List :> D[expr, {x}]
    }
];
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};