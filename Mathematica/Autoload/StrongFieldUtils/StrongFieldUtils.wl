(* ::Package:: *)

(* ::Title:: *)
(*StrongFieldUtils*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*Collection of functions useful in Strong Field Atomic physics*)


(* ::Text:: *)
(*Version: 1.0.0*)


BeginPackage["StrongFieldUtils`"];


(* ::Section:: *)
(*Essenatials*)


(*physical constants in Atomic Units*)
fsAtomic::usage="One femtosecond in atomic units";
cAtomic::usage="Speed of light in atomic units";
IpAtomic::usage="IpAtomic[atom,n] returns n'th (def. n) ionization energy of \"atom\" in atomic units.";

Begin["`Private`"];
fsAtomic=QuantityMagnitude@UnitConvert[Quantity[1,"fs"],"AtomicUnitOfTime"];
cAtomic=QuantityMagnitude@UnitConvert[Quantity["ReducedPlanckConstant"]/( Quantity["FineStructureConstant"] Quantity["BohrRadius"] Quantity["ElectronMass"]),"AtomicUnitOfVelocity"];
IpAtomic[atom_String,n_:1]:=QuantityMagnitude[UnitConvert[ElementData[atom,"IonizationEnergies"]/Quantity["AvogadroConstant"],"Hartrees"][[n]]];
End[];


(*useful conversions for Strong Field physics*)
EfromIAtomic::usage="EfromIAtomic[I] takes intensity (in W/cm^2) and transforms it to electric field intensity in atomic units.";
IfromEAtomic::usage="IfromEAtomic[E] takes electric field in atomic units and transforms it to intensity in W/cm^2.";
\[Omega]from\[Lambda]Atomic::usage="Given number of nm tu`rns into \[Omega] in AtomicUnits";
Tfrom\[Omega]Atomic::usage="Given \[Omega] [a.u.] turns it into period [a.u.]";
QuiverAtomic::usage="Quiver[E,\[Omega]] returns E/\!\(\*SuperscriptBox[\(\[Omega]\), \(2\)]\).";
PonderoAtomic::usage="Pondero[E,\[Omega]] returns \!\(\*SuperscriptBox[\(E\), \(2\)]\)/(2\[Omega]\!\(\*SuperscriptBox[\()\), \(2\)]\)";

Begin["`Private`"];
EfromIAtomic[I_]:=QuantityMagnitude@UnitConvert[Sqrt[Quantity[I,"W/cm^2"]/(1/2*Quantity[1,"ElectricConstant"]*Quantity[cAtomic,"AtomicUnitOfVelocity"])]*Quantity[1,"ElementaryCharge"]*Quantity[1,"BohrRadius"],"Hartrees"];
IfromEAtomic[E_]:=QuantityMagnitude@UnitConvert[(Quantity[E,"Hartrees"]/(Quantity[1,"ElementaryCharge"]*Quantity[1,"BohrRadius"]))^2 (1/2*Quantity[1,"ElectricConstant"]*Quantity[cAtomic,"AtomicUnitOfVelocity"]),"W/cm^2"];
\[Omega]from\[Lambda]Atomic[nm_]:=QuantityMagnitude@UnitConvert[Quantity[1,"SpeedOfLight"]Quantity[1,"PlanckConstant"]/Quantity[nm,"nm"],"Hartrees"];
Tfrom\[Omega]Atomic[\[Omega]_]:=QuantityMagnitude@UnitConvert[ Quantity[1,"PlanckConstant"]/Quantity[\[Omega],"Hartrees"],"AtomicUnitOfTime"];
QuiverAtomic[E_,\[Omega]_]:=E/\[Omega]^2;
PonderoAtomic[E_,\[Omega]_]:=E^2/(2\[Omega])^2;
End[];


(* ::Section:: *)
(*End package*)


EndPackage[];
