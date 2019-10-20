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


fsAtomic::usage="One femtosecond in atomic units";
cAtomic::usage="Speed of light in atomic units";
IpAtomic::usage="IpAtomic[atom,n] returns n'th (def. n) ionization energy of \"atom\" in atomic units.";
EfromIAtomic::usage="EfromIAtomic[I] takes Quantity I of intensity (usually W/\!\(\*SuperscriptBox[\(cm\), \(2\)]\)) and transforms it to electric field intensity in atomic units.";
\[Lambda]to\[Omega]::usage="Given number of nm turns into \[Omega] in AtomicUnits";
\[Omega]toT::usage="Given \[Omega] [a.u.] turns it into period [a.u.]";
\[Omega]toTfs::usage="Given \[Omega] [a.u.] turns it into period [fs]";
Quiver::usage="Quiver[E,\[Omega]] returns E/\!\(\*SuperscriptBox[\(\[Omega]\), \(2\)]\).";
Pondero::usage="Pondero[E,\[Omega]] returns \!\(\*SuperscriptBox[\(E\), \(2\)]\)/(2\[Omega]\!\(\*SuperscriptBox[\()\), \(2\)]\)";
Begin["`Private`"];
fsAtomic=QuantityMagnitude@UnitConvert[Quantity[1,"fs"],"AtomicUnitOfTime"];
cAtomic=UnitConvert[Quantity["ReducedPlanckConstant"]/( Quantity["FineStructureConstant"] Quantity["BohrRadius"] Quantity["ElectronMass"]),"AtomicUnitOfVelocity"];
IpAtomic[atom_,n_:1]:=QuantityMagnitude[UnitConvert[ElementData[atom,"IonizationEnergies"]/Quantity["AvogadroConstant"],"Hartrees"][[n]]];
EfromIAtomic[I_Quantity]:=UnitConvert[Sqrt[I/(1/2*Quantity[1,"ElectricConstant"]*cAtomic)]*Quantity[1,"ElementaryCharge"]*Quantity[1,"BohrRadius"],"Hartrees"];
\[Lambda]to\[Omega][nm_]:=UnitConvert[Quantity[1,"SpeedOfLight"]Quantity[1,"PlanckConstant"]/Quantity[nm,"nm"],"Hartrees"];
\[Omega]toT[\[Omega]_Quantity]:=UnitConvert[ Quantity[1,"PlanckConstant"]/\[Omega],"AtomicUnitOfTime"];
\[Omega]toTfs[\[Omega]_Quantity]:=UnitConvert[ Quantity[1,"PlanckConstant"]/\[Omega],"fs"];
Quiver[E_,\[Omega]_]:=E/\[Omega]^2;
Pondero[E_,\[Omega]_]:=E^2/(2\[Omega])^2;
End[];


(* ::Section:: *)
(*End package*)


EndPackage[];
