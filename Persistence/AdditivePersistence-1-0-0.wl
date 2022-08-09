(* ::Package:: *)

(* ::Title:: *)
(*AdditivePersistence*)


(* ::Text:: *)
(*Compute the additive persistence of an integer*)


(* ::Section:: *)
(*Main definition*)


ClearAll[AdditivePersistence];

Attributes[AdditivePersistence]={Listable};

AdditivePersistence[n_?(IntegerQ[#]&&NonNegative[#]&)]:=
	Length[NestWhileList[Plus@@IntegerDigits[#]&,n,#>=10&]]-1


(* ::Section:: *)
(*Author Notes*)


(* ::Text:: *)
(*Future versions of this will likely contain expansion to allow for integers in bases other than 10.*)
