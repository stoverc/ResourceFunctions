(* ::Package:: *)

(* ::Title:: *)
(*AdditiveDigitalRoot*)


(* ::Text:: *)
(*Compute the additive digital root of an integer*)


(* ::Section:: *)
(*Main definition*)


ClearAll[AdditiveDigitalRoot];

Attributes[AdditiveDigitalRoot]={Listable};

AdditiveDigitalRoot[n_?(IntegerQ[#]&&NonNegative[#]&)]:=
	NestWhileList[Plus@@IntegerDigits[#]&,n,#>=10&][[-1]]


(* ::Section:: *)
(*Author Notes*)


(* ::Text:: *)
(*Future versions of this will likely contain expansion to allow for integers in bases other than 10.*)
