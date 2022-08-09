(* ::Package:: *)

(* ::Title:: *)
(*MultiplicativeDigitalRoot*)


(* ::Text:: *)
(*Compute the multiplicative digital root of an integer*)


(* ::Section:: *)
(*Main definition*)


ClearAll[MultiplicativeDigitalRoot];

Attributes[MultiplicativeDigitalRoot]={Listable};

MultiplicativeDigitalRoot[n_?(IntegerQ[#]&&NonNegative[#]&)]:=
	NestWhileList[Times@@IntegerDigits[#]&,n,#>=10&][[-1]]


(* ::Section:: *)
(*Author Notes*)


(* ::Item:: *)
(*Future versions of this will likely contain expansion to allow for integers in bases other than 10.*)


(* ::Item:: *)
(*As mentioned on the page Problems & Puzzles, Puzzle 341, there is a modified definition of multiplicative persistence due to Erd\[ODoubleAcute]s wherein one only multiplies the nonzero digits at each step. This modification is not implemented herein but may be included in a future update.*)


(* ::Item:: *)
(*In a future update, I'm going to make the "Neat Examples" table look better. That's definitely a version 2 edit!*)
