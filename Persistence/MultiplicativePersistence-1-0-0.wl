(* ::Package:: *)

(* ::Title:: *)
(*MultiplicativePersistence*)


(* ::Text:: *)
(*Compute the multiplicative persistence of an integer*)


(* ::Section:: *)
(*Main definition*)


ClearAll[MultiplicativePersistence];

Attributes[MultiplicativePersistence]={Listable};

MultiplicativePersistence[n_?(IntegerQ[#]&&NonNegative[#]&)]:=
	Length[NestWhileList[Times@@IntegerDigits[#]&,n,#>=10&]]-1


(* ::Section:: *)
(*Author Notes*)


(* ::Item:: *)
(*Future versions of this will likely contain expansion to allow for integers in bases other than 10.*)


(* ::Item:: *)
(*Extensions, including those mentioned in the corresponding MathWorld article, will may also be included in a future update.*)


(* ::Item:: *)
(*As mentioned on the page Problems & Puzzles, Puzzle 341, there is a modified definition of multiplicative persistence due to Erd\[ODoubleAcute]s wherein one only multiplies the nonzero digits at each step. This modification is not implemented herein but may be included in a future update.*)
