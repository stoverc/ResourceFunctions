(* ::Package:: *)

(* ::Title:: *)
(*Fusc*)


(* ::Text:: *)
(*Evaluate the fusc function*)
(**)


ClearAll[Fusc]


Attributes[Fusc]={Listable,NumericFunction};

Fusc[0]:=0;

Fusc[1]:=1;

Fusc[n_Integer?(EvenQ[#]&&#>0&)]:=Fusc[n/2];

Fusc[n_Integer?(OddQ[#]&&#>0&)]:=Module[{
	m=(n-1)/2
},

	Return[Fusc[m]+Fusc[m+1]];
]
