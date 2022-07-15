(* ::Package:: *)

(* ::Title:: *)
(*SumOfIntegerPowers*)


(* ::Text:: *)
(*Express an integer or \[PlusMinus]\[Infinity] as a sum of powers of one or more integer*)


(* ::Subsection::Closed:: *)
(*Auxiliary Functions for Powers of a Single Integer*)


ClearAll[iSingleIntegerList,iSingleIntegerPowers,iSingleIntegerCoeffs]

iSingleIntegerList[n_?(IntegerQ[#]&&#>=0&)]:=iSingleIntegerList[n,2];

iSingleIntegerList[Infinity,q_Integer]:=Return[{{q,Infinity}}];

iSingleIntegerList[n_?(IntegerQ[#]&&#>=0&),q_Integer]:=Module[
{
	n1=n,powers={},p,pp=ResourceFunction["PerfectPower"]
},
	(* n==\[Infinity] is a base case *)
	If[n==Infinity,
		Return[{{q,Infinity}}];
		(* Return[{{q,Inactivate[-Infinity]}}]; *)
	];
	
	(* n==0 is a base case *)
	If[n==0,
		Return[{{q,-Infinity}}];
		(* Return[{{q,Inactivate[-Infinity]}}]; *)
	];
	
	(* n==1 is a base case *)
	If[n==1,
		Return[{{q,0}}];
	];
	
	(* n==q is a base case *)
	If[n==q,
		Return[{{q,1}}];
	];
	
	(* n==q^\[Alpha] is a base case *)
	If[MemberQ[pp[n][[All,1]],q],
		Return[{{q,Log[q,n]}}];
	];
	
	While[n1!=0,
		p=Floor[Log[q,n1]];
		powers=Append[powers,p];
		n1=n1-q^p;
	];
	
	Return[
		Transpose[{
			ConstantArray[q,Length[powers]],
			powers
		}]
	];
]

iSingleIntegerPowers[a___]:=Block[
{
	list=iSingleIntegerList[a],
	tal
},
	If[Dimensions[tal=(Tally[list])[[All,1]]]=={2},
		Return[{tal}],
		Return[tal]
	];
];

iSingleIntegerCoeffs[a___]:=Block[
{
	list=iSingleIntegerList[a]
},
	If[Dimensions[list]=={2},
		Return[{1}],
		Return[Tally[list][[All,2]]]
	];
];


(* ::Subsection::Closed:: *)
(*Auxiliary Functions for Powers of Multiple Integers*)


ClearAll[iMultiIntegerList,iMultiIntegerPowers,iMultiIntegerCoeffs]

Options[iMultiIntegerList]={
	"HoldListProducts"->False
};

(*iMultiIntegerList[n_?(IntegerQ[#]&&#>=0&)]:=iMultiIntegerList[n,2];*)

iMultiIntegerList[Infinity,list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=Return[Join[Transpose[{list,ConstantArray[Infinity,Length[list]]}],{{0}}]];
iMultiIntegerList[-Infinity,list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=Return[Join[Transpose[{list,ConstantArray[Infinity,Length[list]]}],{{0}}]];

iMultiIntegerList[n_?(IntegerQ[#]&&#>=0&),list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=Module[{
	(* Functions *)
	maxat=ResourceFunction["PositionLargest"][#]&,
	ifi=ResourceFunction["InactiveFactorInteger"],
	PrimeMultipleOfCertainPrimesQ,
	(* Constants *)
	n1,holdprod=OptionValue["HoldListProducts"],done=False,
	(* Lists *)
	primes=list,
	floors={},primeprods={},coeffs={},exps={},powers={}
	
},
	PrimeMultipleOfCertainPrimesQ[k_?(IntegerQ[#]&&#>0&),p_List]:= SubsetQ[p,FactorInteger[k][[All,1]]];
	n1=n;
	
	(* n==0 is a base case *)
	If[n==0,
		(* Return[{Transpose[{list,ConstantArray[Inactivate[-Infinity],Length[list]]}]}];*)
		(* Return[{{First@primes,Inactivate[-Infinity]},{0}}]; *)
		Return[Join[Transpose[{list,ConstantArray[-Infinity,Length[list]]}],{{0}}]];
	];
	
	(* n==\[Infinity] is a base case *)
(*	If[n==Infinity,
		(* Return[{Transpose[{list,ConstantArray[Inactivate[-Infinity],Length[list]]}]}];*)
		(* Return[{{First@primes,Inactivate[-Infinity]},{0}}]; *)
		Return[Join[Transpose[{list,ConstantArray[Infinity,Length[list]]}],{{0}}]];
	];*)
	
	(* n==1 is a base case *)
	If[n==1,
		Return[{{First@primes,0},{0}}];
	];
	
	(* n\[Element]primes is a base case *)
	If[MemberQ[primes,n],
		Return[{{n,1},{0}}];
	];
	
	(* n==((product of primes)) may be a base case *)	
	If[!MemberQ[n,primes] && holdprod==True && PrimeMultipleOfCertainPrimesQ[n1,primes],
	(
		done=True;
		Return[{{0,1},{ifi[n1]}}];
	)
	];
	
	While[n1!=0 && n1!=1 && (!PrimeMultipleOfCertainPrimesQ[n1,primes] || (PrimeMultipleOfCertainPrimesQ[n1,primes] && done==False)),(* && !PrimeMultipleOfCertainPrimesQ[n1,primes],*)
		floors=Floor[Log[#,n1]]&/@primes;
		primeprods=MapThread[Power,{primes,floors}];
		i=maxat[primeprods];
		coeffs=Append[coeffs,primes[[i]]];
		exps=Append[exps,floors[[i]]];
		n1=n1-primeprods[[i]];
	];
	
	powers=Transpose[{coeffs,exps}];
	
	If[n1==0,
		powers=Append[powers,{0}];
	];
	
	If[n1==1,
	(
		powers=Append[powers,{Min@list,0}];
		powers=Append[powers,{0}];
	)
	];
	
	If[PrimeMultipleOfCertainPrimesQ[n1,primes],
		powers=Append[powers,{ifi[n1],1}];
	];
	
	(* Note: powers[[-1]] is the constant term! *)
	Return[powers];
];

iMultiIntegerPowers[a___]:=Block[{list=iMultiIntegerList[a]}, Return[Tally[list][[All,1]]];];
iMultiIntegerCoeffs[a___]:=Block[{list=iMultiIntegerList[a]}, Return[Tally[list][[All,2]]];];


(* ::Subsection::Closed:: *)
(*Main Definitions*)


ClearAll[SumOfIntegerPowers]

Options[SumOfIntegerPowers]={
	"Collect"->False,
	"FactorNegative"->False,
	"HoldListProducts"->False
};

SumOfIntegerPowers[n_?(IntegerQ[#]||#==Infinity||#==-Infinity&),opts:OptionsPattern[]]:=SumOfIntegerPowers[n,2,opts];

SumOfIntegerPowers[list:{__?(IntegerQ[#]||#==Infinity||#==-Infinity&)},q_Integer,opts:OptionsPattern[]]:=SumOfIntegerPowers[#,q,opts]&/@list/;Length[list]>1;

SumOfIntegerPowers[n_?(IntegerQ[#]||#==Infinity||#==-Infinity&),q_Integer,opts:OptionsPattern[]]:=Module[{
	list,pow,coeff,op,neg=False,coll=OptionValue["FactorNegative"]
},
	(* Handle negative n first because aux functions don't accept negative inputs *)
	If[n<0 && coll==True,
		Return[Inactive[Times][-1,SumOfIntegerPowers[-n,q,opts]]];
	];
	
	If[n<0 && coll==False,
	(
		neg=True;
		list=iSingleIntegerList[-n,q];
		pow=iSingleIntegerPowers[-n,q];
		coeff=-1*iSingleIntegerCoeffs[-n,q];
	),
	(
		list=iSingleIntegerList[n,q];
		pow=iSingleIntegerPowers[n,q];
		coeff=iSingleIntegerCoeffs[n,q];
	)
	];
	
	If[OptionValue["Collect"]==True,
	(
		op=Inactive[Power]@@@pow;
			
		op=If[coeff[[#]]==1,
			op[[#]],
			If[neg==False && coll==False,
				Inactive[Times][coeff[[#]],op[[#]]],
				Times[coeff[[#]],op[[#]]]
			]
		]&/@Range[1,Length[op]];
			
		op=If[Length[op]==1,
			First@op,
			Inactive[Plus]@@op
		];
	),
	(
		op=Inactive[Power]@@@list; (* can include HoldForm here for better-looking exponents (see below) *)
		
		If[neg==True && coll==False,
			op=-1*op;
		];
			
		op=If[Length[op]==1,
			First@op,
			Inactive[Plus]@@op
		];
	)
	];
	
	Return[op];
];

SumOfIntegerPowers[n:{__?(IntegerQ[#]||#==Infinity||#==-Infinity&)},list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=SumOfIntegerPowers[#,First@list,opts]&/@n/;Length[list]==1;

SumOfIntegerPowers[n:{__?(IntegerQ[#]||#==Infinity||#==-Infinity&)},list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=SumOfIntegerPowers[#,list,opts]&/@n;

SumOfIntegerPowers[n_?(IntegerQ[#]||#==Infinity||#==-Infinity&),list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=SumOfIntegerPowers[n,First@list,opts]/;Length[list]==1;

SumOfIntegerPowers[n_?(IntegerQ[#]||#==Infinity||#==-Infinity&),list:{__?(IntegerQ[#]&&#>1&)},opts:OptionsPattern[]]:=Module[{
	(* ifi=ResourceFunction["InactiveFactorInteger"],*)
	pairs,pow,coeff,
	op,const,oldpow,
	neg=False,coll=OptionValue["FactorNegative"]
},
	(* Handle negative n first because aux functions don't accept negative inputs *)
	If[n<0 && coll==True,
		Return[Inactive[Times][-1,SumOfIntegerPowers[-n,list,opts]]];
	];
	
	If[n<0 && coll==False,
	(
		neg=True;
		pairs=iMultiIntegerList[-n,list,"HoldListProducts"->OptionValue["HoldListProducts"]];
		pow=iMultiIntegerPowers[-n,list];
		coeff=-1*iMultiIntegerCoeffs[-n,list];
	),
	(
		pairs=iMultiIntegerList[n,list,"HoldListProducts"->OptionValue["HoldListProducts"]];
		pow=iMultiIntegerPowers[n,list];
		coeff=iMultiIntegerCoeffs[n,list];
	)
	];
	
	(* If n is a product of primes, return the product and do nothing else *)
	If[pairs[[1,1]]==0,
		Return[First@pairs[[2]]];
	];
	
	If[OptionValue["Collect"]==True,
	(	
		oldpow=pow;
		pow=DeleteCases[pow,_?(#=={0}&)];
		coeff=coeff[[;;-(Length[oldpow]-Length[pow]+1)]];
			
		op=If[Head[Head[#1]]=!=Inactive,
			Inactive[Power][#1,#2],
			#1
		]&@@@pow;
				
		op=If[coeff[[#]]==1,
			op[[#]],
			If[neg==False,
				Inactive[Times][coeff[[#]],op[[#]]],
				Times[coeff[[#]],op[[#]]]
			]
		]&/@Range[1,Length[op]];
			
		If[const!=0,
			op=Append[op,const];
		];
			
		If[Length[op]>1,
			op=Inactive[Plus]@@op,
			op=First@op
		];
	),
	(
(*		Print["Old pairs: "<>ToString[pairs]];*)
		
		const=First@pow[[-1]];
		pairs=pairs[[;;-2]];
		pow=pow[[;;-2]];
		coeff=coeff[[;;-2]];
		
		If[const=!=0,
			pairs=Append[pairs,{const,1}];
		];
		
(*		Print["New pairs: "<>ToString[pairs]];
		
		Print[const]; Print[pow]; Print[coeff];*)
		
		op=If[Head[Head[#1]]=!=Inactive,
			Inactive[Power][#1,#2], (* HoldForm[Power[#1,#2]],*)
			#1
		]&@@@pairs;
			
		If[neg==True && coll==False,
			op=-1*op;
		];
			
		If[Length[op]>1,
			op=Inactive[Plus]@@op,
			op=First@op
		];
	)
	];
	
	Return[op];
];


(* ::Section:: *)
(*Author Notes*)


(* ::ItemNumbered:: *)
(*In future versions, better handling of the "power of list" case will be included. For instance, I would argue that*)


(* ::Input:: *)
(*SumOfIntegerPowers[2^9*3^15*5, {2, 3, 5, 7}, "HoldListProducts" -> True]*)


(* ::Text:: *)
(*should actually be something like*)


(* ::Input:: *)
(*TagBox[StyleBox[RowBox[{RowBox[{"Inactive","[","Times","]"}],"[",RowBox[{RowBox[{RowBox[{"Inactive","[","Power","]"}],"[",RowBox[{"2",",","9"}],"]"}],",",RowBox[{RowBox[{"Inactive","[","Power","]"}],"[",RowBox[{"3",",","15"}],"]"}],",",RowBox[{RowBox[{"Inactive","[","Power","]"}],"[",RowBox[{"5",",","1"}],"]"}],",",RowBox[{RowBox[{"Inactive","[","Power","]"}],"[",RowBox[{"7",",","0"}],"]"}]}],"]"}],ShowSpecialCharacters->False,ShowStringCharacters->True,NumberMarks->True],FullForm]*)


(* ::Text:: *)
(*instead.*)


(* ::Text:: *)
(*I already have a fix written for this. Whenever I implement it, I'll be sure to edit info in "Options", D&O, and any other places.*)


(* ::ItemNumbered:: *)
(*Design changes may be implemented to change the behavior discussed in "Possible Issues":*)


(* ::Input:: *)
(*SumOfIntegerPowers[2^9*3^15*5,{2,3},"HoldListProducts"->True]*)


(* ::Input:: *)
(*SumOfIntegerPowers[2^9*3^15*5,{2,3},"HoldListProducts"->False]*)


(* ::Input:: *)
(*SumOfIntegerPowers[2^9*3^15*5,{2,3,5,7},"HoldListProducts"->True]*)


(* ::Input:: *)
(*SumOfIntegerPowers[2^9*3^15*5,{2,3,5,7},"HoldListProducts"->False]*)


(* ::Text:: *)
(*Perhaps these scenarios should be handled the same?*)
