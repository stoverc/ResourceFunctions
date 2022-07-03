(* ::Package:: *)

(* ::Text:: *)
(*https://en.wikipedia.org/wiki/Benford%27s_law*)
(**)
(*Starting with 0.xx: http://www.clayford.net/statistics/tag/benfords-law/*)


(* ::Text:: *)
(*TODO: *)


(* ::Item:: *)
(*Option to include 0s (for numbers less than 1) *)


(* ::Item:: *)
(*Handle other bases?*)


(* ::Item:: *)
(*Examples from Entity, W|A, etc. data sets*)


(* ::Item:: *)
(*Second, third, etc. digits?*)


(* ::Item:: *)
(*Quantity, etc., objects*)


ClearAll[BenfordAnalysis];

BenfordAnalysis::nonnum="Input list contains non-numerics; these have been ignored.";
BenfordAnalysis::zero="Input list contains zeroes; these have been ignored.";

BenfordAnalysis[l_List]:=Module[{
	numlist=Select[l,NumericQ[#]&],
	digits={1,2,3,4,5,6,7,8,9},
	digitsused,digitsmissing,tallies,
	benford
},
	If[
		numlist != l,
		Message[BenfordAnalysis::nonnum]
	];
	
	If[
		MemberQ[numlist,0],
		(
			Message[BenfordAnalysis::zero];
			numlist=Select[numlist,#!=0&];
		)
	];
	
	digitsused=First/@Flatten/@RealDigits/@(numlist*1.);
	tallies=Tally[digitsused];
	
	digitsused=Sort[DeleteDuplicates[digitsused]];
	digitsmissing=Complement[digits,digitsused];
	
	AppendTo[tallies,{#,0}]&/@digitsmissing;
	tallies=SortBy[tallies,First];
	
	benford={First[#],PercentForm[1.0*Last[#]/Length[numlist]]}&/@tallies;
	
	Return[benford]
]


(* ::Input:: *)
(*BenfordAnalysis[Fibonacci/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[LucasL/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[Table[2.^n,{n,0,95,1}]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[BellB/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[BernoulliB/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[EulerE/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[Zeta[#,1/2]&/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[StirlingS1[#,2]&/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*BenfordAnalysis[StirlingS2[#,2]&/@Range[1,1000]]//Column*)


(* ::Input:: *)
(*(* Populations *)*)


(* ::Input:: *)
(*pops=#["Population"]&/@RandomChoice[EntityList[EntityClass["ZIPCode","POBox"]],500];*)
(*pops=Select[pops,!MissingQ[#]&];*)
(*pops=QuantityMagnitude[#]&/@pops;*)
(*BenfordAnalysis[pops]//Column*)


(* ::Input:: *)
(*(* Bridge Lengths *)*)


(* ::Input:: *)
(*lens=#["Length"]&/@RandomChoice[EntityList[EntityClass["Bridge",All]],50];*)
(*lens=Select[lens,!MissingQ[#]&];*)
(*lens=QuantityMagnitude[#]&/@lens*)
(*BenfordAnalysis[lens]//Column*)


(* ::Input:: *)
(*(* Building Heights *)*)


(* ::Input:: *)
(*heights=#["Height"]&/@RandomChoice[EntityList[EntityClass["Building", {"ConstructionStatus", "Completed"}]],100];*)
(*heights=Select[heights,!MissingQ[#]&];*)
(*heights=QuantityMagnitude[#]&/@heights;*)
(*BenfordAnalysis[heights]//Column*)


(* ::Input:: *)
(*(* Closing prices of all stocks in S&P 500 and NASDAQ *)*)


(* ::Input:: *)
(*SP500=EntityValue[EntityClass["Financial","SP500"],"Close"];*)
(*SP500=Select[SP500,!MissingQ[#]&];*)
(*SP500=QuantityMagnitude[#]&/@SP500;*)
(*BenfordAnalysis[SP500]//Column*)


(* ::Input:: *)
(*NASDAQ=FinancialData[RandomChoice[FinancialData["NASDAQ:*"],500]];*)
(*NASDAQ=Select[NASDAQ,!MissingQ[#]&];*)
(*NASDAQ=QuantityMagnitude[#]&/@NASDAQ;*)
(*BenfordAnalysis[NASDAQ]//Column*)


(* ::Input:: *)
(*(* Land area in mi^2 and km^2 for every country in the world *)*)


(* ::Input:: *)
(*area=#["LandArea"]&/@EntityList[EntityClass["Country",All]];*)
(*mi=QuantityMagnitude[#]&/@area;*)
(*km=QuantityMagnitude[UnitConvert[#,"Kilometers"^2]]&/@area;*)
(*BenfordAnalysis[mi]//Column*)
(*BenfordAnalysis[km]//Column*)


(* ::Input:: *)
(*(* Style Tests *)*)


(* ::Input:: *)
(*Grid[BenfordAnalysis[NASDAQ],Dividers->{{False,All},Flatten[{False,ConstantArray[All,8]}]}]*)
