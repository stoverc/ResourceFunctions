(* ::Package:: *)

(* ::Title:: *)
(*RandomMetalPseudoSubgenre*)


(* ::Text:: *)
(*Generate a new metal subgenre in the spirit of the many metal microgenres*)


(* ::Section:: *)
(*Main definition*)


ClearAll[RandomMetalPseudoSubgenre];

Attributes[RandomMetalPseudoSubgenre]={Listable};

RandomMetalPseudoSubgenre[n_Integer]:=Table[RandomMetalPseudoSubgenre[],{i,1,n}];
RandomMetalPseudoSubgenre[UpTo[n_Integer]]:=Table[RandomMetalPseudoSubgenre[],{i,1,n}];

RandomMetalPseudoSubgenre[]:=Module[{
	absfirst={"new wave of","first wave of","second wave of"},
	first={"alternative","ambient","atmospheric","depressive","experimental","heavy","neo","nu",
		"old school","progressive","traditional"},
	(* \[DownArrow] Handle possibility of "black" and "blackened" together \[DownArrow] *)
	prefixes={"anarcho-","autonomous","avante-garde","biker","black","blackened","brutal","cavernous",
		"Celtic","chaotic","cosmic","crust","dark","death","dissonant","doom","drone",
		"electro","folk","forest","funeral","funk","garage","glam","gore","gothic","grind","groove",
		"hardcore","horror","indie","industrial","instrumental","interdimensional","math","melodic","minimal",
		"necro","neo-classical","noir","noise","pagan","pirate","post-","power","primitive","psychadelic","rap","raw",
		"sci-fi","slam","sludge","speed","stoner","symphonic","technical","thrash","Viking",
		"void","war","witch"},
	locs={"American","Appalachian","Arabic","Austrian","Brazillian","British","California","Chinese",
		"Dutch","Finnish","Florida","French","German","Greek","Hungarian","Icelandic","Iranian","Irish",
			"Latin","Midwest","Norwegian","NYC","Polish","Russian","Scandinavian","Slavic","Southern",
			"Swedish","Tampa","Ukranian"},
	suffixes={"crust","death","djent","doom","screamo","stench","thrash"},
	end={"core","death","doom","fusion","gaze","gothabilly","grind","grunge",(* "metal", *)
		" 'n' roll","psychobilly","punk",(* "rock", *)"sass","scape","thrash","violence","wave"},
	(* etc={"/","-"}, *)
	absrand,abs,firstrand,firstt,prerand,pres,locrand,nat,suffrand,suffs,endrand,endd,list1,list2,list3
},
	absrand=RandomInteger[{0,1}];
	abs=RandomSample[absfirst,absrand];

	firstrand=RandomInteger[{0,1}];
	firstt=RandomSample[first,firstrand];

	prerand=RandomInteger[{1,5}];
	pres=RandomSample[prefixes,prerand];

	locrand=RandomInteger[{0,1}];
	nat=RandomSample[locs,locrand];

	suffrand=RandomInteger[{1,2}];
	suffs=RandomSample[suffixes,suffrand];

	endrand=1;
	endd=RandomSample[end,endrand];

	(* Avoid things like "...thrash...thrash", "...death...death" etc. *)
	If[!DisjointQ[suffs,pres],pres=Complement[pres,suffs]];
	If[!DisjointQ[endd,pres],pres=Complement[pres,endd]];
	If[!DisjointQ[endd,suffs],suffs=Complement[suffs,endd]];

	(* StringReplace avoids things like post-- and anarcho-- *)
	list1=StringReplace[StringRiffle[pres," "],"--"->"-"];
	list2=StringJoin[If[abs!={},abs <> " ",""],If[firstt!={},firstt <> " ",""],If[nat!={},nat <> " ",""],list1];
	list2=StringJoin[If[abs!={},abs <> " ",""],If[nat!={},nat <> " ",""],If[firstt!={},firstt <> " ",""],list1];
	list3=StringJoin[list2," ",suffs,endd];
	list3=Capitalize[list3, "TitleCase"];
	list3=StringReplace[list3,{"- "->"-"}];
	(* \[UpArrow] handles "post- blah" and "anarcho- blah" \[UpArrow] *)

	Return[list3]
];


(* ::Section:: *)
(*Author Notes*)


(* ::Item:: *)
(*There are several other notable metal genre prefixes that could have gone into the lists used herein; however, some prefixes deemed either "trivial" or "obscene" were omitted.*)


(* ::Item:: *)
(*There is a great deal of musical knowledge built in to the Wolfram Language. As a starting point, one can check the entities listed under the "Music" subsection at Guide: Cultural Data.*)
