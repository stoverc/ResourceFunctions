(* ::Package:: *)

(* ::Title:: *)
(*PlayWordle*)


(* ::Text:: *)
(*Play the popular online word game Wordle*)


(* ::Section:: *)
(*Main definition*)


ClearAll[style];

style[y_,color_]:=Highlighted[Style[y,FontSize->28,FontFamily->"Arial",White,FontWeight->Bold]/.{Highlighted[a_,opts___]:>a},Background->color,ImageSize->{40,40},Alignment->{Center,Center}]


ClearAll[PlayWordle]

PlayWordle[]:=DynamicModule[{
	guessnumber=0,
	(* list=Select[WordList[],StringLength[#]==5&],*)
	list=ResourceData[ResourceObject["Wordle Word List"]],
	i,n,keys={},word="",letters={},correct={},partial={},history={},
	green={},gray={},yellow={},colored={},def={},len=True,realword=True,
	temp=ConstantArray["",5],tempgreen={},tempyellow={},
	(*dgy=RGBColor["#3a3a3c"],*)dgy=GrayLevel[0.375],dyw=RGBColor["#eab308"],dgn=RGBColor["#63aa55"],lgy=GrayLevel[0.675]
	},

	n=RandomInteger[{1,Length[list]}];
	word=list[[n]];
	(* word=s; *)
	def=WordDefinition[word];
	word=ToLowerCase[word];
	letters=Characters[word];
	
	keys={
		{"q","w","e","r","t","y","u","i","o","p"},
		{"a","s","d","f","g","h","j","k","l"},
		{"z","x","c","v","b","n","m"}
	}/.{a_String:>Highlighted[Style[a,White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->lgy,ImageSize->40,Frame->True,FrameStyle->Black]};
	keys=Column[keys,Alignment->{Center,Center}];

Manipulate[
	main[str_String,g_Integer]:=Module[{
		guessletters
	},
		guessnumber+=1;
		guessletters=Characters[ToLowerCase[str]];
		correct=Table[letters[[i]]==guessletters[[i]],{i,1,5}];
		temp=ConstantArray["",5];tempgreen={};tempyellow={};
				
		For[i=1,i<=Length[guessletters],i++,
			If[guessletters[[i]]===letters[[i]],
			(
				temp[[i]]="Green";
				AppendTo[tempgreen,guessletters[[i]]];
			)
			]
		];
		
		For[i=1,i<=Length[guessletters],i++,
			If[temp[[i]]!="Green",
				If[!MemberQ[letters,guessletters[[i]]]||(MemberQ[letters,guessletters[[i]]]&&Count[guessletters,guessletters[[i]]]>Count[letters,guessletters[[i]]]&&MemberQ[tempgreen,guessletters[[i]]])||(MemberQ[letters,guessletters[[i]]]&&Count[guessletters,guessletters[[i]]]>Count[letters,guessletters[[i]]]&&MemberQ[tempyellow,guessletters[[i]]]),
					temp[[i]]="Gray",
					(
						temp[[i]]="Yellow";
						AppendTo[tempyellow,guessletters[[i]]];
					)
				];
			];
		];
		
		For[i=1,i<=Length[guessletters],i++,
			If[temp[[i]]=="Green",
			(
				AppendTo[green,guessletters[[i]]];
				AppendTo[colored,style[guessletters[[i]],dgn]]
			),
			(
				If[temp[[i]]=="Yellow",
				(
					AppendTo[yellow,guessletters[[i]]];
					AppendTo[colored,style[guessletters[[i]],dyw]]
				),
				(
					AppendTo[gray,guessletters[[i]]];
					AppendTo[colored,style[guessletters[[i]],dgy]]
				)
				]
			)
			];
		];

		For[i=1,i<=Length[gray],i++,
			keys=(keys/.{Highlighted[Style[gray[[i]],opts___],opts2___]:>gray[[i]]})/.{gray[[i]]->Highlighted[Style[gray[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dgy,ImageSize->40,Frame->True,FrameStyle->Black]};
		];

		For[i=1,i<=Length[yellow],i++,
			keys=(keys/.{Highlighted[Style[yellow[[i]],opts___],opts2___]:>yellow[[i]]})/.{yellow[[i]]->Highlighted[Style[yellow[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dyw,ImageSize->40,Frame->True,FrameStyle->Black]};
		];
		
		For[i=1,i<=Length[green],i++,
			keys=(keys/.{Highlighted[Style[green[[i]],opts___],opts2___]:>green[[i]]})/.{green[[i]]->Highlighted[Style[green[[i]],White,FontWeight->Bold,FontSize->16,FontFamily->"Arial"],Background->dgn,ImageSize->40,Frame->True,FrameStyle->Black]};
		];
		
		If[0<=guesses,
		(
			AppendTo[history,colored];
			colored={};
			(* green={};yellow={};gray={}; *)
			(* If you clear these out each time, the keyboard update is wrong! *)
		)
		];
		
		If[str==word,
		(
			done=True;			
		),
		(
			Return[main[x,g-1]]
		)
		];
	];
	
	Grid[{
	{
		""
	},
	{
		If[ToString[x] != word && guesses > 0,
			Row[{
				Style["Guesses remaining: ",FontWeight->Bold,FontFamily->"Arial",FontSize->32],
				Style[guesses,FontFamily->"Arial",FontSize->32]
			}],
			
			If[ToString[x]==word,
				If[!MissingQ[def],
					Row[{
						Style[Row[{Style["Word: ",FontWeight->Bold], word}],Darker[Green],FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}],
						" ",
						Button[
							"?",
							{
								MessageDialog[
									Column[{
										Style["Definition(s):", FontWeight -> Bold],	
										Column[
											Table[Row[{"   ",i,". ",def[[i]]}],{i,1,Length[def]}]
										]
									}]
								]
							}, BaselinePosition->Bottom,Alignment->Center,Appearance->None,BaseStyle->{Darker[Green]}
						]
					}],
					Style[Row[{Style["Word: ",FontWeight->Bold],word}],Darker[Green],FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}]
				],
				
				If[guesses == 0,
					If[!MissingQ[def],
						Row[{
							Style[Row[{Style["Word: ",FontWeight->Bold],word}],Red,FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}],
							" ",
						Button[
							"?",
							{
								MessageDialog[
									Column[{
										Style["Definition(s):", FontWeight -> Bold],
										Column[
											Table[Row[{"   ",i,". ",def[[i]]}],{i,1,Length[def]}]
										]
									}]
								]
							}, BaselinePosition->Bottom,Alignment->Center,Appearance->None,BaseStyle->{Red}
						]
					}],
					Style[Row[{Style["Word: ",FontWeight->Bold],word}],Red,FontFamily->"Arial",FontSize->32,TextAlignment->{Center, Center}]
					]
				]
			]
		]
	},
	{
		"\n"
	},
	{
		Grid[{
		{
			InputField[Dynamic[x],FieldHint->"Enter your guess",FieldSize->{20,1},BaselinePosition->Scaled[0],BaseStyle->{FontSize->16,FontFamily->"Arial"}],
			SpanFromLeft,
			SpanFromLeft,
			If[ToString[x]!=word && guesses>0,
				Column[{
					Column[
						Table[
							Row[{
								Style[i,FontFamily->"Arial",FontSize->18],
								". ",
								Grid[{history[[i]]},BaselinePosition->Bottom]
							}],
						{i,1,guessnumber}]
					]
				},Alignment->Center,ItemSize->20],
					
				If[ToString[x]==word,
					Column[{
						Column[
							Table[
								Row[{
									Style[i,FontFamily->"Arial",FontSize->18],
									". ",
									Grid[{history[[i]]},BaselinePosition->Bottom]
								}],
							{i,1,guessnumber}]
						]
					},Alignment->Center,ItemSize->20],
				
					If[guesses==0,
						Column[{
							Column[
								Table[
									Row[{
										Style[i,FontFamily->"Arial",FontSize->18],
										". ",
										Grid[{history[[i]]},BaselinePosition->Bottom]
									}],
								{i,1,guessnumber}]
							]
					},Alignment->Center,ItemSize->20]
				]
			]
		]
		}
		},Frame->None]
	},
	{
		"\n"
	},
	{
		Column[
			Row[Riffle[keys[[1,#]]," "]]&/@Range[1,3]
		, Alignment->Center]
	},
	{
		"\n"
	}
	},Frame->None],
	
	Row[{
		Button[
			"Submit Guess",
			{
				If[StringLength[ToString[x]]==5,len=True,(len=False;MessageDialog["Guesses must have exactly five letters. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" has " <> ToString[StringLength[ToString[x]]] <> " letters."])];
				If[MemberQ[list,ToString[x]],realword=True,(realword=False;If[len==True,MessageDialog["Guesses must consist of valid dictionary words. Your guess of \"" <> ToLowerCase[ToString[x]] <> "\" is not in the dictionary."]])];
				If[done==False&&guesses>0&&len==True&&realword==True,main[ToString[x],guesses--]];
			}
		],
		Button[
			"Reset",
			clear[]
		]
	}],
	
	{{x,Null},None},{{guesses,6},None},{{done,False},None},
	
	TrackedSymbols:>{x,guesses,done},
	
	Initialization:>(
		clear[]:=(
			done=False;
			x=Null;
			guesses=6;
			len=True;
			realword=True;
			guessnumber=0;
			n=RandomInteger[{1,Length[list]}];
			word=list[[n]];
			word=ToLowerCase[word];
			def=WordDefinition[word];
			letters=Characters[word];
			correct={};history={};
			temp=ConstantArray["",5];tempgreen={};tempyellow={};
			keys={
				{"q","w","e","r","t","y","u","i","o","p"},
				{"a","s","d","f","g","h","j","k","l"},
				{"z","x","c","v","b","n","m"}
			}/.{a_String:>Highlighted[Style[a,White,FontWeight->Bold,FontSize->16,FontFamily->"Arial",TextAlignment->Center],Background->lgy,ImageSize->40,Frame->True,FrameStyle->Black]};
			keys=Column[keys,Alignment->{Center,Center}];
			green={};gray={};yellow={};colored={};
			dgy=GrayLevel[0.375];dyw=RGBColor["#eab308"];dgn=RGBColor["#63aa55"];lgy=GrayLevel[0.675];
		)
	)
, Alignment->Center,ContentSize->{700,Automatic},FrameLabel->"PlayWordle"]
]


(* ::Section:: *)
(*Author Notes*)


(* ::Item:: *)
(*Online sources suggest that the official Wordle game has a list of words it uses for the game (link) and a separate word list consisting of what it considers "valid guesses" (link).*)


(* ::Subitem:: *)
(*This source says that the daily puzzle words will never be pulled from the "valid guesses" list, but I have not taken the time to investigate the Wordle source code extensively enough to see whether this is true. Future versions may consist of tweaks along these lines.*)


(* ::Subitem:: *)
(*To the best of my knowledge, the resource data object Wordle Word List is a combination of all possible Wordle-related words (i.e. a \!\( *)
(*TagBox[*)
(*ButtonBox["Union",*)
(*BaseStyle->{"Link"},*)
(*ButtonData->"paclet:ref/Union",*)
(*ContentPadding->False],*)
(*MouseAppearanceTag["LinkHand"]]\) of the two above-mentioned word lists).*)


(* ::Item:: *)
(*There are plans for future versions to implement:*)


(* ::Subitem:: *)
(*allowing users to choose from different word lists (the Wordle one, WordList, and maybe others)*)


(* ::Subitem:: *)
(*adjusting the number of initial guesses*)


(* ::Subitem:: *)
(*specifying whether guesses of length len!=5 are allowed*)


(* ::Subitem:: *)
(*specifying whether non-list guesses are allowed*)


(* ::Subitem:: *)
(*(possibly) choosing formatting options (colors, etc.) for the game itself*)


(* ::Item:: *)
(*Special thanks to Bob Sandheinrich for finding a coloring error in a previous version of this resource.*)


(* ::Item:: *)
(*Despite its awesomeness, the recent blog implementation by David Reiss is totally independent from this resource. Even so, the author thanks David for his generous conversations concerning Wordle and this resource in particular.*)
