(*******************************************************************************
 * (C) Copyright IBM Corporation 2017
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    IBM Algorithms & Machines team
 *******************************************************************************)
 
 (* Mathematica Package *)
    
BeginPackage["ExaBoundsGeneric`"]

(* Exported symbols added here with SymbolName::usage *)

CoreLayer::usage =
  "CoreLayer[]"
  
DieLayer::usage =
  "DieLayer[]" 
  
SocketLayer::usage =
  "SocketLayer[]" 
  
CardLayer::usage =
  "CardLayer[]" 
  
RackUnitLayer::usage =
  "RackUnitLayer[]" 
  
RackLayer::usage =
  "RackLayer[]" 
  
AisleLayer::usage =
  "AisleLayer[]" 
  
SystemLayer::usage =
  "SystemLayer[]"  
  
MemoryL1Cache::usage =
  "MemoryL1Cache[]"

MemoryL2Cache::usage =
  "MemoryL2Cache[]"

MemoryL3Cache::usage =
  "MemoryL3Cache[]"
  
MemoryDRAM::usage =
  "MemoryDRAM[]"
  
ExecutionModes::usage =
  "ExecutionModes[]"
  
ExecutionModeSingleThreaded::usage =
  "ExecutionModeSingleThreaded[]"

ExecutionModeMultiThreaded::usage =
  "ExecutionModeMultiThreaded[]"

SMTModes::usage =
  "SMTModes[maxthreads_]"

SMTModeDynamicResources::usage =
  "SMTModeDynamicResources[]"
  
SMTModeDisabled::usage =
  "SMTModeDisabled[]"

IsKeyValueListQ::usage = 
  "IsKeyValueListQ[arg_]"

GetKeyValue::usage = 
  "GetKeyValue[keyValueList_, key_]"
 
SetKeyValue::usage =
  "SetKeyValue[keyValueList_, key_, value_]"
  
MergeKeyValueList::usage =
  "MergeKeyValueList[baseList_, UpdateKeyValuePairs_]"

KeyValueList2Association::usage =
  "KeyValueList2Association[algKeyValueList_]"
  
Association2KeyValueList::usage =
  "Association2KeyValueList[association_]"
  
GetAlgorithmName::usage =
  "GetAlgorithmName[algInfo_, index_]"

GetAlgorithmKeyValueList::usage =
  "GetAlgorithmKeyValueList[algInfo_, algoIndex_,scalingParameters_, dataProfileId_,threadId_]"
  
SetAlgorithmKeyValue::usage =
  "SetAlgorithmKeyValue[algInfo_, sel_List, key_, value_]"
  
GetScalingParameters::usage =
  "GetScalingParameters[algDataAssociation_]"

GetScalingConfigurations::usage =
	"GetScalingConfigurations[algDataAssociation_]"
	
(* Returns the subset of GetScalingConfigurations including only the ones having
the values of scaling parameters as specified in partialScalingParameters.
partialScalingParameters is an association that might include a subset of the overall
scaling parameters. *)
GetScalingConfigurationsHaving::usage =
	"GetScalingConfigurationsHaving[algDataAssociation_,partialScalingConfiguration_] "<>
	"or GetScalingConfigurationsHaving[algInfo_,alg_,partialScalingConfiguration_]"
	
StripScalingParameters::usage = "StripScalingParameters[algInfo_,stripParameters_:Automatic]"

GetDataProfileCount::usage =
  "GetDataProfileCount[algInfo_, alg_, scaling_]"

GetThreads::usage = 
  "GetThreads[algInfo_, alg_, scaling_, profile_]"
  
  
ExaBoundsGetTableValue::usage =
  "ExaBoundsGetTableValue[table_, key_]"

ExaBoundsMergeState::usage =
  "ExaBoundsMergeState[ExaBoundsState_, KeyValueList_]"

ExaBoundsLevelName::usage =
  "ExaBoundsLevelName[level_]"
  
SetExaBoundsFormat::usage = 
  "SetExaBoundsFormat[format_]"
  
GetExaBoundsFormat::usage = 
  "GetExaBoundsFormat[]"
  
ExaBoundsFormatAutoSelect::usage = 
  "ExaBoundsFormatAutoSelect[interactiveOpt_,presentationOpt_,publicationOpt_]"
  
ExaBoundsFormatInteractiveQ::usage = 
  "ExaBoundsFormatPublicationQ[]"
  
ExaBoundsFormatPresentationQ::usage = 
  "ExaBoundsFormatPublicationQ[]"
  
ExaBoundsFormatPublicationQ::usage = 
  "ExaBoundsFormatPublicationQ[]"

ExaBoundsDirectory::usage = "ExaBoundsDirectory[]"

LoadJSON::invalidjson = "Could not load JSON file \"`1`\"."

Begin["`Private`"] (* Begin Private Context *)

(* Get ExaBoundsDirectory based on location of Init package (should always be in main directory)  *)
ExaBoundsDirectory[] = DirectoryName[FindFile["ExaBoundsInit`"]]; 

(* Replacement list to use with level names *)
(*level = {core -> 0, die -> 1, socket -> 2, card -> 3, rackunit -> 4, rack -> 5, aisle -> 6, system -> 7};*)

(* Calculation of layer indices *)
CoreLayer[] = 0;
DieLayer[] = CoreLayer[] + 1;
SocketLayer[] = DieLayer[] + 1;
CardLayer[] = SocketLayer[] + 1;
RackUnitLayer[] = CardLayer[] + 1;
RackLayer[] = RackUnitLayer[] + 1;
AisleLayer[] = RackLayer[] + 1;
SystemLayer[] = AisleLayer[] + 1;

MemoryL1Cache[] = 1;	(* Memory layers start from 1 *)
MemoryL2Cache[] = MemoryL1Cache[] + 1;
MemoryL3Cache[] = MemoryL2Cache[] + 1;
MemoryDRAM[] = MemoryL3Cache[] + 1;

(* Execution and multithreaded modes *)
ExecutionModes[] = {	"single-threaded" -> "Single-threaded execution mode",
						"multi-threaded" -> "Multi-threaded execution mode"	};
						
ExecutionModeSingleThreaded[] = ExecutionModes[][[1,1]];
ExecutionModeMultiThreaded[] = ExecutionModes[][[2,1]];

SMTModes[maxthreads_] :=
Block[{list},
	list = {	0 -> "Each thread may potentially use all resources",
				1 -> "SMT disabled"	};
	If[maxthreads > 1,
		AppendTo[list, # -> "SMT-" <> ToString[#]] & /@ Range[2, maxthreads]
	];
	Return[list];
];

SMTModeDynamicResources[] = SMTModes[0][[1,1]];
SMTModeDisabled[] = SMTModes[0][[2,1]];

(* Return True if the arg is a KeyValueList *)
IsKeyValueListQ[arg_] := Module[{ret=True},
	ret = ListQ[arg];
	If[!ret,Return[False]];
		
	ret = And@@ (Length[#]==2 & /@ arg);
	If[!ret,Return[False]];
		
	ret = And@@ (StringQ[#[[1]]] & /@ arg);
	If[!ret,Return[False]];
	
	Return[True];
];
  
(* Retrieve value associated with key from key-value list *)
(* Previous: ExaBoundsGetPar *)
(* Input: 
   keyValueList: List of tuples (key, value pairs)
   key: name of key to look up
   
   Returns value associated with key
 *)
GetKeyValue[keyValueList_, key_] := 
 Module[{ret, tuple},
   tuple = Flatten[Select[keyValueList, #[[1]] == key &, 1], 1];
   If[ Length[tuple] != 2, 
    ret = 0; 
    If [StringLength[key] != 0,
      Message[KeyValueList::unknownkey, key]
    ], 
    ret = tuple[[2]]
   ];
   ret
 ];
 KeyValueList::unknownkey = "Unknown key \"`1`\""
(* Update the key associated with key in the key-value list with value *)
(* Key has to exist in the list *)
(* Previous: ExaBoundsPutPar *)
(* Input:
   keyValyueList: List of key-value pairs
   key: name of key to update
   value: new value of key
   
   Returns updated key-value list
*)
(* TODO: Add a HoldFirst so there is no need to return. *)
SetKeyValue[keyValueList_, key_, value_] :=
  Module[{ret, tuple},
   
   tuple = Flatten[Select[keyValueList, #[[1]] == key &, 1], 1];
   If[
    Length[tuple] == 0, 
    Message[KeyValueList::unknownkey, key]
    ];
   
   ret = keyValueList /. {key, _} -> {key, value}
];

(* Merge key-value lists. The key in baseList will be overwritten by the value in UpdateKeyValuePairs *)
MergeKeyValueList[baseList_, UpdateKeyValuePairs_] :=
  Module[{ret, param,i},
   ret = baseList;
   
   For[i = 1, i <= Length[UpdateKeyValuePairs], i++, 
    param = UpdateKeyValuePairs[[i]];
    ret = SetKeyValue[ret, param[[1]], param[[2]]]
    ];
   
   Return[ret];
];


(* Translate a keyValueList to an association to ease the data access *)
KeyValueList2Association[keyValueList_] := Association[ Rule@@@keyValueList ];


(* Translate an association to a keyValueList *)
Association2KeyValueList[association_] := {Keys[association], Values[association] } // Transpose;
   
(* Get name of algorithm with index index from algInfo structure *)
GetAlgorithmName[algInfo_, index_] := Keys[algInfo][[index]];
 
(* Overloaded function which accepts a list instead of independent parameters *)
GetAlgorithmName[algInfo_, sel_List] := GetAlgorithmName[algInfo, sel[[1]]];
 
(* Get KeyValue list of algorithm properties given:
	the algorithm algoIndex (* either name or numeric index *), 
	scaling parameters scalingParameters, 
	thread threadID and the dataProfileId.
	
	The default parameters as for an application with no
*)
GetAlgorithmKeyValueList[algInfo_, algoIndex_, scalingConfiguration_: 1, dataProfileId_: 1, threadId_: 1] :=
 Module[{ret,scalingConfigurationAssociation,listOfRuns},
 	If[IntegerQ[scalingConfiguration],(* scalingParameter shall be either an association or an integer index. *) 
 		scalingConfigurationAssociation = GetScalingConfigurations[algInfo[[algoIndex]]] [[scalingConfiguration]];
 		listOfRuns = algInfo[[algoIndex]][scalingConfigurationAssociation];
 		,
 		
 		scalingConfigurationAssociation = KeySort[scalingConfiguration];
 		listOfRuns = algInfo[[algoIndex]][scalingConfigurationAssociation];
	 	If[ MissingQ[listOfRuns], (* perhaps only a partial association was given *)
	 		 scalingConfigurationAssociation = GetScalingConfigurationsHaving[algInfo[[algoIndex]],
	 		 									scalingConfigurationAssociation][[1]];
	 		 If[MissingQ[scalingConfigurationAssociation],
	 		 	Print["The scaling configuration: "<> ToString[scalingConfiguration]<> " was not found."];
	 		 	,
	 		 	(*
	 		 	Print["The scaling configuration: "<> ToString[scalingConfiguration]<> " was not found and fixed to: "
	 		 			<> ToString[scalingConfigurationAssociation]];		*)
 				listOfRuns = algInfo[[algoIndex]][scalingConfigurationAssociation];
	 		 ];
	 	];
 	];
 	Return[If[IntegerQ[threadId],
 		ret = listOfRuns[[dataProfileId]][[threadId]]
 		,
 		ret = listOfRuns[[dataProfileId]][threadId]
 	]];
 ];

(* Overloaded function which accepts a list instead of independent parameters *)
GetAlgorithmKeyValueList[algInfo_, sel_List] := GetAlgorithmKeyValueList[algInfo, sel[[1]], sel[[2]], sel[[3]], sel[[4]]];

(* Function to change the value of an application property *)
SetAlgorithmKeyValue[algInfo_, sel_List, key_, value_] :=
Block[{},	
	algInfo[[sel[[1]]]][[Key[sel[[2]]]]][[sel[[3]]]][[sel[[4]]]] = algInfo[[sel[[1]]]][sel[[2]]][[sel[[3]]]][[sel[[4]]]] /. {key, _} -> {key, value};
];
SetAttributes[SetAlgorithmKeyValue, HoldFirst];
 
(* Get KeyValue association of scaling parameters for algorithm characterization (as ExaBoundsAlgInfo) *)
GetScalingParameters[algDataAssociation_] := algDataAssociation["scalingParameters"];

(* Get all scaling configurations available for the algorithm in algDataAssociation (as ExaBoundsAlgInfo["algoName"]) *)
GetScalingConfigurations[algDataAssociation_] := Select[Keys[algDataAssociation], Head[#] === Association& ];
GetScalingConfigurations[algInfo_, alg_] := If[IntegerQ[alg],
												Select[Keys[algInfo[[alg]]], Head[#] === Association& ],
												Select[Keys[algInfo[alg]], Head[#] === Association& ]
											];

											
(* Returns the subset of GetScalingConfigurations including only the ones having
the values of scaling parameters as specified in partialScalingParameters.
partialScalingParameters is an association that might include a subset of the overall
scaling parameters. *)
GetScalingConfigurationsHaving[algDataAssociation_,partialScalingParameters_] :=
Module[{ret,pars,p,sortedSP},
	sortedSP = KeySort[partialScalingParameters];
	
	ret = GetScalingConfigurations[algDataAssociation];
	
	pars = Keys[sortedSP];
	ret = Select[ret,And @@ ( Reap[Do[Sow[#[p] == sortedSP[p]],{p,pars}]][[2,1]] )&]
];

GetScalingConfigurationsHaving[algInfo_, alg_,partialScalingParameters_]:=
Module[{},
	GetScalingConfigurationsHaving[algInfo[alg],partialScalingParameters]
];

(* Strip the algInfo of all parameters that are either constant or aliased with another-one *)
(* 
	stripParameters is either Automatic to find and remove constant and fully aliased parameters,
						or Association[appName->ListOfParametersToBeStripped]
	
	StripScalingParameters returns as result the list of stripped parameters
	Association[appName->ListOfParametersToBeStripped]
*)
SetAttributes[StripScalingParameters, HoldFirst];
StripScalingParameters[algInfo_,stripParameters_ : Automatic] :=
Module[{algName,algData,algStripParameters,retStripParameters,p,n,p1,n1,configurations,c,values,values1},
	(* Setup result *)
	If[(stripParameters =!= Automatic),
		retStripParameters = stripParameters;,
		retStripParameters = Association[];
	];
	
	
	Do[(* For each algorithm in algInfo *)
		algData = algInfo[[algName]];
		
		(* Get the scaling configurations *)
		configurations = GetScalingConfigurations[algData];
		
		(* Search for parameters to be removed *)
		If[(stripParameters === Automatic),
			algStripParameters = Reap[Sow[Nothing];
				For[n=1, n<=Length[GetScalingParameters[algData]], n=n+1,
					(* For all parameters *)
					p = GetScalingParameters[algData][[n]];
					values = #[[p]] & /@ configurations;
					
					(* Sow constant parameters *)
					If[ Length[Union[values]] <=1,
						Sow[p];
						Continue[];
					];
					
					(* Search for parameters p1 completely aliased with p *)
					For[n1=n+1, n1<=Length[GetScalingParameters[algData]], n1=n1+1,
						p1 = GetScalingParameters[algData][[n1]];
						values1 = #[[p1]] & /@ configurations;
						If[!Length[Union[values1]]<=1,
							If[Correlation[values,values1]>=1,
								Sow[p1];
							];
						];
					];
				];
			][[2,1]];
			
			,(* Fetch the parameters to be stripped *)
			algStripParameters = stripParameters[[algName]];
		];
		
		algStripParameters = Union[algStripParameters];(* Remove duplicates *)
		
		(* Strip the parameters from the data structure *)
		algInfo[[algName]] = Association[Reap[
			Sow["scalingParameters"->Complement[algData[["scalingParameters"]], algStripParameters ] ];
			Do[
				Sow[KeyDrop[c,algStripParameters]-> algData[c]];
			,{c,configurations}];
		][[2,1]] ];
		
		(* Construct the result *)
		If[(stripParameters === Automatic),
			AppendTo[retStripParameters,algName->algStripParameters]; 
		];
		
		
	,{algName,Keys[algInfo]}];
	
	Return[retStripParameters];
];

GetDataProfileCount[algInfo_, alg_, scaling_] := 
Block[{data, scaleValue},
	data = If[IntegerQ[alg], algInfo[[alg]], algInfo[alg] ];
	scaleValue = If[IntegerQ[scaling], GetScalingConfigurations[algInfo, alg][[scaling]], scaling];
	Return[
		Length[data[scaleValue] ]
	];
];

GetThreads[algInfo_, alg_, scaling_, profile_] :=
Block[{data, scaleValue},
	data = If[IntegerQ[alg], algInfo[[alg]], algInfo[alg] ];
	scaleValue = If[IntegerQ[scaling], GetScalingConfigurations[algInfo, alg][[scaling]], scaling];
	data = data[scaleValue];
	data = If[IntegerQ[profile], data[[profile]], data[profile] ];
	Return[
		Keys[data]
	];
];

 
 (* This is similar to a hashmap data structure *)
ExaBoundsGetTableValue[table_, key_] :=
Module[{ret, tuple},
  	tuple = Select[table, #[[1]] == key &];
  
  	If[Length[tuple] == 0 || Length[tuple[[1]]] < 2, 
   		Print["ExaBoundsGetTableValue, bad key " <> key]; ret = 0,
   		ret = tuple[[1]][[2]]
   	];
  
  	ret
];
(* Where is the ExaBoundsState variable initialized? *)
ExaBoundsMergeState[ExaBoundsState_, KeyValueList_] := MergeKeyValueList[ExaBoundsState, KeyValueList];

ExaBoundsLevelName[level_] :=
  Which[
   level == 0, "core",
   level == 1, "die",
   level == 2, "socket",
   level == 3, "card",
   level == 4, "rack unit",
   level == 5, "rack",
   level == 6, "aisle",
   level == 7, "system"
   ];
   
(* Manage plot formats: *)

formatExaBounds = "Interactive";

SetExaBoundsFormat[format_] := Which[
	
	format == "Interactive",
	formatExaBounds = "Interactive",
	
	format == "Presentation",
	formatExaBounds = "Presentation",
	
	format == "Publication",
	formatExaBounds = "Publication",
	
	True,
	SetExaBoundsFormat::unknownFormat = "The plotting format can be either \"Interactive\", \"Publication\" or \"Presentation\".";
	Message[SetExaBoundsFormat::constraint];
];
  
GetExaBoundsFormat[] := formatExaBounds;
  
ExaBoundsFormatInteractiveQ[] := formatExaBounds == "Interactive";
ExaBoundsFormatPresentationQ[] := formatExaBounds == "Presentation";
ExaBoundsFormatPublicationQ[] := formatExaBounds == "Publication";

ExaBoundsFormatAutoSelect[interactiveOpt_,presentationOpt_,publicationOpt_] := Block[{},
	Which[
		formatExaBounds == "Interactive",
		interactiveOpt,
		
		formatExaBounds == "Presentation",
		presentationOpt,
		
		formatExaBounds == "Publication",
		publicationOpt
	]
];
   
End[] (* End Private Context *)

EndPackage[]