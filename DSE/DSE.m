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

BeginPackage["DSE`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]
Needs["CPUPerformanceModels`"]
Needs["CPUPowerModels`"]
Needs["CPUPerformanceVisualization`"]
Needs["Support`"]

Needs["DSEVisualization`"]

DSEGUI::export = 
  "DSEGUI[]" 
  
DSEParetoPlot::export =
  "DSEParetoPlot[]" 
  
DSEInit::export =
  "DSEInit[]"

Begin["`Private`"] (* Begin Private Context *)

(* This code contains some pointers to parallelize the DSE (search for NOTE comments). This indicates some of the potential critical sections. *)
(* Parallelization is not performed as McPAT is the critical part of the model, that takes >>99% of the model time. It cannot be parallelized properly as it uses several GBs of memory, making it potentially impossible to run multiple instances in parallel *)

(* Internal variables for visualization*)
dseProgress = 0;
currentConfig = 0;
(* Internal variables *)
selectionDSE = Null;
expressionDSE = Null;
linkedDSE = Null;
powerDSE = False;
powerLevel = SystemLayer[];

PositiveIntegerQ[x_] := (IntegerQ[x] && x > 0);
PositiveZeroIntegerQ[x_] := (IntegerQ[x] && x >= 0);

(* A list of DSE parameters (as in ExaBoundsState), description, and a input-validation-function. Currently, only single-core parameters *)
DSEparams = {
	{"", "", None, 0},
	{"n0", "Issue width", PositiveIntegerQ, 1},
	{"n0dispatch", "Dispatch width", PositiveIntegerQ, 1},
	{"n0threads", "SMT threads", PositiveIntegerQ, 1},
	{"n0int", "Integer FUs", PositiveIntegerQ, 1},
	{"n0fp", "Floating-point FUs", PositiveIntegerQ, 1},
	{"n0mem", "Load/store FUs", PositiveIntegerQ, 1},
	{"n0control", "Control FUs", PositiveIntegerQ, 1},
	{"n0MSHR", "Miss-status holding registers", PositiveIntegerQ, 1},
	{"n0ROB", "Reorder buffer size", PositiveIntegerQ, 1},
	{"n0IQ", "Issue Queue size", PositiveIntegerQ, 1},
	{"M0L1", "L1 cache size [kB]", PositiveZeroIntegerQ, 1024^1},
	{"T0L1latency", "L1 cache latency [c]", PositiveIntegerQ, 1},
	{"M0L2", "L2 cache size [kB]", PositiveZeroIntegerQ, 1024^1},
	{"T0L2latency", "L2 cache latency [c]", PositiveIntegerQ, 1},
	{"M1", "L3 cache size [MB]", PositiveZeroIntegerQ, 1024^2},
	{"T0L3latency", "L3 cache latency [c]", PositiveIntegerQ, 1},
	{"M2", "DRAM per socket [GB]", PositiveZeroIntegerQ, 1024^3},
	{"T0DRAMlatency", "DRAM latency [c]", PositiveIntegerQ, 1},
	{"B0Dmo", "Core -> L1/L2 bandwidth [GB/s]", PositiveIntegerQ, 1024^3},
	{"B1Dmo", "Total L3 bandwidth [GB/s]", PositiveIntegerQ, 1024^3},
	{"B2Dmo", "DRAM bandwidth [GB/s]", PositiveIntegerQ, 1024^3},
	{"LNode", "Technology node [nm]", PositiveIntegerQ, 1}
}; 

(* Initialize the DSE GUI variables *)
DSEInit[dataDSE_] :=
Block[{selectionCount},
	selectionCount = 10;
	selectionDSE = Table["", {i, selectionCount}];
	expressionDSE = Table[None, {i, selectionCount}];
	linkedDSE = Table["", {i, selectionCount}];
	dataDSE = Null;
];
SetAttributes[DSEInit, HoldAll];

(* This function adds more selection and input boxes such that more parameters can be put in a DSE *)
DSEExtendBoxes[] :=
Block[{extendCount},
	extendCount = 5;
	selectionDSE = Join[selectionDSE, Table["", {i, extendCount}]];
	expressionDSE = Join[expressionDSE, Table[None, {i, extendCount}]];
	linkedDSE = Join[selectionDSE, Table["", {i, extendCount}]];
];
SetAttributes[DSEExtendBoxes, HoldAll];

(* Validate the input parameters. This function tries to take each of the DSE parameters, sees if it is a valid expression and sees if the values match the input-validation function defined in DSEparams *)
DSEValidate[] :=
Block[{expr},
	Do[
		Catch[
			If[selectionDSE[[idx]] != "",
				expr = Check[ToExpression[expressionDSE[[idx]]], Message[DSE::invalidexpression, selectionDSE[[idx]]]; Throw[0]];
				(* Test value *)
				If[expr == None, Message[DSE::invalidvalue, expr, selectionDSE[[idx]]]; Throw[0]];
				(* find param in DSEparams and get check *)
				checkFun = DSEparams[[FirstPosition[DSEparams[[;;,1]], selectionDSE[[idx]]][[1]]]][[3]]; (* Assume that it can always be found *)
				If[Head[expr] === List,
					(* List *)
					Do[
						If[!checkFun[val], Message[DSE::invalidvalue, val, selectionDSE[[idx]]]; Throw[0]];
						,{val, expr}
					];
					,
					(* Non-list *)
					If[!checkFun[expr], Message[DSE::invalidvalue, expr, selectionDSE[[idx]]]; Throw[0]];
				];
			];
		];
		, {idx, Range[Length[selectionDSE]]}
	];
];
DSE::invalidexpression = "Expression for parameter `1` is invalid"
DSE::invalidvalue = "Value `1` for paramtere `2` is invalid"
DSE::missinglinkmparemeter = "Linked parameter `1` for parameter `2` is not defined"

(* Create a list of lists with all DSE parameters and several bookkeeping fields *)
InitNextConfigUpdate[] :=
Block[{list},
	list = {};
	(* Array: name, newvalue, index, isvector, islinked, currentindex, isupdated, value multiplier*)
	If[selectionDSE[[#]] != "",
		AppendTo[list, {
			selectionDSE[[#]],
			0,
			#,
			VectorQ[expressionDSE[[#]]],
			linkedDSE[[#]] != "",
			1,
			False,
			DSEparams[[FirstPosition[DSEparams, selectionDSE[[#]]][[1]], 4]]
		}]
	] & /@ Range[Length[selectionDSE]]; 
	Return[list];
];

(* Indices for the list structure *)
Name[] = 1;
NewValue[] = Name[] + 1;
Index[] = NewValue[] +1;
IsVector[] = Index[] + 1;
IsLinked[] = IsVector[] + 1;
CurrentIndex[] = IsLinked[] + 1;
IsUpdated[] = CurrentIndex[] + 1;
Multiplier[] = IsUpdated[] + 1;

(* we need to update the index for one DSE list parameter  with one to get to the next configuration. When a parameter "wraps around", we need to update the next list parameter with one as well *)
(* we recursively call this function for the next element in the DSE parameter list. If a value is updated, if returns True. Returning False means done *) 
RecursiveUpdateIndex[list_, index_] :=
Block[{newIndex},
	(* Return False if the list is not updated (we're done *)
	(* Did we already update the last elemnt? *)	
	If[index > Length[list], Return[False]];
	
	(* Skip if this entry is no vector *)
	If[list[[index, IsVector[]]] == False,
		Return[RecursiveUpdateIndex[list, index+1]]
	];
	
	(* Skip if this entry is linked, we then use the currentindex of the linked variable*)
	If[list[[index, IsLinked[]]],
		Return[RecursiveUpdateIndex[list, index+1]]
	];
	
	(* Calculate the new vector index *)
	newIndex = list[[index, CurrentIndex[]]] + 1;
	(* Check if the new index is longer than the length of the vector *)
	If[newIndex <= Length[expressionDSE[[list[[index, Index[]]]]]],
		(* If not, update *)
		list[[index, CurrentIndex[]]] = newIndex;
		Return[True],
		(* If it is, we set it to 1 and increment the index of the next element *)
		list[[index, CurrentIndex[]]] = 1;
		Return[RecursiveUpdateIndex[list, index+1]]
	];
];
SetAttributes[RecursiveUpdateIndex, HoldFirst];

(* If an DSE parameter is linked, find the current index of the parameter which it is linked to to determine the index of this parameter *)
RecursiveGetVectorIndex[list_, index_] :=
Block[{},
	Return[If[list[[index, IsLinked[]]],
		(* It is linked, get the vector index from the linked variable *)
		RecursiveGetVectorIndex[list, FirstPosition[list, linkedDSE[[list[[index,Index[]]]]]][[1]]],
		(* It is not linked, get this vector index *)
		list[[index, CurrentIndex[]]]
	]]; 
];

(* Solve a specific configuration (this function isn't really recursive...) The function returns the updated values in the argument list (HoldFirst is enabled). *)
RecursiveResolveConfiguration[list_, index_, archProperties_] :=
Block[{expr, vectorIndex},
	(* Check if this constraint is already solved and return value*)
	If[list[[index, IsUpdated[]]],
		Return[list[[index, NewValue[]]]]
	];
		
	(* Retrieve the expression *)
	If[list[[index, IsVector[]]],
		(* It is a vector, get the vectorIndex first *)
		vectorIndex = RecursiveGetVectorIndex[list, index];		
		expr = expressionDSE[[list[[index,Index[]]], vectorIndex]],
		expr = expressionDSE[[list[[index,Index[]]]]]
	];
	
	(* Set the expression and update IsUpdated[] *)
	list[[index, NewValue[]]] = Evaluate[list[[index, Multiplier[]]] * expr];
	list[[index, IsUpdated[]]] = True;
	(* Return the expression *)
	Return[expr];
];
SetAttributes[RecursiveResolveConfiguration, HoldFirst];

(* Walk through all DSE parameters to find its next value. The index variables pointing to each DSE parameter element to use for the update has been performed earlier *)
ResolveConfiguration[list_, archProperties_] :=
Block[{},
	(* Reset all IsUpdated fields to False as we need to find the new updated configuration *)
	(list[[#, IsUpdated[]]] = False) & /@ Range[Length[list]];
	(* Call recursive solve on all elements *)
	RecursiveResolveConfiguration[list, #, archProperties] & /@ Range[Length[list]];
];
SetAttributes[ResolveConfiguration, HoldFirst];

(* resolves the next configuration to test in the DE. Returns True if a new configuration is available. When False is returned, the DSE is finished *)
GetNextConfigUpdate[list_, archProperties_] := (* NOTE: Critical section? *)
Block[{firstRun},
	If[ValueQ[list] == False, firstRun = True; list = InitNextConfigUpdate[], firstRun = False];

	(* If we are not in the first run, update all index fields which point to a specific DSE parameter setting. If this is the first run, we use the default of index 1 for each parameter (the first configuration) *)
	If[firstRun == False,
		(* update currentindex *)
		If[RecursiveUpdateIndex[list, 1] == False, (* NOTE: Critical section? *)
			(* No updated index, we're finished *)
			Return[False]
		]
	];
	
	(* Find the new configuration for the current setting of currentindex *)
	ResolveConfiguration[list, archProperties];
	
	Return[True];
];
SetAttributes[GetNextConfigUpdate, HoldFirst];

(* Update the archProperties with the next configuration. configUpdate is a list with all DSE parameters, new values and bookkeeping. Indexed using the indexing functions *)
UpdateArchProperties[archProperties_, configUpdate_] :=
Block[{},
	(* One property at a time, update archProperties *)
	(archProperties = SetKeyValue[archProperties, configUpdate[[#, Name[]]], configUpdate[[#, NewValue[]]]]) & /@ Range[Length[configUpdate]];
]
SetAttributes[UpdateArchProperties, HoldFirst];

(* Random number of maximum architectures in the DSE, just to prevent it from running indefinitely *)
MaxCount[] = 1000;

(* Perform a DSE *)
DSERun[archProperties_, algProperties_] :=
Block[{localArchProperties, dseData, configUpdate, finished, maxcount, archPropertiesBackup, performance, power, startTime, totalTime},
	(* Initialize vars *)
	maxcount = 0;
	dseData = {};
	(* Start showing progress bars *)
	dseProgress+=0.1;
	currentConfig=1;
	
	(*archPropertiesBackup = archProperties;*)
	localArchProperties = archProperties;
	
	startTime = SessionTime[];
	
	While[GetNextConfigUpdate[configUpdate, localArchProperties] && maxcount < MaxCount[], (* NOTE: Parallelize *)  (* NOTE: Critical section? *)
		UpdateArchProperties[localArchProperties, configUpdate];  (* NOTE: Critical section? configUpdate is shared *)
		(* Get performance of new architecture *)
		performance = d0[localArchProperties, algProperties];
		(*performance = Total[ZuriModel[localArchProperties, algProperties]];*)
		If[powerDSE,
			power = Poweri[powerLevel, localArchProperties, algProperties],
			power = 0
		];
		(* Append to data structure *)
		AppendTo[dseData, {{performance, power}, Transpose[{Transpose[configUpdate][[Name[]]], Transpose[configUpdate][[NewValue[]]]}]}];
		(* Update visuals *)
		dseProgress+=0.1; (*NOTE: critical section *)
		currentConfig+=1; (*NOTE: critical section *)
		(* Increase max count for time-out *)
		maxcount++ 
	];
	
	dseProgress=0;
	totalTime = IntegerPart[SessionTime[] - startTime];
	
	Message[DSERun::dseruntime, maxcount, totalTime, IntegerPart[totalTime/maxcount]];
		
	(*archProperties = archPropertiesBackup;*)
	
	Return[dseData];
];
DSERun::dseruntime = "Evaluated `1` design points `2` seconds (average: `3` seconds/design)."

(* Show the configuration GUI for the DSE *)
DSEGUI[dseData_, archProperties_, algProperties_] :=
Block[{gui},
	gui = Dynamic[
		Panel[Column[
			Flatten[
				Join[
					Table[
						Tooltip[
							Row[{
								PopupMenu[With[{i=i}, Dynamic[selectionDSE[[i]]]], Transpose[DSEparams][[1]]],
								InputField[With[{i=i},Dynamic[expressionDSE[[i]]]]],
								Style[" Synchronised with "],
								PopupMenu[With[{i=i}, Dynamic[linkedDSE[[i]]]], Transpose[DSEparams][[1]]]
							}],
							With[{i=i}, Dynamic[
								DSEparams[[
									FirstPosition[DSEparams, selectionDSE[[i]]][[1]],
									2
								]]
							]]
						],
						{i, Length[selectionDSE]}
					],
					{
						Row[{
							Style[" Enable McPAT "],
							Checkbox[Dynamic[powerDSE]]
						}],
						Row[{
							Style[" Power analysis at level "],
							PopupMenu[Dynamic[powerLevel], {CoreLayer[]->"Core layer", DieLayer[] -> "Die layer", SocketLayer[] -> "Socket layer", SystemLayer[]->"System layer"}]
						}],
						Row[{
							Button["Clear", DSEInit[dseData]],
							Button["More variables", DSEExtendBoxes[]],
							Button["Validate", DSEValidate[]],
							Button["Start", dseData = DSERun[archProperties, algProperties], Method->"Queued"],
							If[dseProgress == 0, Invisible, Identity]@Row[{Style[" "], ProgressIndicator[dseProgress, Indeterminate], Style[" Current configuration: " <> ToString[currentConfig]]}]
						}]
					}
				]
			]
		]]
	];

	Return[gui];
];
SetAttributes[DSEGUI, HoldAll];

(* Function to extract pareto points from an input list. Returns a lists of two lists: with pareto and other points *)
ExtractParetoPoints[list_] :=
Block[{paretoPoints, otherPoints},
	paretoPoints = {};
	otherPoints = {};
	Function[{p},
		(* Find all pareto poins *)
		If[NoneTrue[list, (#[[1,1]] <= p[[1,1]] && #[[1,2]] <= p[[1,2]] && #[[1]] != p[[1]] ) &],
			AppendTo[paretoPoints, {p[[1]], p[[2]]}], 
			AppendTo[otherPoints, {p[[1]], p[[2]]}]
		]
	] /@ list;
		
	Return[{paretoPoints, otherPoints}];
];

(* Non-local variables for dynamic updating *)
selectedPoint = 1;
groupPoints = "optimal";
localArchProperties = {};
paretoPoints = {};
lowestPowerDelayPoints = {};
plotParetoPoints = None;
plotPowerDelayPoints = None;

(* Functionality to show a pareto plot *)
DSEParetoPlot[dseData_, archProperties_, algProperties_] :=
Block[{plots, sortedData, otherPoints, paretoListPlot, otherListPlot, powerDelayData, powerDelayPlot, f0, lowestPowerDelayParetoPlot, lowestPowerDelayData, lowestPowerDelayPlot, point},
	If[dseData == Null, Return[]];
	
	sortedData = ExtractParetoPoints[dseData];
	
	f0 = GetKeyValue[archProperties, "f0"]; (* TODO: fix f0 if f0 is DSE'ed*)
	
	powerDelayData = Table[{dseData[[i, 1, 1]] * dseData[[i, 1, 2]] / f0, dseData[[i, 2]]}, {i, Length[dseData]}];
	
	lowestPowerDelayPoints = dseData[[#[[1]]]] & /@ Position[powerDelayData[[;;,1]], Min[powerDelayData[[;;,1]]]];
	lowestPowerDelayData = {{#[[1]], powerDelayData[[#[[1]],1]]}, powerDelayData[[#[[1]],2]]} & /@ Position[powerDelayData[[;;,1]], Min[powerDelayData[[;;,1]]]];
	
	paretoPoints = sortedData[[1]];
	otherPoints = sortedData[[2]];
	
	selectedPoint = 1; (* Reset GUI *)
	
	localArchProperties = archProperties;
	
	paretoListPlot = ListPlot[
						ShowTooltips[paretoPoints[[;;,1]], Table[ToString[par], {par, paretoPoints[[;;,2]]}], TooltipPrefix -> "Parameters"],
						PlotLegends->{Style["Pareto-optimal architectures",14]},
						PlotStyle->Orange,
						PlotMarkers->{"\[Times]",20},
						BaseStyle->{Medium, Bold},
						Frame->True,
						FrameLabel->{"Performance [CPI]", "Power consumption [W]"}
					];

	otherListPlot = ListPlot[
						ShowTooltips[otherPoints[[;;,1]], Table[ToString[par], {par, otherPoints[[;;,2]]}], TooltipPrefix -> "Parameters"],
						PlotLegends->{Style["Sub-optimal architectures",14]},
						PlotMarkers->{"\[FilledCircle]",12},
						BaseStyle->{Medium, Bold},
						Frame->True,
						FrameLabel->{"Performance [CPI]", "Power consumption [W]"}
					];
	lowestPowerDelayParetoPlot = ListPlot[
						ShowTooltips[lowestPowerDelayPoints[[;;,1]], Table[ToString[par], {par, lowestPowerDelayPoints[[;;,2]]}], TooltipPrefix -> "Parameters"],
						PlotLegends->{Style["Lowest power-performance product",14]},
						PlotStyle->Green,
						PlotMarkers->{"\[FilledDiamond]",20},
						BaseStyle->{Medium, Bold},
						Frame->True,
						FrameLabel->{"Performance [CPI]", "Power consumption [W]"}
					];
					
	powerDelayPlot = ListPlot[
						ShowTooltips[powerDelayData[[;;,1]], Table[ToString[par], {par, powerDelayData[[;;,2]]}], TooltipPrefix -> "Parameters"],
						PlotLegends->{"Operating point"},
						BaseStyle->{Medium, Bold},
						AxesOrigin->{1, Automatic},
						FrameTicks->{Range[Length[powerDelayData]], Automatic},
						Frame->True,
						FrameLabel->{"Architecture", "Power-Delay product [J/I]"}
	];
	lowestPowerDelayPlot = ListPlot[
						ShowTooltips[lowestPowerDelayData[[;;,1]], Table[ToString[par], {par, lowestPowerDelayData[[;;,2]]}], TooltipPrefix -> "Parameters"],
						PlotLegends->{"Best Power-Delay product"},
						PlotStyle->Green,
						BaseStyle->{Medium, Bold},
						AxesOrigin->{1, Automatic},
						Frame->True,
						FrameLabel->{"Architecture", "Power-Delay product [J/I]"}
	];
					
	plots = Panel[
				Column[{
					plotParetoPoints = Show[otherListPlot, paretoListPlot, lowestPowerDelayParetoPlot, BaseStyle->{FontSize->16}, PlotRange->All, ImageSize->500],
					ExportImageAsVectorDialogButton[plotParetoPoints],
					plotPowerDelayPoints = Show[powerDelayPlot, lowestPowerDelayPlot, ImageSize->500],
					ExportImageAsVectorDialogButton[plotPowerDelayPoints],
					"",
					Dynamic[
						Row[{
							Style["Analyze operating point: "],
							PopupMenu[Dynamic[selectedPoint],	Switch[groupPoints,
																	"all", selectedPoint=1; Table[i->dseData[[i,2]],{i,Length[dseData]}],
																	"pareto", selectedPoint=1; Table[i->paretoPoints[[i,2]],{i,Length[paretoPoints]}],
																	"optimal", selectedPoint=1; Table[i->lowestPowerDelayPoints[[i,2]],{i,Length[lowestPowerDelayPoints]}]
																]
							],
							RadioButtonBar[Dynamic[groupPoints], {"all"->"All points", "pareto"->"Pareto points", "optimal"->"Best Power-Delay product"}]
						}]
					,TrackedSymbols:>{groupPoints}],
					"",
					Dynamic[
						point = Switch[groupPoints,
																											"all", dseData[[selectedPoint]],
																											"pareto", paretoPoints[[selectedPoint]],
																											"optimal", lowestPowerDelayPoints[[selectedPoint]]
																										];
						(localArchProperties = SetKeyValue[localArchProperties, #[[1]], #[[2]] ]) & /@ point[[2]];	
						Column[{
							SummaryCPUCorePerformance[localArchProperties, algProperties, powerDSE],
							VisualizeCPUCorePerformance[localArchProperties, algProperties]
						}]
					,TrackedSymbols:>{selectedPoint, groupPoints}]
				}]
	];
	
	Return[plots];
];

End[] (* End Private Context *)

EndPackage[]