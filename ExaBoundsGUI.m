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
 
 (* ::Package:: *)

(* Mathematica Package *)

BeginPackage["ExaBoundsGUI`"]
(* Exported symbols *)

Needs["ExaBoundsGeneric`"]
Needs["PreDefinedMachineConfigs`"]
Needs["CPUPerformanceModels`"]
Needs["CPUMultithreadedModels`"]
Needs["CPUPowerModels`"]
Needs["CPUPerformanceVisualization`"]
Needs["VectorSizeConvert`"]
Needs["PreDefinedNetworkConfigs`"]
Needs["PreDefinedMemorySpec`"]

EnableSaveDefinitions::usage = "Set to False for versioning, to True for creating CDFs"

selectedAlgorithm::usage = "" 

(* The sub-elements of selectedAlgorithm need to be exposed for Dynamic[] updates in other packages *)
selAlgorithm::usage = "" 
selScaling::usage = "" 
selData::usage = "" 
selThread::usage = "" 

ShowArchitectureAdminGUI::usage = 
"ShowArchitectureAdminGUI[]"

pickMachineConfig::usage =
"pickMachineConfig[ getConfigFunction_, configList_ ]"
  
AlgorithmPicker::usage =
  "AlgorithmPicker[selectedAlgorithm_, algInfoStructure_]"
  
showClearCacheButtons::usage =
	"showClearCacheButtons[]"
	
MulticoreModelInterface::usage =
	"MulticoreModelInterface[archProperties_, algInfoStructure_]"
	
ConfigurationInterface::usage =
	"ConfigurationInterface[]"

Begin["`Private`"] (* Begin Private Context *) 

(* ::Text::Italic:: *)
(* ExaBounds GUI *)

(* rjonger: EnableSaveDefinitions enables or disables saving definitions in
Manipulate[] statements. Set to False for versioning, to True for creating CDFs *)
EnableSaveDefinitions = False;

ClearPerformanceModelCaches[] := Block[{},
	ClearCPUPerformanceModelsCache[];
	ClearCPUMultithreadedModelsCache[];
];

ClearPowerModelCaches[] := Block[{},
	ClearCPUPowerModelsCache[];
];	
ClearAllCaches[] := Block[{},
	ClearPerformanceModelCaches[];
	ClearPowerModelCaches[];
];

(* Buttons to clear the caches *)
showClearCacheButtons[] := Block[{},
	Return[Panel[Row[{
		Button["Clear all caches", ClearAllCaches[]],
		Button["Clear performance model caches", ClearPerformanceModelCaches[]],
		Button["Clear power model caches", ClearPowerModelCaches[]]
	}]]];
];

(* Show buttons to load architecture JSON files *)
ShowArchitectureAdminGUI[] :=
Block[{indexProcArch, indexMemArch, indexNetworkArch},
	indexProcArch = {}; (* Need to initialize the local variables *)
	indexMemArch = {}; 
	indexNetworkArch = {}; 
	Return[Panel[Grid[{
		{
			Style["Imported processor architectures:"],
			Style["Imported memory architectures:"],
			Style["Imported network architectures:"]
		},{
			Dynamic[ListPicker[indexProcArch, If[Keys[predefinedConfigID2Name]=={},{Blank[]},Keys[predefinedConfigID2Name]], Multiselection->False]],
			Dynamic[ListPicker[indexMemArch, If[Keys[predefinedConfigID2NameMem]=={},{Blank[]},Keys[predefinedConfigID2NameMem]], Multiselection->False]],
			Dynamic[ListPicker[indexNetworkArch, If[Keys[predefinedConfigID2NameNetwork]=={},{Blank[]},Keys[predefinedConfigID2NameNetwork]], Multiselection->False]]
		},{
			Button["Import processor architecture file (JSON format)", jsonFile = SystemDialogInput["FileOpen"]; If[jsonFile =!= $Canceled, LoadArchJSON[jsonFile]], Method->"Queued"],
			Button["Import memory architecture file (JSON format)", jsonFile = SystemDialogInput["FileOpen"]; If[jsonFile =!= $Canceled, LoadMemJSON[jsonFile]], Method->"Queued"],
			Button["Import network architecture file (JSON format)", jsonFile = SystemDialogInput["FileOpen"]; If[jsonFile =!= $Canceled, LoadNetworkJSON[jsonFile]], Method->"Queued"]
		}
	}]]];
];

(* show table of the system parameters in <settings> *)
showMachineConfigTable[settings_] := Module[{},
  (*Twistie w/table of settings*)
  OpenerView[{Style[
     "Table of pre-defined parameter values for selected configuration.     \t\t\t\t\t\t\t",
     FontFamily -> "Helvetica", Medium, Bold, Gray],
    
    Style[TableForm[Select[settings, !(StringMatchQ[#[[1]], __ ~~ "MIN"] || StringMatchQ[#[[1]], __ ~~ "MAX"] ) &], (* Show all settings dat do not end with MIN or MAX *)
      TableHeadings -> {None, {Style["\nParameter Name", Bold], Style["\nParameter Value\t\t\t\t", Bold]}},
      TableSpacing -> {2, 25}
      ],
     FontFamily -> "Helvetica", FontSize -> 14, Gray,
     Background -> Directive[LightBlue, Opacity[0.4]]]}, False
  ]
]

configname = "Xeon E5-2697 v3";
networkconfigname = "fat-tree-2L";
(* show selector from <configlist> of pre-defined system configurations *)
pickMachineConfig[ archProperties_, configList_, networkconfigList_] :=
Block[{},
	Return[Panel[
		Column[{
			Grid[{
				{
					Style["Pre-Defined Model Parameter Configuration", FontFamily -> "Helvetica", FontSize -> 16, Gray],
					PopupMenu[Dynamic[configname], configList]
				},
				{
					Style["Pre-Defined Network Parameter Configuration", FontFamily -> "Helvetica", FontSize -> 16, Gray],
					PopupMenu[Dynamic[networkconfigname], networkconfigList]
				}
			}, Alignment -> {{Right, Left}, {Right, Left}}],
			Dynamic[
				archProperties = ExaBoundsMergeState[archProperties, predefinedconfig[configname]];
				archProperties = ExaBoundsMergeState[archProperties, GetNetworkConfig[networkconfigname]];
				showMachineConfigTable[archProperties],
				TrackedSymbols :> {configname, networkconfigname}
			]
		}]
	]];
];
SetAttributes[pickMachineConfig, HoldFirst];

selectedAlgorithm := {selAlgorithm, selScaling, selData, selThread};
selAlgorithm = 1;
selScaling = 1;
selData = 1;
selThread = 1;
AlgorithmPicker[algInfoStructure_] :=
Block[{},
	Return[Panel[
		Column[{
			Row[{Style["Select algorithm "],
				Dynamic[
					PopupMenu[
						Dynamic[selAlgorithm], Table[i->GetAlgorithmName[algInfoStructure, i], {i, Length[algInfoStructure]}]
					],
					TrackedSymbols :> {algInfoStructure}
				]
			}],
			Row[{Style["Select scaling parameters "],
				Dynamic[
					selScaling = 1;
					PopupMenu[
						Dynamic[selScaling], Table[i->GetScalingConfigurations[algInfoStructure, selAlgorithm][[i]], {i, 1, Length[GetScalingConfigurations[algInfoStructure, selAlgorithm]]}]
					],
					TrackedSymbols :> {selAlgorithm}
				]
			}],
			Row[{Style["Select data profile "],
				Dynamic[
					selData = 1;
					PopupMenu[
						Dynamic[selData], Table[i, {i, GetDataProfileCount[algInfoStructure, selAlgorithm, selScaling]}] 
					],
					TrackedSymbols :> {selAlgorithm, selScaling}
				]
			}],
			Row[{Style["Select thread "],
				Dynamic[
					selThread = 1;
					PopupMenu[
						Dynamic[selThread], Table[i->GetThreads[algInfoStructure, selAlgorithm, selScaling, selData][[i]], {i, Length[GetThreads[algInfoStructure, selAlgorithm, selScaling, selData]]}]
					],
					TrackedSymbols :> {selAlgorithm, selScaling, selData}
				]
			}]
		}]
	]];
];
SetAttributes[AlgorithmPicker, HoldFirst]; (* HoldFirst, such that we can update the picker on both parameter changes *)

(* A function to display a GUI element to play with the multi-core model *)
multicoreSelected = {{1,1,1,1}};
multicoreEnabled = {True};
heterogeneousWorkload = False;
homogeneousCoreRunCount = 1;
selectedCore = 1;
MulticoreModelInterface[archProperties_, algInfoStructure_] :=
DynamicModule[{corecount, coremapping, startcount, updateEffectiveCacheSize, effectiveCacheSizes, summaryPower},
	updateEffectiveCacheSize = 0; (* Variable for dynamic *)
	corecount = GetKeyValue[archProperties, "n1"];	
	homogeneousCoreRunCount = 1;
	startcount = Length[multicoreSelected];
	summaryPower = False; (* Always disable power model when refresh*)
	If[corecount > startcount,
		Do[AppendTo[multicoreSelected, {1,1,1,1}]; AppendTo[multicoreEnabled, If[startcount==1, True, False]], corecount-startcount]
	];
	Panel[
		Column[{
			"Selected processor: " <> (configname /. predefinedConfigID2Name),
			"Core count: " <> ToString[corecount],
			Row[{"Heterogeneous workload ", Control[{heterogeneousWorkload, {True, False}}], " ", Button["Enable all cores", Do[multicoreEnabled[[i]] = True, {i, corecount}]], " ", Button["Disable all cores", Do[If[i==1, multicoreEnabled[[i]] = True, multicoreEnabled[[i]] = False], {i, corecount}]]}],
			Dynamic[
				If[heterogeneousWorkload,
					Column[Map[
						Row[{
							Checkbox[Dynamic[multicoreEnabled[[#]], TrackedSymbols :> {multicoreEnabled}], Enabled->If[#==1,False,True]]
							,
							" Core/thread " <> ToString[#] <> " enabled "
							,
							Dynamic[MulticoreModelAlgorithmPicker[multicoreSelected[[#]], algInfoStructure, multicoreEnabled[[#]]], TrackedSymbols :> {multicoreEnabled}]
						}] &
						, Range[corecount]
					]]
					,
					multicoreEnabled[[1]]=True;
					Row[{
						MulticoreModelAlgorithmPicker[multicoreSelected[[1]], algInfoStructure, True],
						" Run on ",
						Dynamic[PopupMenu[
							Dynamic[homogeneousCoreRunCount],
							Range[corecount]
						], TrackedSymbols :> {corecount}],
						"cores"
					}]
						
				]
			, TrackedSymbols :> {heterogeneousWorkload}
			],
			Button["Update results", updateEffectiveCacheSize++], (* Button ups updateEffectiveCacheSize to renew the effective cache size*)
			Graphics[Line[{{0,0},{1000,0}}],ImageSize->1000],
			"Effective cache sizes:",
			(* This is the dynamic statement that is called when the upddate effective cache size is pressed *)
			Dynamic[coremapping = GetCoreMapping[archProperties, algInfoStructure, updateEffectiveCacheSize]; effectiveCacheSizes = Quiet[SolveCachePressure[archProperties, coremapping], {FindRoot::lstol}]; MulticoreModelShowEffectiveCacheSizes[MemoryL3Cache[], effectiveCacheSizes], TrackedSymbols :> {updateEffectiveCacheSize}, SynchronousUpdating->False],
			"Performance (Update results first):",
			Dynamic[
				selectedCore = FirstPosition[multicoreEnabled, True][[1]];
				PopupMenu[
					Dynamic[selectedCore],
					Table[i->"Core/thread " <> ToString[i], {i, Pick[Range[corecount], Take[multicoreEnabled, corecount]]}]
				]
				,
				TrackedSymbols :> {multicoreEnabled}
			],
			Dynamic[
				Row[{
					Quiet[VisualizeCPUCorePerformance[archProperties, coremapping, selectedCore], {FindRoot::lstol}],
					" ",
					Column[{
						Row[{Checkbox[Dynamic[summaryPower]], " Enable power model"}],
						Dynamic[Quiet[SummaryCPUCorePerformance[archProperties, coremapping, summaryPower, selectedCore], {FindRoot::lstol}], TrackedSymbols :> {summaryPower}, SynchronousUpdating -> False]
					}]
				}],
				TrackedSymbols :> {coremapping, selectedCore}, SynchronousUpdating->False
			],
			"These calculations assume that all applications are repeated indefinitely. It does not take into account that some applications finish early and release resources."
		}]
	]
];

(* Get the core mapping, the last argument is not used in the function, but needed to have the dynamic evaluate *)
GetCoreMapping[archProperties_, algInfoStructure_, x_] :=
Block[{coremapping, corecount},
	corecount = If[heterogeneousWorkload, GetKeyValue[archProperties, "n1"], homogeneousCoreRunCount];
	coremapping = {};
	
	(* Create a mapping from core number to algProprties *)
	Do[
		If[heterogeneousWorkload == False,
			(* All cores the same workload *)
			AppendTo[coremapping, i -> GetAlgorithmVectorProperties[archProperties, GetAlgorithmKeyValueList[algInfoStructure, multicoreSelected[[1]]]]],
			(* Otherwise, all cores different workload depending on enabled box *)
			If[multicoreEnabled[[i]],
				AppendTo[coremapping, i -> GetAlgorithmVectorProperties[archProperties, GetAlgorithmKeyValueList[algInfoStructure, multicoreSelected[[i]]]]]
			]
		]
		, {i, corecount}
	];

	Return[coremapping];
];

MulticoreModelShowEffectiveCacheSizes[layer_, effectiveSizes_] :=
Block[{key, data, labels},
	Switch[layer,
		MemoryL3Cache[],
		key = "M1",
		_,
		Return[None];
	];

	(* Get data *)	
	data = (key /. #) & /@ effectiveSizes[[;;,2]];

	(* Static conversion to MB *)
	data /= 1024^2;
	
	(* Labels *)
	labels = effectiveSizes[[;;,1]];
	
	Return[
		BarChart[{data},
			BarOrigin->Left,
			ChartLayout->"Stacked",
			ChartLabels->{Placed[labels, Center]},
			Frame->Bottom,
			FrameLabel->{None,"Cumulative effective cache size per core/thread [MB]"},
			BaseStyle->{FontSize->Medium},
			AspectRatio->0.1,
			ImageSize->1000
		]
	];
];

MulticoreModelAlgorithmPicker[selected_, algInfoStructure_, enabled_] :=
Block[{},
	Return[
		Panel[
			Row[{
				Row[{Style["Select algorithm "],
					Dynamic[
						PopupMenu[
							Dynamic[selected[[1]], (selected[[1]] = #; selected[[2]]=1) &], Table[i->GetAlgorithmName[algInfoStructure, i], {i, Length[algInfoStructure]}],
							Enabled->enabled
						],
						TrackedSymbols :> {multicoreSelected, multicoreEnabled}
					]
				}],
				Row[{Style["Select scaling parameters "],
					Dynamic[
						PopupMenu[
							Dynamic[selected[[2]], (selected[[2]] = #; selected[[3]]=1) &], Table[i->GetScalingConfigurations[algInfoStructure, selected[[1]]][[i]], {i, 1, Length[GetScalingConfigurations[algInfoStructure, selected[[1]]]]}],
							Enabled->enabled
						],
						TrackedSymbols :> {multicoreSelected, multicoreEnabled}
					]
				}],
				Row[{Style["Select data profile "],
					Dynamic[
						PopupMenu[
							Dynamic[selected[[3]], (selected[[3]] = #; selected[[4]]=1) &], Table[i, {i, GetDataProfileCount[algInfoStructure, selected[[1]], selected[[2]]]}],
							Enabled->enabled
						],
						TrackedSymbols :> {multicoreSelected, multicoreEnabled}
					]
				}],
				Row[{Style["Select thread "],
					Dynamic[	
						PopupMenu[
							Dynamic[selected[[4]]], Table[i->GetThreads[algInfoStructure, selected[[1]], selected[[2]], selected[[3]]][[i]], {i, Length[GetThreads[algInfoStructure, selected[[1]], selected[[2]], selected[[3]]]]}],
							Enabled->enabled
						],
						TrackedSymbols :> {multicoreSelected, multicoreEnabled}
					]
				}]
			}]
		]
	];
];
SetAttributes[MulticoreModelAlgorithmPicker, HoldFirst];

ConfigurationInterface[] :=
Block[{},
	Return[Panel[
		Row[{
			Style["DRAM power model: "],
			Dynamic[
				PopupMenu[
					Dynamic[DRAMmodel], AvailablePowerModels[]
				],
				TrackedSymbols :> {DRAMmodel}
			]
		}]
	]];
];

End[] (* End Private Context *)

EndPackage[]