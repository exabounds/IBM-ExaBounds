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

BeginPackage["ExtrAxGUI`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["AlgorithmProperties`"];
Needs["AlgorithmFromFileJSON`"];
Needs["ExaBoundsGeneric`"];
Needs["ThreadClustering`"];
Needs["SingleThreadExtrapolation`"];
Needs["MultiThreadExtrapolation`"];

SetupExtrAxProjectDirectoryGUI::usage =
	"SetupExtrAxProjectDirectoryGUI[]"
	
LoadPreexistingDataGUI::usage =
	"LoadPreexistingDataGUI[]"
	
ExtrAxPass1GUI::usage =
	"ExtrAxPass1GUI[ExtrAxTrainAlgInfo_, ExtrAxTestAlgInfo_, targetApplicationName_]"
	
ExtrAxPass2GUI::usage = 
	"ExtrAxPass2GUI[classificationTrain, classificationTest, clusteringOptions,
	ExtrAxTrainAlgInfo, ExtrAxTestAlgInfo, targetApplicationName, extrapolationModel]"
	
ExtrAxPass3GUI::usage = 
	"ExtrAxPass3GUI[extrapolationList, mainComplexities, ExtrAxTrainAlgInfo, targetApplicationName]"
	
ExtrAxPass4GUI::usage = 
	"ExtrAxPass4GUI[classificationTrain,
	ExtrAxTrainAlgInfo, targetApplicationName,
	extrapolationList, mainComplexities, extrapolationModel]"
	
ExtrAxPass5GUI::usage = 
	"ExtrAxPass5GUI[classificationTrain, classificationTest,
	ExtrAxTrainAlgInfo, ExtrAxTestAlgInfo, targetApplicationName,
	extrapolationList, mainComplexities, extrapolationModel]"
	
ExtrAxPass6GUI::usage = 
	"ExtrAxPass5GUI[ExtrAxTrainAlgInfo, extrapolationModel,
				targetApplicationName, targetScale, predictedAlgorithm]"
	
	
ExtrAxHelpStyle::usage = "HelpStyle[message_]"
ExtrAxBodyStyle::usage = "ExtrAxBodyStyle[message_]"
ExtrAxBoldStyle::usage = "ExtrAxBoldStyle[message_]" 
ExtrAxOKStyle::usage = "ExtrAxOKStyle[message_]"
ExtrAxNOKStyle::usage = "ExtrAxNOKStyle[message_]"

Begin["`Private`"] (* Begin Private Context *) 

(* Internal project variables *)
projectDirectory = If[!StringQ[projectDirectory],
	If[DirectoryQ["C:\\ExtrAx-projects\\"],
		"C:\\ExtrAx-projects\\",
		""
	]
	,
	projectDirectory
];


pisaTrainDirectory = "train-set";
pisaTestDirectory = "test-set";


pisaTrainExtension = ".profile";
pisaTestExtension =  ".profile";

(* Overall ExtrAx pass definition variables *)
ExtrAxPasses = 6;
passesFileNames = ( "pass"<>ToString[#]<>".m" ) & /@ Range[ExtrAxPasses];

(* Formatting styles *)
HelpStyle[message_] := Style[message, 18, FontFamily->"Times"];
ExtrAxBodyStyle[message_] := Style[message, 18 ];
ExtrAxBoldStyle[message_] := Style[message, 18, Bold];
ExtrAxOKStyle[message_] := Style[message,Darker[Green]];
ExtrAxNOKStyle[message_] := Style[message,Darker[Red]];

(* Overall accessory functions *)
IsAlgInfoQ[algInfo_] := Module[(* Check if algInfo is initialized and filled with some data *)
	{},
	Quiet[Check[
		If[!Association[algInfo],
			Return[False];
		];
		
		If[Length[algInfo] == 0,
			Return[False];
		];
		
		If[AnyTrue[Keys[algInfo], Not[StringQ[#]]& ],
			Return[False];
		];
	
		If[AnyTrue[Values[algInfo], Length[GetScalingConfigurations[#]] == 0& ],
			Return[False];
		];
		
		,
		Return[False];
	]];
	
	Return[True];
];


AlgInfoAppCount[algInfo_] := If[IsAlgInfoQ[algInfo],
	Length[algInfo],
	0
];

GetWholePassFilePath[passNumber_] :=
	FileNameJoin[
		{If[projectDirectory == "",Directory[],projectDirectory],
			passesFileNames[[passNumber]]
		}
	];

SetAttributes[BackupButton, HoldAll];
BackupButton[passNumber_,backupList_] := (*Module[{tmp},*)
	Grid[{
		{Button[
			Dynamic[
				"Backup pass"<>ToString[passNumber]<>": "<>
				GetWholePassFilePath[passNumber]
			,TrackedSymbols :> {projectDirectory}],
			
			If[FileExistsQ[GetWholePassFilePath[passNumber]],
				If[ChoiceDialog["File exists, overwrite it?\n "<> GetWholePassFilePath[passNumber]],
					Put[GetWholePassFilePath[passNumber]];
					Save[GetWholePassFilePath[passNumber],backupList];
				];
				,
				Put[GetWholePassFilePath[passNumber]];
				Save[GetWholePassFilePath[passNumber],backupList];
			];
				
			,
			Method->"Queued"
		]},
		{Button[
			Dynamic[
				"Load data for pass"<>ToString[passNumber]<>": "<>
				GetWholePassFilePath[passNumber]
			,TrackedSymbols :> {projectDirectory}],
			
			If[!FileExistsQ[GetWholePassFilePath[passNumber]],
				MessageDialog[ "File not found: "<> GetWholePassFilePath[passNumber] ];
				,
				Get[GetWholePassFilePath[passNumber]];
				preloadedData[[passNumber]] = True;
			];
				
			,
			Method->"Queued"
		]}
	}];
(*];*)

(************ GUI **********)
SetAttributes[ExtrAxPass1GUI, HoldAll];
SetAttributes[ExtrAxPass2GUI, HoldAll];
SetAttributes[ExtrAxPass3GUI, HoldAll];
SetAttributes[ExtrAxPass4GUI, HoldAll];
SetAttributes[ExtrAxPass5GUI, HoldAll];
SetAttributes[ExtrAxPass6GUI, HoldAll];



SetupExtrAxProjectDirectoryGUI[] := Module[{helpMessage, result, tempPath, 
											folderRowContent},
	helpMessage = HelpStyle["
	IBM Exascale Extrapolator is a tool for extrapolating application profiles.
	It is organized in passes:
	1) Loading input profiles
	2) Thread classification
	3) Setup the extrapolation methods
	4) Construction of the extrapolation models
	5) Model inspection and adjustment
	6) Profile extrapolation
	
	Some of these passes can take significantly long. For example the model selection (3).
	You can here setup a project directory where to save/load the output of each pass.
	"];
	
	folderRowContent = 
	{
		ExtrAxBodyStyle["Project directory: "],
		InputField[Dynamic[projectDirectory], String],
		Button["Browse",
				tempPath = SystemDialogInput@@ {"Directory",
												If[projectDirectory!="" , projectDirectory , Nothing]
												};
				If[tempPath =!= $Canceled, projectDirectory = tempPath],
				Method->"Queued"]
	};
	
	result = Panel[
		Column[{
			Row[{helpMessage}],
			Row[folderRowContent,Frame->True,Alignment->{Center,Center}]
		},Alignment->{Center,Center}]
	];

	Return[result];
];

GetExtrAxFileName[passNumber_] := FileNameJoin[
											{projectDirectory, passesFileNames[[passNumber]]} 
							];

ExtrAxFileExistsQ[passNumber_] := FileExistsQ[ 
											GetExtrAxFileName[passNumber] 
							];

preloadedData = (False) & /@ Range[ExtrAxPasses];

PreloadControlRow[passNumber_] := Module[{fileStatus,rowBody, loadButton, enableLoad, loadStatus,
	fileName},
	fileName = passesFileNames[[passNumber]];
	If[ExtrAxFileExistsQ[passNumber],
		fileStatus = ExtrAxOKStyle[ExtrAxBodyStyle["Found: "<> fileName ]];
		enableLoad = True;
		
		,
		fileStatus = ExtrAxBodyStyle["Not found: "<>fileName ];
		enableLoad = False;
	];
	loadButton = Button["Load",
					Check[
						Get[GetExtrAxFileName[passNumber]];
						preloadedData[[passNumber]] = True;
						,
						preloadedData[[passNumber]] = False;
					]
					,
				Method->"Queued",
				Enabled -> enableLoad];
	
	loadStatus = Dynamic[
		If[preloadedData[[passNumber]],
			ExtrAxOKStyle[ExtrAxBodyStyle["Data loaded"]], 
			If[enableLoad,
				ExtrAxNOKStyle[ExtrAxBodyStyle["Not yet loaded"]]
				,
				ExtrAxBodyStyle["Not previously saved"]
			]
		]
	,TrackedSymbols :> {preloadedData}];
	
	rowBody = {
		ExtrAxBoldStyle[ ToString[passNumber] ],
		fileStatus,
		loadButton,
		loadStatus
	};
	
	Return[rowBody];
];

LoadPreexistingDataGUI[] := Module[{helpMessage, passesDashboard, enableGlobalLoad, globalLoad,
									p, result},
	result = Dynamic[
		(* Project directory changed, data in to be preloaded could be new *)
		preloadedData = (False) & /@ Range[ExtrAxPasses];
	
		helpMessage = HelpStyle["Searched for preexisting data in the project directory: \""
								<>projectDirectory<>
								"\""];
		
		passesDashboard = Grid[
							Prepend[
									PreloadControlRow[#] & /@ Range[ExtrAxPasses],
									{
										ExtrAxBoldStyle["Pass"],ExtrAxBoldStyle["Data file status"],
										Null, ExtrAxBoldStyle["Loading status"]
									}
							],
			Frame -> All, FrameStyle -> Gray];
		
		enableGlobalLoad = AnyTrue[ ExtrAxFileExistsQ [#] & /@ Range[ExtrAxPasses] , TrueQ ];
		
		globalLoad = Button[
			"Load all preexisting data"
			,
			Do[
				If[ExtrAxFileExistsQ[p],
					Check[
						Get[GetExtrAxFileName[p]];
						preloadedData[[p]] = True;
						,
						preloadedData[[p]] = False;
					]
				];
			,{p,Range[ExtrAxPasses] }];
			
			,
			Enabled -> enableGlobalLoad
			,
			Method->"Queued"
		];
	
		Panel[
			Column[{
				Row[{helpMessage}],
				Row[{
					Column[{
						passesDashboard,globalLoad
					},Frame->True]
				}]
			},Alignment->{Center,Center}]
		]
	,TrackedSymbols :> {projectDirectory,preloadedData}];
	
	Return[result];
];

(* Pass1 GUI *)
GetPISAFiles[pisaDirectory_,pisaExtension_] := Module[{res},
	res = FileNames["*"<>pisaExtension, pisaDirectory];
	
	If[Length[res] == 0,
		res = FileNames["*"<>pisaExtension, FileNameJoin[{projectDirectory, pisaDirectory}]];
	];
	
	Return[res];
];

PISADirControl[isTrain_] := Module[{tempPath},
	{
		ExtrAxBodyStyle[If[isTrain,"Train","Test"] <> " profile directory: "],
		If[isTrain,
			InputField[Dynamic[pisaTrainDirectory], String],
			InputField[Dynamic[pisaTestDirectory], String]
		],
		Button["Browse",
				tempPath = SystemDialogInput@@ {	"Directory",
													If[	
														If[isTrain,
															pisaTrainDirectory,
															pisaTestDirectory
														] != "" ,
														
														If[isTrain,
															pisaTrainDirectory,
															pisaTestDirectory
														] ,
														
														If[projectDirectory!="" , projectDirectory ,
															Nothing[]
														]
													]
												};
				If[tempPath =!= $Canceled, 
					If[isTrain,
						pisaTrainDirectory = tempPath,
						pisaTestDirectory = tempPath
					];
				];,
				Method->"Queued"
			]
	}
];

PISAExtensionControl[isTrain_] := Module[{pisaFileNames, pisaMessage},
	{
		ExtrAxBodyStyle[If[isTrain,"Train","Test"] <> " file extension: "],
		If[isTrain,
			InputField[Dynamic[pisaTrainExtension], String],
			InputField[Dynamic[pisaTestExtension], String]
		],
		
		Dynamic[
			pisaFileNames = If[isTrain,
								GetPISAFiles[pisaTrainDirectory,pisaTrainExtension],
								GetPISAFiles[pisaTestDirectory,pisaTestExtension]
							];
			pisaMessage = ExtrAxBodyStyle["Files found: " <>
									ToString[
										Length[ pisaFileNames ]
									]
								];
			If[Length[pisaFileNames] > 0,
				ExtrAxOKStyle[pisaMessage],
				ExtrAxNOKStyle[pisaMessage]
			] 
			
			,If[isTrain, 
				TrackedSymbols :> {pisaTrainDirectory,pisaTrainExtension, projectDirectory},
				TrackedSymbols :> {pisaTestDirectory,pisaTestExtension, projectDirectory}
			]
		]
	}
];

pisaFileTrainLoaded = 0;
pisaFileTestLoaded = 0;

ExtrAxPass1GUI[ExtrAxTrainAlgInfo_, ExtrAxTestAlgInfo_, targetApplicationName_] := Module[
	{
		helpMessage, trainDashboard, testDashboard,
		loadAllButton, fileName, algoData, loadStatus, manipulateAppName, app,
		stripPar = Automatic
	},
	
	(* help message *)
	helpMessage = Grid[{
			{HelpStyle["1) Aim at train and test profile files."]},
			{HelpStyle["2) Load all files by clicking the \"Load profiles\" button."]},
			{HelpStyle["3) Select the target application name and verify that "
							<> "the applications status matches your expectations."]},
			{HelpStyle["4) Backup your data such as to avoid the long import procedure next time."]},
			{HelpStyle["Note: clicking twice the \"Load profiles\" button may"
						<>" load the same profiles twice"<>
						" and cause possible problems. "
						<>"Thus: first setup train and test paths, then load all profiles once."]},
			{HelpStyle["Note: a test set is recommended but not required to continue"<>
				" through the next passes."]}
		},Alignment->{Left,Center}];
	
	(* Train profiles dashboard *)
	trainDashboard = Grid[{
						PISADirControl[True],
						PISAExtensionControl[True]
					},Frame->True];
	
	(* Test profiles dashboard *)
	testDashboard = Grid[{
						PISADirControl[False],
						PISAExtensionControl[False]
					},Frame->True];
	
	loadAllButton = Button["Load profiles",
							Do[
								algoData = LoadAlgorithmJSON[fileName];
								AppendAlgorithmAnalysis[ExtrAxTrainAlgInfo, algoData, "Safe" -> True];
								pisaFileTrainLoaded = pisaFileTrainLoaded +1;
								
							,{fileName, GetPISAFiles[pisaTrainDirectory,pisaTrainExtension]}];
							
							Do[
								algoData = LoadAlgorithmJSON[fileName];
								AppendAlgorithmAnalysis[ExtrAxTestAlgInfo, algoData, "Safe" -> True];
								pisaFileTestLoaded = pisaFileTestLoaded +1;
								
							,{fileName, GetPISAFiles[pisaTestDirectory,pisaTestExtension]}];
							
							If[Length[ExtrAxTrainAlgInfo]>=1,
								stripPar = StripScalingParameters[ExtrAxTrainAlgInfo];
								If[Length[ExtrAxTrainAlgInfo]>=1,
									StripScalingParameters[ExtrAxTestAlgInfo,stripPar];
								];
							];
							
							,
						Method->"Queued"];
	
	loadStatus = Grid[{
						{
							(* Header *)
							ExtrAxBoldStyle["Train set status"]
							,
							ExtrAxBoldStyle["Test set status"]
						}
						,
						{
							(* Statuses: 	1 -> loaded files, 2 -> applications count *)
							Grid[{
								{Dynamic[
									ExtrAxBodyStyle[
										ToString[pisaFileTrainLoaded] <> " files loaded" 
									]
								,TrackedSymbols :> {pisaFileTrainLoaded}]},
								{Dynamic[
									If[AlgInfoAppCount[ExtrAxTrainAlgInfo]>0,
										ExtrAxBodyStyle[
											"Applications: " <>
											ToString[AlgInfoAppCount[ExtrAxTrainAlgInfo]] 
										]
										,
										ExtrAxNOKStyle[ExtrAxBodyStyle[
											"Applications: " <>
											ToString[AlgInfoAppCount[ExtrAxTrainAlgInfo]] 
										]]
									]
								,TrackedSymbols :> {ExtrAxTrainAlgInfo}]}
							}]
						
							,
							Grid[{
								{Dynamic[
									ExtrAxBodyStyle[
										ToString[pisaFileTestLoaded] <> " files loaded" 
									]
								,TrackedSymbols :> {pisaFileTestLoaded}]},
								{Dynamic[
									If[AlgInfoAppCount[ExtrAxTestAlgInfo]>0,
										ExtrAxBodyStyle[
											"Applications: " <>
											ToString[AlgInfoAppCount[ExtrAxTestAlgInfo]] 
										]
										,
										ExtrAxNOKStyle[ExtrAxBodyStyle[
											"Applications: " <>
											ToString[AlgInfoAppCount[ExtrAxTestAlgInfo]] 
										]]
									]
								,TrackedSymbols :> {ExtrAxTestAlgInfo}]}
							}]
						}(*,
							(* If test applications loaded, these are checked against the train ones *)
						If[IsAlgInfoQ[ExtrAxTestAlgInfo],
							If[Sort[ Keys[ExtrAxTestAlgInfo] ] =!= Sort[ Keys[ExtrAxTrainAlgInfo] ],
								{ExtrAxNOKStyle[ ExtrAxBodyStyle[
									"Some application may be available for training but not for test set"
								] ]}
								,
								{ExtrAxOKStyle[ ExtrAxBodyStyle[
									"All applications have a test set"
								] ]}
							]
							,
							Nothing
						]*)
				}
				,Frame -> All];
	
	(* Select the application name and report statistics about it. *)
	manipulateAppName = Dynamic[
		Manipulate[
			If[app==="" ,
				targetApplicationName = "";
				ExtrAxNOKStyle[ExtrAxBodyStyle[ "Before proceeding you have to"
											<>" select a target application" ]]
				,
				targetApplicationName = app;
				Grid[{
						(* Train *)
						{
							
							If[Length[
									GetScalingConfigurations[
										ExtrAxTrainAlgInfo[[ Evaluate[targetApplicationName] ]]
									] 
								] == 0,
								ExtrAxNOKStyle,
								Style
							]@@{
									ExtrAxBodyStyle[ 
										"Scaling configurations in train set: " <>
										ToString[Length[
											GetScalingConfigurations[
												ExtrAxTrainAlgInfo[[ Evaluate[targetApplicationName] ]]
											] 
										]]
								]}
						},
						(* Test *)
						{
							ExtrAxBodyStyle[
								"Scaling configurations in test set: " <> 
								If[!MissingQ[ExtrAxTestAlgInfo[[
																Evaluate[targetApplicationName] ]]],
									ToString[Length[
										GetScalingConfigurations[ExtrAxTestAlgInfo[[
																	Evaluate[targetApplicationName] ]]]
																	
									]] 
									,
									"0"
								]
							]
						}
				}]
			]
			
			,{
				{app,targetApplicationName,ExtrAxBodyStyle["Target application: "]},
				Dynamic[
					If[
						AlgInfoAppCount[ExtrAxTrainAlgInfo] > 0,
						Keys[ExtrAxTrainAlgInfo],
						{""}
					] 
				,TrackedSymbols :> {ExtrAxTrainAlgInfo}]
			}
			,ControlType -> PopupMenu
		]
		,
		TrackedSymbols :> {targetApplicationName}
	];
	
	Return[
		Panel[Column[{
			Row[{helpMessage}],
			Row[{trainDashboard}],
			Row[{testDashboard}],
			Row[{loadAllButton}],
			Row[{loadStatus}],
			Row[{manipulateAppName}],
			
			(* Success message *)
			Row[{Dynamic[
				If[targetApplicationName != "",
					If[
						Length[
							GetScalingConfigurations[ExtrAxTrainAlgInfo[[targetApplicationName]]]
						] > 1,
						ExtrAxOKStyle[ExtrAxBodyStyle["Pass1 ok, you can proceed to pass2."] ]
						,
						ExtrAxNOKStyle[ExtrAxBodyStyle["Pass1 not yet ok, you need to set the"
													<>" target application name and make sure that"
													<>" at least two scaling configurations "
													<>" are available in the training set."
													] ]
					]
					,
					ExtrAxNOKStyle[ExtrAxBodyStyle["Pass1 not yet ok, you need to set the"
												<>" target application name and make sure that"
												<>" at least two scaling configurations "
												<>" are available in the training set."
												] ]
				]
			,TrackedSymbols :> {ExtrAxTrainAlgInfo,targetApplicationName}]}],
			
			Row[{BackupButton[1,{ExtrAxTrainAlgInfo, ExtrAxTestAlgInfo, targetApplicationName}]}]
		},Alignment->{Center,Center}]]
	];
];

(* Pass2 GUI *)
ExtrAxPass2GUI::noTrainAlgorithm = "Train algorithm data not set correctly, review pass1.";
ExtrAxPass2GUI[classificationTrain_, classificationTest_, clusteringOptions_, ExtrAxTrainAlgInfo_, 
				ExtrAxTestAlgInfo_, targetApplicationName_, extrapolationModel_] := Module[
	{
		helpMessage,
		clusteringOptionsSelection,
		quickClusteringButton,
		clusteringGUI, plot
	},

	(* Help message *)
	helpMessage = Grid[{
			{HelpStyle["1) Setup the clustering options."]},
			{HelpStyle["2) Execute the clustering and classification process "
							<> "by clicking the button: \"Classify\"."]},
			{HelpStyle["3) Backup your data such to avoid the long "
							<> "classification procedure next time."]},
			{HelpStyle["4) Visualize the classification results."]},
			{HelpStyle["Note: The graphics generated in this step may slow down the next passes. "
						<>"We recommend to hide the graphics before proceeding further."]}
		},Alignment->{Left,Center}];
		
	(* Classification options *)
	clusteringOptionsSelection = Grid[{
		{ 
			ExtrAxBodyStyle["Classification method: "],
			SetterBar[Dynamic[ clusteringOptions[[2]]],
				{
					False -> "General communication and processing analysis",
					True -> "Communication pattern regularity analysis"
				}
			]
		},
		{ ExtrAxBodyStyle["Number of classes: "],InputField[Dynamic[ clusteringOptions[[1]]] ] }
	},Frame->True];
	
	(* Classification button *)
	quickClusteringButton = Button[
		"Classify"
		,
		
		If[ !IsAlgInfoQ[ExtrAxTrainAlgInfo] || MissingQ[ExtrAxTrainAlgInfo[[targetApplicationName]] ],
			Message[ExtrAxPass2GUI::noTrainAlgorithm];
			, 
			(* Classification of training runs *)
			classificationTrain[[targetApplicationName]] = MultiplerunsThreadClustering[
				ExtrAxTrainAlgInfo[[targetApplicationName]],
				"Clusters" -> clusteringOptions[[1]],
				"CommPatternClustering" -> clusteringOptions[[2]]
			];
			If[ IsAlgInfoQ[ExtrAxTestAlgInfo] && !MissingQ[ExtrAxTestAlgInfo[[targetApplicationName]] ],
				(* Classification of test runs *)
				classificationTest[[targetApplicationName]] = ExtendMultiplerunsThreadClustering[
					classificationTrain[[targetApplicationName]], 
					ExtrAxTrainAlgInfo[[targetApplicationName]],
					ExtrAxTestAlgInfo[[targetApplicationName]]
   				];
			];
		];
		
		clusteringOptions[[1]] = classificationTrain[[targetApplicationName]]["clusteringOptions"]["Clusters"];
		clusteringOptions[[2]] = classificationTrain[[targetApplicationName]]["clusteringOptions"]["CommPatternClustering"];
	,Method->"Queued"];
	
	plot = ExtrAxBodyStyle["Clustering results hidden."];
	clusteringGUI = Grid[{
		{
			Button["Show results",
				plot = 
				If[! AssociationQ[classificationTrain] || 
					MissingQ[classificationTrain[[targetApplicationName]]],
					If[targetApplicationName == "",
						ExtrAxNOKStyle["Target application name not set, review pass1."],
						ExtrAxNOKStyle["Classification results not found for: " <> targetApplicationName]
					]
					,
					
					MultiplerunsThreadClusteringGUI[
						classificationTrain[[targetApplicationName]]
						, classificationTest[[targetApplicationName]]
						, ExtrAxTrainAlgInfo[[targetApplicationName]]
						, If[!MissingQ[ExtrAxTestAlgInfo[[targetApplicationName]]],
							ExtrAxTestAlgInfo[[targetApplicationName]],Association[]
						  ]
						, "ClusteringControl" -> False
						, "Predictor"->If[MultiThreadModelQ[extrapolationModel],
												Null(* extrapolationModel *)
												(*When the extrapolation model does not include some of the metrics
												required in the thread dimension reducer, it is plenty of errors *),
												Null
										]
					]
				]
			,Method->"Queued"],
			
			Button["Hide results",
				plot = ExtrAxBodyStyle["Clustering results hidden."];
			,Method->"Queued"]
		 }
		,
		{Dynamic[plot,SynchronousUpdating->False],SpanFromLeft}
	},Frame->True];
	
	Return[
		Panel[Column[{
			Row[{ helpMessage }],
			Row[{ clusteringOptionsSelection }],
			Row[{ quickClusteringButton }],
			
			(* Success message *)
			Row[{Dynamic[
				If[!MissingQ[classificationTrain[[targetApplicationName]] ],
					ExtrAxOKStyle[ExtrAxBodyStyle["Pass2 ok, you can proceed to pass3."] ]
					,
					ExtrAxNOKStyle[ExtrAxBodyStyle["You need to generate the thread classification."] ]
				]
			,TrackedSymbols :> {classificationTrain,targetApplicationName}]}],
			
			Row[{ BackupButton[2,{classificationTrain, classificationTest, clusteringOptions}] }],
			Row[{clusteringGUI}]
		},Alignment->{Center,Center}]]
	];
]

(* Pass3 GUI *)
pass3OK[extrapolationList_, algInfo_] := Module[
	{
		configurations, availableMetrics, metricMissing,
		res
	},
	
	res = Range[8];
	
	(* Initialize the available metrics *)
	If[!MissingQ[algInfo] && Length[algInfo]>1,
		configurations 		= GetScalingConfigurations[algInfo];
		availableMetrics 	= Flatten[(Values[algInfo[#]] [[1, All, All, 1]] & /@ configurations),1]; 
		availableMetrics 	= Intersection@@ availableMetrics;
		AppendTo[availableMetrics, "NThreads"];
		,
		availableMetrics 	= {};
	];
	
	metricMissing[mName_] :=
		If[MemberQ[availableMetrics, mName],
			False
			,
			True
		];
	
	res = AnyTrue[(metricMissing[#] & /@ extrapolationList[[All,1]]),#&];
	
	res = res || (Length[extrapolationList[[All,1]]] != Length[Union[extrapolationList[[All,1]]]]); 
	
	Return[!res];
];

ExtrAxPass3GUI[extrapolationList_,mainComplexities_,ExtrAxTrainAlgInfo_,targetApplicationName_] := Module[
	{
		helpMessage
	},
	(* Help *)
	helpMessage =  Grid[{
			{HelpStyle["1) Make sure that all metrics you want to extrapolate are listed here once."]},
			{HelpStyle["2) Set the order of complexity as Mathematica functions of "<>
						"the scaling parameters."]},
			{HelpStyle["Consult the IBM Exascale Extrapolator manual for the format to use when describing the options " <>
						"of the extrapolation method."]},
			{HelpStyle["Terms example: {True, {x0, x0^2, x1*x0}"]},
			{HelpStyle["Constraint example: {True, {\"x0\",Ture,{x0>1}}, {\"x1\",False,{1<x1<x0}}}"]},
			{HelpStyle["It is recommended to backup the extrapolation list and complexity declaration "<>
						"by clicking the Backup button."]}
		},Alignment->{Left,Center}];
		
	
	(* Visualization *)
	Return[
		Panel[Column[{
			Row[{ helpMessage }],
			Row[{ ExtrapolationListControlGUI[extrapolationList,mainComplexities,ExtrAxTrainAlgInfo,
				targetApplicationName,"Verbose"->True] }],
			
			
			Row[{Dynamic[
					If[pass3OK[extrapolationList, ExtrAxTrainAlgInfo[[targetApplicationName]] ],
						ExtrAxOKStyle[ExtrAxBodyStyle["Make sure that all metrics to be extrapolated "
												<> "are listed. Then continue to pass4."]]
						,
						ExtrAxNOKStyle[ExtrAxBodyStyle["Some of metrics are missing "<>
													"from the training profiles, or "<>
													"have been listed twice. \n"<>
													"Polish the list of extrapolation metrics and "<>
													"verify that the profiles were correctly load "<>
													"during (pass1)."]]
					]
			,TrackedSymbols :> {extrapolationList, ExtrAxTrainAlgInfo}
			,SynchronousUpdating->False]}],
			Row[{ BackupButton[3,{extrapolationList,mainComplexities}] }]
		},Alignment->{Center,Center}]]
	];
];

(* Pass4 GUI *)
pass4Enabled[ExtrAxTrainAlgInfo_, targetApplicationName_,classificationTrain_, extrapolationList_] :=( 
				IsAlgInfoQ[ExtrAxTrainAlgInfo] && !MissingQ[ExtrAxTrainAlgInfo[[targetApplicationName]] ]
				&& AssociationQ[classificationTrain]
				&& !MissingQ[classificationTrain[[targetApplicationName]] ]
				&& (Length[extrapolationList] >= 1)
				)

ExtrAxPass4GUI[classificationTrain_,
	ExtrAxTrainAlgInfo_, targetApplicationName_,
	extrapolationList_, mainComplexities_, extrapolationModel_] := Module[{helpMessage,autosave=True},
	
	helpMessage =  Grid[{
			{HelpStyle["When everything is ready, launch the model costruction by clicking "<>
						"the button: \"Launch model construction\"."]},
			{HelpStyle["Depending on the selected metrics, model types and training data, "<>
						"model construction may take several hours or even a day."]},
			{HelpStyle["Progresses will be printed out everytime a model has been constructed for "<>
						"one of the specified metrics."]},
			{HelpStyle["If the autosave option is selected, the model if be automatically saved at "<>
						"at the end of the model construction (possibly overwriting previously saved models)."]}
		},Alignment->{Left,Center}];
	
	Return[
		Panel[Column[{
			Row[{ helpMessage }],
			
			Row[{ExtrAxBodyStyle["Autosave: "], Checkbox[Dynamic[autosave]],
				Dynamic[Button["Launch model construction",
					extrapolationModel = FitMultiThreadExtrapolationModel[
						ExtrAxTrainAlgInfo[[targetApplicationName]], 
						classificationTrain[[targetApplicationName]], "Verbose" -> True, 
						"ExtrapolationList" -> extrapolationList, 
						"MainComplexities" -> mainComplexities
					];
					If[autosave,
						Put[GetWholePassFilePath[4]];
						Save[GetWholePassFilePath[4],{extrapolationModel}]
					]
					,
					Method->"Queued"
					,
					Enabled -> pass4Enabled[ExtrAxTrainAlgInfo, targetApplicationName
											,classificationTrain, extrapolationList]]
			,TrackedSymbols :> {extrapolationList, ExtrAxTrainAlgInfo,
								targetApplicationName,classificationTrain}]
			}],
			
			Row[{ BackupButton[4,{extrapolationModel}] }],
			Row[{Dynamic[
					If[!pass4Enabled[ExtrAxTrainAlgInfo, targetApplicationName
									,classificationTrain, extrapolationList],
						ExtrAxNOKStyle[ExtrAxBodyStyle["Some previous passes 1--3 needs further revisions."]]
						,
						If[Length[extrapolationModel] != classificationTrain[[
															targetApplicationName,
															"clusteringOptions",
															"Clusters"]],
							ExtrAxNOKStyle[ExtrAxBodyStyle[ "Extrapolation model not ready yet. "<>
															"You may need to launch the model "<>
															"construction or the training is "<>
															"still in progress or it terminated with "<>
															"failure." ]]
							,
							ExtrAxOKStyle[ExtrAxBodyStyle[ "Extrapolation model ready, "<>
															"continue to pass5." ]]
						]	
					]
			,TrackedSymbols :> {ExtrAxTrainAlgInfo, targetApplicationName
									,classificationTrain, extrapolationList, extrapolationModel}
			,SynchronousUpdating->False]}]
		},Alignment->{Center,Center}]]
	];
];
	
(* Pass5 GUI *)
ExtrAxPass5GUI[classificationTrain_, classificationTest_,
	ExtrAxTrainAlgInfo_, ExtrAxTestAlgInfo_, targetApplicationName_,
	extrapolationList_, mainComplexities_, extrapolationModel_] := Module[
	{
		helpMessage,
		extrapolationGUI, plot
	},

	(* Help message *)
	helpMessage = Grid[{
			{HelpStyle["Visualize the extrapolation models once they are available."]},
			{HelpStyle["Investigate the extrapolation trends of each metric of interest."]},
			{HelpStyle["If, for a given metric, there are clear deviations from your expectations, "<>
						"edit the extrapolation options and retrain the specific model."]},
			{HelpStyle["You may want to edit the constraints to force function monotonicity."]},
			{HelpStyle["\tConstraint example: {True, {\"x0\",Ture,{x0>1}}, {\"x1\",False,{1<x1<x0}}}"]},
			{HelpStyle["You may also reconsider the terms."]},
			{HelpStyle["\tTerms example: {True, {x0, x0^2, x1*x0}"]},
			{HelpStyle["You may experiment with different model types."]},
			{HelpStyle["Remember to verify the metrics for each process class."]},
			{HelpStyle["If changes are made for the NThreads model, it is recommended to step back "<>
				"to pass4 and retrain all models for all thread classes."]},
			{HelpStyle["If changes are made for the LSys model, it is recommended retrain all "<>
				"vinstrPercentage and instrPercentage metrics."]},
			{HelpStyle["Note: The graphics generated in this step may slow down the next passes. "
						<>"We recommend to hide the graphics before proceeding further."]}
		},Alignment->{Left,Center}];
		
		
		plot = ExtrAxBodyStyle["Extrapolation interface hidden."];
		extrapolationGUI = Grid[{
		{
			Button["Show models",
				plot = If[AssociationQ[ExtrAxTestAlgInfo],
					If[!MissingQ[ExtrAxTestAlgInfo[targetApplicationName]],
						FitMultiThreadModelGUI[extrapolationModel, ExtrAxTrainAlgInfo,
												classificationTrain, ExtrAxTestAlgInfo,
												classificationTest, targetApplicationName, extrapolationList,
												mainComplexities, "Verbose" -> True]
						,
						FitMultiThreadModelGUI[extrapolationModel, ExtrAxTrainAlgInfo,
												classificationTrain, Association[],
												Association[], targetApplicationName, extrapolationList,
												mainComplexities, "Verbose" -> True]
					]
					,
					FitMultiThreadModelGUI[extrapolationModel, ExtrAxTrainAlgInfo,
											classificationTrain, Association[],
											Association[], targetApplicationName, extrapolationList,
											mainComplexities, "Verbose" -> True]
				];
			,Method->"Queued"],
			
			Button["Hide models",
				plot = ExtrAxBodyStyle["Extrapolation interface hidden."];
			,Method->"Queued"]
		 }
		,
		{Dynamic[plot,SynchronousUpdating->False],SpanFromLeft}
	},Frame->True];
	
	
	Return[
		Panel[Column[{
			Row[{ helpMessage }],
			
			(* Success message *)
			Row[{Dynamic[
					If[!pass4Enabled[ExtrAxTrainAlgInfo, targetApplicationName
									,classificationTrain, extrapolationList] ||
									Length[extrapolationModel] != classificationTrain[[
															targetApplicationName,
															"clusteringOptions",
															"Clusters"]],
						ExtrAxNOKStyle[ExtrAxBodyStyle["Some previous passes 1--4 needs further revisions."]]
						,
						ExtrAxOKStyle[ExtrAxBodyStyle[ "Extrapolation models ready, "<>
														"investigate its quality and then proceed to pass6." ]]
					]
			,TrackedSymbols :> {ExtrAxTrainAlgInfo, targetApplicationName
									,classificationTrain, extrapolationList, extrapolationModel}
			,SynchronousUpdating->False]}],

			(* Backup and graphics *)			
			Row[{ BackupButton[5,{extrapolationList, mainComplexities, extrapolationModel}] }],
			Row[{extrapolationGUI}]
		},Alignment->{Center,Center}]]
	];
];
		
(* Pass6 GUI *)
ExtrAxPass6GUI[ExtrAxTrainAlgInfo_, extrapolationModel_,
				targetApplicationName_, targetScale_, predictedAlgorithm_] := Module[
	{
		helpMessage,scaleSelector = Nothing ,extrapolateButton, parameterNames = {}, 
		targetConfigurations = {Association[]}, p, v, algInfoOkQ, okMessage,
		modelOkQ, scaleOkQ, extrapolationsOkQ
	},
	
	(* Setup checking functions *)
	algInfoOkQ[algInfo_,appName_] := If[IsAlgInfoQ[algInfo],
					If[!MissingQ[algInfo[appName]],
						True,
						False
					]
					,False
				];
				
	modelOkQ[model_] := MultiThreadModelQ[model];
	
	scaleOkQ[scaleDef_] := If[AssociationQ[scaleDef],
					If[Length[scaleDef]>0,
						If[AllTrue[(ListQ[#]&/@Values[scaleDef]),#&] ,
							If[AllTrue[((Length[#]>=1)&/@Values[scaleDef]),#&] ,
								True
								,
								False
							]
							,
							False
						]
						,
						False
					]
					,
					False
				];
				
	extrapolationsOkQ[predictions_,appName_,target_] := Module[{res, predConfs, confs,pNames},
		Quiet[Check[
			(* Available predictions *)
			predConfs = GetScalingConfigurations[predictions[[appName]]]; 
			
			(* Requested predictions *)
			pNames = Keys[target];
			confs = {Association[]};
			Do[
				confs = Reap[Do[
					Sow[ Append[#,p->v] ] & /@ confs;
				,{v,target[p]}]][[2,1]];
			,{p,pNames}];
			
			(* Are all requested predictions computed? *)
			res = AllTrue[confs,MemberQ[predConfs,#]& ];
			,
			res = False;
		]];
		res
	];
	
	(* Help message *)
	helpMessage = Grid[{
			{HelpStyle["Once the extrapolation model is ready, setup the target scaling "<>
				"configurations and click the extrapolation button."]},
			{HelpStyle["Than backup the results."]},
			{Row[{HelpStyle["In the"],HelpStyle[ExtrAxBoldStyle["target scale definition form"]],
				HelpStyle[" for each scaling parameter you should declare the list of values to ."],
				HelpStyle["be considered during extrapolation."]}]},
			{HelpStyle["After defining the target scale, click the extrapolation button to "<>
						"generate the extrapolation data."]},
			{HelpStyle["Finally click the backup button to save the output in the file: "<>passesFileNames[[6]]]},
			{Row[{HelpStyle["In the "],HelpStyle[ExtrAxBoldStyle["target scale definition form"]],
				HelpStyle[" the target values of each scaling parameter should be defined as a mathematica list, e.g:"]}]},
			{"-\t{1,5,6}"},
			{"-\tRange[100,200,20]"},
			{"-\t(2^#) &/@ Range[10,20]"}
		},Alignment->{Left,Center}];
		
	(* Refresh button should setup the scaleSelector and, if needed, the targetScale *)
	scaleSelector = Dynamic[
		If[algInfoOkQ[ExtrAxTrainAlgInfo,targetApplicationName],
			(* Get parameter names *)
			parameterNames = GetXNames[ExtrAxTrainAlgInfo[[targetApplicationName]]];
			
			(* If needed, reset the target scale *)
			If[ Keys[targetScale] =!= parameterNames,
				targetScale = Association[#->{}&/@parameterNames];
			]; 
			
			(* Reset the scaleSelector *)
			Grid[Reap[
				Sow[{ExtrAxBodyStyle[ExtrAxBoldStyle["Target scale definition form"] ],SpanFromLeft}];
				Sow[{ExtrAxBodyStyle[#], InputField[Dynamic[targetScale[#] ] ] }] & /@ parameterNames;
			][[2,1]],Frame->All]
			
			,
			(* AlgInfo still uninitialized *)
			Grid[Reap[
				Sow[{ExtrAxBodyStyle[ExtrAxBoldStyle["Target scale definition form"] ]}];
				Sow[{ExtrAxBodyStyle[ ExtrAxNOKStyle["Scaling parameters not correctly identified, "<>
													"make sure to have loaed the profile data in pass1"] ] }]
			][[2,1]],Frame->All]
		]
	
	,TrackedSymbols :> {ExtrAxTrainAlgInfo,targetApplicationName}
	,SynchronousUpdating->False];
	
	(* Extrapolation button *)
	extrapolateButton = Button["Extrapolate",
		(* Setup the target configurations *)
		targetConfigurations = {Association[]};
		Do[
			targetConfigurations = Reap[Do[
				Sow[ Append[#,p->v] ] & /@ targetConfigurations;
			,{v,targetScale[p]}]][[2,1]];
		,{p,parameterNames}];
		
		(* Extrapolate *)
		predictedAlgorithm = Association[
			targetApplicationName -> Association[Reap[
				Sow["scalingParameters"->parameterNames];
				Sow[#->{ExtrapolateMultiThreadAlgorithm[extrapolationModel,#]}] & /@ targetConfigurations;
			][[2,1]]]
		];
		
		(* Button enabling *)
		,Enabled -> Dynamic[ 
						modelOkQ[extrapolationModel] && scaleOkQ[targetScale]
						,TrackedSymbols:>{extrapolationModel,targetScale}
					]
	];
	
	(* Ok message *)
	okMessage = Dynamic[
		If[!algInfoOkQ[ExtrAxTrainAlgInfo,targetApplicationName],
			ExtrAxBodyStyle[ ExtrAxNOKStyle["Make sure that pass1 was carried out correctly."]]
			,
			If[!modelOkQ[extrapolationModel],
				ExtrAxBodyStyle[ ExtrAxNOKStyle["The extrapolation model is not ready, review pass4 and pass5."]]
				,
				If[!scaleOkQ[targetScale],
					Row[{ExtrAxBodyStyle[ ExtrAxNOKStyle["The "]],
						ExtrAxNOKStyle[ExtrAxBodyStyle[ ExtrAxBoldStyle["Target scale definition form "]]], 
						ExtrAxBodyStyle[ ExtrAxNOKStyle["is not correctly filled, make sure that all "<>
							"parameters are declared as a non-empty list."]]}]
					,
					If[!extrapolationsOkQ[predictedAlgorithm,targetApplicationName,targetScale],
						ExtrAxBodyStyle[ ExtrAxNOKStyle["Extrapolations not ready yet or errors encountered. "<>
														"Did you already click the extrapolation button?"]]
						,
						ExtrAxBodyStyle[ ExtrAxOKStyle["Extrapolatins correctly generated, you can save the "<>
														"output by clicking the backup button."]]
					]
				]
			]									
		]
	,TrackedSymbols:>{ExtrAxTrainAlgInfo,targetApplicationName,extrapolationModel,targetScale,predictedAlgorithm}];
	
	(* Return graphics *)
	Return[
		Panel[
			Column[{
				Row[{helpMessage}],
				Row[{scaleSelector}],
				Row[{extrapolateButton}],
				Row[{okMessage}],
				Row[{BackupButton[6,{targetScale,predictedAlgorithm}]}]
			},Alignment->{Center,Center}]
		]
	];
];
	
	
	
	
	
	
End[] (* End Private Context *)
EndPackage[]
