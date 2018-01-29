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

BeginPackage["MultiThreadExtrapolation`", { "SingleThreadExtrapolation`", "ThreadClustering`", "InstructionMix`", "ExaBoundsGeneric`", "AlgorithmProperties`"}]
Needs["Support`"]
(* Exported symbols added here with SymbolName::usage *)  

FitMultiThreadExtrapolationModel::usage =
  "FitMultiThreadExtrapolationModel[algAssociation_, clusteringData_,
  \"ExtrapolationList\"->extrapolationLists_]"

ExtrapolateMultiThreadAlgorithm::usage =
	"ExtrapolateMultiThreadAlgorithm[algorithmExtrapolationModel_, scalingParametersAssociation_]"
	
MultiThreadModelQ::usage =
	"MultiThreadModelQ[model_]"
	
FitMultiThreadModelGUI::usage =
 "FitMultiThreadModelGUI[algoModel_,ExtrAxTrainAlgInfo_,classificationTrain_,
 						ExtrAxTestAlgInfo_,classificationTest_,targetApplicationName_,
 						extrapolationList_, mainComplexities_]"
 						
SingleThreadRepresentingCluster::usage =
	"SingleThreadRepresentingCluster[algAssociation_,clusteringData_,clusterId_]"
	
SingleThreadMetricRepresentation::usage =
	"SingleThreadMetricRepresentation[threadsInCluster_,m_ ]"

GetDefaultExtrapolationListMultiThreaded::usage =
	"GetDefaultExtrapolationListMultiThreaded[]"

ClearMultiThreadExtrapolationCache::usage = "ClearMultiThreadExtrapolationCache[]"

Begin["`Private`"] (* Begin Private Context *) 

ClearMultiThreadExtrapolationCache[] :=
Block[{},
	ClearCache[SingleThreadRepresentingCluster];
	ClearCache[SingleThreadMetricRepresentation];
	ClearCache[CachedFitThreadCount];
	ClearSingleThreadExtrapolationCache[];
	ClearThreadClusteringCache[];
	
];

CachedFitThreadCount[algAssociation_,ceList_,mainComplexities_,verbose_] :=
CachedFitThreadCount[algAssociation,ceList,mainComplexities,verbose] =
Module[{representativeAlgAssociation},
	
	If[verbose == True,
		Print["Fitting "<>threadCountName<>" for the whole application"];
	];
	representativeAlgAssociation = SingleThreadRepresentingCluster[algAssociation];(* By default it is assumed a single cluster. *)
				
	Return[FitSingleThreadExtrapolationModel[representativeAlgAssociation,
				"ExtrapolationList"->ceList,"MainComplexities"->mainComplexities,
				"Verbose"->verbose]];
];

MultiThreadModelQ[model_] := Module[{},
	
	If[ !AssociationQ[model], Return[False] ];
	
	If[ !(And@@ (SingleThreadModelQ[#] & /@ Values[model])), Return[False] ];
	
	Return[True];
];

(* Definition of threadCount. *)
threadCountName = "NThreads";
defaultThreadCountExtrapolationLineTXT =
"NThreads			numeric				LinearRegression		.	.	False	{1,Infinity}"
defaultThreadCountExtrapolationLine = ImportString[defaultThreadCountExtrapolationLineTXT, "Table"][[1]];

defaultExtrapolationListMultiThreaded =
	Prepend[DefaultExtrapolationList[],defaultThreadCountExtrapolationLine];
GetDefaultExtrapolationListMultiThreaded[] := defaultExtrapolationListMultiThreaded;

(* Evaluate the represenation of the metric m for the threads represented in threadsInCluster
	that are a list of associations (whose keys are the metrics) *)
SingleThreadMetricRepresentation[threadsInCluster_,m_ ] := 
		SingleThreadMetricRepresentation[threadsInCluster,m] = 
Module[{metricTypes,allMemfunctionValues,allMemX,threads,t,memX,val,memIter,dl,vl,vlData,dlData,vmixData},
	(* Initialization *)
	metricTypes = GetExtrapolationMetricTypeOptions[];
	threads = Length[threadsInCluster];
	
	(* Compute the metric representation *)
	Which[
		MemberQ[metricTypes[[m]], "notSupported" ],(* It is known that this metric would generate error if processed otherwise. *)
				Return[0.];
		,
		
		MemberQ[metricTypes[[m]], "memFunction" ],(* The metric is a memory function *)
		
			allMemfunctionValues = #[[m]]& /@ threadsInCluster;
			allMemX = Union[ Flatten[ allMemfunctionValues[[All,All,1]] ] ];
			
			If[Length[allMemX]>1,
				(* Extend CDF when allMemfunctionValues are not there, forall threads *)
				Do[
					memIter=1;
					allMemfunctionValues[[t]] = Reap[Do[(* I am assuming that memory CDFs are sorted *)
						Which[allMemfunctionValues[[t,memIter,1]]>memX,
							(* memX is not there but there will be a next one. Sow the previous CDF, keep the current memIter value and increase memX *)
							If[memIter > 1,
								val = allMemfunctionValues[[t,memIter-1,2]];
								,
								val = allMemfunctionValues[[t,memIter,2]];
							];
							
							,
							memIter == Length[allMemfunctionValues[[t]] ],
							(* memX is not there and there will not be any	. *)
							val = allMemfunctionValues[[t,memIter,2]];
							
							,
							True,
							(* memX is there (it has to be on memIter!). Sow current CDF, increase memIter and memX. *)
							val = allMemfunctionValues[[t,memIter,2]];
							memIter = memIter+1;
						];
						Sow[{memX,val}];
					,{memX,allMemX}]][[2,1]];
				,{t,Range[threads]}];
				
				(* Return the mean value of the memFunction *)
				Return[Mean[allMemfunctionValues]];
				
				,
				(* The metric is not there, return the default value *)
				Return[GetKeyValue[GetDefaultAlgorithmProperties[],m]];
			];
			
		,
		
		(MemberQ[metricTypes[[m]], "vinstrPercentage" ] || MemberQ[metricTypes[[m]], "vnumeric" ])
														&& ListQ[threadsInCluster[[1]][[m]]]
			(* The metric is vinstrPercentage or vnumeric *),
			vmixData = Reap[Do[
				vlData = Reap[Do[
					dlData = Mean[(VMixGetData[#[[m]],"VectorLength"->vl,"DataLength"->dl] & /@ threadsInCluster )];
					Sow[{dl,dlData}];
				,{dl,InstructionMix`Private`dataLengths}]][[2,1]];
				
				Sow[{vl,vlData}];
			,{vl,InstructionMix`Private`vectorLengths}]][[2,1]];
			
			Return[vmixData];
			
		,
		MemberQ[metricTypes[[m]], "commVector" ] && ListQ[threadsInCluster[[1]][[m]]],
			(*Print["TODO: SingleThreadMetricRepresentation to be implemented for commVector"];*)
			Return[GetKeyValue[GetDefaultAlgorithmProperties[],metricTypes[[m]]]];
		
		,
		True,(* The metric is a scalar *)
			Return[Mean[#[[m]]& /@ threadsInCluster ]];
	];
];

(* Strips all scaling configurations observed in algAssociation to represent a singleThread application
	that refers to the cluster clusterId. *)
SingleThreadRepresentingCluster[algAssociation_,clusteringDataArg : Except[_Rule] : Association[],clusterId : Except[_Rule] : 1] :=
	SingleThreadRepresentingCluster[algAssociation,clusteringDataArg,clusterId] =
Module[{singleThreadAlgAssociation,scalingConfigurations,s,threadRepresentation,
	threadsInCluster,metrics,threadsCount,m,clusterStructure,clusteringData},
	(* Initialization *)
	scalingConfigurations = GetScalingConfigurations[algAssociation];
	clusteringData = clusteringDataArg;
	(* By default, the algAssociation describes the cluster as it is. *)
	If[clusterId ==1 && clusteringData===Association[],
		clusterStructure = Association[# -> {{Keys[ algAssociation[[Key[#],1]] ]}} & /@ scalingConfigurations];
		clusteringData = Association["clusterStructure"->clusterStructure];
	];
	
	(* Strip the representing cluster forall scaling configurations *)
	singleThreadAlgAssociation = Association[Reap[
		Sow["scalingParameters" -> algAssociation[["scalingParameters"]] ];
		Do[
			(*Print["Processing scaling configuration: "<>ToString[s]];*)
			
			threadsInCluster = KeySelect[algAssociation[[Key[s],1]] ,
									MemberQ[clusteringData[["clusterStructure",Key[s],1,clusterId]], # ]& ];
									
			threadsCount = Length[threadsInCluster];
			threadsInCluster = Values[threadsInCluster];
			threadsInCluster = KeyValueList2Association[#]&/@threadsInCluster;
			
			metrics = Keys[threadsInCluster[[1]]];
			
			If[threadsCount == 1 && MemberQ[metrics,threadCountName],(* This actually represents a cluster *)
				threadsCount = threadsInCluster[[1,threadCountName]]
			];
			
			threadRepresentation = Reap[
				Sow[{threadCountName,threadsCount}];
				
				Do[
					Sow[{m,SingleThreadMetricRepresentation[threadsInCluster,m ]}];
				,{m,metrics}];
			][[2,1]];
			
			
			Sow[s->{Association[{0,0}(* Pid=0, Tid=0 *)->threadRepresentation]}];
		,{s,scalingConfigurations}];
	][[2,1]]];
	
	Return[singleThreadAlgAssociation];
];

MultipleThreadRepresentingCluster[algAssociation_,clusteringData_,c_] :=
Module[{res,s,inCluster,threadsCount},
	res = Association[Reap[
						Sow["scalingParameters" -> algAssociation[["scalingParameters"]] ];
						Do[
							inCluster = KeySelect[algAssociation[[Key[s],1]] ,
											MemberQ[clusteringData[["clusterStructure",Key[s],1,c]], # ]& ];
							threadsCount = Length[inCluster];
							
							PrependTo[inCluster[[#]],{threadCountName,threadsCount}] & /@ Range[threadsCount]; 
							Sow[s->{inCluster}];
						,{s,GetScalingConfigurations[algAssociation]}]
					][[2,1]]];
	Return[res];
];

Options[FitMultiThreadExtrapolationModel] = {"Verbose"->False,
	"ExtrapolationList"->defaultExtrapolationListMultiThreaded,
	"MainComplexities"->Automatic};
FitMultiThreadExtrapolationModel[algAssociation_, clusteringData_,OptionsPattern[]] :=
Module[{algoModel,c,clusters,eList,ceList,model,
	representativeAlgAssociation,selTermsNThreads={},nThreadModels,mainComplexities
},
	(* Initialization *)
	clusters = Length[clusteringData[["clusterStructure",1,1]]];
	eList = OptionValue["ExtrapolationList"];
	mainComplexities = OptionValue["MainComplexities"];
	
	(* check if the extrapolation list is the same for all clusters or a different one per each cluster *)
	If[Head[eList[[1,1]]] =!= List ,
		eList = eList & /@ Range[clusters];
	];
	
	(* Train nThreads for each cluster. *)
	nThreadModels = Association[Reap[
		Sow[Nothing];
		
		For[c = 1, c<=clusters, c++,
			representativeAlgAssociation = SingleThreadRepresentingCluster[algAssociation,clusteringData,c];
			
			If[MemberQ[eList[[c,All,1]], threadCountName],
				If[OptionValue["Verbose"] == True,
					Print["Fitting "<>threadCountName<>" for cluster: "<>ToString[c]];
				];
				
				ceList = Select[eList[[c]],#[[1]]==threadCountName&];
				
				model = FitSingleThreadExtrapolationModel[representativeAlgAssociation,
					"ExtrapolationList"->ceList,"MainComplexities"->mainComplexities,
					"Verbose"->OptionValue["Verbose"]];
				Sow[c->model];
			];
		];
		
		(* Generate also the overall parallelization. In fact cluster's models might be significantly deviated due to noises. *)
		If[c>2,(* Note that if there is a single cluster, at the exit of previous for loop, c=2. *)
			If[MemberQ[eList[[c-1,All,1]], threadCountName],(* FIXME: this is the last ceList, no additional one for the overall clusters *)
				If[OptionValue["Verbose"] == True,
					Print["Fitting "<>threadCountName<>" for the whole application"];
				];
				
				model = FitSingleThreadExtrapolationModel[SingleThreadRepresentingCluster[algAssociation],
					"ExtrapolationList"->ceList,"MainComplexities"->mainComplexities,
					"Verbose"->OptionValue["Verbose"]];
				Sow[Null->model];
			];
		];
	][[2,1]]];
	
	(* Identify selTermsNThreads *)
	If[Length[nThreadModels]>0,
		selTermsNThreads = Reap[
			If[MemberQ[{"LinearRegression","WeightedLinearRegression"},#[[1,"modelType"]]],
				Sow[#[[1,"model"]] @ {#[[1,"predictorTerms"]]}]
			]& /@ Values[nThreadModels];
		][[2]];
		
		If[Length[selTermsNThreads]>=1,
			selTermsNThreads = selTermsNThreads[[1]];
			selTermsNThreads = Union[selTermsNThreads];(*remove duplicates*)
		];
		If[Length[selTermsNThreads]>=1,
			selTermsNThreads = Select[1/selTermsNThreads,!NumberQ[#]&];
		];
		
		If[OptionValue["Verbose"] == True,
			Print["Parallelization terms: ", selTermsNThreads ];
		];
	];
	
	(* Train extrapolation models for each cluster *)
	algoModel = Association[Reap[
		For[c = 1, c<=clusters, c++,
			representativeAlgAssociation = SingleThreadRepresentingCluster[algAssociation,clusteringData,c];
			(*representativeAlgAssociation = MultipleThreadRepresentingCluster[algAssociation,clusteringData,c];*)
			
			
			If[OptionValue["Verbose"] == True,
				Print["Fitting for the thread cluster: "<>ToString[c]];
			];
			
			ceList = Select[eList[[c]],#[[1]]!=threadCountName&];
				
			model = FitSingleThreadExtrapolationModel[representativeAlgAssociation,
				"ExtrapolationList"->ceList,"SelTermsNThreads"->selTermsNThreads,
				"MainComplexities"->mainComplexities,
				"Verbose"->OptionValue["Verbose"]];
			Sow[c->model];
		];
	][[2,1]]];
	
	(* include the NThread model within the clustermodels *)
	Do[
		If[KeyExistsQ[nThreadModels,c],
			PrependTo[algoModel[[c]],nThreadModels[[c,1]]];
		];
	,{c,Keys[algoModel]}];
	
	Return[algoModel];
];

ExtrapolateMultiThreadAlgorithm[algorithmExtrapolationModel_, scalingParametersAssociation_] :=
Module[{newThread,sThreadModel,extrapolatedRun,clusters,c},
	
	(* Initialization *)
	clusters = Length[algorithmExtrapolationModel];
	
	(* Predict each cluster *)
	extrapolatedRun = Association[
		Reap[Do[
			sThreadModel = algorithmExtrapolationModel[[c]];
			newThread = ExtrapolateSingleThreadAlgorithm[sThreadModel,scalingParametersAssociation];
			Sow["cluster"<>ToString[c] -> newThread[[1]] ];
		,{c,Range[clusters]}]][[2,1]]
	];
	
	
	Return[extrapolatedRun];
];


Options[FitMultiThreadModelGUI] = {
	(* Clustering options *)
	"ReducerMethod" -> "PCA",
	"ReducerDimensions" -> Automatic,
	"ClusteringMethod" -> {"Agglomerate","Linkage"->"Median","SignificanceTest"->"Silhouette"},
	"Clusters" -> Automatic,
	(* Extrapolation options *)
	"Verbose"->False};
SetAttributes[FitMultiThreadModelGUI,HoldAll]
FitMultiThreadModelGUI[algoModel_,ExtrAxTrainAlgInfo_,classificationTrain_,
 						ExtrAxTestAlgInfo_,classificationTest_,targetApplicationName_,
 						extrapolationList_, mainComplexities_,
 						OptionsPattern[]] :=
Module[{verbose,mThreads,
		clusters,c,clusteringNeeded=False,extensionClusteringNeeded=False,
		reducerMethod,reducerDimensions,clusteringMethod,testFlag,
		SingleThreadGUIs,representativeTrain,representativeTest,selTermsNThreads,
		representativeAlgAssociation,model,ceList,trainAlgAssociation,testAlgAssociation,
		trainClusteringData,testClusteringData},
	(* Initialization *)
	verbose 			= OptionValue["Verbose"];
		(* options for clustering *)
	reducerMethod 		= OptionValue["ReducerMethod"];
	reducerDimensions 	= OptionValue["ReducerDimensions"];
	clusteringMethod 	= OptionValue["ClusteringMethod"];
	clusters		 	= OptionValue["Clusters"];
	trainAlgAssociation = ExtrAxTrainAlgInfo[[targetApplicationName]];
	testAlgAssociation  = ExtrAxTestAlgInfo[[targetApplicationName]];
	testFlag = AssociationQ[testAlgAssociation] && (Length[testAlgAssociation] >= 1 );
	
	
	(* Run initial clustering *)
	If[!AssociationQ[classificationTrain[[targetApplicationName]] ],
		clusteringNeeded = True;
		,
		If[!MemberQ[Keys[classificationTrain[[targetApplicationName]] ],"clusterStructure"],
			clusteringNeeded = True;
		];
	];
	
	If[clusteringNeeded,
		If[verbose,
			Print["Clustering training data."];
		];
		classificationTrain[[targetApplicationName]] = MultiplerunsThreadClustering[trainAlgAssociation,
			"ReducerMethod" -> reducerMethod, "ReducerDimensions" -> reducerDimensions,
			"ClusteringMethod" -> clusteringMethod, "Clusters" -> clusters
		];	
		extensionClusteringNeeded = True;
		,
		If[!AssociationQ[classificationTest[[targetApplicationName]] ],
			extensionClusteringNeeded = True;
			,
			If[!MemberQ[Keys[classificationTest[[targetApplicationName]]],"clusterStructure"],
				extensionClusteringNeeded = True;
			];
		];
	];
	
	If[testFlag && extensionClusteringNeeded,
		If[verbose,
			Print["Extending clustering to test data."];
		];
		classificationTest[[targetApplicationName]] =	ExtendMultiplerunsThreadClustering[
								classificationTrain[[targetApplicationName]],trainAlgAssociation,testAlgAssociation];
	];
	
	(* Simplify further clustering access because it is not necessary to write them anymore. *)
	trainClusteringData = classificationTrain[[targetApplicationName]];
	If[testFlag,
		testClusteringData = classificationTest[[targetApplicationName]];
		,
		testClusteringData = Association[];
	];
	
	
	(* Train initial extrapolation model. *)
	clusters = Length[trainClusteringData[["clusterStructure",1,1]]];
			
	(* First train the algorithm extrapolation model if currently uninitialized *)
	If[!ValueQ[algoModel],
		If[verbose,
			Print["Training preliminary models."];
		];
		algoModel = FitMultiThreadExtrapolationModel[trainAlgAssociation,
						"ExtrapolationList"->extrapolationList,"Verbose"->verbose,
						"MainComplexities"->mainComplexities];
	];
	
	(* Extract the nThread models *)
	selTermsNThreads = Reap[
		If[verbose,
			Print["Collecting thread count models."];
		];
		Do[
			mThreads = FirstPosition[algoModel[[c]], MultiThreadExtrapolation`Private`threadCountName];
			If[!MissingQ[mThreads],
				Sow[(algoModel[[c,mThreads[[1]],"model"]] @ {algoModel[[c,mThreads[[1]],"predictorTerms"]]})];
				,
				Sow[1.];
			];
		,{c,Range[clusters]}];
		
		(* Generate also the overall parallelization. In fact cluster's models might be significantly deviated due to noises. *)
		If[clusters>1,
			
			ceList = {SelectFirst[extrapolationList,#[[1]] == MultiThreadExtrapolation`Private`threadCountName&]};
			
			model = CachedFitThreadCount[trainAlgAssociation,
				ceList,mainComplexities,
				Evaluate[OptionValue["Verbose"]] ];
			Sow[(model[[1,"model"]]@ {model[[1,"predictorTerms"]]}) ];
		];
	][[2,1]];
	selTermsNThreads = Union[selTermsNThreads];(* remove duplicates *)
	
	If[Length[selTermsNThreads]>=2,
		AppendTo[selTermsNThreads,Total[selTermsNThreads]];
	];
	If[Length[selTermsNThreads]>=1,
		selTermsNThreads = Select[1/selTermsNThreads,!NumberQ[#]&];
	];
	
	(* Initialize the SingleThreadGUI representing each cluster *)
	SingleThreadGUIs = (
		If[verbose,
			Print["Preparing GUI for cluster "<> ToString[#]];
		];
		representativeTrain = SingleThreadRepresentingCluster[trainAlgAssociation,trainClusteringData,#];
		If[testFlag,
			representativeTest = SingleThreadRepresentingCluster[testAlgAssociation,testClusteringData,#];
		];

		FitSingleThreadModelGUI[algoModel,
				Association[targetApplicationName -> Evaluate[representativeTrain]],
				Evaluate[If[testFlag,
					Association[targetApplicationName -> representativeTest],
					Association[]
				]],
				targetApplicationName, extrapolationList, mainComplexities, selTermsNThreads,
				"Verbose"->verbose, "ClusterId"->#
		]
	)& /@ Range[clusters];
	
	(* Once selected the cluster to be studied, open the GUI for that cluster *)
	Manipulate[
		
		
		SingleThreadGUIs[[c]]
		,
		{
			{c,1,Style["Class to analyze:","Section"]},
			(#-> FromCharacterCode[ToCharacterCode["A"]+#-1] & /@Range[clusters]),
			BaseStyle -> {Large,Bold}
		}
	]
];

End[] (* End Private Context *)

EndPackage[]