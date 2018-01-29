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

BeginPackage["ThreadClustering`", { "ExaBoundsGeneric`", "AlgorithmProperties`", "AlgorithmExtrapolation`"}]
Needs["SingleThreadExtrapolation`"]
Needs["MultiThreadExtrapolation`"]
Needs["Support`"]
Needs["InstructionMix`"]
Needs["CommunicationPatternVisualization`"]
Needs["CommunicationPatternClustering`"]
(* Exported symbols added here with SymbolName::usage *)  

(* Radar plot for a single cluster. *)	
ClusterPlot::usage =
	"ClusterPlot[threads_]"

(* clustering_ is a set clusters.
	Each cluster is a set of threadId used in the association threads_ *)
SinglerunClusterPlot::usage =
	"SinglerunClusterPlot[threads_,clustering_]"

(* algAssociation_ is an association of:
	scalingConfiguration -> runs (where runs is a set of thread-sets)
	clusterStructure_ is a structure as the one returned from MultiplerunsThreadClustering:
	scalingConfiguration -> clusteredRuns (each run is a set of clusters)
*)
MultiplerunsClusterPlot::usage =
	"MultiplerunsClusterPlot[algAssociation_,clusterStructure_]"

(*
	Cluster threads within each application run (different scaling parameters).
	Sort clusters per clusterId over different runs.
	Each run will have the same number of clusters, but clusters might be empty.
*)	
ThreadDimensionReduction::usage =
	"ThreadDimensionReduction[threads_]"
	
(* Generate a function to reduce the dimensionality of the threads. *)	
MultiplerunsThreadClustering::usage =
	"MultiplerunsThreadClustering[trainAlgAssociation_]"
	
(* Considering a clustering preidentified for trainAlgAssociation,
	extend the clustering to testAlgAssociation
	(i.e. use the ThreadDimensionReduction found for the training such to have the same space and then
	compute the ordering for clusters found in testAlgAssociation). *)	
ExtendMultiplerunsThreadClustering::usage =
	"MultiplerunsThreadClustering[trainClusteringResults_,trainAlgAssociation_,testAlgAssociation]"
	
(* Generate a function to reduce the dimensionality of the threads. *)	
MultiplerunsThreadClusteringGUI::usage =
	"MultiplerunsThreadClusteringGUI[	clusteringDataTrain_,clusteringDataTest_,
										trainAlgAssociation_,testAlgAssociation_]"

ClearThreadClusteringCache::usage = "ClearThreadClusteringCache[]"

Begin["`Private`"] (* Begin Private Context *) 

ClearThreadClusteringCache[] :=
Block[{},
	ClearCache[GetUpperBound];
	ClearCache[GetLowerBound];
	ClearCache[GetCDF];
	ClearCache[GetICDF];
	ClearCache[DF];
];

(******************************** Plotting capabilities ********************************)
AxisDescription2AxisName[axisDescription_] :=
	If[Head[axisDescription]===String,
		axisDescription 
		, 
		axisDescription[[1]]<>(*"_"<>*)axisDescription[[2]]<>(*"_"<>*)
					StringJoin[ StringSplit[ ToString[axisDescription[[3]]], "."] ] 
	]
									
Options[ClusterPlot] = {
	"axis" -> {"LSys",
				(*{"D0ireuse","ICDF",0.5},{"D0dreuse","ICDF",0.5},*)
			"F0mem","F0int","F0fp","F0control","F0other","ILP0"
			(*{"D0dreuse","ICDF",0.25},{"D0ireuse","ICDF",0.5}*)
				(*{"D0ireuse","CDF",1024},{"D0dreuse","CDF",1024}*)
			(*"LSys",
				(*{"D0ireuse","ICDF",0.5},{"D0dreuse","ICDF",0.5},*)
			"F0load","F0store","F0int","F0fp","F0control","F0other","ILP0"
			(*{"D0dreuse","ICDF",0.25},{"D0ireuse","ICDF",0.5}*)
				(*{"D0ireuse","CDF",1024},{"D0dreuse","CDF",1024}*)
			*)},
	"preprocesses" -> Association["LSys"->(Log[#]&)],
	"upperBoundaries" -> Association[], "lowerBoundaries" -> Association[],
	"showAverage" -> True,
	"predictor" -> Null (* 	Null if no prediction.
							Actual prediction forall clusters forall scaling configurations for MultiplerunsClusterPlot.
							Actual prediction forall clusters for SinglerunClusterPlot.
							Actual prediction for a singleCluster for ClusterPlot. *)
};
Options[SinglerunClusterPlot] = Append[Options[ClusterPlot],
	"showCommunicationGraph" -> True
];

Options[MultiplerunsClusterPlot] = Options[SinglerunClusterPlot];


(* This has been moved into Support!
(* linear interpolation for managing the memFunctions *)
LinearInterpolation[monotonicSortedPointList_,xIndex_,at_] :=
Module[{before,after,yIndex},
	yIndex = If[xIndex==1,2,1];
	
	before = Select[monotonicSortedPointList,#[[xIndex]] <= at&];
	If[ Length[before] == 0,
		before = monotonicSortedPointList[[1]];
		,
		before = before[[-1]];
	];
	
	after = SelectFirst[monotonicSortedPointList,#[[xIndex]] >= at&,before];
					
	If[before[[xIndex]] == after[[xIndex]],
		before[[yIndex]]
		,
		before[[yIndex]] + ((at-before[[xIndex]])*(after[[yIndex]]-before[[yIndex]]) / (after[[xIndex]]-before[[xIndex]]))
	]
];*)

GetCDF[cdf_,at_] := GetCDF[cdf,at] = LinearInterpolation[cdf,1,at];
GetICDF[cdf_,at_] := GetICDF[cdf,at] = LinearInterpolation[cdf,2,at];

GetUpperBound[algAssociation_,metricName_] := GetUpperBound[algAssociation,metricName] = 
Module[{crtAxisName,mAt,getMetric,mVector,wholeThreadAssociationPool,r,k,scalingConfigurations},
	(* collect wholeThreadAssociationPool *)
	scalingConfigurations = GetScalingConfigurations[algAssociation];
	wholeThreadAssociationPool = Reap[
		Do[
			Do[
				Sow[KeyValueList2Association[#]] & /@ Values[r];
			,{r,algAssociation[k]}];
		,{k,scalingConfigurations}];
	][[2,1]];
	
	(* Settings to manage scalar metrics as well as memFunction *)
	If[Head[metricName]===List,
		(* Memfunction *)
		Which[metricName[[2]]==="CDF", 
			mAt = metricName[[3]];
			getMetric[cdf_] := GetCDF[cdf,mAt];
			,
				
				
			metricName[[2]]==="ICDF",
			mAt = metricName[[3]];
			getMetric[cdf_] := GetICDF[cdf,mAt];
			,
				
			True,
			getMetric[cdf_] := Missing["Unknown "<>metricName[[1]]<>":"<>metricName[[2]]];
		];
		crtAxisName = metricName[[1]];
		,
		
		(* scalar metric or vinstrPercentage *)
		crtAxisName = metricName;
		If[!MemberQ[GetExtrapolationMetricTypeOptions[][metricName],"vinstrPercentage"] &&
			!MemberQ[GetExtrapolationMetricTypeOptions[][metricName],"vnumeric"],
			(* scalar *)
			getMetric[metric_] := metric;
			,
			(* vinstrPercentage or vnumeric *)
			getMetric[metric_] := If[ListQ[metric], VMixGetData[metric], metric] ;
		];
	];
	
	(* Get the metric *)
	mVector = getMetric[#[crtAxisName]] & /@ wholeThreadAssociationPool;
	
	Return[N[Max[mVector]]];
];

GetLowerBound[algAssociation_,metricName_] := GetLowerBound[algAssociation,metricName,GetExaBoundsFormat[]] =
Module[{crtAxisName,mAt,getMetric,mVector,wholeThreadAssociationPool,r,k,scalingConfigurations},
	(*Return[0];*)
	(* collect wholeThreadAssociationPool *)
	scalingConfigurations = GetScalingConfigurations[algAssociation];
	wholeThreadAssociationPool = Reap[
		Do[
			Do[
				Sow[KeyValueList2Association[#]] & /@ Values[r];
			,{r,algAssociation[k]}];
		,{k,scalingConfigurations}];
	][[2,1]];
	
	(* Settings to manage scalar metrics as well as memFunction *)
	If[Head[metricName]===List,
		(* Memfunction *)
		Which[metricName[[2]]==="CDF", 
			mAt = metricName[[3]];
			getMetric[cdf_] := GetCDF[cdf,mAt];
			,
				
				
			metricName[[2]]==="ICDF",
			mAt = metricName[[3]];
			getMetric[cdf_] := GetICDF[cdf,mAt];
			,
				
			True,
			getMetric[cdf_] := Missing["Unknown "<>metricName[[1]]<>":"<>metricName[[2]]];
		];
		crtAxisName = metricName[[1]];
		,
		
		(* scalar metric or vinstrPercentage or vnumeric *)
		crtAxisName = metricName;
		If[!MemberQ[GetExtrapolationMetricTypeOptions[][metricName ],"vinstrPercentage"] &&
			!MemberQ[GetExtrapolationMetricTypeOptions[][metricName ],"vnumeric"],
			(* scalar *)
			getMetric[metric_] := metric;
			,
			(* vinstrPercentage or vnumeric *)
			getMetric[metric_] := If[ListQ[metric], VMixGetData[metric], metric] ;
		];
	];
	
	(* Get the metric *)
	mVector = getMetric[#[crtAxisName]] & /@ wholeThreadAssociationPool;
	
	(*Return[N[If[Min[mVector]<0,Min[mVector],0.]]];*)
	(*Return[ ExaBoundsFormatAutoSelect[N[Min[mVector]],N[Min[mVector]],0] ];*)
	Return[ ExaBoundsFormatAutoSelect[N[Min[mVector]],N[Min[mVector]],N[Min[mVector]]] ];
];

MultiplerunsClusterPlot[algAssociation_,clusterStructure_,OptionsPattern[]] :=
Module[{showAverage,showCommunicationGraph,predictor,axis,preprocesses,upperBoundOpt,lowerBoundOpt,
		name,
		clusters,
		scalingConfigurations},
	(* Fetch the options *)
	showAverage 	= OptionValue["showAverage"];
	predictor  		= OptionValue["predictor"];
	axis 			= OptionValue["axis"];
	preprocesses	= OptionValue["preprocesses"];
	upperBoundOpt	= OptionValue["upperBoundaries"];
	lowerBoundOpt	= OptionValue["lowerBoundaries"];
	showCommunicationGraph = OptionValue["showCommunicationGraph"];
	
	(* Initialization *)
	scalingConfigurations = GetScalingConfigurations[algAssociation];
	
	
	(* For each metric, search maximum and minimums for all threads in all runs. *)
	If[axis === Automatic,axis = Keys[algAssociation[[1,1,1,All,1]]] ];
	Do[
		(* upper bound *)
		If[	upperBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[upperBoundOpt, Association[ name ->
				N[GetUpperBound[algAssociation,name] ]]
			];
			,
			upperBoundOpt[name] = Max[upperBoundOpt[name],
				N[GetUpperBound[algAssociation,name]]];
		];
		
		(* lower bound *)
		If[	lowerBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[lowerBoundOpt, Association[ name -> 
				N[GetLowerBound[algAssociation,name]] ]
			];
			,
			lowerBoundOpt[name] = Min[lowerBoundOpt[name],
				N[GetLowerBound[algAssociation,name]]];
		];
		
	,{name,axis}];
	
	(* setup the grid and generate SinglerunClusterPlot *)
	clusters = Length[clusterStructure[[1,1]]];(* Cluster count (fisrt scaling configuration, first run). *)
	Grid[
		{Grid[
			{
				Reap[ Sow[Style[ToString[#],Bold]]; 
					Sow[SpanFromLeft]&/@Range[clusters-If[showCommunicationGraph,0,1] ]; 
				][[2,1]]
				,
				SinglerunClusterPlot[algAssociation[#],clusterStructure[#],
					"showAverage" -> showAverage,(*"prediction" -> prediction,*)"axis"->axis,
					"preprocesses"->preprocesses,
					"upperBoundaries"->upperBoundOpt,"lowerBoundaries"->lowerBoundOpt,
					"predictor" ->If[predictor=!=Null,predictor[[Key[#],1]](*ExtrapolateMultiThreadAlgorithm[predictor,#]*)
						, Null],
					"showCommunicationGraph" -> showCommunicationGraph
				]
				[[1,1]] (* Remove the gridding and keep the list *)
			}
		,Frame->All,FrameStyle->LightGray]} & /@ scalingConfigurations
	,Frame->All, FrameStyle->Thick]
];

SinglerunClusterPlot[threads_,clusters_,OptionsPattern[]] :=
Module[{showAverage,predictor,axis,preprocesses,upperBoundOpt,lowerBoundOpt,
		name,crtAxisName,mAt,getMetric,
		mVector,
		visualizedThreads,
		visualizedClusters,
		showCommunicationGraph, communicationGraph = Null,
		graphicList,
		run = 1},
	(* Fetch the options *)
	showAverage 	= OptionValue["showAverage"];
	predictor  		= OptionValue["predictor"];
	axis 			= OptionValue["axis"];
	preprocesses	= OptionValue["preprocesses"];
	upperBoundOpt	= OptionValue["upperBoundaries"];
	lowerBoundOpt	= OptionValue["lowerBoundaries"];
	showCommunicationGraph = OptionValue["showCommunicationGraph"];
	
	(* Visualize a single run *)
	visualizedThreads = threads[[run]];
	visualizedClusters = clusters[[run]];
	
	(* For each metric, search maximum and minimums for all threads in all runs. *)
	If[axis === Automatic,
		If[Head[visualizedThreads[[1]] ]===Association,
			axis = Keys[visualizedThreads[[1]]],(* From Association *)
			axis = visualizedThreads[[1,All,1]](* From keyValueList *)
		];
	];
	Do[
		(* Settings to manage scalar metrics as well as memFunction *)
		If[Head[name]===List,
			(* Memfunction *)
			Which[name[[2]]==="CDF", 
				mAt = name[[3]];
				getMetric[cdf_] := GetCDF[cdf,mAt];
				,
				
				
				name[[2]]==="ICDF",
				mAt = name[[3]];
				getMetric[cdf_] := GetICDF[cdf,mAt];
				,
				
				True,
				getMetric[cdf_] := Missing["Unknown "<>name[[1]]<>":"<>name[[2]]];
			];
			crtAxisName = name[[1]];
			,
			
			(* scalar metric or vinstrPercentage or vnumeric *)
			crtAxisName = name;
			If[!MemberQ[GetExtrapolationMetricTypeOptions[][name ],"vinstrPercentage"]&&
				!MemberQ[GetExtrapolationMetricTypeOptions[][name ],"vnumeric"],
				(* scalar *)
				getMetric[metric_] := metric;
				,
				(* vinstrPercentage or vnumeric *)
				getMetric[metric_] := If[ListQ[metric], VMixGetData[metric], metric] ;
			];
		];
		(* Get the metric *)
		mVector = getMetric[GetKeyValue[#,crtAxisName]] & /@ visualizedThreads;
		
		(* upper bound *)
		If[	upperBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[upperBoundOpt, Association[ name -> N[Max[mVector]] ]];
		];
		
		(* lower bound *)
		If[	lowerBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[lowerBoundOpt, Association[ name -> N[Min[0,mVector]]] ];
		];
		
	,{name,axis}];
	
	(* Generate the clustering plot *)
	graphicList = {ClusterPlot[Lookup[visualizedThreads, visualizedClusters[[#]]] ,
			"showAverage" -> showAverage,(*"prediction" -> prediction,*)"axis"->axis,
			"preprocesses"->preprocesses,
			"upperBoundaries"->upperBoundOpt,"lowerBoundaries"->lowerBoundOpt,
			"predictor" -> If[predictor=!=Null, predictor[[#]], Null ]
		] & /@ Range[Length[visualizedClusters]]};
	
	
	(* Generate the communication graph *)
	If[showCommunicationGraph,
		communicationGraph = GenerateCommunicationGraph[visualizedThreads,
			"Clustering"->visualizedClusters];
		AppendTo[graphicList[[1]] , communicationGraph];
	];	
	
	Return[Grid[graphicList]];
];

(* Plot a single cluster *)
ClusterPlot[threads_,OptionsPattern[]] :=
	Module[{showAverage,predictor,predictorFlag,axis,preprocesses,upperBoundOpt,lowerBoundOpt, axisN,
		name,crtAxisName,mAt,getMetric,
		radarPlot,nameDisplay,
		preprocessesV={},lowerBounds={},upperBounds={},
		matrix={},newVector,
		associativeThreads},
	
	(* Define plotting function *)
	radarPlot[elementMatrixOpen_,axisLabels_] := Module[{labelsOrganization,elementMatrix,plotStyle,meanVector},
		labelsOrganization = ({	Range[0, Length[axisLabels] - 1]*2 Pi/Length[axisLabels]
								, axisLabels, 0.01 & /@axisLabels})  // Transpose;
		
		elementMatrix = Append[	elementMatrixOpen // Transpose,
							(elementMatrixOpen // Transpose)[[1]]]
							// Transpose;
		
		If[!(showAverage || predictorFlag),
			plotStyle = Gray;
			,
			
			plotStyle = Directive[ExaBoundsFormatAutoSelect[Lighter[Gray],Darker[Green],Lighter[Gray]],Thin] 
									&/@ Range[Length[elementMatrix]];
			If[predictorFlag,
				plotStyle[[-1]] = Directive[ExaBoundsFormatAutoSelect[Red,Lighter[Red],Red],Dashed,Thick];
			];
			If[showAverage,
				plotStyle = Insert[plotStyle,Directive[ExaBoundsFormatAutoSelect[Black,Darker[Blue],Black],Thick],-If[predictorFlag,2,1]];
			];
		];
		(* Managing average and prediction *)
		If[showAverage,
			If[predictorFlag,
				meanVector = Mean[elementMatrix[[1;;-2]]];(* Last one is the prediction *)
				,
				meanVector = Mean[elementMatrix];
			];
			elementMatrix = Insert[elementMatrix,meanVector,-If[predictorFlag,2,1]];
		];
		
		ListPolarPlot[elementMatrix, DataRange -> {0, 2*Pi}, Joined -> True
						,GridLinesStyle -> Directive[LightGray, Dashed, Thin]
						,PolarGridLines -> {Automatic,{0.2,0.4,0.6,0.8,1}},PolarAxesOrigin -> {0,1}
						,PolarAxes -> Automatic, PolarTicks -> {labelsOrganization, Automatic}
						,PlotStyle-> plotStyle,
						PlotRangePadding->ExaBoundsFormatAutoSelect[Scaled[0.25],Scaled[0.13],Scaled[0.13]],
						TicksStyle -> Directive[Black, ExaBoundsFormatAutoSelect[6, 10.4,10.4]]
					]
	]; 
	
	(* Organize easy2access threads' data *)
	associativeThreads = KeyValueList2Association[#] & /@ threads;
	
	(* Fetch the options *)
	showAverage 	= OptionValue["showAverage"];
	predictor  		= OptionValue["predictor"];		predictorFlag = predictor=!=Null;
	axis 			= OptionValue["axis"];
	preprocesses	= OptionValue["preprocesses"];
	upperBoundOpt	= OptionValue["upperBoundaries"];
	lowerBoundOpt	= OptionValue["lowerBoundaries"];
	
	(* Organize easy2access threads' data *)
	associativeThreads = KeyValueList2Association[#] & /@ If[predictorFlag,Append[threads,predictor],threads];
	
	axisN = Length[axis];
	
	(* Organize vector representation. While doing that, sow the matrix elements. *)
	If[axis === Automatic,axis = Keys[associativeThreads[[1]]] ];
	matrix = Reap[Do[
		(* Settings to manage scalar metrics as well as memFunction *)
		If[Head[name]===List,
			(* Memfunction *)
			Which[name[[2]]==="CDF", 
				mAt = name[[3]];
				getMetric[cdf_] := GetCDF[cdf,mAt];
				,
				
				
				name[[2]]==="ICDF",
				mAt = name[[3]];
				getMetric[cdf_] := GetICDF[cdf,mAt];
				,
				
				True,
				getMetric[cdf_] := Missing["Unknown "<>name[[1]]<>":"<>name[[2]]];
			];
			crtAxisName = name[[1]];
			,
			
			(* scalar metric or vinstrPercentage or vnumeric *)
			crtAxisName = name;
			If[!MemberQ[GetExtrapolationMetricTypeOptions[][name],"vinstrPercentage"]&&
				!MemberQ[GetExtrapolationMetricTypeOptions[][name ],"vnumeric"],
				(* scalar *)
				getMetric[metric_] := metric;
				,
				(* vinstrPercentage or vnumeric *)
				getMetric[metric_] := If[ListQ[metric], VMixGetData[metric], metric] ;
			];
		];
		
		(* preprocesses *)
		If[	preprocesses[name] === Missing["KeyAbsent",name],
			AppendTo[preprocessesV,#&];
			,
			AppendTo[preprocessesV,preprocesses[name]];
		];
		
		(* data *)
		newVector = preprocessesV[[-1]][N[getMetric[#[crtAxisName]]]] & /@ associativeThreads;
		
		(* upper bound *)
		If[	upperBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[upperBounds, N[Max[newVector]] ];
			,
			AppendTo[upperBounds,preprocessesV[[-1]][N[upperBoundOpt[name]]] ];
		];
		
		(* lower bound *)
		If[	lowerBoundOpt[name] === Missing["KeyAbsent",name],
			AppendTo[lowerBounds, N[Min[0,newVector]] ];
			,
			AppendTo[lowerBounds,preprocessesV[[-1]][N[lowerBoundOpt[name]]] ];
		];
		If[!NumberQ[lowerBounds[[-1]]],  lowerBounds[[-1]] = 0];
		
		(* matrix, append the new metric *)
		If[upperBounds[[-1]] != lowerBounds[[-1]],
			newVector = (newVector - lowerBounds[[-1]]) / (upperBounds[[-1]] - lowerBounds[[-1]]);
		];
		Sow[newVector];
		
		
	,{name,axis}] ][[2,1]] // Transpose;
	(* If predictorFlag, last row of the matrix is the prediction, not the actual measurement! *)
	
	
	
	(* Format axis labels *)
	nameDisplay = ExaBoundsFormatAutoSelect[
		
		Grid[	{{ToString[ preprocessesV[[#]][
									Symbol[AxisDescription2AxisName[ axis[[#]] ]] ] ]
								},
								{ToString["["<>
											If[lowerBounds[[#]] > 1000 || lowerBounds[[#]] < 0.01,
												ToString[ScientificForm[lowerBounds[[#]],2 ],TraditionalForm]
												,
												ToString[NumberForm[lowerBounds[[#]],3]]
											]
											<>";;"<>
											If[upperBounds[[#]] > 1000 || upperBounds[[#]] < 0.01,
												ToString[ScientificForm[upperBounds[[#]],2 ],TraditionalForm]
												,
												ToString[NumberForm[upperBounds[[#]],3]]
											]
										,TraditionalForm]<>"]"}}
						] & /@ Range[axisN]
		,
		ToString[ preprocessesV[[#]][
									Symbol[AxisDescription2AxisName[ axis[[#]] ]] ] ]
								 & /@ Range[axisN]
		,
		Style[ToString[ preprocessesV[[#]][
									Symbol[AxisDescription2AxisName[ axis[[#]] ]] ] ],Larger]
								 & /@ Range[axisN]
	];
					
	(* plot *)
	radarPlot[matrix,nameDisplay]
];

(******************************** Vector management ********************************)

Options[ThreadDimensionReduction] = {
	"Method" -> "PCA",
	"IncludeCommVector"->True,
	"Dimensions" -> Automatic
};
(* 	
	Generates a DimensionReducerFunction from the threads parameter. It organizes the functionalities
	for reorganizing thread-data into constant-dimension vectors to be passed to the DimensionReducerFunction.
	
	threads is a list of threads, not an association. Thus, it is possible to merge threads from different
	runs if needed (they might have overlapping ids).
*)
ThreadDimensionReduction[threads_,OptionsPattern[]] :=  Module[{associativeThreads,metricTypeDefinition,
												requiredMetrics,metric,
												memMetrics,viMixMetrics,iMixMetrics,instrCountMetric,otherMetrics,
												cvMetrics,cvLengths,length,
												points, memFunctionPoints, 
												organizerFunction,organizerModule,organizedThreads,
												reducerFunction,reducerModule,reducer,
												means,norms,nConstantFlags,normalizeFunction,normalizedThreads,
												method,dimensions,reducedMetricNames,
												dimMissing,includeCommVector},
	(* Load options *)
	method 				= OptionValue["Method"];
	dimensions 			= OptionValue["Dimensions"];
	includeCommVector 	= OptionValue["IncludeCommVector"];
	
	(* Load metric types and available consistent metrics *)
	associativeThreads = KeyValueList2Association[#] & /@ threads;
	
	metricTypeDefinition = GetExtrapolationMetricTypeOptions[];
	requiredMetrics = Intersection[(*Keys[Select[metricTypeDefinition,!MemberQ[#,"commVector"]&]]*)
						Keys[Select[metricTypeDefinition,#=!={"notSupported"}&]]	(* All known metrics. *)
						,threads[[1,All,1]]			(* Key of keyvaluelist for thread 1. *)
					];
	nConstantFlags = Association[Reap[
						Sow[Nothing]; (* This solves bugs when nothing is sowed during the loop *)
						Do[
							Sow[metric -> (#[[metric]] === associativeThreads[[1,metric]] &/@associativeThreads)] 
						,{metric,requiredMetrics}]
					][[2,1]]];
	requiredMetrics = Select[requiredMetrics,!(And @@ nConstantFlags[#]) &];
	memMetrics = Select[requiredMetrics,MemberQ[metricTypeDefinition[#],"memFunction"]& ];
	viMixMetrics = Select[requiredMetrics,
							MemberQ[metricTypeDefinition[#],"vinstrPercentage"] || 
							MemberQ[metricTypeDefinition[#],"vnumeric"] &
					];
	iMixMetrics = Complement[Select[requiredMetrics,MemberQ[metricTypeDefinition[#],"instrPercentage"]& ],viMixMetrics];
	instrCountMetric = SingleThreadExtrapolation`Private`instrCountName;
	cvMetrics = Select[requiredMetrics,MemberQ[metricTypeDefinition[#],"commVector"]& ];
	otherMetrics = Complement[requiredMetrics,Join[memMetrics,viMixMetrics,iMixMetrics,cvMetrics]];
	
	
	
	(* Check for common error *)
	If[Head[threads] =!= List,
		Print["Error: ThreadDimensionReduction expects a List of thread, not an Association."];
	];
	
	(* Get maximum observed legth of each memfunction *)
	If[Length[memMetrics]>0,
		memFunctionPoints = Association[Reap[
			Sow[Nothing]; (* This solves bugs when nothing is sowed during the loop *)
			Do[
				points = #[[metric,All,1]] & /@ associativeThreads;
				points = Union@@points;
				points = 2^# & /@ Range[Ceiling[Log2[Max[1,Min[points]]]],Floor[Log2[Max[1,points]]]];
				Sow[metric->points];
			,{metric,memMetrics}];
		][[2,1]]];
		,
		memFunctionPoints = Association[];
	];
	
	(* Get maximum length of communication vector *)
	If[Length[cvMetrics] > 0,
		cvLengths = Association[Reap[
			Sow[Nothing]; (* This solves bugs when nothing is sowed during the loop *)
			Do[
				length = Max[Length[#[[metric]] ]&/@ associativeThreads];
				Sow[metric->length];
			,{metric,cvMetrics}];
		][[2,1]]];
		,
		cvLengths = Association[];
	];
	
	(* Setup the organizer function *)
	
	
	organizerFunction = Function[thread,
		organizerModule[inThreadO_] := Module[{associativeThread,m,expectedPoints,foundPoints,referenceLocations,p,
												selection,memVector,wholeMemVector,otherVector,viMixVector,
												iMixVector,instrCount,wholeCVVector,cvVector,dummyVector,expectedLength},
			associativeThread = KeyValueList2Association[inThreadO];
			
			instrCount = associativeThread[injectInstrCountMetric];
			
			otherVector = N[#] & /@ Values[KeyTake[associativeThread,injectOtherMetrics]];(* Get and test generic metrics *)
			If[Length[otherVector] < Length[injectOtherMetrics],
				dimMissing = Select[injectOtherMetrics,!MemberQ[Keys[associativeThread],#] &];
				ThreadDimensionReduction::dimensionMismatch = "Dimension reducer did not find the following expected dimensions: "
														<> dimMissing;
				Message[ThreadDimensionReduction::dimensionMismatch]; 
			];
			
			viMixVector = N[VMixGetData[#]] (**instrCount*) & /@ Values[KeyTake[associativeThread,injectVIMixMetrics]];(* Get and test imix *)
			If[Length[viMixVector] < Length[injectVIMixMetrics],
				dimMissing = Select[injectVIMixMetrics,!MemberQ[Keys[associativeThread],#] &];
				ThreadDimensionReduction::dimensionMismatch = "Dimension reducer did not find the following expected dimensions: "
														<> dimMissing;
				Message[ThreadDimensionReduction::dimensionMismatch]; 
			];

			iMixVector = N[#] (**instrCount*) & /@ Values[KeyTake[associativeThread,injectIMixMetrics]];(* Get and test imix *)
			If[Length[iMixVector] < Length[injectIMixMetrics],
				dimMissing = Select[injectIMixMetrics,!MemberQ[Keys[associativeThread],#] &];
				ThreadDimensionReduction::dimensionMismatch = "Dimension reducer did not find the following expected dimensions: "
														<> dimMissing;
				Message[ThreadDimensionReduction::dimensionMismatch]; 
			];
			
			wholeMemVector = Flatten[Reap[
				Sow[Nothing];
				Do[ (* Get memory vector. No need to test because missing values are interpolated. *)
					expectedPoints = injectMemFunPoints[[m]];
					
					foundPoints = KeyValueList2Association[ associativeThread[[m]] ];
					referenceLocations = Keys[foundPoints];
					referenceLocations = Select[referenceLocations,MemberQ[
							Flatten[Join[{Min[referenceLocations],Max[referenceLocations]},injectMemFunPoints[[m]] ]]
							,
							#]&
						];
					
					referenceLocations = Reap[
						Sow[Nothing]; (* This solves bugs when nothing is sowed during the loop *)
						Do[
							selection = Select[referenceLocations,#<=p&];
							If[selection =!= {},
								selection = Max[selection];
								,
								selection = Min[Select[referenceLocations,#>p&]];
							]; 
							Sow[ selection];
						,{p,expectedPoints}]
					][[2,1]];
					
					memVector = foundPoints[[Key[#]]] & /@ referenceLocations;
					
					If[Length[memVector]>0,
						Sow[N[#] & /@ memVector];
						,
						Sow[Nothing];
					];
				,{m,injectMemMetrics}]
			][[2,1]]];
			
			
			If[injectIncludeCommVector,(* Get communication vector. *)
				wholeCVVector = Reap[Do[(* Get communication vector. *)
						expectedLength = injectCVLengths[m];
						If[MissingQ[expectedLength],expectedLength=0;];
						
						
						dummyVector = SortBy[associativeThread[[m]] , First];
						
						cvVector = 0 & /@ Range[expectedLength*3];
						cvVector[[1;;(Length[dummyVector]*3) ]] = Flatten[dummyVector];
						
						Sow[N[#] & /@ cvVector];
					,{m,injectCVMetrics}]][[2]];(* See if anything was Sowed, perhaps commVector is missing! *)
				If[Length[wholeCVVector]>=1, wholeCVVector = Flatten[wholeCVVector[[1]]]; ];
				
				,
				wholeCVVector = {};
			];
			
			Join[otherVector,viMixVector,iMixVector,wholeMemVector,wholeCVVector]
		];
		
		organizerModule[thread]
	];
	organizerFunction = organizerFunction /. injectMemFunPoints -> memFunctionPoints;
	organizerFunction = organizerFunction /. injectMemMetrics -> memMetrics;
	organizerFunction = organizerFunction /. injectInstrCountMetric -> instrCountMetric;
	organizerFunction = organizerFunction /. injectVIMixMetrics -> viMixMetrics;
	organizerFunction = organizerFunction /. injectIMixMetrics -> iMixMetrics;
	organizerFunction = organizerFunction /. injectCVMetrics -> cvMetrics;
	organizerFunction = organizerFunction /. injectCVLengths -> cvLengths;
	organizerFunction = organizerFunction /. injectOtherMetrics -> otherMetrics;
	organizerFunction = organizerFunction /. injectIncludeCommVector -> includeCommVector;
	
	(* Organize the threads *)
	organizedThreads = organizerFunction[#] &/@ threads;
	
	(* Standardization coefficients *)
	means = Mean[organizedThreads]; (* Mean works on columns *)
	norms = Norm[#](*Norm[#]*) 
				& /@ ((organizedThreads- ConstantArray[means,Length[organizedThreads]]) // Transpose);
	(*stdDevs = StandardDeviation[#] & /@ (organizedThreads // Transpose);*)
	nConstantFlags = Min[#] != Max[#] & /@ (organizedThreads // Transpose);
	normalizeFunction = Function[orgThread,
		(Pick[orgThread,injectedFlag] - Pick[injectedMeans,injectedFlag]) / Pick[injectedNorms,injectedFlag] 
	];
	
	normalizeFunction = normalizeFunction /. injectedFlag -> nConstantFlags;
	normalizeFunction = normalizeFunction /. injectedMeans -> means;
	normalizeFunction = normalizeFunction /. injectedNorms -> norms;
	
	normalizedThreads = normalizeFunction[#] & /@ organizedThreads;
	
	
	(* Generate the actual reducer *)
	Which[
		method == "PrincipalComponentAnalysis" || method == "PCA",
		(* This runs under Mathematica 10.1 *)
		If[dimensions===Automatic,
			
			dimensions =CountsBy[(* Kaiser's criterion *)
							Eigenvalues[
									Correlation[normalizedThreads]
							],
						#>=1&] [True];
		];
		reducer = DimensionReduction[normalizedThreads,dimensions,Method->"PrincipalComponentsAnalysis"];
			
		,
		
		True,
		Print["Error: the selected method is unknown"];
	];
	
	(* Organize the overall ThreadReducerFunction *)
	reducedMetricNames = method <> ToString[#] & /@ Range[dimensions]; 
	 
	reducerFunction = Function[thread,
		reducerModule[inThreadR_] := Module[{organizedVector},
			organizedVector = injectOrganizerFunction[inThreadR];
			organizedVector = injectNormalizeFunction[organizedVector];
			
			
			Re[#] & /@ injectReducer[organizedVector]
		];
		
		{injectReducedMetricNames,reducerModule[thread]} // Transpose
	];
	reducerFunction = reducerFunction /. injectOrganizerFunction -> organizerFunction;
	reducerFunction = reducerFunction /. injectNormalizeFunction -> normalizeFunction;
	reducerFunction = reducerFunction /. injectReducer -> reducer;
	reducerFunction = reducerFunction /. injectReducedMetricNames -> reducedMetricNames;
	
	(* This function provides reduction of any thread based on the observed threads. *)
	reducerFunction
];

(******************************** Clustering ********************************)

Silhouette[clusters_,dataset_] := Module[{c,notC,notInC,i,a,b,S},
	S = Reap[Do[(* Forall clusters c *)
		notC = Complement[clusters,{c}];(* List of other clusters *)
		If[Length[c]>1, (* If c includes a single point its inner dissimilarity is undefined *)
			Do[(* Forall i in clusters *)
				a = Mean[EuclideanDistance[dataset[i],dataset[#]] & /@ Complement[c,{i}] ];
				b = Reap[Do[
					Sow[Mean[EuclideanDistance[dataset[i],dataset[#]] & /@ notInC ] ];
				,{notInC,notC}]][[2,1]];
				b = Min[b];
				Sow[If[Max[a,b]==0.,-1,(b-a)/Max[a,b]]];
			,{i,c}];
			
			,
			(*Sow[Null];*)
			Sow[0];
		];
	,{c,clusters}]][[2,1]];
	
	S = Select[S,#=!=Null&];
	If[Length[S]>=1,
		Return[Mean[S]];
		,
		Return[1];
	];
];


(*	Considering the cluster Ids for the reference node are sorted (i.e. {1,2,3,4}),
	the matchingIds for the target node reports:
 	pos->id. E.g. matchingIds = {1,3,2,4} means that the second (pos=2) cluster of target
 	node is matched to the third cluster of the reference node (matchingIds[[pos]]) *)
InverseMatchingIds[matchingIds_] := Module[{c,ret=matchingIds},
(* Revert matchingIds such to let reference become the target and vice versa *)
	Do[
		ret[[c]] = Position[matchingIds,c][[1,1]];
	,{c,Length[matchingIds] }];
	ret
];

SortingVector[matchingIds_] := 
(* Returns a vector to be used as index to sort the target node content such to align to the reference *)
	InverseMatchingIds[matchingIds];(* It turns out that it is the same *)
	
InverseSortingVector[matchingIds_] := 
(* Returns a vector to be used as index to sort the reference node content such to align to the target *)
	matchingIds;(*SortingVector[InverseMatchingIds[matchingIds]];*)(* I turns out that it is the same *)

SetAttributes[MatchClusters,HoldAll];
MatchClusters[clusterThreadStructure_,reducedThreadStructure_] := Module[{k,dest,r,e,c,i,metrics,clusters,runs,
	varyingIds,(* Identifying scaling parameters that changes (path-search dimensions) *)
	keys,edgeList={},partialOrdering={},pathDirection,pathNegMList,GetMinimumPartialOrdering,
	pcaMedianClusters,pcaMedianRunsClusters,ComputeFeatures,featureClusterStructure,
	combinations,kAbsoluteMatchingIds,destAbsoluteMatchingIds,eRelativeMatchingIds,inverseTraversing,
	DF,Score,scores,relativeEdgeMatchingIds,absoluteEdgeMatchingIds,
	reachableKeys,expandedKeys,crtEdges,matchCount,missingEdges,
	sortingVector,inverseSortingVector,sortReachableKeysCode
	},
	
	(* Init *)
	keys = GetScalingConfigurations[reducedThreadStructure];
	metrics = Length[keys[[1]]];
	varyingIds = Select[Range[metrics], Min[keys[[All,#]]]!= Max[keys[[All,#]]] &];
	clusters = Length[ clusterThreadStructure[[1,1]] ];(* Nclusters constant for: runs, scaling configuration *)
	runs = Length[clusterThreadStructure[[1]]];(* Runs constant for: scaling configuration *)
	
	If[clusters == 1, Return[Association[]]];
	
	GetMinimumPartialOrdering[] := Module[{DominateQ,reachInTwo},
		(* Get the minimum partial ordering for the current direction *)
		DominateQ[k1_,k2_] := And @@
								( k1[[#]] * pathDirection[[#]] <=
								  k2[[#]] * pathDirection[[#]] & /@ Range[metrics]);
								
		(* Pareto compare all 'ordered' couple of points *)  
		partialOrdering = Select[Union[Subsets[keys,{2}],Reverse[#]& /@ Subsets[keys,{2}]],
								DominateQ[#[[1]], #[[2]] ]&
							];
							
		(* Compute 2 step edges *)
		reachInTwo = Select[Union[Subsets[partialOrdering,{2}],Reverse[#]& /@ Subsets[partialOrdering,{2}]],
							#[[1,2]] === #[[2,1]] (* edges are connected *) &
					];
		reachInTwo = {#[[1,1]],#[[2,2]]} & /@ reachInTwo;
		
		(* Fileter away two step edges *)
		partialOrdering = Complement[partialOrdering,reachInTwo];
	];
	
	(* Defining sorting function for deciding which node to expand next *)
	SetAttributes[sortReachableKeysCode,HoldFirst];
	sortReachableKeysCode[rKeys_,mCount_] := Module[{keyScores,localK,localScore},
		keyScores = Association[Reap[Do[
			localScore = Values[Sort[mCount[localK],Greater]];
			localScore = 2*localScore[[1]] - Total[localScore];
			
			Sow[localK->localScore];
			
		,{localK,rKeys}]][[2,1]]];
		
		rKeys = Keys[Sort[keyScores,Greater]];
	];
	
	(* Find the proximity graph. Along all directions. *)
	Do[
		pathDirection = ConstantArray[1,Length[keys[[1]]]];
		pathDirection[[pathNegMList]]=-1;
		
		GetMinimumPartialOrdering[(*keys,pathDirection*)];
		
		edgeList = Union[edgeList, Sort[#]&/@partialOrdering(* These are all undirected edges *)];
		
		,{
			pathNegMList,
			
			Subsets[Complement[varyingIds,{varyingIds[[1]]}]] (* Start from 2 because if you will negate all
								directions including the first one you will obtain the same partial ordering. *)
		 }
	];(* edgeList proximity graph. *)
	
	(* Compute clustering features for matching *)
	ComputeFeatures[threadsData_,cluster_] := Median[threadsData[[Key[#],All,2]] & /@ cluster];
	
	featureClusterStructure = Association[Reap[
		Do[
			pcaMedianRunsClusters = Reap[
				Do[
					pcaMedianClusters = ComputeFeatures[ reducedThreadStructure[[Key[k],r]], #] 
													& /@ clusterThreadStructure[[Key[k],r]];
					Sow[pcaMedianClusters];
				,{r, runs }];
			][[2,1]];
			Sow[k->pcaMedianRunsClusters];
		,{k,keys}];
	][[2,1]] ]; 
	
	
	(* Match features for each edge in the graph by correlating each node in the edgelist. Breadth first traversing *)
	combinations = Permutations[Range[clusters],{clusters}];
	DF[tf1_,tf2_] := (DF[tf1,tf2] = EuclideanDistance[tf1,tf2]; DF[tf2,tf1] = DF[tf1,tf2]);
	Score[featureCluster1_,featureCluster2_,combination_] :=
		Total[ DF[ featureCluster1[[combination[[#]]]],featureCluster2[[#]] ] & /@Range[clusters] ];
	
	relativeEdgeMatchingIds =	Association[Reap[
		Do[
			scores = Reap[Do[
				Sow[Mean[
					Score[featureClusterStructure[ e[[1]] ][[#]], featureClusterStructure[ e[[2]] ][[#]], c ]
					& /@ Range[runs]
				]];
			,{c,combinations}]][[2,1]];
			
			i = SelectFirst[Range[Length[combinations]], scores[[#]] == Min[scores]& ];
			Sow[e-> combinations[[i]] ];
		,{e,edgeList}];
	][[2,1]] ];
	
	(* Propagate ordering in reference to root node (any node can be root, here we use the the first node) *)
	reachableKeys = {keys[[1]]};
	matchCount = Association[#->Association[] & /@ keys ];
	matchCount[keys[[1]]] = Merge[{matchCount[keys[[1]]], {Range[clusters]->1}},Total];
	expandedKeys = {};
	missingEdges = edgeList;
	absoluteEdgeMatchingIds = Association[];
	
	While[reachableKeys=!={},
		(* Init expansion *)
		(* TODO we may significantly improve clustering accuracy by
			taking the scaling configuration having the most certain
			classification (i.e. having congruent votes)... *)
		k = reachableKeys[[1]];
		
		crtEdges = Select[missingEdges,MemberQ[#,k]&];
		
		(* Update matches. I.e. expand k by traversing all edges. *)
		Do[
			kAbsoluteMatchingIds = Keys[Sort[matchCount[k],Greater]][[1]];(* Highest vote wrt the root node *)
			
			If[e[[1]] === k,
				dest = e[[2]]; inverseTraversing = False;
				,
				dest = e[[1]]; inverseTraversing = True;
			];
			
			(* Retrive absolute ordering (wrt the root node) *)
			eRelativeMatchingIds = relativeEdgeMatchingIds[e];
			If[ inverseTraversing,
				eRelativeMatchingIds = InverseMatchingIds[eRelativeMatchingIds];
			];
			destAbsoluteMatchingIds = kAbsoluteMatchingIds[[InverseSortingVector[eRelativeMatchingIds]]];
			
			(* Update absoluteEdgeMatchingIds including edge traversing direction. *)
			AssociateTo[absoluteEdgeMatchingIds,{k,dest}-> destAbsoluteMatchingIds];
			
			(* Update match count *)
			matchCount[dest] = Merge[{matchCount[dest], {destAbsoluteMatchingIds->1}},Total];
			
			
		,{e,crtEdges}];
		
		(* Terminate expansion *)
		AppendTo[expandedKeys,k];
		AppendTo[reachableKeys,#]& /@ Complement[Flatten[crtEdges],reachableKeys];
		reachableKeys = Drop[reachableKeys,{1}];
		If[Length[reachableKeys]>0,
			sortReachableKeysCode[reachableKeys,matchCount];
		];
		missingEdges = Complement[missingEdges,crtEdges];
	];
	
	(* Sort the clustering structure with the identified matching. *)
	Do[
		kAbsoluteMatchingIds = Keys[Sort[matchCount[k],Greater]][[1]];
		sortingVector = SortingVector[kAbsoluteMatchingIds];
		inverseSortingVector = InverseSortingVector[kAbsoluteMatchingIds];
		
		(* Change ordering *)
		Do[
			clusterThreadStructure[[Key[k],r]] = clusterThreadStructure[[Key[k],r]] [[sortingVector]];
		,{r,runs}];
		
		(* Update voting in reference to new ordering (matchCount and absoluteEdgeMatchingIds) *)
		matchCount[k] = Association[ Reap[ 
							Sow[#[[sortingVector]]->matchCount[k,#]]& /@ Keys[matchCount[k]] 
						][[2,1]] ];
		
		crtEdges = Select[ Keys[absoluteEdgeMatchingIds], MemberQ[#,k]& ];
		Do[
			If[ k === e[[2]],
				(* absoluteEdgeMatchingIds is in reference to the root node (first node in "keys").
				Resorting e[[1]] does not matter because it is neither the reference nor the target.
				e[[1]] only indicates the path along which the matching was computed. *)
				absoluteEdgeMatchingIds[[Key[e]]] = absoluteEdgeMatchingIds[[Key[e]]] [[sortingVector]];
			];
		,{e,crtEdges}];
		
	,{k,keys}];
	
	Return[absoluteEdgeMatchingIds];
];


SetAttributes[ExtendMatchClusters,HoldAll];
ExtendMatchClusters[trainClusteringResults_,testClusterThreadStructure_,testReducedThreadStructure_] :=
Module[{k,dest,r,e,c,i,metrics,clusters,testRuns,
	varyingIds,(* Identifying scaling parameters that changes (path-search dimensions) *)
	testKeys,trainKeys,keys,edgeList={},partialOrdering={},pathDirection,pathNegMList,GetMinimumPartialOrdering,
	pcaMedianClusters,pcaMedianRunsClusters,ComputeFeatures,featureClusterStructure,
	combinations,kAbsoluteMatchingIds,destAbsoluteMatchingIds,eRelativeMatchingIds,inverseTraversing,
	DF,Score,scores,relativeEdgeMatchingIds,absoluteEdgeMatchingIds,
	reachableKeys,expandedKeys,crtEdges,matchCount,missingEdges,
	sortingVector,inverseSortingVector,
	trainClusterThreadStructure,trainReducedThreadStructure,
	clusterThreadStructure,reducedThreadStructure
	},
	
	(* Init *)
	trainClusterThreadStructure = trainClusteringResults["clusterStructure"];
	trainReducedThreadStructure = trainClusteringResults["reducedStructure"];
	
	testKeys = GetScalingConfigurations[testReducedThreadStructure];
	trainKeys = GetScalingConfigurations[trainReducedThreadStructure];
	keys = Union[testKeys,trainKeys];
	reducedThreadStructure = Join[testReducedThreadStructure,trainReducedThreadStructure];
	clusterThreadStructure = Join[testClusterThreadStructure,trainClusterThreadStructure];
	
	
	metrics = Length[keys[[1]]];
	varyingIds = Select[Range[metrics], Min[keys[[All,#]]]!= Max[keys[[All,#]]] &];
	clusters = Length[ testClusterThreadStructure[[1,1]] ];(* Nclusters constant for: testRuns, scaling configuration *)
	testRuns = Length[ testClusterThreadStructure[[1]]];(* Runs constant for: scaling configuration *)
	
	GetMinimumPartialOrdering[] := Module[{DominateQ,reachInTwo},
		(* Get the minimum partial ordering for the current direction,
			considering only edges involving at least one test key *)
		DominateQ[k1_,k2_] := And @@
								( k1[[#]] * pathDirection[[#]] <=
								  k2[[#]] * pathDirection[[#]] & /@ Range[metrics]);
								
		(* Pareto compare all 'ordered' couple of points *)  
		partialOrdering = Select[Union[Subsets[keys,{2}],Reverse[#]& /@ Subsets[keys,{2}]],
								DominateQ[#[[1]], #[[2]] ]&
							];
							
		(* Compute 2 step edges *)
		reachInTwo = Select[Union[Subsets[partialOrdering,{2}],Reverse[#]& /@ Subsets[partialOrdering,{2}]],
							#[[1,2]] === #[[2,1]] (* edges are connected *) &
					];
		reachInTwo = {#[[1,1]],#[[2,2]]} & /@ reachInTwo;
		
		(* Fileter away two step edges *)
		partialOrdering = Complement[partialOrdering,reachInTwo];
		
		(* Fileter away those not involving any testKey *)
		partialOrdering = Select[partialOrdering, Intersection[#,testKeys] =!= {} &];
	];
	
	(* Find the proximity graph. Along all directions. *)
	Do[
		pathDirection = ConstantArray[1,Length[keys[[1]]]];
		pathDirection[[pathNegMList]]=-1;
		
		GetMinimumPartialOrdering[(*keys,pathDirection*)];
		
		edgeList = Union[edgeList, Sort[#]&/@partialOrdering(* These are all undirected edges *)];
		
		,{
			pathNegMList,
			
			Subsets[Complement[varyingIds,{varyingIds[[1]]}]] (* Start from 2 because if you will negate all
								directions including the first one you will obtain the same partial ordering. *)
		 }
	];(* edgeList proximity graph. *)
	
	(* Compute clustering features for matching *)
	ComputeFeatures[threadsData_,cluster_] := Median[threadsData[[Key[#],All,2]] & /@ cluster];
	
	featureClusterStructure = Association[Reap[
		Do[
			pcaMedianRunsClusters = Reap[
				Do[
					pcaMedianClusters = ComputeFeatures[ reducedThreadStructure[[Key[k],r]], #] 
													& /@ clusterThreadStructure[[Key[k],r]];
					Sow[pcaMedianClusters];
				,{r, testRuns }];
			][[2,1]];
			Sow[k->pcaMedianRunsClusters];
		,{k,keys}];
	][[2,1]] ]; 
	
	
	(* Match features for each edge in the graph by correlating each node in the edgelist. Breadth first traversing *)
	combinations = Permutations[Range[clusters],{clusters}];
	DF[tf1_,tf2_] := (DF[tf1,tf2] = EuclideanDistance[tf1,tf2]; DF[tf2,tf1] = DF[tf1,tf2]);
	Score[featureCluster1_,featureCluster2_,combination_] :=
		Total[ DF[ featureCluster1[[combination[[#]]]],featureCluster2[[#]] ] & /@Range[clusters] ];
	
	relativeEdgeMatchingIds =	Association[Reap[
		Do[
			scores = Reap[Do[
				Sow[Mean[
					Score[featureClusterStructure[ e[[1]] ][[#]], featureClusterStructure[ e[[2]] ][[#]], c ]
					& /@ Range[testRuns]
				]];
			,{c,combinations}]][[2,1]];
			
			i = SelectFirst[Range[Length[combinations]], scores[[#]] == Min[scores]& ];
			Sow[e-> combinations[[i]] ];
		,{e,edgeList}];
	][[2,1]] ];
	
	(* Propagate ordering in reference to root node.
		The root is in the trainingKeys and all trainingKeys have been previously reached and sorted. *)
	reachableKeys = Intersection[trainKeys,Union[Flatten[ edgeList ]] ] ;
	matchCount = Association[#->Association[] & /@ testKeys ];
	AppendTo[matchCount, Association[#->Association[Range[clusters]->1] ] ] &/@reachableKeys;
	expandedKeys = {};
	missingEdges = edgeList;
	absoluteEdgeMatchingIds = Association[];
	
	While[reachableKeys=!={},
		(* Init expansion *)
		k = reachableKeys[[1]];
		
		crtEdges = Select[missingEdges,MemberQ[#,k]&];
		
		(* Update matches. I.e. expand k by traversing all edges. *)
		Do[
			kAbsoluteMatchingIds = Keys[Sort[matchCount[k],Greater]][[1]];(* Highest vote wrt the root node *)
			
			If[e[[1]] === k,
				dest = e[[2]]; inverseTraversing = False;
				,
				dest = e[[1]]; inverseTraversing = True;
			];
			
			(* Retrive absolute ordering (wrt the root node) *)
			eRelativeMatchingIds = relativeEdgeMatchingIds[e];
			If[ inverseTraversing,
				eRelativeMatchingIds = InverseMatchingIds[eRelativeMatchingIds];
			];
			destAbsoluteMatchingIds = kAbsoluteMatchingIds[[InverseSortingVector[eRelativeMatchingIds]]];
			
			(* Update absoluteEdgeMatchingIds including edge traversing direction. *)
			AssociateTo[absoluteEdgeMatchingIds,{k,dest}-> destAbsoluteMatchingIds];
			
			(* Update match count *)
			matchCount[dest] = Merge[{matchCount[dest], {destAbsoluteMatchingIds->1}},Total];
			
			
		,{e,crtEdges}];
		
		(* Terminate expansion *)
		AppendTo[expandedKeys,k];
		AppendTo[reachableKeys,#]& /@ Complement[Flatten[crtEdges],reachableKeys];
		reachableKeys = Drop[reachableKeys,{1}];
		missingEdges = Complement[missingEdges,crtEdges];
	];
	
	(* Sort the clustering structure with the identified matching. *)
	Do[
		kAbsoluteMatchingIds = Keys[Sort[matchCount[k],Greater]][[1]];
		sortingVector = SortingVector[kAbsoluteMatchingIds];
		inverseSortingVector = InverseSortingVector[kAbsoluteMatchingIds];
		
		(* Change ordering *)
		Do[
			testClusterThreadStructure[[Key[k],r]] = testClusterThreadStructure[[Key[k],r]] [[sortingVector]];
		,{r,testRuns}];
		
		(* Update voting in reference to new ordering (matchCount and absoluteEdgeMatchingIds) *)
		matchCount[k] = Association[ Reap[ 
							Sow[#[[sortingVector]]->matchCount[k,#]]& /@ Keys[matchCount[k]] 
						][[2,1]] ];
		
		crtEdges = Select[ Keys[absoluteEdgeMatchingIds], MemberQ[#,k]& ];
		Do[
			If[ k === e[[2]],
				(* absoluteEdgeMatchingIds is in reference to the root node (first node in "keys").
				Resorting e[[1]] does not matter because it is neither the reference nor the target.
				e[[1]] only indicates the path along which the matching was computed. *)
				absoluteEdgeMatchingIds[[Key[e]]] = absoluteEdgeMatchingIds[[Key[e]]] [[sortingVector]];
			];
		,{e,crtEdges}];
		
	,{k,testKeys}];
	
	(* absoluteEdgeMatching has to be completed by prepending the results from the training sorting. *)
	absoluteEdgeMatchingIds =
		Join[trainClusteringResults["absoluteEdgeMatchingIds"],absoluteEdgeMatchingIds];
	
	Return[absoluteEdgeMatchingIds];
];

(* This returns clusterThreadStructure. *)
ClusterSearchCode[targetClusters_,reducedThreadStructure_,keys_,clusteringMethod_] :=
Module[{
	reducedRuns,clusteredRuns,clusteredRun,
	r,k,res
},
	res = Association[Reap[
		Do[
			reducedRuns = reducedThreadStructure[k];
			
			clusteredRuns = Reap[
				Do[
					If[targetClusters=!=Automatic,
					
						clusteredRun = FindClusters[	Values[r[[All,All,2]]]->Keys[r],targetClusters,
														DistanceFunction -> EuclideanDistance,
														Method->clusteringMethod];
						,
						
						clusteredRun = FindClusters[	Values[r[[All,All,2]]]->Keys[r],
														DistanceFunction -> EuclideanDistance,
														Method->clusteringMethod];
					];
					Sow[clusteredRun];
					
				,{r,reducedRuns}];
			][[2,1]];
			
			Sow[{k->clusteredRuns}]; 
		,{k,keys}];
	][[2,1]]];
	
	Return[res];
];

(* This is half-tail, right? Thus 5%

 0.1%. Following values in:
 http://vlado.fmf.uni-lj.si/info/ifcs06/symbolic/NBclust.pdf *)
Options[SingleClusterBetter] = {"ConfidenceLevel"->"Best"};
SingleClusterBetter[clusterThreadStructure_,reducedThreadStructure_,keys_,OptionsPattern[]] := 
Module[{dudaHartTestCode,baeleTestCode,testInitCode,squareDistanceCode,computeJXYCode,
		confidenceLevel,singleWorseCount,singleBetterCount,k,r},
	(* 
	Implementation of Duda-Hart & Baele tests: duda1973, baeleXYZ
	pg.28:
	https://theses.lib.vt.edu/theses/available/etd-12062005-153906/unrestricted/Proposal-Face.pdf
	pg. 6:
	https://www.jstatsoft.org/article/view/v061i06/v61i06.pdf
	*)
	(* Initialization *)
	confidenceLevel = OptionValue["ConfidenceLevel"];
	singleBetterCount = 0;
	singleWorseCount = 0;
	
	SetAttributes[testInitCode,HoldAll];
	testInitCode[d_,n_,s_,clusterStructure_,threadStructure_] :=Module[{},
		s = Length[clusterStructure];
		If[s != 2,
			SingleClusterBetter::dimensionMismatch =
				"Warning: Duda-Hart test shold compare a 2-clusters solution with a 1-cluster solution. However "
				<> ToString[s] <>
				"-clusters solution passed instead of a 2-clusters one.";
			Message[SingleClusterBetter::dimensionMismatch]; 
		];
		n = Length[threadStructure ];(* threads *)
		d = Length[threadStructure[[1]] ];(* params in profile (PCA) *)
	];
	
	squareDistanceCode[points_] :=Module[{center},
		center = Mean[points];
		Total[(EuclideanDistance[center,#]^2) &/@ points]
	];
	SetAttributes[computeJXYCode,HoldAll];
	computeJXYCode[J21_,J22_,threadStructure_,clusterStructure_] := Module[{points},
		(* Compute sum square distance when having 1 cluster *)
		points = #[[All, 2]] & /@ Values[threadStructure];
		J21 = squareDistanceCode[points];
		
		(* Compute sum square distance when having 2 clusters *)
		points = #[[All, 2]] & /@ Values[KeySelect[threadStructure,MemberQ[clusterStructure[[1]],#]& ]];
		J22 = squareDistanceCode[points];
		
		points = #[[All, 2]] & /@ Values[KeySelect[threadStructure,MemberQ[clusterStructure[[2]],#]& ]];
		J22 = J22 + squareDistanceCode[points];
	];
	
	(* Duda-Hart test code *)
	dudaHartTestCode[clusterStructure_,threadStructure_,cl_] := Module[
				{J22,J21,ref,d,n,s,zeta},
		(* init d,n,s as: params in profiles (PCA); number of threads; number of clusters. *)
		testInitCode[d,n,s,clusterStructure,threadStructure];
		
		(* Compute sum square distance when having 1 cluster j21 *)
		(* Compute sum square distance when having 2 clusterS j22 *)
		computeJXYCode[J21,J22,threadStructure,clusterStructure];
		
		(* Compute reference cutting value *)
		If[cl == "Best",
			zeta = 3.2;
			,
			zeta = Abs[N[InverseCDF[NormalDistribution[0, 1], cl]]];
		];
		
		
		ref = N[1 - (2/(Pi*d)) - zeta * Sqrt[(2*(1-8/(Pi*Pi*d)))/(d*n)]];
		
		J22/J21 > ref 
	];
	
	(* Baele test code *)
	baeleTestCode[clusterStructure_,threadStructure_,cl_] := Module[
				{J22,J21,ref,d,n,s, effe},
		(* init d,n,s as: params in profiles (PCA); number of threads; number of clusters. *)
		testInitCode[d,n,s,clusterStructure,threadStructure];
		If[n <= 2, Return[False];];
		
		(* Compute sum square distance when having 1 cluster j21 *)
		(* Compute sum square distance when having 2 clusterS j22 *)
		computeJXYCode[J21,J22,threadStructure,clusterStructure];
		
		(* Compute reference cutting value *)
		If[cl == "Best",
			effe = N[InverseCDF[FRatioDistribution[d, (n-2)*d], 1-0.1]];
			,
			effe = N[InverseCDF[FRatioDistribution[d, (n-2)*d], 1-cl]];
		];
		
		
		
		ref = effe;
		(*Print[ToString[((J21-J22)/J21 ) / N[( ( ((n-1)/(n-2)) *(2^(2/d)) ) -1)]]<>"<"<> ToString[ref] ];*)
		((J21-J22)/J21 ) / N[( ( ((n-1)/(n-2)) *(2^(2/d)) ) -1)] < ref 
	];
	
	
	
	
	Do[
		Do[
			If[baeleTestCode(*dudaHartTestCode*)[
						clusterThreadStructure[k][[r]] , 
						reducedThreadStructure[k][[r]] ,
						confidenceLevel
					],
				singleBetterCount = singleBetterCount +1;
				,
				singleWorseCount = singleWorseCount +1;
			];
		,{r,Range[Length[clusterThreadStructure[k]]]}];
	,{k,keys}];
	
	
	
	Return [singleBetterCount>singleWorseCount];
];

Options[MultiplerunsThreadClustering] = {
	"ReducerMethod" -> "PCA",
	"ReducerDimensions" -> Automatic,
	"ClusteringMethod" -> {"Agglomerate","Linkage"->"Average","SignificanceTest"->"Silhouette"},
	"Clusters" -> Automatic,
	"IncludeCommVector" -> False,
	"CommPatternClustering" -> False (* If this is set to True, then the wu2011 algorithm is adopted
										as implemented in CommunicationPatternClustering.m *)
};
(*
	All thread of all runs are used together to identify a dimension reduction funtion.
	Different runs are associated with different scaling parameters.
	
	Having an unique reduction function enables to have an unique space for all clusters
	in all runs. Thus clusters behavior over varying scaling parameters can be observed
	in an unique space.
	
	1st: identify that space (one may play with rotating the space such to
	correlate-scorrelate the dimensions with the scaling parameters).
	2nd: identify the number of clusters (this number cannot change with the scaling
	parameters but threads in a cluster may become 0 if needed).
	3rd: find clusters for all runs.
	4th: associate the clusters over the different runs.
	5th: return the cluster structure.
 *)
MultiplerunsThreadClustering[trainAlgAssociation_,OptionsPattern[]] :=
Module[{wholeThreadPool,trainKeys,threadDimensionReducer,
		(* 	reducedThreadStructure and clusterThreadStructure are associations
			with the same structure of trainAlgAssociation but each algorithm
			run is described in terms of its reduced dimensionalities. *)
		options,
		reducedThreadStructure, reducedRun, reducedRuns,
		clusterThreadStructure, tmpRun,
		k,r,
		reducerMethod,reducerDimensions,clusteringMethod,
		clusters,threadCounts,
		maxClusters,minClusters,
		silhouettes,silhouetteMatrix,weights,clusters2Silhouette,
		absoluteEdgeMatchingIds,
		maxCommCount,includeCommVectorOpt,commPatternClusteringOpt,includeCommVector,
		cpcRes
		},

	(* Initialization *)
	reducerMethod 		= OptionValue["ReducerMethod"];
	reducerDimensions 	= OptionValue["ReducerDimensions"];
	clusteringMethod 	= OptionValue["ClusteringMethod"];
	clusters		 	= OptionValue["Clusters"];
	includeCommVectorOpt= OptionValue["IncludeCommVector"];
	commPatternClusteringOpt= OptionValue["CommPatternClustering"];
	
	If[commPatternClusteringOpt,
		cpcRes = MultipleRunsPatternClustering[trainAlgAssociation];
		cpcRes = CommClusterStructure2CommGroupsClusterStructure[cpcRes];
		cpcRes = CommGroupsClusterStructure2MultipleRunsClusteringStructure[cpcRes];
		
		Return[cpcRes];
	];
	
	options = Association["ReducerMethod"->reducerMethod,"ReducerDimensions"->reducerDimensions,
							"ClusteringMethod"->clusteringMethod,"Clusters"->clusters,
							"CommPatternClustering" -> False];
	
	trainKeys = GetScalingConfigurations[trainAlgAssociation];		(* scaling configurations *)
	threadCounts = Association[#->0& /@ trainKeys ];
	maxCommCount = Association[#->0& /@ trainKeys ];
	
	wholeThreadPool = Reap[
		Do[
			Do[
				Sow[#] & /@ Values[r];
			,{r,trainAlgAssociation[k]}];
			
			threadCounts[k] = Min[Length[#] &/@ trainAlgAssociation[k]];
			maxCommCount[k] = Max[Length[GetKeyValue[#,"MPIcommVector"] ] 
									&/@ trainAlgAssociation[k][[1]] ];
		,{k,trainKeys}];
	][[2,1]];
	
	maxCommCount = Values[maxCommCount];
	maxCommCount = Union[maxCommCount];
	includeCommVector = (Length[maxCommCount] <= 1) && includeCommVectorOpt;
	
	(* The number of clusters must always be lower than (or equal to) the minimun number of threads *)
	If[Min[threadCounts] == 1, clusters=1 ];
	
	(* Get the dimensionReducer function *)
	threadDimensionReducer = ThreadDimensionReduction[wholeThreadPool,
								"Method"->reducerMethod,"Dimensions"->reducerDimensions,
								"IncludeCommVector"->includeCommVector];
	reducerDimensions = Length[threadDimensionReducer[
							trainAlgAssociation[[ Key[ trainKeys[[1]] ] ,1,1]]
						]];
	
	(* Reduce all threads for all runs. *) 
	reducedThreadStructure = Association[];
	clusterThreadStructure = Association[];
	
	Do[
		reducedRuns = Reap[
			Do[
				reducedRun = Map[threadDimensionReducer,r];
				Sow[reducedRun];
				
			,{r,trainAlgAssociation[k]}];
		][[2,1]];
		
		AppendTo[reducedThreadStructure,Association[k->reducedRuns]];
	,{k,trainKeys}];
	
	(* generate initial tentative clustering. *)
	clusterThreadStructure = ClusterSearchCode[clusters,reducedThreadStructure,
												trainKeys,clusteringMethod];
	
	
	(* If number of cluster is preselected, match clusters and return *)
	If[clusters =!= Automatic,
		absoluteEdgeMatchingIds = MatchClusters[ clusterThreadStructure,reducedThreadStructure];
		Return[
			Association[{"clusterStructure"->clusterThreadStructure
						,"reducedStructure"->reducedThreadStructure,
						"absoluteEdgeMatchingIds"->absoluteEdgeMatchingIds,
						"threadDimensionReducer"->threadDimensionReducer,
						"clusteringOptions"->options}]
		]
	];
	
	(* Get minimum and maximum clusters range. *)
	clusters = Reap[
		Do[
			Do[
				Sow[Length[r]];
			,{r,clusterThreadStructure[k]}];
		,{k,trainKeys}];
	][[2,1]];
	maxClusters = Max[clusters];
	minClusters = Min[clusters];
	
	
	If[maxClusters <= 2,
		(* Test for clustering significance. *)
		ClusterSearchCode[2,reducedThreadStructure,trainKeys,clusteringMethod];(* Runs could be clustered using 1 cluster. *)
		If[SingleClusterBetter[clusterThreadStructure,reducedThreadStructure,trainKeys],
			clusterThreadStructure = ClusterSearchCode[1,reducedThreadStructure,
															trainKeys,clusteringMethod];
			(* Folding into single cluster. *)
			maxClusters = 1;
			minClusters = 1;
			,
			clusterThreadStructure = ClusterSearchCode[2,reducedThreadStructure,
																	trainKeys,clusteringMethod];
			maxClusters = 2;
			minClusters = 2;
		];
	];
	
	If[maxClusters == minClusters,
		options["Clusters"] = maxClusters;
		absoluteEdgeMatchingIds = MatchClusters[ clusterThreadStructure,reducedThreadStructure];
		Return[
			Association[{"clusterStructure"->clusterThreadStructure
						,"reducedStructure"->reducedThreadStructure,
						"absoluteEdgeMatchingIds"->absoluteEdgeMatchingIds,
						"threadDimensionReducer"->threadDimensionReducer,
						"clusteringOptions"->options}]
		];
	];
	
	maxClusters = Min[maxClusters,20];
	maxClusters = Min[maxClusters,Min[threadCounts]];
	minClusters = Max[minClusters,2];
	
	(* Compute all silhouettes *)		
	weights = Reap[(* The weight is the number of processes in an execution. *)
		Do[
			Do[
				Sow[Length[Flatten[clusterThreadStructure[k][[r]], 1  ] ]];
			,{r,Range[Length[ clusterThreadStructure[k] ]]}];
		,{k,trainKeys}];
	][[2,1]];
		
	clusters2Silhouette = Association[#->0 & /@ Range[minClusters,maxClusters] ];
	silhouetteMatrix = Association[Reap[For[clusters = minClusters, clusters <= maxClusters, clusters++,
		clusterThreadStructure = ClusterSearchCode[clusters,reducedThreadStructure,
													trainKeys,clusteringMethod];
		
		silhouettes = Reap[
			Do[
				Do[
					tmpRun = reducedThreadStructure[k][[r]];
					Sow[Silhouette[clusterThreadStructure[k][[r]],tmpRun]];
				,{r,Range[Length[clusterThreadStructure[k]]]}];
			,{k,trainKeys}];
		][[2,1]];
		
		
		Sow[clusters->silhouettes];
	]][[2,1]]];
	
	Do[
		clusters2Silhouette[clusters] =
			Dot[silhouetteMatrix[clusters],weights]/Total[weights] ;
	,{clusters,Range[minClusters,maxClusters]}];
	
	
	(* Get best clusters *)
	(*Print[ToString[clusters2Silhouette]];*)
	clusters = SelectFirst[	{Keys[clusters2Silhouette],Values[clusters2Silhouette]}//Transpose,
							#[[2]] == Max[Values[clusters2Silhouette]]& ][[1]];
	options["Clusters"] = clusters;
	clusterThreadStructure = ClusterSearchCode[clusters,reducedThreadStructure,
												trainKeys,clusteringMethod];
							
	If[clusters == 2,
		(* Test for clustering significance. *)
		If[SingleClusterBetter[clusterThreadStructure,reducedThreadStructure,trainKeys],
			clusterThreadStructure = ClusterSearchCode[1,reducedThreadStructure,
														trainKeys,clusteringMethod];
			options["Clusters"] = 1;
			Print["Folding into single cluster"];
			,
			clusterThreadStructure = ClusterSearchCode[2,reducedThreadStructure,
														trainKeys,clusteringMethod];
			options["Clusters"] = 2;
		];
		
	];
	
	absoluteEdgeMatchingIds = MatchClusters[ clusterThreadStructure,reducedThreadStructure];
	
	(* Returns the processing results. *)
	Return[
		Association[{"clusterStructure"->clusterThreadStructure
					,"reducedStructure"->reducedThreadStructure,
					"absoluteEdgeMatchingIds"->absoluteEdgeMatchingIds,
					"threadDimensionReducer"->threadDimensionReducer,
					"clusteringOptions"->options}]
	];
];



(* ExtendMultiplerunsThreadClustering extends the clustering methodology applied to the training set
	towards the test set. The reduced space over which to perform the clustering has been defined within 
	trainClusteringResults["threadDimensionReducer"] while clustering the training. The options for
	clustering are defined in trainClusteringResults["clusteringOptions"]. The ordering has to be extended
	starting from "absoluteEdgeMatchingIds". *)
ExtendMultiplerunsThreadClustering[trainClusteringResults_,trainAlgAssociation_,testAlgAssociation_] :=
Module[{
	clusterThreadStructure,reducedThreadStructure,absoluteEdgeMatchingIds,threadDimensionReducer,options,
	clusters,clusteringMethod,
	reducedRun,reducedRuns,k,r,
	testKeys,trainKeys,useCommunicationPatternClustering,cpcRes
},
	(* Initialization *)
	testKeys = GetScalingConfigurations[testAlgAssociation];
	trainKeys = GetScalingConfigurations[trainAlgAssociation];
	
	(* Retrive training data to be extended *)
	options = trainClusteringResults["clusteringOptions"];
	threadDimensionReducer = trainClusteringResults["threadDimensionReducer"];
	clusters = options["Clusters"];
	clusteringMethod = options["ClusteringMethod"];
	useCommunicationPatternClustering = options["CommPatternClustering"];
	
	If[useCommunicationPatternClustering,
		cpcRes = MultipleRunsPatternClustering[testAlgAssociation];
		cpcRes = CommClusterStructure2CommGroupsClusterStructure[cpcRes];
		cpcRes = CommGroupsClusterStructure2MultipleRunsClusteringStructure[cpcRes];
		
		Return[cpcRes];
	];
	
	
	(* Applying dimension reduction *)
	reducedThreadStructure = Association[];
	
	Do[
		reducedRuns = Reap[
			Do[
				reducedRun = Map[threadDimensionReducer,r];
				Sow[reducedRun];
				
			,{r,testAlgAssociation[k]}];
		][[2,1]];
		
		AppendTo[reducedThreadStructure,Association[k->reducedRuns]];
	,{k,testKeys}];
	
	(* Applying clustering *)
	clusterThreadStructure = ClusterSearchCode[clusters,reducedThreadStructure,
												testKeys,clusteringMethod];
	
	(* Extending ordering to the test set. *)
	absoluteEdgeMatchingIds = ExtendMatchClusters[trainClusteringResults,
													clusterThreadStructure,reducedThreadStructure];

	(* Returns the processing results. *)
	Return[
		Association[{"clusterStructure"->clusterThreadStructure
					,"reducedStructure"->reducedThreadStructure,
					"absoluteEdgeMatchingIds"->absoluteEdgeMatchingIds,
					"threadDimensionReducer"->threadDimensionReducer,
						"clusteringOptions"->options}]
	];
];

(******************************** GUI ********************************)


SetAttributes[SwapClusters, HoldFirst];
SwapClusters[clusteringData_,thisScalingConfiguration_,id0_,id1_] := Module[{dummy,eList},
	(*Print["Swap: "<> ToString[id0] <> " " <> ToString[id1] ];*)
	
	(* Sort clusterStructure *)
	dummy = clusteringData[["clusterStructure",Key[thisScalingConfiguration],1,id0]];
	clusteringData[["clusterStructure",Key[thisScalingConfiguration],1,id0]] =
		clusteringData[["clusterStructure",Key[thisScalingConfiguration],1,id1]];
	clusteringData[["clusterStructure",Key[thisScalingConfiguration],1,id1]] = dummy;
	
	(* Sort absoluteEdgeMatchingIds *)
	eList = Select[Keys[clusteringData["absoluteEdgeMatchingIds"]]
								,#[[2]] === thisScalingConfiguration&];
	
	(
		dummy = clusteringData[["absoluteEdgeMatchingIds",Key[#],id0]];
		clusteringData[["absoluteEdgeMatchingIds",Key[#],id0]] =
			clusteringData[["absoluteEdgeMatchingIds",Key[#],id1]];
		clusteringData[["absoluteEdgeMatchingIds",Key[#],id1]] = dummy;
	) & /@ eList;
];

SetAttributes[SwapClusterGraphic, HoldFirst];
SwapClusterGraphic[basicGraphicGrid_,id0_,id1_] := Module[{dummy},
	(* Sort clusters in the graphics *)
	dummy = basicGraphicGrid[[1,2,id0]];
	basicGraphicGrid[[1,2,id0]] = basicGraphicGrid[[1,2,id1]];
	basicGraphicGrid[[1,2,id1]] = dummy;
];

SetAttributes[ClusterOrderingContrlButton, HoldAll];
ClusterOrderingContrlButton[clusteringData_,basicGraphicGrid_,clusterId_,thisScalingConfiguration_] :=
Module[{res,bLeft,bRight,clusters},
	
	(* Initialization *)
	clusters = Length[clusteringData[["clusterStructure",1,1]]];(* First scaling configuration, first run. *)
	
	
	bLeft = Button["<",
				SwapClusters[clusteringData,injectScalingConfiguration,injectId,injectId-1];
				SwapClusterGraphic[basicGraphicGrid,injectId,injectId-1];
			,Enabled -> clusterId > 1 ];
	bRight = Button[">",
				SwapClusters[clusteringData,injectScalingConfiguration,injectId,injectId+1];
				SwapClusterGraphic[basicGraphicGrid,injectId,injectId+1];
			,Enabled -> clusterId < clusters ];
			
	bLeft = bLeft 	/. {injectId -> clusterId, injectScalingConfiguration -> thisScalingConfiguration};
	bRight = bRight /. {injectId -> clusterId, injectScalingConfiguration -> thisScalingConfiguration};
	
	res = bLeft bRight;
	
	Return[res]; 
];

SetAttributes[ConfigurationControlCell, HoldAll];
ConfigurationControlCell[basicGraphicGrid_,focusScalingConfiguration_,thisScalingConfiguration_,
	highlightVoters_,highlightVoted_,clusteringData_,absoluteEdgeMatchingIds_] :=
Module[{res,isVoter,isVoted,underControl,clusterVotesAndControl,c,cc,cCode,
	clusters,votes,voteAssociation,relationsOfInterests, colors
},
	(* Initialization *)
	clusters = Length[clusteringData[["clusterStructure",1,1]]];
	colors = ColoringFunction[1,clusters];
	
	relationsOfInterests = Select[Keys[absoluteEdgeMatchingIds]
									,MemberQ[#,focusScalingConfiguration]&];
	If[focusScalingConfiguration === thisScalingConfiguration,
		isVoter = False;
		isVoted = False;
		,
		isVoter = MemberQ[relationsOfInterests[[All,1]], thisScalingConfiguration ];
		isVoted = MemberQ[relationsOfInterests[[All,2]], thisScalingConfiguration ];
	];
	
	(* Check if necessary to introduce control row *)
	underControl = 	(focusScalingConfiguration === Null) || isVoter || isVoted ||
					(focusScalingConfiguration === thisScalingConfiguration);

	(* Format the control row *)
	If[underControl,
		votes = KeySelect[clusteringData["absoluteEdgeMatchingIds"], #[[2]] === thisScalingConfiguration&  ];
		
		If[Length[votes]>=1,
			(* Not the root node, controls needed. *)
			clusterVotesAndControl = Reap[Do[(* Voters count for each cluster *)
				cCode = FromCharacterCode[ToCharacterCode["A"]+c-1];
				
				voteAssociation = Association[Reap[Do[
					Sow[cc -> Length[Select[votes,#[[c]]==cc &] ] ]; 
				,{cc,Range[clusters]}]][[2,1]] ];
				voteAssociation = KeyMap[FromCharacterCode[ToCharacterCode["A"]+#-1]&,voteAssociation];
				
				Sow[(* It's sowing the control cell below the cluster 'c', named cCode and voted voteAssociation *)
					Grid[Reap[
						If[ExaBoundsFormatInteractiveQ[],
							Sow[{Style["Selected type: "<> cCode,Bold,colors[c] ]}];
							
							Sow[{Style["Voted "<> # <>" by: "<> ToString[voteAssociation[#]],Small]}]
								& /@ Keys[voteAssociation];
							Sow[{ClusterOrderingContrlButton[clusteringData,basicGraphicGrid,c,thisScalingConfiguration]}];
						,
							Sow[{Style["Type: "<> cCode,Bold,colors[c] ]}];
						];
					][[2,1]]]
				];
				
			,{c,Range[clusters]}]][[2,1]];
			
			
			,
			
			(* Root node *)
			clusterVotesAndControl = Reap[Do[(* Voters count for each cluster *)
				cCode = FromCharacterCode[ToCharacterCode["A"]+c-1];
				Sow[Grid[
					If[ExaBoundsFormatInteractiveQ[],
						{
							{Style["Root scaling configuration.",Bold]},
							{Style["Cluster type definition.",Bold]},
							{Style["Type: "<> cCode,Bold,colors[c]]}
						}
						,
						{	{Style["Type: "<> cCode,Bold,colors[c]]}}
					]
				]];
			,{c,Range[clusters]}]][[2,1]];
		];
		res = Insert[basicGraphicGrid,clusterVotesAndControl,{1,3}];
		
		,
		res = basicGraphicGrid;
	];
	
	If[isVoter && highlightVoters,
		res = Grid[First[res],Options[res], Background -> LightYellow ];
	];
	If[isVoted && highlightVoted,
		res = Grid[First[res],Options[res], Background -> LightBlue ];
	];
	
	Return[res];
];

(*Options[InteractiveMultiplerunsClusterPlot] = {"ShowCommunicationGraph"->False};*)
SetAttributes[InteractiveMultiplerunsClusterPlot, HoldAll];
InteractiveMultiplerunsClusterPlot[trainClusteringData_,trainClustersGraphic_,
									testClusteringData_,testClustersGraphic_,testFlag_] :=
Module[{
	res,s,highlightVoters=True,highlightVoted=True,focusOn=Null,
	clusters,scalingConfigurations,trainScalingConfigurations,testScalingConfigurations,clustersControl,
	absoluteEdgeMatchingIds,visualizingClusteringCode
	(*,
	(* Being a dynamic module, this will be consistent along Dynamic evaluations *)
	trainClustersGraphic = trainClustersGraphicParam *)
},
	(* Initialization *)
	trainScalingConfigurations = Keys[trainClusteringData[["clusterStructure"]]];
	If[testFlag,
		testScalingConfigurations = Keys[testClusteringData[["clusterStructure"]]];
		absoluteEdgeMatchingIds = testClusteringData[["absoluteEdgeMatchingIds"]]; 
		,
		testScalingConfigurations = {};
		absoluteEdgeMatchingIds = trainClusteringData[["absoluteEdgeMatchingIds"]];
	];
	scalingConfigurations = Join[trainScalingConfigurations,testScalingConfigurations];
	clusters = Length[trainClusteringData[["clusterStructure",1,1]]];(* First scaling configuration, first run.*)
	
	
	(* Common table visualization code *)
	SetAttributes[visualizingClusteringCode,HoldAll];
	visualizingClusteringCode[scalingConfigurationsList_,clusterGraphic_,clusteringData_] :=
	Do[
		Sow[{
			Dynamic[If[
				MemberQ[(* scalingConfigurations[[injectS]] should be a neighbour of focusOn *)
					Flatten[Select[(* Takes all neighbours of focusOn *)
						Keys[absoluteEdgeMatchingIds],
						MemberQ[#,focusOn]& 
					]]
					,scalingConfigurationsList[[injectS]]
				] 
				|| focusOn === Null
				,
				
				ConfigurationControlCell[clusterGraphic[[1,injectS,1]], focusOn,
					injectScalingConf,highlightVoters,highlightVoted,clusteringData,
					injectAbsoluteEdgeMatchingIds],
				Style[ToString[scalingConfigurationsList[[injectS]]],Bold ]
			],TrackedSymbols :> {focusOn,clustersControl,highlightVoters,
								highlightVoted,clusteringData}]
			/.	{
					injectS -> s, clustersControlInject -> clustersControl,
					injectScalingConf -> scalingConfigurationsList[[s]],
					injectAbsoluteEdgeMatchingIds -> absoluteEdgeMatchingIds
				}
			,
			"Focus on this:"RadioButton[Dynamic[focusOn],scalingConfigurationsList[[s]] ]
		}];
	,{s,Length[scalingConfigurationsList]}];
	

	(* Reformatting *)
	res = Reap[(* Each Sow, generates a line (i.e. a list of two columns) *)
		
		(* Control grid for focus *)
		Sow[{
			Grid[{
				{ Style["Highlight voters",Darker[Yellow]] Checkbox[Dynamic[highlightVoters]] ,
				 Style["Highlight voted",Blue] Checkbox[Dynamic[highlightVoted]] }
			}]
			,
			Grid[{
				{ "Show all:" RadioButton[Dynamic[focusOn],Null]} ,
				 {"Hide all:" RadioButton[Dynamic[focusOn],0] }
			}]
			
		}];
		
		(* Section Title *)
		Sow[{Style["Training Clustering Data","Section"],SpanFromLeft}];
		
		(* Visualizing train clustering data *)
		visualizingClusteringCode[Evaluate[trainScalingConfigurations],trainClustersGraphic,trainClusteringData];
		
		If[testFlag,
			(* Section Title *)
			Sow[{Style["Test Clustering Data","Section"],SpanFromLeft}];
			
			(* Visualizing test clustering data *)
			visualizingClusteringCode[Evaluate[testScalingConfigurations],testClustersGraphic,testClusteringData];
		];
		
	][[2,1]];
	
	res = Grid[res, Frame->All, FrameStyle->Thick];
	Return[res];
];
									

Options[MultiplerunsThreadClusteringGUI] =
Join[Options[MultiplerunsThreadClustering],
	{	
		"Predictor" -> Null,
		"ShowCommunicationGraph" -> True,
		"ClusteringControl" -> True
	}
];
SetAttributes[MultiplerunsThreadClusteringGUI, HoldAll];
MultiplerunsThreadClusteringGUI[	clusteringDataTrain_,clusteringDataTest_,
									trainAlgAssociation_,testAlgAssociation_
									,OptionsPattern[]] :=
DynamicModule[{
	reducerMethod, reducerDimensions, clusteringMethod, clusters,
	axis,showReducedSpace=False,a,
	pcaUpperBoundaries = Association[],pcaLowerBoundaries = Association[],
	upperBoundaries = Association[],lowerBoundaries = Association[],
	visualizeUpperBoundaries = Association[],visualizeLowerBoundaries = Association[],
	allAxisNames,axisName2axis,axisBCK,axisTMP,
	metricVisualizatorSelector,globalClusteringManipulation,
	(* The dynamic module saves the trainClustersGraphic variable to be updated during within Dynamic. *)
	trainVisualizeStructure,trainClustersGraphic,
	testFlag,testVisualizeStructure,testClustersGraphic,
	clusteringNeeded=False,extensionClusteringNeeded=False,predictorModel,predictorVisualizeStructure,
	predictorStructure=Null,reducedPredictorStructure=Null,allScalingConfigurations,threadDimensionReducer,
	showCommunicationGraph, clusteringControl
},		
	(* Initialization *)
	reducerMethod 		= OptionValue["ReducerMethod"];
	reducerDimensions 	= OptionValue["ReducerDimensions"];
	clusteringMethod 	= OptionValue["ClusteringMethod"];
	clusters		 	= OptionValue["Clusters"];
	predictorModel			= OptionValue["Predictor"];
	showCommunicationGraph	= OptionValue["ShowCommunicationGraph"];
	clusteringControl		= OptionValue["ClusteringControl"];
	testFlag = Length[testAlgAssociation] >= 1 ;
	If[!testFlag,
		allScalingConfigurations = GetScalingConfigurations[trainAlgAssociation];,
		allScalingConfigurations = Union[	GetScalingConfigurations[trainAlgAssociation],
											GetScalingConfigurations[testAlgAssociation]];
	];
	
	(* Run initial clustering *)
	If[!AssociationQ[clusteringDataTrain],
		clusteringNeeded = True;
		,
		If[!MemberQ[Keys[clusteringDataTrain],"clusterStructure"],
			clusteringNeeded = True;
		];
	];
	
	If[clusteringNeeded,
		clusteringDataTrain = MultiplerunsThreadClustering[trainAlgAssociation,
			"ReducerMethod" -> reducerMethod, "ReducerDimensions" -> reducerDimensions,
			"ClusteringMethod" -> clusteringMethod, "Clusters" -> clusters
		];	
		extensionClusteringNeeded = True;
		,
		If[!AssociationQ[clusteringDataTest],
			extensionClusteringNeeded = True;
			,
			If[!MemberQ[Keys[clusteringDataTest],"clusterStructure"],
				extensionClusteringNeeded = True;
			];
		];
	];
	
	
	If[testFlag && extensionClusteringNeeded,
		clusteringDataTest = ExtendMultiplerunsThreadClustering[clusteringDataTrain,trainAlgAssociation,
								testAlgAssociation];
	];
	
	
	(* Setup default values *)
	clusters = Length[clusteringDataTrain["clusterStructure"][[1,1]]];
	axis = Association[Options[MultiplerunsClusterPlot]][["axis"]];
	
	axisBCK={};
	
	allAxisNames = GetExtrapolationMetricTypeOptions[];
	allAxisNames = KeySelect[allAxisNames,!MemberQ[allAxisNames[#],"memFunction"]&];
	allAxisNames = Keys[allAxisNames];
	allAxisNames = Select[allAxisNames,#!=MultiThreadExtrapolation`Private`threadCountName&];
	axisName2axis = Association[Reap[
		Sow[#->#] & /@ allAxisNames;
		Sow[{"D0dreuse","ICDF",#} -> AxisDescription2AxisName[{"D0dreuse","ICDF",#}] ] &/@ {0.25,0.5,0.75};
		Sow[{"D0dreuse","CDF",#} -> AxisDescription2AxisName[{"D0dreuse","CDF",#}] ] &/@ {512,1024,2048};
		(*Sow[{"D0ireuse","ICDF",#} -> AxisDescription2AxisName[{"D0ireuse","ICDF",#}] ] &/@ {0.25,0.5,0.75};
		Sow[{"D0ireuse","CDF",#} -> AxisDescription2AxisName[{"D0ireuse","CDF",#}] ] &/@ {512,1024,2048};*)
	][[2,1]]];
	
	If[testFlag,
		lowerBoundaries = Association[Reap[Do[
			Sow[
				a->Min[GetLowerBound[trainAlgAssociation,a],GetLowerBound[testAlgAssociation,a]]
			];
		,{a,Keys[axisName2axis]}] ][[2,1]] ];
		upperBoundaries = Association[Reap[Do[
			Sow[
				a->Max[GetUpperBound[trainAlgAssociation,a],GetUpperBound[testAlgAssociation,a]]
			];
		,{a,Keys[axisName2axis]}] ][[2,1]] ];
		
		
		pcaLowerBoundaries = Association[Reap[
			Do[
				Sow[
					a->Max[GetLowerBound[clusteringDataTrain["reducedStructure"],a],
						GetLowerBound[clusteringDataTest["reducedStructure"],a]
					]
				];
			,{a,clusteringDataTrain["reducedStructure"][[1,1,1,All,1]]}]
		][[2,1]] ];
		pcaUpperBoundaries = Association[Reap[
			Do[
				Sow[
					a->Max[GetUpperBound[clusteringDataTrain["reducedStructure"],a],
						GetUpperBound[clusteringDataTest["reducedStructure"],a]
					]
				];
			,{a,clusteringDataTrain["reducedStructure"][[1,1,1,All,1]]}]
		][[2,1]] ];
		
		visualizeUpperBoundaries = upperBoundaries;
		visualizeLowerBoundaries = lowerBoundaries;
	];
	If[predictorModel =!= Null,
		predictorStructure = Association[#->{ExtrapolateMultiThreadAlgorithm[predictorModel,#]}
								& /@allScalingConfigurations
							];
		threadDimensionReducer = clusteringDataTrain[["threadDimensionReducer"]];
		reducedPredictorStructure = Map[threadDimensionReducer,predictorStructure,{3}];
		,
		predictorStructure = Null;
		reducedPredictorStructure = Null;
	];
	
	
	trainVisualizeStructure = trainAlgAssociation;
	predictorVisualizeStructure = predictorStructure;
	trainClustersGraphic = MultiplerunsClusterPlot[trainVisualizeStructure,
		clusteringDataTrain["clusterStructure"],"axis"->axis,
		"upperBoundaries" -> visualizeUpperBoundaries,"lowerBoundaries" -> visualizeLowerBoundaries,
		"predictor" -> predictorVisualizeStructure,
		"showCommunicationGraph"->showCommunicationGraph];
	If[testFlag,
		testVisualizeStructure = testAlgAssociation;
		testClustersGraphic = MultiplerunsClusterPlot[testVisualizeStructure,
			clusteringDataTest["clusterStructure"],"axis"->axis,
			"upperBoundaries" -> visualizeUpperBoundaries,"lowerBoundaries" -> visualizeLowerBoundaries,
			"predictor" -> predictorVisualizeStructure,
			"showCommunicationGraph"->showCommunicationGraph];
	];
		
						
		
	globalClusteringManipulation = InteractiveMultiplerunsClusterPlot[
									clusteringDataTrain,trainClustersGraphic,
									clusteringDataTest,testClustersGraphic,testFlag];
	
									
	
	(* Main control bar *)
	metricVisualizatorSelector = Grid[{
		{(* Title *)
			Style["Thread clustering analysis","Title"]
		},
		{(* Play with the number of clusters *)
			If[clusteringControl,
				Grid[
					{
						{
							Style["Select number of clusters","Section"],SpanFromLeft,SpanFromLeft
						},
						{
							Style[
								"Suggested clusters: " <>
								ToString[clusters](* This is not dynamically upated. *),
								Gray
							]
							,
							Style[
								"Selected clusters: "	InputField[Dynamic[clusters],Number]
							]
							,
							Button["Recompute",
								If[clusters != Length[clusteringDataTrain["clusterStructure"][[1,1]]],
									(* Regenerate the clustering for the selected number of clusters *)
									clusteringDataTrain = MultiplerunsThreadClustering[trainAlgAssociation,
										"ReducerMethod" -> reducerMethod,
										"ReducerDimensions" -> reducerDimensions,
										"ClusteringMethod" -> clusteringMethod, "Clusters" -> clusters
									];
											
									If[testFlag,
										clusteringDataTest =
											ExtendMultiplerunsThreadClustering[clusteringDataTrain,
												trainAlgAssociation,testAlgAssociation];
									];
								];
								trainClustersGraphic = MultiplerunsClusterPlot[trainVisualizeStructure,
									clusteringDataTrain["clusterStructure"],"axis"->axis,
									"upperBoundaries" -> visualizeUpperBoundaries,
									"lowerBoundaries" -> visualizeLowerBoundaries,
									"predictor" -> predictorVisualizeStructure,
									"showCommunicationGraph"->showCommunicationGraph];
								If[testFlag,
									testClustersGraphic = MultiplerunsClusterPlot[testVisualizeStructure,
										clusteringDataTest["clusterStructure"],"axis"->axis,
										"upperBoundaries" -> visualizeUpperBoundaries,
										"lowerBoundaries" -> visualizeLowerBoundaries,
										"predictor" -> predictorVisualizeStructure,
										"showCommunicationGraph"->showCommunicationGraph];
								];
								globalClusteringManipulation = InteractiveMultiplerunsClusterPlot[
																clusteringDataTrain,trainClustersGraphic,
																clusteringDataTest,testClustersGraphic,testFlag];
							]
						}
					}
				,Frame->All]
				,
				Nothing
			]
		},
		{(* Axis selection for visualization *)
			Grid[{
				{
					Style["Visualized metrics","Section"]
				},
				{
					Style["Principal component analysis."] Checkbox[Dynamic[showReducedSpace, showReducedSpace=#;
						axisTMP = axis; axis = axisBCK; axisBCK = axisTMP;
						If[axis==={},
							If[showReducedSpace,
								axis = clusteringDataTrain["reducedStructure"][[1,1,1,All,1]];
								,
								axis = Association[Options[Plot]][["axis"]];
							];
						];
						
						trainVisualizeStructure=If[showReducedSpace,
												clusteringDataTrain["reducedStructure"],
												trainAlgAssociation
						];
						
						If[testFlag,
							testVisualizeStructure=If[showReducedSpace,
													clusteringDataTest["reducedStructure"],
													testAlgAssociation
							];
						];
						
						visualizeUpperBoundaries=If[showReducedSpace,
												pcaUpperBoundaries,
												upperBoundaries
						];
						visualizeLowerBoundaries=If[showReducedSpace,
												pcaLowerBoundaries,
												lowerBoundaries
						];
						
						If[predictorModel =!= Null,
							predictorVisualizeStructure=If[showReducedSpace,
													reducedPredictorStructure,
													predictorStructure
							];
						];
						
						&
					]]
				},
				{
					Dynamic[CheckboxBar[Dynamic[axis],
						If[showReducedSpace,
							clusteringDataTrain["reducedStructure"][[1,1,1,All,1]]
							,
							Normal[axisName2axis]
						]
					],TrackedSymbols :> {showReducedSpace}]
				},
				{
					Button["Refresh",
						If[axis === {},
							If[showReducedSpace,
								axis = clusteringDataTrain["reducedStructure"][[1,1,1,All,1]];
								,
								axis = Association[Options[Plot]][["axis"]];
							];
						];
						trainClustersGraphic = MultiplerunsClusterPlot[
							trainVisualizeStructure,clusteringDataTrain["clusterStructure"],
								"axis"->axis,
								"upperBoundaries" -> visualizeUpperBoundaries,
								"lowerBoundaries" -> visualizeLowerBoundaries,
								"predictor"->predictorVisualizeStructure,
								"showCommunicationGraph"->showCommunicationGraph];
						If[testFlag,
							testClustersGraphic = MultiplerunsClusterPlot[testVisualizeStructure,
								clusteringDataTest["clusterStructure"],"axis"->axis,
								"upperBoundaries" -> visualizeUpperBoundaries,
								"lowerBoundaries" -> visualizeLowerBoundaries,
								"predictor"->predictorVisualizeStructure,
								"showCommunicationGraph"->showCommunicationGraph];
						];
						globalClusteringManipulation = InteractiveMultiplerunsClusterPlot[
														clusteringDataTrain,trainClustersGraphic,
														clusteringDataTest,testClustersGraphic,testFlag];
					]
				}
			},Frame->All]
			
		}
	},Frame->All,FrameStyle->Thick];
	
	Panel[
		Grid[{{metricVisualizatorSelector},{Dynamic[globalClusteringManipulation]}}]
	]
	(*trainClustersGraphic*)
];

End[] (* End Private Context *)

EndPackage[]