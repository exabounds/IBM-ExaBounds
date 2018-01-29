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

BeginPackage["CommunicationPatternClustering`"]
Needs["Support`"]
Needs["ExaBoundsGeneric`"]
Needs["ComputerArithmetic`"]
(* Exported symbols added here with SymbolName::usage *)
(* This implements a modified version of the communication group identification described in:
	 Wu, X. & Mueller, F.
	 ScalaExtrap: Trace-based Communication Extrapolation for SPMD Programs
	 Proceedings of the 16th ACM Symposium on Principles and Practice of Parallel Programming, ACM, 2011, 113-122 
*)

SingleRunPatternClustering::usage =
	"SingleRunPatternClustering[threads_]" 

MultipleRunsPatternClustering::usage =
	"MultipleRunsPatternClustering[algAssociation_]"

FindCommGroups::usage =
	"FindCommGroups[threads_,clusteredCommVectors_]"
	
GetCommGroup::usage =
	"GetCommGroup[c_,v_]"
	
GetCommGroups::usage =
	"GetCommGroups[param_]"
	
CommClusterStructure2CommGroupsClusterStructure::usage =
	"CommClusterStructure2CommGroupsClusterStructure[commPatternClusterStructure_]"

CommGroupsClusterStructure2MultipleRunsClusteringStructure::usage =
	"CommGroupsClusterStructure2MultipleRunsClusteringStructure[commGroupsPatternClusterStructure_]"
	
ExplodeCommGroup::usage =
	"ExplodeCommGroup[commGroup_]"
	
RowMajorClusterSorting::usage =
	"RowMajorClusterSorting[clusters_]"

Begin["`Private`"] (* Begin Private Context *) 

(* This does not include the comm group identification and compression *)
SingleRunPatternClustering[threads_] :=
Module[{k,keys,clusteredCommVectors,unclusteredCommVectors,commVector,patternList,pattern,
		c,sizes,msgs,src},
	(* collect the communication vectors *)
	unclusteredCommVectors = Association[ 
			# -> GetKeyValue[threads[#],"MPIcommVector"  ]
		 & /@ Keys[threads] ];
	
	
	(* Identify all possible patterns in the communication graph. *)
	(* A pattern is a list of source nodes. *)
	(* We assume that edges in the communication vectors are sorted by source id (offset) *)
	patternList = Association[];
	
	Do[
		pattern = unclusteredCommVectors[k] [[All,1]];
		
		If[MissingQ[ patternList[pattern] ],
			(* New pattern, save the key *)
			AppendTo[patternList,pattern -> {k} ];
			
			,
			(* Known pattern, add the key *)
			AppendTo[patternList[pattern], k ];
		];
		
	,{k,Keys[threads]}];
	
	(* Collect cluster statistics *)
	clusteredCommVectors = Association[Reap[Sow[Nothing];
		Do[
			(* Ids of processes in the cluster *)
			keys = patternList[pattern];
			
			commVector = Reap[Sow[Nothing];
				Do[
					src = pattern[[c]];
					
					sizes = Values[KeyTake[unclusteredCommVectors,keys] [[All,c,2]]];
					msgs = Values[KeyTake[unclusteredCommVectors,keys] [[All,c,3]]];
					
					Sow[
						Association[
							{	
								"relativeSrc" -> pattern[[c]],
								
								"minSize" -> Min[sizes], "maxSize" -> Max[sizes],
								"medianSize" -> Median[sizes], "meanSize" -> Mean[sizes],
								"stdevSize" -> If[Length[sizes]>1,StandardDeviation[sizes],0],
								
								"minMsgs" -> Min[msgs], "maxMsgs" -> Max[msgs],
								"medianMsgs" -> Median[msgs], "meanMsgs" -> Mean[msgs],
								"stdevMsgs" -> If[Length[msgs]>1,StandardDeviation[msgs],0]
							} 
						]
					];
				,{c,Range[Length[pattern]]}];
			][[2,1]];
			
			Sow[Sort[keys]->commVector];
		,{pattern,Keys[patternList]}];
	][[2,1]]];
	
	(* Sort clusters in row-major fashion *)
	Return[KeyTake[clusteredCommVectors, RowMajorClusterSorting[ Keys[clusteredCommVectors]] ] ];
];

(* This does not include the comm group identification and compression *)
MultipleRunsPatternClustering[algAssociation_] :=
Module[{c,configurations,clusteringStructure,runs,threads},
	(* init *)
	configurations = GetScalingConfigurations[algAssociation];
	
	(* for each scaling configuration run SingleRunPatternClustering *)
	clusteringStructure = Association[Reap[ Sow[Nothing];
		Do[
			runs = algAssociation[c];
			
			(* for each run, run SingleRunPatternClustering *)
			clusteringStructure = Association[Reap[ Sow[Nothing];
				Do[
					Sow[ SingleRunPatternClustering[threads] ];
				,{threads,runs}]
			][[2,1]]];
			
			Sow[c -> clusteringStructure]
		,{c,configurations}]
	][[2,1]]];
	
	Return[clusteringStructure];
];

(*
	MultipleRunsPatternClustering[algAssociation_] returns a structure similar to:
	MultiplerunsThreadClustering[algAssociation_].
	
	The result of MultipleRunsPatternClustering[algAssociation_] is such that threads in the same communication group
	are clustered together. However the clusters do not include a description of the communication groups.
	The communication groups are generated here.
	
	Search for dimensions in scaling configurations that seems to have all dimensions developed.
	Apply dimension structure to all other scaling configurations.
*)
CommClusterStructure2CommGroupsClusterStructure[commPatternClusterStructure_] := 
Module[{c,configurations, result,
		commGroupsCount, conf2group2dimensions, group2dimensions, g, good, 
		dimIds, dimId, globalDimId2dimIds, globalDimId, vals2dimIds, val, d,
		globalSet, crtSet, globalSetId, globalDimId2crtVals,oldVals,newVals},
	
	configurations = GetScalingConfigurations[commPatternClusterStructure];
	
	(* Apply the commGroupSeed to all configurations *)
	result = Association[Reap[ Sow[Nothing];
		Do[
			Sow[ c -> ( GetCommGroups[ commPatternClusterStructure[c] ] ) ];
		,{c,configurations}];
	][[2,1]]];
	
	(* Check that the number of communication groups is the same for each scaling configuration *)
	commGroupsCount = Length[ result[#] ] & /@ configurations;
	If[ Length[Union[commGroupsCount]] != 1,
		CommClusterStructure2CommGroupsClusterStructure::varyingClusters =
			"Clusters based on communication graph change from run to run. Not possible to identify communication groups.";
		Message[CommClusterStructure2CommGroupsClusterStructure::varyingClusters];
		Return[result];
	];
	commGroupsCount = commGroupsCount[[1]];
	
	(* Compute dimensions of each communication group *)
	group2dimensions = Association[ #->0 & /@ Range[commGroupsCount] ] ;
	conf2group2dimensions = Association[ # -> group2dimensions & /@ configurations ];
	
	Do[
		Do[
			conf2group2dimensions[c][g] = result[c][[g]] ["dimensions"];
		,{g,Range[commGroupsCount]}];
	,{c,configurations}];
	
	Do[
		group2dimensions[g] = Max[ conf2group2dimensions[#][g] & /@ configurations ];
	,{g,Range[commGroupsCount]}];
	
	
	(* Take only the configurations GOOD that have all dimensions for all communication groups *)
	good = Association[ #->True & /@ configurations ];
	Do[
		Do[
			good[c] = good[c] && conf2group2dimensions[c][g] == group2dimensions[g];
		,{g,Range[commGroupsCount]}];
	,{c,configurations}];
	
	
	
	(* Assign ids to dimensions by matching them along the different
		communication groups along the different GOOD configurations *)
	dimIds = Reap[ Sow[Nothing];
		Do[
			Sow[{g,#}] & /@ Range[ group2dimensions[g] ];
		,{g,Range[commGroupsCount]}];
	][[2,1]];
	
	globalDimId2dimIds = Association[1->dimIds]; (* First we assume a single global dimension. Then we break it down step by step. *)
	globalDimId = 2;
	
	Do[
		(* Forall c:, partition the dimIds {groupId, d} by means of the values they assume {stride, iter} *)
		vals2dimIds = Association[];
		
		Do[(* loop over any possible dimension {groupId, d} *)
			g = dimId[[1]]; 
			d = dimId[[2]]; 
			val = Values[result[c][[g]]["dimension"<>ToString[d]] ];
			
			If[MissingQ[vals2dimIds[val]],
				vals2dimIds[val] = {dimId};
				,
				AppendTo[vals2dimIds[val],dimId];
			];
		,{dimId,dimIds}];
		
		(* Breakdown globalDimId2dimIds such to make sure all dimensions that are different in c have different globalIds *)
		Do[
			crtSet 		= Select[vals2dimIds,MemberQ[#,dimId]& ] [[1]];(* this must be a single element association *)
			globalSet 	= Select[globalDimId2dimIds,MemberQ[#,dimId]& ]; (* this must be a single element association *)
			globalSetId = Keys[globalSet][[1]]; 
			globalSet 	= globalSet[[1]]; 
			
			If[Complement[globalSet,crtSet] =!= {},(* We need to breakdown *)
				globalDimId2dimIds[globalSetId] = Intersection[globalSet,crtSet];
				globalDimId2dimIds[globalDimId] = Complement[globalSet,globalDimId2dimIds[globalSetId]];
				globalDimId = globalDimId+1;
			];
		,{dimId,dimIds}];
		
		
	,{c,Keys[Select[good, # == True &]]}];
	
	(* Fix the BAD configurations *)
	Do[
		(* Find dimension values by using the groups that have all dimensions *)
		globalDimId2crtVals = Association[# -> { NaN, 0} & /@ Keys[globalDimId2dimIds] ];(* Default value if dimension is not found *)
		
		Do[
			If[result[c][[g]]["dimensions"] == group2dimensions[g] && group2dimensions[g] > 0 ,
				(* The group has no missing dimension, store the data! *)
				Do[
					val = Values[result[c][[g]]["dimension"<>ToString[d]] ];
					
					dimId = {g,d};
					globalSet 	= Select[globalDimId2dimIds,MemberQ[#,dimId]& ]; (* this must be a single element association *)
					globalSetId = Keys[globalSet][[1]];
					
					If[val =!= globalDimId2crtVals[globalSetId] && globalDimId2crtVals[globalSetId] =!= { NaN, 0},
						(* This configuration has a set of dimensions different from what we ever observed. *)
						CommClusterStructure2CommGroupsClusterStructure::unkownDimension =
							"When reconstructing communication graph dimensions, an unexpected error has happened!";
						Message[CommClusterStructure2CommGroupsClusterStructure::unkownDimension];
						Return[result];
					]; 
					
					globalDimId2crtVals[globalSetId] = val;
				,{d,Range[group2dimensions[g]]}];
			];
		,{g,Range[commGroupsCount]}];
		
		(* Reconstruct the missing dimensions *)
		Do[
			If[result[c][[g]]["dimensions"] < group2dimensions[g] && group2dimensions[g] > 0 ,
				(* The group has missing dimensions, store the data! *)
				oldVals = Values[result[c][[g]]["dimension"<>ToString[#]] ] & /@ Range[result[c][[g]]["dimensions"]];
				
				result[[Key[c], g, Key["dimensions"]]] = group2dimensions[g];
				
				Do[
					dimId = {g,d};
					globalSet 	= Select[globalDimId2dimIds,MemberQ[#,dimId]& ]; (* this must be a single element association *)
					globalSetId = Keys[globalSet][[1]];
					
					val = globalDimId2crtVals[globalSetId];
					
					result[[Key[c], g, Key["dimension"<>ToString[d]]]] =
						Association[ {"stride" -> val[[1]] , "iterations" ->val[[2]]}];
				,{d,Range[group2dimensions[g]]}];
				
				newVals = Values[result[c][[g]]["dimension"<>ToString[#]] ] & /@ Range[result[c][[g]]["dimensions"]];
				If[Complement[oldVals,newVals] =!= {},
					(* This configuration has a set of dimensions different from what we ever observed. *)
					CommClusterStructure2CommGroupsClusterStructure::unkownDimension =
						"When reconstructing communication graph dimensions, an unexpected error has happened!";
					Message[CommClusterStructure2CommGroupsClusterStructure::unkownDimension];
					Return[result];
				];
			];
		,{g,Range[commGroupsCount]}];
		
	,{c,Keys[Select[good, # == False &]]}];
	
	Return[result];
];





(* 
	This function translates the a communication-groups clustering structure into a
	clustering structure as the one in use by the clustering plotting functionality
	in ThreadClustering, that matches the extrapolation interface in MultipleThreadExtrapolation.
	Notice that, in a first moment we wanted to address the extrapolation of the communication
	pattern specified in a communication-groups clustering structure. This is still on todo list...
*)
CommGroupsClusterStructure2MultipleRunsClusteringStructure[commGroupsPatternClusterStructure_] := Module[
	{clusterThreadStructure, reducedThreadStructure,absoluteEdgeMatchingIds,threadDimensionReducer,
	 c, scalingConfigurations, clusters},
	 
	clusters = Length[ commGroupsPatternClusterStructure[[1]] ];
	scalingConfigurations = Keys[commGroupsPatternClusterStructure];
	
	(* Get the clustering structure *)
	clusterThreadStructure = Association[Reap[
	    Do[Sow[
	    	c -> {(First[ExplodeCommGroup[#]] & /@ commGroupsPatternClusterStructure[c])}];
	    ,{c,scalingConfigurations}];
    ][[2,1]]];
    
	(* Zero the reducedClusteringStructure *)
	reducedThreadStructure = Association[Reap[
	    Do[
	    	Sow[c->{Association[
	    		(# ->{{"PCA1",0.0}}) &/@ Sort[Flatten[clusterThreadStructure[c],2]]
	    	]}];
	    ,{c,scalingConfigurations}];
    ][[2,1]]];
    
    (* Zeros the absoluteEdgeMatchingIds *)
    absoluteEdgeMatchingIds = Association[];
    
    (* Zeros the threadDimensionReducer *)
    threadDimensionReducer = Function[thread,0.0];
    
	
	(* Returns the results. *)
	Return[
		Association[{"clusterStructure"->clusterThreadStructure
					,"reducedStructure"->reducedThreadStructure,
					"absoluteEdgeMatchingIds"->absoluteEdgeMatchingIds,
					"threadDimensionReducer"->threadDimensionReducer,
					"commGroupsClusterStructure" -> commGroupsPatternClusterStructure,
					"clusteringOptions"->Association["ReducerMethod"->"PCA","ReducerDimensions"->Automatic,
							"ClusteringMethod"->{"Agglomerate","Linkage"->"Average","SignificanceTest"->"Silhouette"},"Clusters"->clusters,
							"CommPatternClustering" -> True]
					}]
	];
];






(* Get the communication group for a given cluster c (and commVector v) *)
GetCommGroup[c_,v_] := Module[{processIds,strides,strideOptions,s,crtStride,startRank,dimensions,result,
								iterations,positions, explodedCommGroup },
								
(* Communication group. *)
(* tuple including:
1 -> dimensions (different strides),
2 -> start rank,
3 -> dimension1 stride
4 -> dimension1 iterations
5 -> dimension2 stride
6 -> dimension2 iterations
... -> dimensionN stride
... -> dimensionN iterations

**************************  Figure 2 wu2011 
*)
	(*<|
		"startRank" -> startRank, "dimensions" -> dims,
			"dimension1" -> <| "stride"->str, "iterations"->iters |>
			...
			"dimensionN" -> <| "stride"->str, "iterations"->iters |>
			
		"commVector" -> {
							<| 
								"relSrc" -> c,
								
								"minSize" -> Min[sizes], "maxSize" -> Max[sizes],
								"medianSize" -> Median[sizes], "meanSize" -> Mean[sizes],
								"stdevSize" -> If[Length[sizes]>1,StandardDeviation[sizes],0],
								
								"minMsgs" -> Min[msgs], "maxMsgs" -> Max[msgs],
								"medianMsgs" -> Median[msgs], "meanMsgs" -> Mean[msgs],
								"stdevMsgs" -> If[Length[msgs]>1,StandardDeviation[msgs],0]
							|>, ... <| for each source |> ...
						}
	|>*)
	
	(* Init *)
	processIds = Sort[ c[[All,1]] ];
	startRank = Min[processIds];
	
	strides = Differences[processIds];
	
	strideOptions = Sort[Union[strides],Greater];
	
	dimensions = Length[strideOptions];
	
	(* Collect data *)
	result = Association[Reap[
		Sow["startRank" -> startRank];
		Sow["dimensions" -> dimensions];
		
		(* Dimensions data. *)
		Do[(* For each dimension starting from the largest to the smallest stride *)
			crtStride = strideOptions[[s]];
			
			positions = Flatten[Position[strides,crtStride]];
			
			(* Iterations: number of times the stride appears *)
			iterations = Length[positions];
			
			(* Truncate stride list to fold into the lower dimensions. *)
			strides = strides[[1;;(positions[[1]]-1) ]];
			
			(* The actual stride should account for the iterations along the lower dimensions *)
			crtStride = crtStride + Total[strides];
			
			(* Sow dimension data *)
			Sow[ "dimension"<>ToString[s]->
				Association[
					"iterations" -> iterations,
					"stride" -> crtStride
				]
			];
			
		,{s,Range[dimensions]}];
		
		Sow["commVector"->v];
	][[2,1]] ];
	
	(* Check for validity and/or irregularities. *)
	explodedCommGroup = ExplodeCommGroup[result];
	If[Sort[ First[explodedCommGroup] ] =!= Sort[c],
		GetCommGroup::irregularity =
			"Reconstructed communication differs from the original one. Possible irregularity found.";
		Message[GetCommGroup::irregularity];
	];
	 
	
	Return[result];
];

(*
	Compute communication groups for a single run assuming that all dimensions of each comm group are developed.
	(i.e. impossible 1x1 squares).
	
	clusteredCommVectors is the result of a SingleRunPatternClustering.
*)
GetCommGroups[clusteredCommVectors_] :=
Module[{sortedClusters,cluster,commGroups},
	(* Sort clusters by means of their lowest processId (and second threadId) *)
	sortedClusters = RowMajorClusterSorting[ Keys[clusteredCommVectors] ];
	
	
	(* For each cluster there is a communication group! *)
	commGroups = Reap[ Sow[Nothing];
		Do[
			Sow[ GetCommGroup[cluster, clusteredCommVectors[cluster] ] ];
		,{ cluster,sortedClusters }];
	][[2,1]];
	
	Return[commGroups];
];

ExplodeCommGroup[commGroup_] :=
Module[{commVector, threadId = 0, processorIds, ids, dimensions, d, stride, iterations},
	commVector = commGroup["commVector"];
	
	processorIds = {commGroup["startRank"]};
	
	(* Iteratively accumulate the processorIds along the different dimensions *)
	dimensions = commGroup["dimensions"];
	Do[
		stride = commGroup[["dimension"<>ToString[d],"stride"]];
		iterations = commGroup[["dimension"<>ToString[d],"iterations"]];
		
		processorIds = Flatten[NestList[#+stride& , processorIds , iterations]];
	,{d,Range[dimensions]}];
	
	ids = Sort[{#,threadId} & /@ processorIds ];
	
	Return[ids -> commVector];
];

RowMajorClusterSorting[clusters_] :=
Module[{sortedClusters,sortedIds,id,cluster,min},
	(* Sort clusters by means of their lowest processId (and second threadId) *)
	sortedIds = Reap[ Sow[Nothing];(* For each cluster keep the ids having the lowest processId *)
		Do[
			min = Min[ cluster[[All,1]] ];
			Sow[ Select[cluster,#[[1]] == min& ] ];
		,{cluster,clusters}];
	][[2,1]];
	
	sortedIds = Reap[ Sow[Nothing];(* For each cluster keep the ids having the lowest threadId *)
		Do[
			min = Min[ cluster[[All,2]] ];
			Sow[ Select[cluster,#[[2]] == min& ] ];
		,{cluster,sortedIds}];
	][[2,1]];
	
	sortedIds = Sort[sortedIds[[All,1]]];
	sortedClusters = Reap[ Sow[Nothing];(* For each cluster keep the ids having the lowest processId *)
		Do[
			Sow[ SelectFirst[clusters,MemberQ[#,id] & ] ];
		,{id,sortedIds}];
	][[2,1]];
	
	Return[sortedClusters];
];


End[] (* End Private Context *)

EndPackage[]