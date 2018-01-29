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
(*
	Implementation of ding2003.
	For each XX% of reuse reference (CDF), a model is implemented to fit their distance.
	In this implementation predictions are obtained by evaluating knotMemModels, sorting their results
	and iterpolating for reuse distances equal to powers of 2. 
	
	[ding2003] Chen Ding and Yutao Zhong. 2003. Predicting whole-program locality through reuse distance analysis.
	In Proceedings of the ACM SIGPLAN 2003 conference on Programming language design and implementation
	(PLDI '03). ACM, New York, NY, USA, 245-257.
	DOI=http://dx.doi.org/10.1145/781131.781159
*)

BeginPackage["InversePerBinModels`", { "ExaBoundsGeneric`","Support`"}]
(* Exported symbols added here with SymbolName::usage *)

(* X is an array whose lines are the pridictor variables.
Last column is expected to be the memX, a separate model is generated for each value of memX observed in the training set.
It is returned a function whose input are the predictor variables and output is the prediction. *)
InversePerBinModel::usage =
	"InversePerBinModel[X_,Y_,PerBinType_,modelTypeProperties_]"

Begin["`Private`"] (* Begin Private Context *) 
InversePerBinModel[X_,Y_,InversePerBinType_,modelTypeProperties_] :=
Module[{CDFmax,CDFmin,memXid,memXidCandidates,mask,normKnotMemXids,totalKnots,predictors,
	PartitionXMonothonicallyOverId,partitions,IsPartitionSingleObservation,cdfValues,res},
	(* Identify memXid as follows:
		1) memX is always an integer number and are never negative.
		2) memX is always monotonic within an observation.
		3) Within the same observation, all variables preciding memX are constant.
		4) After memX there are always 2 indices + 2x knotMemXmodels.
	*)
	
	predictors = Length[ X[[1]] ];
	memXidCandidates = Reverse[Range[predictors-2,1,-2]];(* Init candidates as 4. *)
	
	(* Remove memXinCandidates as in 1. *)
	mask = (And@@ (#==Ceiling[#] &/@ X[[All,#]])) && (And@@ (#>=0 &/@ X[[All,#]])) & /@ memXidCandidates;
	memXidCandidates = Pick[memXidCandidates,mask];
	
	(* Define partitioning by observation given monotonic memX assumption. 2. *)
	PartitionXMonothonicallyOverId[x_,id_] := Module[{crtXidVal,crtXidIter,crtPartition,thisRes},
		crtPartition = 0;
		crtXidVal = x[[1,id]];
		
		thisRes = ConstantArray[0,Length[x]];
		For[crtXidIter = 1,crtXidIter <= Length[x], crtXidIter = crtXidIter +1,
			If[crtXidVal >= x[[crtXidIter,id]], (* Decrease may mean only a change in partition. Additionally memX must  increase within the observation. i.e. we are at a new observation. *)
				crtPartition++;
			];
			
			crtXidVal = x[[crtXidIter,id]];
			thisRes[[crtXidIter]] = crtPartition;
		];
		Return[thisRes];
	];
	
	partitions = PartitionXMonothonicallyOverId[X,#] &/@ memXidCandidates;
	
	(* Exclude candidates whose X before the id are not constant for each observation. 3. *)
	IsPartitionSingleObservation[x_,id_,partition_] := Module[{partitionId,partitionMax,observationsInPartition,iter},
		partitionMax = partition[[-1]];
		iter = 1;
		Do[
			observationsInPartition = Reap[While[partition[[iter]]==partitionId,
				If[id>1,Sow[x[[iter,1;;id-1]] ];,Sow[{}];];
				iter = iter+1;
				If[iter > Length[x],Break[];];
			]][[2,1]];
			
			If[iter > Length[x],Break[];];
			observationsInPartition = Union[observationsInPartition];
			If[Length[observationsInPartition]>1,
				Return[False];
			];
			
		,{partitionId,Range[partitionMax]}];
		Return[True];
	];
	
	mask = IsPartitionSingleObservation[X,memXidCandidates[[#]],partitions[[#]] ] & /@ Range[Length[memXidCandidates]];
	memXidCandidates = Pick[memXidCandidates, mask];
	partitions = Pick[partitions, mask];
	
	(* Select memXid *)
	Which[Length[memXidCandidates]>1,
		(* If there are multiple, select the one with the fewest partition first and the rightmost second. *)
		partitions = partitions[[All,-1]];
		mask = #==Min[partitions] & /@ partitions;
		memXidCandidates = Pick[memXidCandidates,mask];
		memXid = memXidCandidates[[-1]];
		
		,(* If we have 1 candidate we are good. *)
		Length[memXidCandidates]==1,
		memXid = memXidCandidates[[-1]];
		
		,
		True,(* If there are no candidates it is a problem. *)
		res = Function[x,Module[{vCase},
			vCase[xcrt_] := 0.;
			
			If[MatrixQ[x],
				vCase[#] & /@ x
				,
				vCase[x]
			]
		]];
		Return[res];
	];
	
	(* Select normKnotMemXids. *)
	normKnotMemXids = Range[memXid + 1,predictors,2];(* + 1 let us start from maxMemX *)
	totalKnots = Length[normKnotMemXids];(* +1 let us include the 0 *)
	
	(* Storing minimum and maximum CDF *)
	CDFmin = Min[Flatten[Y]];
	CDFmax = Max[Flatten[Y]];
	cdfValues = Subdivide[CDFmin,CDFmax,totalKnots];
	
	
	(* note that: normMemX__ = memX/memX__ --> memX__ = memX / normMemX__ *)
	(* The model needs to compute all memX__, sorting them,
		distributing CDF values to them and returning the CDF value at the memX. *)
	
	
	(* Setup extrapolation function. *)
	res = Function[x,Module[{generateCDF,
							vCase},
			(* x shall a single vector or a list of vector. Each vector of the form: {predictors,bin} *)
			
			(* Declaring function to compute the cdf given the predictorVector that includes the memory indices. *)
			generateCDF[xcrt_] := Module[{knotLocations,memX},
				memX = xcrt[[localMemXid]];
				knotLocations = Flatten[{0,N[memX/xcrt[[#]]]&/@ localNormKnotMemXids}];
				knotLocations = If[#<0.,0.,#] & /@ knotLocations;
				knotLocations = Sort[knotLocations];
				
				{knotLocations,localCdfValues}// Transpose
			];
			
			(* Declaring function for solving the vector case. *)
			vCase[xcrt_] := Module[{memX,cdf},
				memX = xcrt[[localMemXid]];
				cdf = generateCDF[xcrt];
				
				LinearInterpolation[cdf,1,memX]
			];
			
			If[MatrixQ[x],
				vCase[#] & /@ x
				,
				vCase[x]
			]
		]
	];
	
	res = res /. localMemXid -> memXid;
	res = res /. localNormKnotMemXids -> normKnotMemXids;
	res = res /. localCdfValues -> cdfValues;
	
	
	
	Return[res];
];

End[] (* End Private Context *)

EndPackage[]