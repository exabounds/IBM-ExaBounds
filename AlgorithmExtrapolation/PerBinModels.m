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
	Implementation of marin2004. For each bin, a model is implemented.
	For a better marin04 implementation, you should train on the total access count rather than on CDF and use PerBinLinearRegression.
	knotMemModels are not used and only linear models are applied (no model selection).
*)

BeginPackage["PerBinModels`", { "ExaBoundsGeneric`"}]
(* Exported symbols added here with SymbolName::usage *)

(* X is an array whose lines are the pridictor variables.
Last column is expected to be the memX, a separate model is generated for each value of memX observed in the training set.
It is returned a function whose input are the predictor variables and output is the prediction. *)
PerBinModel::usage =
	"PerBinModel[X_,Y_,PerBinType_,modelTypeProperties_]"

Begin["`Private`"] (* Begin Private Context *) 
PerBinModel[X_,Y_,PerBinType_,modelTypeProperties_] :=
Module[{modelType,XYpb,XYcrt,XYall,res,observedBins,observedPredictors,b,bestB,p,xy,predictors,pbModels
	(*,localPerBinModels,localObservedBins, localPredictors *)},
	Which[(* Select model type *)
		PerBinType == "PerBinLinearRegression",
		modelType = "LinearRegression";,
		(*
		PerBinType == "PerBinWeightedLinearRegression",
		modelType = "WeightedLinearRegression";,*)
		
		PerBinType == "PerBinNearestNeighbors",
		modelType = "NearestNeighbors";,
		
		PerBinType == "PerBinNeuralNetwork",
		modelType = "NeuralNetwork";,
		
		True,
		Assert[True,"Per bin model type error"];
	];
	
	(* Organize observations *)
	predictors = Length[X[[1]]] - 1;
	
	observedBins 		= Union[X[[All,  predictors+1]]];
	observedPredictors 	= Union[X[[All, 1;;predictors]]];
	
	XYall = Append[X//Transpose, Flatten[Y]] // Transpose;
	XYpb = Reap[(* XYpb will be a list of Arrays. Each array for each bin. Each array is a list of points including {predictors, bin, y} *)
		Do[
			XYcrt = Select[XYall,#[[1;;predictors]] == p &]; 
			Do[(* If some bins are missing, retrive them using the previous bin (or 0 if missing) *)
				xy=SelectFirst[XYcrt,#[[predictors+1]] == b &];
				
				If[xy === Missing["NotFound"],
					bestB = Select[XYcrt[[All,predictors+1]] , # <= b& ];
					If[Length[bestB]==0,
						xy =Flatten[{p,b, 0 }];
						,
						bestB = Max[bestB];
						xy =Flatten[{p,b, SelectFirst[XYcrt,#[[predictors+1]] == bestB &][[predictors+2]] }];
					];
				];
				
				Sow[xy , b];
				
			,{b,observedBins}];
		,{p,observedPredictors}];
	,observedBins][[2]];
	
	XYpb = Association[Reap[(* XYpb will be an Association from the bin to the couple {X, Y} *)
				Do[
					Sow[p[[1,predictors+1]] -> {p[[All,1;;predictors]], p[[All,predictors+2]]} ];
				,{p,Flatten[XYpb,1]}];
			][[2,1]]
	];
	
	(* Fit the models *)
	pbModels = Association[Reap[
			Do[
				Sow[b-> Predict[XYpb[b][[1]] -> Flatten[XYpb[b][[2]]], Method -> modelTypeProperties[modelType]  ] ];
			,{b,observedBins}];
		][[2,1]]
	];
	(*
	res = Function[x,Evaluate[
			(* x shall a single vector or a list of vector. Each vector of the form: {predictors,bin} *)
			
			(* Declaring function for solving the vector case. *)
			vCase[xcrt_] := Module[{localBestBin},
				localBestBin = xcrt[[predictors+1]];
				If[!MemberQ[observedBins,localBestBin],
					localBestBin = Max[ Select[observedBins,#<=localBestBin &]];
				];
				
				If[localBestBin == -Infinity,
					0.
					,
					
					pbModels[localBestBin][xcrt[[1;;predictors]]]
				]
			];
			
			If[MatrixQ[x],
				vCase[#] & /@ x
				,
				vCase[x]
			]
		]
	];*)
	
	res = Function[x,Module[{(*localPerBinModels=Evaluate[pbModels],localObservedBins = Evaluate[observedBins],
							 localPredictors = Evaluate[predictors],*)
							vCase},
			(* x shall a single vector or a list of vector. Each vector of the form: {predictors,bin} *)
			
			(* Declaring function for solving the vector case. *)
			vCase[xcrt_] := Module[{localBestBin},
				localBestBin = xcrt[[localPredictors+1]];
				If[!MemberQ[localObservedBins,localBestBin],
					localBestBin = Max[ Select[localObservedBins,#<=localBestBin &]];
				];
				
				If[localBestBin == -Infinity,
					0.
					,
					
					localPerBinModels[localBestBin][xcrt[[1;;localPredictors]]]
				]
			];
			
			If[MatrixQ[x],
				vCase[#] & /@ x
				,
				vCase[x]
			]
		]
	];
	
	res = res /. localPerBinModels -> pbModels;
	res = res /. localObservedBins -> observedBins;
	res = res /. localPredictors -> predictors;
	
	
	
	Return[res];
];

End[] (* End Private Context *)

EndPackage[]