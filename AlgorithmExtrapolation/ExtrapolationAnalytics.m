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

BeginPackage["ExtrapolationAnalytics`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["AlgorithmExtrapolation`"]
Needs["SingleThreadExtrapolation`"]
Needs["ComputerArithmetic`"]
Needs["ExaBoundsGeneric`"]
Needs["Support`"]

ExtrapolationDiagnosticRE::usage =
  "ExtrapolationDiagnosticRE[model_,testAlgDataAssociation_,metric,
  \"ClusteringData\"->clusteringData]"
  
(* R2 is measured in reference to the squared error from an observation mean, to obtain
 	this mean we need at least 2 algorithms in testAlgDataAssociation When passing a 
 	multi-threaded model, the clustering structure in reference to the
	testAlgDataAssociation is needed to identify what thread should be predicted with 
	what model.
*)
ExtrapolationDiagnosticR2::usage =
  "ExtrapolationDiagnosticR2[model_,testAlgDataAssociation_,metric,
  \"ClusteringData\"->clusteringData]"
  
(* Mean does not handle the NaN values, this does. *)
ErrNaNMean::usage =
  "NaNMean[vector_]"

(*
	memX Prediction Ratio.
	Returns the number of columns (memX) in the predicted histograms that are present
	in the actual histogram relatively to the number of columns in the actual histogram.
	When passing a multi-threaded model, the clustering structure in reference to the
	testAlgDataAssociation is needed to identify what thread should be predicted with 
	what model.
*)
ExtrapolationDiagnosticMemXPR::usage =
	"ExtrapolationDiagnosticMemXPR[model_,testAlgDataAssociation_,metric,
	\"ClusteringData\"->clusteringData]"
  
  
ClearExtrapolationAnalyticsCache::usage = "ClearExtrapolationAnalyticsCache[]"
  

Begin["`Private`"] (* Begin Private Context *) 
ClearExtrapolationAnalyticsCache[] :=
Block[{},
	ClearCache[ExtrapolationDiagnosticRE]; 
	ClearCache[ExtrapolationDiagnosticR2];
	ClearCache[ExtrapolationDiagnosticMemXPR];
];

Options[ExtrapolationDiagnosticRE] 		= {"ClusteringData"->Null};
Options[ExtrapolationDiagnosticR2] 		= {"ClusteringData"->Null};
Options[ExtrapolationDiagnosticMemXPR] 	= { "ClusteringData"->Null};

(* Extrapolation Relative Error *)
(* SingleThreaded -> Table whose rows are metrics and columns are runs *)
ExtrapolationDiagnosticRE[modelParameter_,testAlgAssociationParameter_, metric : Except[_Rule] : {},OptionsPattern[]] :=
ExtrapolationDiagnosticRE[modelParameter,testAlgAssociationParameter, metric] = 
Module[{allResults, metricList, metricType, xs,
		mask,keys, RE,clusteringData,c,testAlgDataAssociation,counterName,finalRes},
	
	clusteringData = OptionValue["ClusteringData"];
	
	If[Length[metric] == 0 && !StringQ[metric],
		metric = modelParameter[[1,All,"metricName"]]
	];
	
	If[!StringQ[metric],
		finalRes = ExtrapolationDiagnosticRE[	modelParameter,testAlgAssociationParameter,
											#,"ClusteringData"->clusteringData] & /@ metric; 
		Return[finalRes];
	];
	
	RE[key_,model_] := Module[{a, predictions, actuals, result,
						Xact, Yact, Xall, Ypred, Xpred,i, predPos,actPos},
						
		predictions = ExtrapolateSingleThreadAlgorithm[model, key][[1(* The first thread *)]];
		actuals = testAlgDataAssociation[key][[1,1(* The first thread *)]];
		metricList = Transpose[{model[[All,"metricName"]] , model[[All,"metricType"]]}];
		
		
		If[!(MemberQ[metricList[[All,1]], metric ]),
			Print["The metric "<> metric <> " is not predicted by the extrapolation model"];
			Return[{}];
		];
			
		metricType = GetKeyValue[metricList, metric];
		
		(* memFunction has a list of predictions, one for each evaluated memory distance *)
		If[metricType == "memFunction", 
			(* Set up prediction evaluation *)
			Yact = GetKeyValue[actuals,metric][[All,2]];
			Xact = GetKeyValue[actuals,metric][[All,1]];
			xs = Reap[Sow[Nothing]; NestWhile[(Sow[#]; #*2) &, 1, # < Max[Xact] &]][[2, 1]];
			mask = (#!=0 && MemberQ[xs,#]) & /@ Xact;
			Yact = Pick[Yact,mask];
			Xact = Pick[Xact,mask];
			Ypred = GetKeyValue[predictions,metric][[All,2]];
			Xpred = GetKeyValue[predictions,metric][[All,1]];
			mask = MemberQ[Xact,#] &/@Xpred;
			Ypred = Pick[Ypred,mask];
			Xpred = Pick[Xpred,mask];
			
			Xall = Union[Xact,Xpred];
			
			result = {Xall , {}};
				
			
			For[ i=1 , i<= Length[Xall ] , i++,
					
				If[MemberQ[Xpred, Xall[[i]] ] && MemberQ[Xact, Xall[[i]] ],
						
					predPos = Position[Xpred, Xall[[i]] ][[1,1]];
					actPos = Position[Xact, Xall[[i]] ][[1,1]];
					AppendTo[result[[2]], 
							If[Yact[[actPos]]>=0.001,
								Abs[(Max[{Ypred[[predPos]],0}] - Yact[[actPos]])(*/Yact[[actPos]]*)],
								NaN
							]
						];
					,
					AppendTo[result[[2]], NaN];
				];
			];
			result[[2]] = Flatten[result[[2]] ];
			result = Transpose[result];
			
			
			,(* Not a memory function. *)
			If[metricType == "vinstrPercentage" || metricType == "vnumeric",
				a=	VMixGetData[GetKeyValue[actuals,metric]] ;
				If[a == 0,
					result = NaN;
					,
					result = (VMixGetData[GetKeyValue[predictions,metric]]  - a) / a;
				];
				
				,
				
				a=GetKeyValue[actuals,metric] ;
				If[a == 0,
					result = NaN;
					,
					result = (GetKeyValue[predictions,metric]  - a) / a;
				];
			];
		];
			
			
		{metric, N[result]}
	];
	
	keys = GetScalingConfigurations[testAlgAssociationParameter];
	Which[
		SingleThreadModelQ[modelParameter],
			
		testAlgDataAssociation = testAlgAssociationParameter;
		Return[RE[#,modelParameter] & /@ keys ];
		,
		
		MultiThreadModelQ[modelParameter] && clusteringData=!=Null,
		
		allResults = Reap[Do[
			testAlgDataAssociation = SingleThreadRepresentingCluster[testAlgAssociationParameter,clusteringData,c];
			
			allResults = (RE[#,modelParameter[[c]]] & /@ keys);
			allResults = allResults[[All,2]];
			allResults = Prepend[allResults,metric];
			
			Sow[allResults ];
		,{c,Range[Length[modelParameter]]}]][[2,1]];
		Return[allResults];
		,
		
		True,
		ExtrapolationDiagnosticRE::constraint = "The model is wrong or some data is not available.";
		Message[ExtrapolationDiagnosticRE::constraint];
	];
];

(* Coefficient of determination measured on the list of algorithms (take care, there must be at least 2 algorithms) *)
ExtrapolationDiagnosticR2[model_,testAlgDataAssociation_, metric : Except[_Rule] : {},OptionsPattern[]] :=
ExtrapolationDiagnosticR2[model,testAlgDataAssociation, metric] = 
Module[{result, actualList,actualVals, predictionList, predictionVals, metricList, metricType,
		Xact, Yact, Xall, Ypred, Xpred, i,j, predPos,actPos,m, keys,
		SStot, SSres,constant,clusteringData,
		representativeAlgAssociation,c},
	clusteringData = OptionValue["ClusteringData"];
		
	keys = GetScalingConfigurations[testAlgDataAssociation];
	Which[
		StringQ[metric],
			(* Check wheather there are at least 2 algorithms. *)
			If[Length[keys]<2 ,
				Return[NaN];
			];
			
			(* Verify the available metrics *)
			metricList = Transpose[{model[[All,"metricName"]] , model[[All,"metricType"]]}];
			If[!(MemberQ[metricList[[All,1]], metric ]),
				Print["The metric "<> metric <> " is not predicted by the extrapolation model"];
				Return[{}];
			];
			metricType = GetKeyValue[metricList, metric];
			
			(* Extrapolate *)
			actualList = testAlgDataAssociation[#][[1,1(* The first thread *)]] & /@ keys;
			predictionList = ExtrapolateSingleThreadAlgorithm[model, #][[1(* The first thread *)]] & /@ keys;
			
		
		
			(* Organize the predictions and observations *)
			actualVals = GetKeyValue[#, metric] & /@ actualList;
			predictionVals = GetKeyValue[#, metric] & /@ predictionList;
			
			constant = # == actualVals[[1]] & /@ actualVals;  
			If[!MemberQ[constant,False],
				Return[{metric, Null}];
			];
			
			(* memFunction has a list of predictions, one for each evaluated memory distance *)
			If[metricType == "memFunction", 
				result = Reap[For[j=1 , j<= Length[keys ] , j++,(* Iterate on the available algorithm execution. *)
					(* Set up prediction evaluation *)
					Xact = actualVals[[j,All,1]];
					Xpred = predictionVals[[j,All,1]];
					Xall = Union[Xact,Xpred];
					Yact = actualVals[[j,All,2]];
					Ypred = predictionVals[[j,All,2]];
					
					
					For[ i=1 , i<= Length[Xall ] , i++,
						If[MemberQ[Xpred, Xall[[i]] ] && MemberQ[Xact, Xall[[i]] ],
							
							predPos = Position[Xpred, Xall[[i]] ][[1,1]];
							actPos = Position[Xact, Xall[[i]] ][[1,1]];
							Sow[{Ypred[[predPos]], Yact[[actPos]] }];
						];
					];
					
				];][[2,1]];
				actualVals = Transpose[result][[2]];
				predictionVals = Transpose[result][[1]];
			];
			
			If[metricType == "vinstrPercentage" || metricType == "vnumeric",
				actualVals = Flatten[ VMixFlatten[#]&/@actualVals ];
				predictionVals = Flatten[ VMixFlatten[#]&/@predictionVals ];
			];
			
			SStot = Sum[( N[Mean[actualVals]] - N[actualVals[[i]]])^2,		{i,Length[actualVals]}];
			SSres = Sum[( N[Flatten[predictionVals][[i]]] - N[actualVals[[i]]])^2,	{i,Length[actualVals]}];
			result = (SStot-SSres) / SStot;	
			
			
			Return[{metric, result}];
		 
		,
		Length[metric] == 0,(* TODO: multithread support *)
			m = model[[All,"metricName"]];
			Return[ExtrapolationDiagnosticR2[model,testAlgDataAssociation,m]];
		,
		
		Length[metric] >= 1,
			Return[ExtrapolationDiagnosticR2[model,testAlgDataAssociation,#] & /@ metric ];
	];
];

(* MemX Prediction Ratio reports the fraction of the MemX axis that appears both in the prediction and in the actual measurements *)
ExtrapolationDiagnosticMemXPR[model_,testAlgDataAssociation_, metric : Except[_Rule] : {},OptionsPattern[]] :=
ExtrapolationDiagnosticMemXPR[model,testAlgDataAssociation, metric] = 
Module[{result, actuals, predictions, metricList, metricType, Xact, Xpred,m,keys
	,MemXPR,clusteringData,representativeAlgAssociation},
	clusteringData = OptionValue["ClusteringData"];
	
	MemXPR[key_] := Module[{},
		Which[
			StringQ[metric],
			predictions = ExtrapolateSingleThreadAlgorithm[model, key][[1(* The first thread *)]];
			actuals = testAlgDataAssociation[key][[1,1(* The first thread *)]];
			metricList = Transpose[{model[[All,"metricName"]] , model[[All,"metricType"]]}];
		
		
			If[!(MemberQ[metricList[[All,1]], metric ]),
				Print["The metric " <> metric <> " is not predicted by the extrapolation model"];
				Return[{}];
			];
			
			metricType = GetKeyValue[metricList, metric];
			
			If[metricType == "memFunction", (* memFunction has a list of predictions, one for each evaluated memory distance *)
				(* Set up prediction evaluation *)
				Xact = GetKeyValue[actuals,metric][[All,1]];
				Xpred = GetKeyValue[predictions,metric][[All,1]];
	
				
				result = N[Length[Intersection[Xact,Xpred]]/Length[Xact]];
				,
				result = "NA";	
			];
			
			
			{metric, result}
			 
			,
			Length[metric] == 0,
			m = model[[All,"metricName"]];
			ExtrapolationDiagnosticRE[model,testAlgDataAssociation,m]
			,
			
			Length[metric] >= 1,
			ExtrapolationDiagnosticRE[model,testAlgDataAssociation,#] & /@ metric 
		]
	];
	
	(* TODO: multithread support *)
	keys = GetScalingConfigurations[testAlgDataAssociation];
	Return[MemXPR[#] & /@ keys ];
];


ErrNaNMean[vector_] := Module[{noNaNv,v},
	If[Length[vector]>=1,
		If[Or @@ (NumericQ[#] & /@ vector),
			noNaNv = Reap[Do[ If[ NumericQ[v], Sow[v] ] ,{v,vector}] ][[2,1]];
			(*Return[Mean[Abs[noNaNv]]];
			Return[HarmonicMean[Abs[noNaNv]]];
			Return[GeometricMean[Abs[noNaNv]]];*)
			Return[ExtrapolationAlgorithmErrorAggregation[Abs[noNaNv]]];
		];
	];
	Return[Null];
];

End[] (* End Private Context *)

EndPackage[]