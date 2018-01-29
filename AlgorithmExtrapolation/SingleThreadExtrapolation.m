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

BeginPackage["SingleThreadExtrapolation`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["ExtrAxGUI`"]
Needs["MultiThreadExtrapolation`"]
Needs["ComputerArithmetic`"]
Needs["ExtrapolationAnalytics`"]
Needs["ExaBoundsGeneric`"]
Needs["PerBinModels`"]
Needs["InversePerBinModels`"]
Needs["Support`"]
Needs["InstructionMix`"]

FitSingleThreadExtrapolationModel::usage =
  "FitSingleThreadExtrapolationModel[algAssociation_, \"ExtrapolationList\"->extrapolationList,
  										 \"Verbose\"->verbose]"

ExtrapolateSingleThreadAlgorithm::usage =
  "ExtrapolateSingleThreadAlgorithm[algorithmExtrapolationModel_, scalingParametersAssociation_]"
  
SingleThreadModelQ::usage =
	"SingleThreadedModelQ[model_]"
  
FitSingleThreadModelGUI::usage =
  "FitSingleThreadModelGUI[algoModel_,ExtrAxTrainAlgInfo_,ExtrAxTestAlgInfo_,targetApplicationName_,
						extrapolationList_, mainComplexities_, selTermsNThreads_]"

ExtrapolationListControlGUI::usage = 
	"ExtrapolationListControlGUI[extrapolationList_,mainComplexities_,trainAlgAssociation_]"
 

ExtrapolationAlgorithmErrorAggregation::usage =
	"ExtrapolationAlgorithmErrorAggregation[errorVector_]" 
	
GetExtrapolationMetricTypeOptions::usage =
	"GetExtrapolationMetricTypeOptions[]" 

GetExtrapolationMetricType2ModelTypes::usage =
	"GetExtrapolationMetricType2ModelTypes[]"
	
DefaultExtrapolationList::usage =
	"DefaultExtrapolationList[]"
	
DefaultComplexities::usage =
	"DefaultComplexities[]"
	
GetXNames::usage =
	"GetXNames[algAssociation_]"
	
ClearSingleThreadExtrapolationCache::usage = "ClearSingleThreadExtrapolationCache[]"

FormatXY::usage = "FormatXY[XY_,symbols_,terms_]"

Begin["`Private`"] (* Begin Private Context *)
(* General interface for algoModel fitting and feature extrapolation. *)

ClearSingleThreadExtrapolationCache[] :=
Block[{},
	ClearCache[ExtrapolateSingleThreadAlgorithm];
	(*ClearCache[TryReduce];*)
	(*ClearCache[MyCachedSimplify];*)(*TODO recache this by managing it as in TimedCacheReduce*) 
	ClearCache[CachedReduce];
	ClearCache[GenerateConstraint];
	(*ClearCache[HomoscedasticWhiteTest];*)
];

SingleThreadModelQ[model_] := Module[{
	requiredData={"metricName","metricName","metricType","predictorNames","predictorTerms","model","modelType"}},
	
	If[ !ListQ[model], Return[False] ];
	
	If[ !(And@@ (AssociationQ[#] & /@ model)), Return[False] ];
	
	If[ !(And@@ ((Complement[requiredData,Keys[#]] === {}) & /@ model)), 
		Return[False]
	];
	
	Return[True];
];


(*
	For the user the order in this list does not matter.
	When editing this list, note that InstructionMix.m migth also need to be reviewed.
*)
metricTypeOptions = Association[
	"LSys" -> {"numeric"}, (* The first must be the instr. count required for instrPercentage *)
	"F0mem" -> {"vinstrPercentage","vnumeric"},
	"F0addr" -> {"vinstrPercentage","vnumeric"},
	"F0int" -> {"vinstrPercentage","vnumeric"},
	"F0intOnly" -> {"vinstrPercentage","vnumeric"},
  	"F0intmul" -> {"vinstrPercentage","vnumeric"},
  	"F0intdiv" -> {"vinstrPercentage","vnumeric"},
  	"F0fp" -> {"vinstrPercentage","vnumeric"},
  	(*"F0DPFP" -> {"vinstrPercentage","vnumeric"},*)
  	"F0fpmul" -> {"vinstrPercentage","vnumeric"},
  	"F0fpdiv" -> {"vinstrPercentage","vnumeric"},
	"F0control" -> {"vinstrPercentage","vnumeric"},
	"F0load" -> {"vinstrPercentage","vnumeric"},
	"F0store" -> {"vinstrPercentage","vnumeric"},
	"F0vector" -> {"vinstrPercentage","vnumeric"},
	"F0other" -> {"vinstrPercentage","vnumeric"},
	"D0ireuse" -> {"memFunction"},
	"D0dreuse" -> {"memFunction"},
	"D0dreuse128" -> {"memFunction"},
	"F0bestMispredict" -> {"numeric"(*,"instrPercentage"*)}, 
	"H0branch" -> {"numeric"},
	(*"ALP0" -> {"numeric","instrPercentage"},*)
	"ILP0" -> {"numeric","instrPercentage"},
	"ILP0mem" -> {"numeric","instrPercentage"},
	"ILP0int" -> {"numeric","instrPercentage"},
	"ILP0control" -> {"numeric","instrPercentage"},
	"ILP0fp" -> {"numeric","instrPercentage"},
	"F0regreads" -> {"numeric","instrPercentage"},
	"F0regwrites" -> {"numeric","instrPercentage"},
  	"MPIinstructions" -> {"numeric"},
  	"MPIisend" -> {"numeric"},
	"MPIirecv" -> {"numeric"},
  	"MPIsend" -> {"numeric"},
	"MPIrecv" -> {"numeric"},
  	"MPIbcast" -> {"numeric"},
	"MPIbarrier" -> {"numeric"},
	"MPIreduce" -> {"numeric"},
	"MPIsentBytes" -> {"numeric"},
	"MPIrecvBytes" -> {"numeric"},
	"MPIsentMessages" -> {"numeric"},
	"MPIrecvMessages" -> {"numeric"},
	"OpenMPtotal" -> {"numeric"},
	"OpenMPsynchronization" -> {"numeric"},
	"OpenMPatomic" -> {"numeric"},
	"OpenMPstartupShutdown" -> {"numeric"},
	"OpenMPparallel" -> {"numeric"},
	"OpenMPthreadInfo" -> {"numeric"},
	"OpenMPworkSharing" -> {"numeric"},
	"OpenMPthreadPrivateDataSupport" -> {"numeric"},
	"OpenMPtaskingSupport" -> {"numeric"},
	"OpenMPothers" -> {"numeric"},
	"MPIcommVector" -> {"notSupported"},(*{"commVector"},*)(* not implemented *)
	"NThreads" -> {"numeric"},
	(*
		LibraryCalls is itself e list of numeric metrics. Extrapoation is not supported,
		the atomic numeric metric should be listed separately
	*)
	"LibraryCalls" -> {"notSupported"}
];

GetExtrapolationMetricTypeOptions[] := metricTypeOptions;

perBinModelTypeDefinition = {"PerBinLinearRegression","PerBinNeuralNetwork"(*,"PerBinWeightedLinearRegression"*)};
inversePerBinModelTypeDefinition = {"InversePerBinModel"};
metricType2ModelTypes = Association[
	"numeric" -> {"LinearRegression","WeightedLinearRegression","NearestNeighbors","NeuralNetwork","RandomForest"},
	"instrPercentage" -> {"LinearRegression","WeightedLinearRegression"},
	"vnumeric" -> {"LinearRegression","WeightedLinearRegression","NearestNeighbors","NeuralNetwork","RandomForest"},
	"vinstrPercentage" -> {"LinearRegression","WeightedLinearRegression"},
	"memFunction" -> {"LinearRegression","WeightedLinearRegression","NearestNeighbors","NeuralNetwork","RandomForest",
						"PerBinLinearRegression","PerBinNeuralNetwork"(*,
						"InversePerBinModel"*)},
	"commVector" -> {}
];
GetExtrapolationMetricType2ModelTypes[] := metricType2ModelTypes;

defaultModelTypeProperties = Association[
"LinearRegression" -> {"LinearRegression","L2Regularization"->0},
"NearestNeighbors" -> "NearestNeighbors" ,
"NeuralNetwork" -> "NeuralNetwork",
"RandomForest" -> "RandomForest"
(*"NeuralNetwork" -> {"NeuralNetwork", 
					"CrossValidation" -> True, "EarlyStopping" -> False,
					"OptimizationMethod" -> Automatic, "WeightInitializationMethod" -> Automatic,
					"HiddenLayers" -> {{3,"Tanh"}}}*)
					
(*"RandomForest" -> "RandomForest",*)
];

WLRfunctions = {(*(1/Sqrt[#]&),*)(1/#&),(1/(#^2)&),(1&)};

(* Return the pvalue for the F-test. Homoscedasticity refused if p < 0.05 . *)
FTest[numerator_,nDegree_,denumerator_,dDegree_] := Block[{ratio,fDistribution,pValue},
	ratio = N[numerator/ denumerator];
	
	fDistribution = FRatioDistribution[nDegree,dDegree];
	
	pValue = 1 - CDF[fDistribution,ratio];
	
	Return[pValue];
];

(* Return the pvalue for the LM-test. Homoscedasticity refused if p < 0.05 . *)
LMTest[R2_,fDegree_,observations_] := Block[{chi2Distribution,pValue},
	
	chi2Distribution = ChiSquareDistribution[fDegree];
	
	pValue = 1 - CDF[chi2Distribution,observations * R2];
	
	Return[pValue];
];

HomoscedasticWhiteTest[X_, Y_] := (*HomoscedasticWhiteTest[X, Y] =*)
Block[{samples,
	nDegree,dDegree,SSM,SSE,alpha=0.05,fittedModel,Yout,residuals,
	squaredResidualTrend(* no trend if homoscedastic *), squaredResiduals,pVal,yTerms},
	
	
	samples = Length[X];
	
	
	Quiet[
		fittedModel =(*Generalized*)LinearModelFit[	
											Append[X // Transpose, Y] // Transpose ,
											Symbol["x" <> ToString[#]] & /@ Range[Length[X[[1]]]  ], 
											Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]  ]  (*,
											ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
										];
	];
										
	residuals = fittedModel["FitResiduals"]^2;
	Yout = fittedModel["PredictedResponse"];
	If[Min[Yout] != Max[Yout],
		Yout = {Yout,Yout^2};
		yTerms = {Symbol["yHat"] , Symbol["yHatSquared"]};
		,
		Yout = {Yout};
		yTerms = {Symbol["yHat"]};
	];
	
	nDegree = 2;
	dDegree = Length[Y] - nDegree;
	
	squaredResiduals = residuals^2;
		
	Quiet[
		squaredResidualTrend = LinearModelFit[	
										Append[Yout , squaredResiduals] // Transpose ,
										yTerms,
										yTerms  (*,
										ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
									];
	];
	SSM = Total[#^2 & /@ (squaredResidualTrend["PredictedResponse"] - Mean[squaredResiduals])];
	SSE = Total[#^2 & /@ (squaredResidualTrend["FitResiduals"])];
	
	If[Min[Flatten[Yout]]!=Max[Flatten[Yout]],
		(*Sow[FTest[SSM/nDegree,nDegree,SSE/dDegree,dDegree]];*)
		Quiet[Check[
			pVal = LMTest[squaredResidualTrend["RSquared"],nDegree,nDegree+nDegree];
			,
			pVal = 1;
		]];
		,
		pVal=1;
	];
	
	
	If[pVal <= alpha,(* White test for heteroscedasticity *)
		Return[True];
	];
	Return[False];
];

(* Apply crossvalidation on the Mean Squared Error. *)
SelectWeightingFunctionCrossvalidationBased[ X_, Y_ ] :=
Block[{Xcpy,Ycpy,samples,mask,rules,partialMask,weightingFun,weightDomain,res,
	SSE,scores,fittedModel, dummyPredNames,posMonotonConstraints},
	
	If[HomoscedasticWhiteTest[X, Y],(* White test for heteroscedasticity *)
		Return[1&];
	];
	
	samples = Length[X];
	mask = True & /@ Range[samples];
	Do[
		weightDomain = FunctionDomain[weightingFun[y],y];
							
		rules = ToRules[ y == N[#] ] & /@ Y;
		partialMask = (weightDomain /. #) & /@ rules;
		mask = (And@@ #) &/@ ({partialMask,mask} // Transpose);
	,{weightingFun,WLRfunctions}];
	
	Xcpy = Pick[X,mask];
	Ycpy = {#}&/@Pick[Y,mask];
	
	If[Length[Ycpy] == 0, Return[1&] ];
	
	dummyPredNames = "x"<>ToString[#] & /@ Range[Length[Xcpy[[1]] ] ];
	posMonotonConstraints = Flatten[{False,({#,False,True}&/@dummyPredNames)},1]; 
	(* StringJoin[Flatten[{"{False,",StringRiffle[()&/@dummyPredNames,","]}]]; *)
	
	scores = Flatten[Reap[Do[
			Sow[
				TenFoldCrossvalidation[Xcpy,dummyPredNames,Ycpy,Symbol[# ] & /@ dummyPredNames, "WeightedLinearRegression",
										posMonotonConstraints, {} , "WeightingFunction"->weightingFun]
			];
		,{weightingFun,WLRfunctions}];
	][[2,1]]];
	
	
	res = SelectFirst[{WLRfunctions,scores}//Transpose,#[[2]] == Min[scores]&];
	Return[res[[1]]];
];

(* Runs the White's test after compensating the residuals with 1/f[y]. *)
SelectWeightingFunctionTestBased[ X_, Y_] := (*SelectWeightingFunctionTestBased[ X, Y] =*)
Block[{Xcpy,Ycpy,samples,mask,rules,partialMask,weightingFun,weightDomain,res,
	nDegree,dDegree,SSM,SSE,scores,alpha=0.05,fittedModel,Yout,residuals,
	squaredResidualTrend(* no trend if homoscedastic *), weightedSqaredResiduals,pVal,yTerms},
	samples = Length[X];
	mask = True & /@ Range[samples];
	Do[
		weightDomain = FunctionDomain[weightingFun[y],y];
							
		rules = ToRules[ y == N[#] ] & /@ Y;
		partialMask = (weightDomain /. #) & /@ rules;
		mask = (And@@ #) &/@ ({partialMask,mask} // Transpose);
	,{weightingFun,WLRfunctions}];
	
	Xcpy = Pick[X,mask];
	Ycpy = Pick[Y,mask];
	
	Quiet[
		fittedModel =(*Generalized*)LinearModelFit[	
											Append[Xcpy // Transpose, Ycpy] // Transpose ,
											Symbol["x" <> ToString[#]] & /@ Range[Length[X[[1]]]  ], 
											Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]  ]  (*,
											ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
										];
	];
										
	residuals = fittedModel["FitResiduals"]^2;
	Yout = fittedModel["PredictedResponse"];
	If[Min[Yout] != Max[Yout],
		Yout = {Yout,Yout^2};
		yTerms = {Symbol["yHat"] , Symbol["yHatSquared"]};
		,
		Yout = {Yout};
		yTerms = {Symbol["yHat"]};
	];
	
	nDegree = 2;
	dDegree = Length[Ycpy] - nDegree;
	
	scores = Reap[Do[
		weightedSqaredResiduals = (residuals^2)/(weightingFun[#]&/@Ycpy);
		
		Quiet[
			squaredResidualTrend = LinearModelFit[	
											Append[Yout , weightedSqaredResiduals] // Transpose ,
											yTerms,
											yTerms  (*,
											ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
										];
		];
		SSM = Total[#^2 & /@ (squaredResidualTrend["PredictedResponse"] - Mean[weightedSqaredResiduals])];
		SSE = Total[#^2 & /@ (squaredResidualTrend["FitResiduals"])];
		
		If[Min[Flatten[Yout]]!=Max[Flatten[Yout]],
			(*Sow[FTest[SSM/nDegree,nDegree,SSE/dDegree,dDegree]];*)
			Quiet[Check[
				pVal = LMTest[squaredResidualTrend["RSquared"],nDegree,nDegree+nDegree];
				,
				pVal = 1;
			]];
			Sow[pVal];
			,
			Sow[1];
		];
	,{weightingFun,WLRfunctions}]][[2,1]];
	
	res = {WLRfunctions,scores}//Transpose;
	
	If[MemberQ[res[[All,1]], 1&],(* White test for heteroscedasticity *)
		If[SelectFirst[res,#[[1]] === (1&)&][[2]] <= alpha,
			Return[1&];
		];
	];
	
	If[Min[scores]>0.9,(* All compensations are very wrong... *)
		Return[1&];
	];
	
	res = SelectFirst[{WLRfunctions,scores}//Transpose,#[[2]] == Min[scores]&];
	Return[res[[1]]];
];

(* If White's does not pass, weights are computed for WLS by regressing the residual itself. *)
SelectWeightingFunctionRegressionBased[ X_, Y_] := (*SelectWeightingFunctionRegressionBased[ X, Y] =*)
Block[{samples,
	nDegree,dDegree,SSM,SSE,alpha=0.05,fittedModel,Yout,residuals,squaredResiduals,
	squaredResidualTrend(* no trend if homoscedastic *), pVal,yTerms},
	samples = Length[X];
	
	
	Quiet[
		fittedModel =(*Generalized*)LinearModelFit[	
											Append[X // Transpose, Y] // Transpose ,
											Symbol["x" <> ToString[#]] & /@ Range[Length[X[[1]]]  ], 
											Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]  ]  (*,
											ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
										];
	];
										
	residuals = fittedModel["FitResiduals"]^2;
	Yout = fittedModel["PredictedResponse"];
	If[Min[Yout] != Max[Yout],
		Yout = {Yout,Yout^2};
		yTerms = {Symbol["yHat"] , Symbol["yHatSquared"]};
		,
		Yout = {Yout};
		yTerms = {Symbol["yHat"]};
	];
	
	nDegree = 2;
	dDegree = Length[Y] - nDegree;
	
	squaredResiduals = residuals^2;
		
	Quiet[
		squaredResidualTrend = LinearModelFit[	
										Append[Yout , squaredResiduals] // Transpose ,
										yTerms,
										yTerms  (*,
										ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
									];
	];
	SSM = Total[#^2 & /@ (squaredResidualTrend["PredictedResponse"] - Mean[squaredResiduals])];
	SSE = Total[#^2 & /@ (squaredResidualTrend["FitResiduals"])];
	
	If[Min[Flatten[Yout]]!=Max[Flatten[Yout]],
		(*Sow[FTest[SSM/nDegree,nDegree,SSE/dDegree,dDegree]];*)
		Quiet[Check[
			pVal = LMTest[squaredResidualTrend["RSquared"],nDegree,nDegree+nDegree];
			,
			pVal = 1;
		]];
		,
		pVal=1;
	];
	
	
	If[pVal <= alpha,(* White test for heteroscedasticity *)
		Return[1&];
	];
	
	Return[ If[#!=0,1/#,1] & /@ squaredResidualTrend["PredictedResponse"] ];
];

(* This function is taken from almeida2002.
	But it is wrong since the selection is based on their own personal reasons.
	Some statistical tests whould be implemented.
	However, the mechanism to cope with the right heteroscedasticity is ok,
	just selection criteria is needed. *)
SelectWeightingFunction[ X_, Y_, constraints_] := (*SelectWeightingFunction[ X, Y, constraints] =*)
Module[{weightingFun,weightDomain,mask,partialMask,rules,Xcpy,Ycpy,samples,
	scores,relResiduals,form,res},
	
	samples = Length[X];
	mask = True & /@ Range[samples];
	Do[
		weightDomain = FunctionDomain[weightingFun[y],y];
							
		rules = ToRules[ y == N[#] ] & /@ Y;
		partialMask = (weightDomain /. #) & /@ rules;
		mask = (And@@ #) &/@ ({partialMask,mask} // Transpose);
	,{weightingFun,WLRfunctions}];
	
	Xcpy = Pick[X,mask];
	Ycpy = Pick[Y,mask];
	
	scores = Reap[Do[
		
		Quiet[(* The quiet here removes annoying comments when X is constant... *)
			form = (*Generalized*)LinearModelFit[	Append[Xcpy // Transpose, Ycpy] // Transpose ,
													Symbol["x" <> ToString[#]] & /@ Range[Length[X[[1]]]  ], 
													Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]  ]  (*,
													ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
													, Weights -> (weightingFun[#2] &)];
		];
		relResiduals = Abs[form["FitResiduals"]/form["Response"]];
		
		Sow[Total[relResiduals]];
	,{weightingFun,WLRfunctions}]][[2,1]];
	
	res = SelectFirst[{WLRfunctions,scores}//Transpose,#[[2]] == Min[scores]&];
	Return[res[[1]]];
];

TrainingFunction::NAN = "The predictions are not a number.";
Options[TrainingFunction] = {"WeightingFunction"-> Automatic}
TrainingFunction[X_,Y_,modelType_, coefficientConstraintsLRFormat_(*:{{},{True}}*), memMask_,OptionsPattern[]] :=
Module[{form,constraints,coefficients,coeffValues,res,ok,(* following vars NearestNeighbor for memXFunctions *)
	subModels,innerModels,outerModel,subX,i,scalarModel,
	weightingFun,weightDomain,mask,rules,Xcpy,Ycpy,
	aic,bic,rSquared,pValues, meanX,meanY,stdX,stdY},
	Quiet[
		Which[
			modelType == "WeightedLinearRegression" || modelType == "LinearRegression",
			Quiet[Check[(* Try to train the model and check if it fits the monotonicity constraints. If not, return a mean model. *)
						If[Max[Y]!=Min[Y],
							If[modelType == "WeightedLinearRegression",
								weightingFun = OptionValue["WeightingFunction"];
								If[weightingFun === Automatic,
									weightingFun = SelectWeightingFunctionCrossvalidationBased[ X, Y];
								];
								
								If[ListQ[weightingFun],
									weightDomain = True;(* This is to support actual weights rather than weighting functions.*)
									,
									weightDomain = FunctionDomain[weightingFun[y],y];
								];
								
								rules = ToRules[ y == N[#] ] & /@ Y;
								mask = (weightDomain /. #) & /@ rules;
								Xcpy = Pick[X,mask];
								Ycpy = Pick[Y,mask];
								If[Length[Ycpy]>1 && (Max[Ycpy]!=Min[Ycpy]),
									form = (*Generalized*)LinearModelFit[	Append[Xcpy // Transpose, Ycpy] // Transpose ,
															Symbol["x" <> ToString[#]] & /@ Range[Length[Xcpy[[1]]]  ], 
															Symbol["x" <> ToString[#]] & /@  Range[Length[Xcpy[[1]]]  ]  (*,
															ExponentialFamily -> "Gaussian" , LinkFunction -> Identity *)
															, Weights -> If[ListQ[weightingFun],weightingFun,(weightingFun[#2] &)]];
									,
									form = Function[x,Which[x==="AIC",Infinity,x==="BIC",Infinity,x==="ParameterPValues",pvals,True,meanVal]];
									form = form /. meanVal -> N[Mean[Y]];
									form = form /. pvals ->  (1 & /@Range[Length[X[[1]]]+1]);
								];
								
								,
								form = (*Generalized*)LinearModelFit[	Append[X // Transpose, Y] // Transpose ,
														Symbol["x" <> ToString[#]] & /@ Range[Length[X[[1]]]  ], 
														Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]  ]  (*,
														ExponentialFamily -> "Gaussian" , LinkFunction -> Identity*)];
							];
							
							(* Constraint check *)
							coefficients = coefficientConstraintsLRFormat[[1]];
							coeffValues = Flatten[{Coefficient[ form["BestFit"],  Symbol["x" <> ToString[#]] & /@  Range[Length[X[[1]]]]],
													form@@ (0 &/@ X[[1]]) }];
							constraints = coefficientConstraintsLRFormat[[2]];
							ok = And @@ (constraints /. ToRules[coefficients == coeffValues]);
							(*ok = Resolve[And @@ (constraints /. ToRules[coefficients == coeffValues])];*)
							,
							
							ok =False;
						];
						If[!ok,
							TrainingFunction::constraint = "constraints not met";
							Message[TrainingFunction::constraint];
						];
	
						
						,
						res = Function[x1,Which[x1==="AIC",Infinity,x1==="BIC",Infinity,x1==="RSquared",0,x1==="ParameterPValues",pvals,True,meanVal]];
						res = res /. meanVal -> N[Mean[Y]];
						res = res /. pvals -> (1&/@Range[Length[X[[1]]]+1]);
						Return[res];
			]];
			
			aic = Quiet[Check[(* some badly poorly posed regressions (e.g. 0 dispersions) return error *)
					form["AIC"]
					,
					Infinity
				]];
			bic = Quiet[Check[(* some badly poorly posed regressions (e.g. 0 dispersions) return error *)
					form["BIC"]
					,
					Infinity
				]];
			rSquared = Quiet[Check[(* some badly poorly posed regressions (e.g. 0 dispersions) return error *)
					form["RSquared"]
					,
					-Infinity
				]];
				
			pValues = form["ParameterPValues"];
				
			res = Function[x,
					Which[
						x === "AIC",
						localAIC
						,
						
						x === "BIC",
						localBIC
						,
						
						x === "RSquared",
						localRSquared
						,
						
						x === "ParameterPValues",
						localPValues
						,					
						
						True,
						If[Length[x]==1,
							localModel @@ x[[1]]
							,
							(localModel @@ #) & /@ x
						]
				]
			];
			res = res /. localModel -> form;
			res = res /. localAIC -> aic;
			res = res /. localBIC -> bic;
			res = res /. localRSquared -> rSquared;
			res = res /. localPValues -> pValues;
			
			Return[res];
			
			
			,
			MemberQ[perBinModelTypeDefinition,modelType], (* Per bin models, for memFunction *)
			Return[PerBinModel[X,Y,modelType,defaultModelTypeProperties]];
			
			,
			MemberQ[inversePerBinModelTypeDefinition,modelType], (* Inverse per bin models, for memFunction *)
			Return[InversePerBinModel[X,Y,modelType,defaultModelTypeProperties]];
			
			
			,
			modelType == "NearestNeighbors" && memMask =!= {} && (Or@@memMask != And@@memMask), (* Take the memFunction from the nearest observation *)
			
			If[And[AnyTrue[memMask, # &], AnyTrue[memMask, ! # &]],(* We have some variables that depend on the memory and some that do not *)
				subX = Union[Pick[#, (Not[#] & /@ memMask)] & /@ X];(* observations *)
				subModels = Flatten[Reap[
						For[i=1,i<=Length[X],i++,
							Sow[Pick[X[[i]],memMask] -> Y[[i]], {Pick[X[[i]],(Not[#]&/@memMask)]} ];
						];
					,subX][[2]],1];
				innerModels = Nearest[#] & /@ subModels;
				outerModel = Nearest[(subX[[#]] -> innerModels[[#]]) & /@ Range[Length[innerModels]]];
				
				res = Function[x,
				   scalarModel[x0_] :=
				    Module[{outX, inX},
				     outX = Pick[x0, (Not[#] & /@ localMemMask)];
				     inX = Pick[x0, localMemMask];
				     Quiet[
					     	((localOuterModel[outX][[1]])[inX])[[1]]
					     	,
					     	NearestFunction::neard
					     	(* In Mathematica 11.0, when a symbol is passed rather than a number,
					     		then this error would be shown. Symbols are preliminary passed to
					     		this function when plotting the results.
					     	*)
				     	]
				     ];
				   scalarModel[#] & /@ x];
				res = res /. localMemMask -> memMask;
				res = res /. localOuterModel -> outerModel;
				,
				(* Either all variables depend on the memory or none of them *)
				outerModel = Nearest[(#[[1]] -> #[[2]]) & /@ ({X, Y} // Transpose)];
				res = Function[x,
					scalarModel[x0_] := Quiet[
						localOuterModel[x0]
				     	,
				     	NearestFunction::neard
				     	(* In Mathematica 11.0, when a symbol is passed rather than a number,
				     		then this error would be shown. Symbols are preliminary passed to
				     		this function when plotting the results.
				     	*)
					];
					scalarModel[#] & /@ x
				];
				res = res /. localOuterModel -> outerModel;
			];
			
			Return[res];
			
			
			,
			True,(* All other model types are implemented using Predict :) *)
			If[AnyTrue[Max[#]==Min[#] & /@ Transpose[X] ] || Max[Flatten[Y]]==Min[Flatten[Y]]  ,
				(* standardization problems when constant are met, much faster to check it first *)
				res = Function[x1,Which[x1==="AIC",Infinity,x1==="BIC",Infinity,x1==="RSquared",0,x1==="ParameterPValues",pvals,True,meanVal]];
				res = res /. meanVal -> N[Mean[Y]];
				res = res /. pvals -> (0&/@Range[Length[X[[1]]]+1]);
				Return[res];
			];
				
			Quiet[Check[
					meanX = Mean[X];
					meanY = Mean[Flatten[Y]];
					stdX = StandardDeviation[X];
					stdY = StandardDeviation[Flatten[Y]];
					
					form = Predict[Standardize[X] -> Standardize[Flatten[Y]],
										Method -> defaultModelTypeProperties[modelType]];
					
					res = Function[x,
						subModels[xx_] := Module[{localRes,standardizedX,standardizedY},
							standardizedX = (x- ConstantArray[localMeanX,Length[x] ])/ConstantArray[localStdX,Length[x] ];
							
							localRes = localForm[standardizedX];
							
							If[Head[localRes] === List,
								If[Length[localRes] == 1,
									standardizedY = localRes[[1]];
									(standardizedY * localStdY) + localMeanY
									,
									standardizedY = Flatten[localRes];
									(standardizedY * localStdY) + localMeanY
								]
								,
								standardizedY = localRes;
								(standardizedY * localStdY) + localMeanY
							]
							
						];
						
						Quiet[subModels[x],{Dot::dotsh}]

					];
					res = res /. localForm -> form;
					res = res /. localMeanX -> meanX;
					res = res /. localMeanY -> meanY;
					res = res /. localStdX -> stdX;
					res = res /. localStdY -> stdY;
				,	
					(* If Dot::dotsh generated a problem, the resulting neural network might work or not. Let's test it! *)
					Quiet[Check[
						
						If[
							!NumberQ[res[ {X[[1]]} ]]||(* Try it on the very first X *)
							!NumberQ[res[ {X[[-1]]} ]]||(* Try it on the very last X *)
							!NumberQ[form[ConstantArray[0.,Length[X[[1]]]]]](* Try just the model on its central value *)
							,
							Message[TrainingFunction::NAN];
						]
						,
						(* We get here if the model generated error messages or predicted NaN at some point *)
						res = Function[x1,Which[x1==="AIC",Infinity,x1==="BIC",Infinity,x1==="RSquared",0,x1==="ParameterPValues",pvals,True,meanVal]];
						res = res /. meanVal -> N[Mean[Y]];
						res = res /. pvals -> (0&/@Range[Length[X[[1]]]+1]);
					]];
			],{
				Dot::dotsh(* NN numerical instability *),
				Standardize::scl2(* Standardize a constant *),
				Predict::wcol(* Not a numbers (due to standardization it seems) *),
				Predict::mlbddata,(* This started appearing with Mathematica 11.0.0 *)
				Nearest::neard(* This started appearing with Mathematica 11.0.0 *)
			}];
			
			Return[ res ];
		];
	,{FittedModel::varzero}];
];

(* Defines what has to be extrapolated.
	col1) Metric name (if it is a number it represents the number of memory knots to use
			for the prediction for a memFunction type declared in previous line). To change
			parameters of the memXaxis or maxMemX predictions, use this next line.
	col2) Model type -> numeric, instrPercentage, vnumeric, vinstrPercentage, memFunction
			(instrPercentage features are first ported to absolute count
			and then fitted; vinstrPercentage and vnumeric represent associations of:
			{vLength,dLength}->(instrPercentage or numeric))
			(memFunction are predicted as a function of the scaling parameters
			and of the distance. i.e. the list of predictors are the
			scalingParametersAssociation_ AND the mem-distance.)
	col3) Model type (for now, linear regression only).   
	col4) If list of Strings:
			Names of parameters to be used in model selection.
		  If list of terms (defined upon variable names but not in string form):
			Terms used during model fitting (separated by commas)
			-> Described as a list of
			expressions, functions of the data Features. Suggestion, place
			at least the order of complexity O(#). For memFunction types,
			you can express terms as function of the memory axis memX.
			By placing "." in this column, the terms are set as the dataFeature
			names.
	col5) Positivity constraint followed by predictors range and monotonicity constraints.
			The positivity constraint is either True or False, in the former case the metric
			is forced to be positive within the valid predictor ranges.
			Predictors range and monotonicity constraints is list of
			{predName,forceMonotone,{predRange}}. E.g.:
			{True, {"x0",Ture,{x0>=1}}, {"x1",False,{1<=x1<=x0}}}
			This forces the positivity constraint first. 
			Then, it forces a monotonicity constraint w.r.t. dx0 for all valid range,
			i.e. x0>=1, 1<=x1<=x0. The validity space is the cartesian product
			of the validity ranges defined for all parameters. Within the validity
			space, the monotonicity constraints are forced for all parameters
			listed with True. If a parameter appear in the list as False, its
			validity range is accounted for generating the validity space but its
			monotonicity constraint is not forced. 
			If a predictor xi does not appear in the list,
			the function is not forced to be monotone for xi and its validity
			range is set to -Inf<xi<Inf.
			NOTE: that placing 0  in the constraints might generate trouble for the Log[]
			complexity. Placing '.' -> no parameters will be subject to monotonicity
			constraints.
			NOTE: for validity range it is suggested to do not account for regions
			that are not observed during the experimental campain and are not relevant
			for the study. For example, if monotonicity shall be valid for a scaling
			parameter p whose value is expected to grow above 100 and in the training set
			it was examinated within 10 and 100, using a validity range p > 1 may lead
			to poor solutions because low values of p might be fitted poorly. Use
			p > 10 in this case.  
	col6)	True -> Fast training based on Schwarz's Bayesian Information Criterion (BIC)
			False-> Slow training based on 10 fold crossvalidate mean squared error
			(accounting for heteroschedasticity in both cases).
	col7)	Metric boundaries. Note that for instrPercentage (and vinstrPercentage) the boundaries (and the
			monotonicity constraints) are applied to the denormalized function (i.e. on the absolute
			count).
*)
defaultExtrapolationListTXT = "
  LSys								numeric					WeightedLinearRegression		.	.	False	{1,Infinity}
  F0mem								vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0addr							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0int								vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0intmul							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0intdiv							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0fp								vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0fpmul							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0fpdiv							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0control							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0load							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0store							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0vector							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  F0other							vinstrPercentage		WeightedLinearRegression		.	.	False	{0,Infinity}
  D0ireuse							memFunction				NearestNeighbors				.	.	False	{0,1}
  D0dreuse							memFunction				NearestNeighbors				.	.	False	{0,1}
  F0bestMispredict					numeric					NeuralNetwork					.	.	False	{0,Infinity}
  H0branch							numeric					NeuralNetwork					.	.	False	{0,1}
  ILP0								numeric					NeuralNetwork					.	.	False	{0,Infinity}
  ILP0mem							numeric					NeuralNetwork					.	.	False	{0,Infinity}
  ILP0int							numeric 				NeuralNetwork					.	.	False	{0,Infinity}
  ILP0control						numeric 				NeuralNetwork					.	.	False	{0,Infinity}
  ILP0fp							numeric 				NeuralNetwork					.	.	False	{0,Infinity}
  F0regreads						instrPercentage			WeightedLinearRegression		.	.	False	{0,Infinity}
  F0regwrites						instrPercentage			WeightedLinearRegression		.	.	False	{0,Infinity}
  MPIsentBytes						numeric					WeightedLinearRegression		.	.	False	{0,Infinity}
  MPIrecvBytes						numeric					WeightedLinearRegression		.	.	False	{0,Infinity}
  MPIsentMessages					numeric					WeightedLinearRegression		.	.	False	{0,Infinity}
  MPIrecvMessages					numeric					WeightedLinearRegression		.	.	False	{0,Infinity}
";
defaultExtrapolationList = {#[[1]], #[[2]], #[[3]], #[[4]], #[[5]], #[[6]], #[[7]]} & /@
									Select[ImportString[defaultExtrapolationListTXT, "Table"], Length[#] >= 1 &];
defaultMemXExtrapolationRow={};

(* Metric name for instructions count. When predict the instruction mix as absolut count, denormalize-normalize by "LSys". *)
instrCountName = "LSys";

(* Define name and user side symbol for the memory dimension. *)
(* The normMemX and lognormMemX for the knot at <percentile> are named "normMemX<percentile>", "lognormMemX<percentile>"*)
memXAxisName = "memX";
normMemXAxisName = "normMemX";
lognormMemXAxisName = "lognormMemX";
(*memXIndexes = {memXAxisName, normMemXAxisName, lognormMemXAxisName};*)
Options[GetMemXIndicesNames] = {"Unique" -> True (* returns only the models that are different from each others *)
								}
(* Given a set of knotMemModels, returns the memXIndecesNames available *)
GetMemXIndicesNames[knotMemModels_,OptionsPattern[]] := Module[{res,at,mName,modelData,saw,unique},
	unique = OptionValue["Unique"];
	(* 
		The names are taken from the model.
	*)
	saw = {};
	res = Reap[
		(* Traditional indices alway available *)
		Sow[memXAxisName];
		Sow[normMemXAxisName];
		Sow[lognormMemXAxisName];
		
		If[SingleThreadModelQ[knotMemModels],
			(* param is a knotMemModels. *)
			Do[
				mName = modelData["metricName"];
				If[StringMatchQ[mName,RegularExpression[memXAxisName<>"[0-9]+"]],
					If[!unique || MissingQ[SelectFirst[saw,modelData["model"]===#&]],
						at = StringReplace[mName,memXAxisName->""];
						
						Sow[normMemXAxisName<>at];
						Sow[lognormMemXAxisName<>at];
						
						If[unique,
							AppendTo[saw,modelData["model"]];
						];
					];
				];
			,{modelData, knotMemModels}];
		];
	][[2,1]];
	Return[res];
];

(* The input can be either a probability value or the name of a memXIndex (e.g. lognormMemX50).
	The output is the name of the memXmodel.
	NOTE: it is not working for normMemXAxisName and and lognormMemXAxisName. Only percentile models in knotMemModels *)
GetMemXModelName[memXIndex_] := Module[{},
	Which[NumberQ[memXIndex],
		Return[memXAxisName<>ToString[NumberForm[memXIndex*1000,{3,0},NumberPoint->""]]];
		
		,
		StringQ[memXIndex],
		Which[StringMatchQ[memXIndex,RegularExpression[normMemXAxisName<>"[0-9]+"]],
			Return[StringReplace[memXIndex,normMemXAxisName->memXAxisName]];
			
			,
			StringMatchQ[memXIndex,RegularExpression[lognormMemXAxisName<>"[0-9]+"]],
			Return[StringReplace[memXIndex,lognormMemXAxisName->memXAxisName]];
			
			,
			True, Return[""];
		];
		
		,
		True, Return[""];
	];
];

(* Return the matrix of values for the memXIndices. *)
GetMemXIndicesPredValues[noMemPredictorVector_,noMemPredictorNames_,maxMemXModel_,knotMemModels_] :=
Module[{noMemTerms,formattedPredictorVector,maxMem,refMem,memX,k,
		res={}},
	(* Retrieve the the memXIndices *)
 	noMemTerms = maxMemXModel["predictorTerms"];
	formattedPredictorVector = FormatXY[{{noMemPredictorVector},{}},Symbol[#] & /@ noMemPredictorNames,noMemTerms ][[1]];
 			
	maxMem = Ceiling[Max[maxMemXModel["model"] @@ {formattedPredictorVector},1]];
	memX = NestWhileList[# * 2 &, 1 , Log2[#]< Log2[maxMem] &];
	If[!MemberQ[memX,maxMem],memX = Append[memX,Ceiling[maxMem]];];
	
	res = Reap[
		Sow[memX];				(* memX *)
		Sow[N[memX/maxMem]]; 	(* normMemX *)
		Sow[If[maxMem>1, (1+N[Log[#]]/N[Log[maxMem]] & /@memX) , {1}] ]; (* lognormMemX *)
		
		If[Length[knotMemModels]>0,
			Do[
 				noMemTerms = k["predictorTerms"];
				formattedPredictorVector = FormatXY[{{noMemPredictorVector},{}},Symbol[#] & /@ noMemPredictorNames,noMemTerms ][[1]];
				
				refMem = k["model"] @@ {formattedPredictorVector};
				Sow[N[memX/refMem]]; 	(* normMemXCC *)
				Sow[If[refMem>1, (1+N[Log[#]]/N[Log[refMem]] & /@memX) , ConstantArray[1,Length[memX]]] ]; (* lognormMemXCC *)
			,{k,knotMemModels}];
		];
		
	][[2,1]];
	
	Return[Transpose[res]];(* matrix whose columns are memXIndices. *)
];

(* Return the names of dataKeyValue (data features) that are not constant in the algrithm list algAssociation_ *)
GetXNames[algAssociation_] :=
	Module[{predictorNames,nAlgos,keyValueLists,nKeyValues,sameFeatures,f,currentName,values,scalingParameters},
		(* Get the list of all the dataKeyValue (data features) *)
	scalingParameters = algAssociation["scalingParameters"];
	
	keyValueLists = GetScalingConfigurations[algAssociation];
	nAlgos = Length[keyValueLists];
		
		(* Check features consistency, algorithms in algAssociation_ must have the same data features *)
	nKeyValues = Length[ keyValueLists[[1]] ];
	sameFeatures = Union[Keys[#]] === Union[scalingParameters] & /@ keyValueLists;
	If[Select[sameFeatures, # != True &, 1] != {},(* If an algo-characterization has a different number of data features *)
		GetXNames::scalingParameterNotSupported =
			"Extrapolation for algorithms having unconsistent scaling parameters is not supported."; 
		Message[GetXNames::scalingParameterNotSupported]; Return[Null];
	];
	
	
	predictorNames = {};
	For[f=1, f<=nKeyValues, f++, (* For all data features f *)
		currentName = scalingParameters[[f]];
		values = #[currentName] & /@ keyValueLists;
		If[Length[Union[values]] > 1, (* If the current data feature is not constant *)
			 AppendTo[predictorNames,currentName];
		];
	];
	
	Return [predictorNames];
];

MultipleObservationsQ[observationSet_]:= 
Module[{ret,multi=False},
	ret = ListQ[observationSet];(* List of runs *)
	If[!ret,Return[False]];
	multi = multi || Length[observationSet]>1;
	
	ret = And@@(AssociationQ[#]&/@observationSet);(* List of threads *)
	If[!ret,Return[False]];
	multi = multi || Length[ observationSet[[1]] ]>1;
	
	ret = And@@(IsKeyValueListQ[#]&/@Flatten[(Values[#]&/@observationSet),1](*Flatten[observationSet,2]*));(* KeyValueList *)
	If[!ret,Return[False]];
	Return[multi]
];
Options[GetXY] = {"ShowAbsolute"->True};
(* Return {X,Y,Yvariance} the predictor matrix X and the metric vector to fit Y
	and its estimated uncertainty if multiple threads or runs are executed:
	- predictorNames is a list containing the names of the data features to be used as predictor variables
	- X is the observed predictor vector
	- Y is the observed metric
	- Yvariance is either Automatic for single thread without repetitions or the sigma^2 uncertainty at estimated at each design configurations.
	- (if metricType == "memFunction") Appends the mem distances (bins) to the vectors X
	- (if metricType == "memFunction") Appends the normalized mem distances (bins) to the vectors X
	- (if metricType == "instrPercentage") Appends "LSys" to the vectors X.
	- (if metricType == "instrPercentage") The Y is denormalized by "LSys". Normalization is reintroduced after prediction.
	- (if metricType == "vinstrPercentage") The same transformations as for instrPercentage + Y is an association {vl,dl}->Ydata
	- (if metricType == "vnumeric") Y is an association {vl,dl}->Ydata
	
	This function is used only when training. This is because the need of having LSys available.
*)
GetXY[metricName_, metricType_ ,predictorNames_, algAssociation_,modelType_,knotValues_,OptionsPattern[]] :=
	Module[{X,Y,multipleObservations,showAbsolute,
		nAlgos,keyValueLists,nPredictors,newXVector,p,baseY,baseX,maskX,mask,
		memXY,memX,normMemX,lognormMemX,memY,newX,instrCount,memInstrCount,a,
		k,memRef,observedThreads,vimixKeys},
	showAbsolute = OptionValue["ShowAbsolute"];
	X={};
	Y={};
	keyValueLists = GetScalingConfigurations[algAssociation];
	nAlgos = Length[keyValueLists];
	nPredictors = Length[predictorNames];
	
	multipleObservations = And@@ (MultipleObservationsQ[Flatten[algAssociation[#]]]& /@ keyValueLists);
	
	For[p=1, p<=nPredictors, p++, (* For all predictor variables *)
		newXVector = #[predictorNames[[p]]] & /@ keyValueLists;
		X = AppendTo[X,newXVector  ];
	];
	
	If[!multipleObservations,
		Y = { GetKeyValue[#,metricName] & /@ (algAssociation[#][[1,1]] & /@ keyValueLists) }; 
		Y = Transpose[Y]; (* column vector of the observed metric values (or vector of memFunction) *)
		
		,
		Y = Reap[Do[
			Sow[GetKeyValue[#,metricName]&/@(Flatten[Values[#]&/@algAssociation[a],1] )];
		,{a,keyValueLists}]][[2,1]];
	];
	X = Transpose[X]; (* Matrix whose columns are the predictors (data features) are the rows are the observations *)
	
	(* Deal with instrPercentage, i.e. denormalize Y and add LSys as last column of X *)
	If[((metricType == "instrPercentage")	) 
		&& showAbsolute,
		instrCount = GetKeyValue[#,instrCountName] & /@ (algAssociation[#][[1,1]] & /@ keyValueLists); 
		If[metricType == "instrPercentage",
			Y = Y * instrCount;
			If[MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType],
				X = Append[X//Transpose,Flatten[instrCount]] // Transpose;
			];
		];
	];
	
	(* Deal with vinstrPercentage and vnumeric, i.e. organize an association of Y and add LSys as last column of X *)
	If[metricType == "vinstrPercentage" || metricType == "vnumeric",
	 	vimixKeys = Flatten[Table[{vl, dl}, 
		 							{vl, GetVectorLengths[]}, 
		 							{dl, GetDataLengths[]}
		 					], 1];
		 		
		instrCount = GetKeyValue[#,instrCountName] & /@ (algAssociation[#][[1,1]] & /@ keyValueLists);
		Y =Association[Reap[Do[
			Sow[k->(VMixGetData[#[[1]],"VectorLength"->k[[1]],"DataLength"->k[[2]] ] &/@ Y) ];
		,{k,vimixKeys}]][[2,1]]];
		If[showAbsolute && metricType == "vinstrPercentage",
			Y = instrCount*# & /@ Y;
		];
		If[MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType] &&
				metricType == "vinstrPercentage",
			X = Append[X//Transpose,Flatten[instrCount]] // Transpose;
		];
	];
	
	If[!(metricType == "memFunction"),
		(* This is not a memFunction. Conclude here *)
		If[multipleObservations,
			
			If[Or@@ (#==0. &/@ Flatten[N[StandardDeviation[#]]&/@Y]),	
				Return[{X,{N[Mean[#]]}&/@Y,Automatic}]
			];
			Return[{X,{N[Mean[#]]}&/@Y,N[StandardDeviation[#]]&/@Y}]
			,
			Return[{X,Y,Automatic}];
		];
	];
	
	If[multipleObservations,
		Y = Reap[Do[(* TODO This code still do not manage multiple runs (observations of multiple threads) *)
				observedThreads = Values[algAssociation[[Key[a],1]]];
				observedThreads = KeyValueList2Association[#]&/@observedThreads;
				
				Sow[{SingleThreadMetricRepresentation[observedThreads,metricName]}];
			,{a,keyValueLists}]][[2,1]];
	];
	
	(* Deal with memFunction, i.e. explode the function *)
		
		baseX = X;
		baseY = Y;
		X = {};
		Y = {};
		(* For each observed algorithm run *)
		For[a=1, a<=nAlgos, a++,
			memXY = Flatten[baseY[[a]],1];
			memX  = memXY[[All,1]];(* Memory axis, e.g. reuse distance *)
			normMemX = N[memX / Max[memX]];
			lognormMemX = 1+N[Log[#]] / N[Log[Max[memX]]] & /@ memX;
			memY  = memXY[[All,2]];(* Memory function, e.g. CDF *)
			
			newX = ConstantArray[baseX[[a]], Length[memX]]; (* Keep the data features related to the run *)
			newX = Insert[ newX//Transpose , memX, nPredictors+1] // Transpose; (* Add as last column the memory axis *)
			newX = Insert[ newX//Transpose , normMemX, nPredictors+2] // Transpose; (* Add as last column the normMemory axis *)
			newX = Insert[ newX//Transpose , lognormMemX, nPredictors+3] // Transpose; (* Add as last column the lognormMemory axis *)
			
			(* add normMemXCC and lognormMemXCC for each cc in in knotValues *)
			If[Length[knotValues] > 0,
				Do[
					memRef = Max[LinearInterpolation[memXY,2,knotValues[[k]]],2];
					normMemX = N[memX / memRef];
					lognormMemX = 1+N[Log[#]] / N[Log[memRef]] & /@ memX;
					newX = Insert[ newX//Transpose , normMemX, nPredictors+3+(k-1)*2+1] // Transpose; (* Add as last column the normMemory axis *)
					newX = Insert[ newX//Transpose , lognormMemX, nPredictors+3+(k-1)*2 +2] // Transpose; (* Add as last column the lognormMemory axis *)
				,{k,Range[Length[knotValues]]}];
			];
			
			X = AppendTo[X,newX];
			Y = AppendTo[Y,memY];
		];
		X = Flatten[X,1];
		Y = Flatten[Y,1];
		Y = Transpose[{Y}];
	maskX = (And @@ (NumberQ[#] & /@X[[#]]) ) & /@ Range[Length[X]];
	(*maskY = (And @@ (# =!= 0 & /@Y[[#]]) ) & /@ Range[Length[X]];
	mask = And[maskY[[#]],maskX[[#]]] & /@ Range[Length[X]];*)
	mask = maskX;
	X = Pick[X, mask];
	Y = Pick[Y,mask];
	
	Return[{X,Y,Automatic}];
];

(*	This return the XY for the prediction of the memory axis of a memFunction type.
	The CDF option defines the probability distribution for which the memX should be computed (Y).
	CDF=1 implies the prediction of the maximum memX.
*)
Options[GetXYmem] = {"CDF"-> 1}
GetXYmem[metricName_, predictorNames_, algAssociation_, OptionsPattern[] ] := 
	Module[{XY,X={},Y,keyValueLists,p,a,newXVector,cdf},
	cdf = OptionValue["CDF"];
		
	keyValueLists=GetScalingConfigurations[algAssociation];
	If[And@@ (MultipleObservationsQ[Flatten[algAssociation[#]]]& /@ keyValueLists),
		XY={0,0,0};
		For[p=1, p<=Length[predictorNames], p++, (* For all predictor variables *)
			newXVector = #[predictorNames[[p]]] & /@ keyValueLists;
			X = AppendTo[X,newXVector  ];
		];
		
		XY[[1]] = Transpose[X];
		
		Y = Reap[Do[
				Sow[
					Max[LinearInterpolation[GetKeyValue[#,metricName],2,cdf],2] 
					(*Max[GetKeyValue[#,metricName][[All,1]]]*)
						&/@ (Flatten[Values[#]&/@algAssociation[a],1] )
				];
			,{a,GetScalingConfigurations[algAssociation]}]][[2,1]];
		
		XY[[2]] = {N[Mean[#]]} & /@ Y;
		XY[[3]] = N[StandardDeviation[#]] & /@ Y;
		
		,
		(* Trick for singleThreaded applications *)
		XY = GetXY[metricName, "numeric", predictorNames, algAssociation,Null,{}];
		(*XY[[2]] = Max[LinearInterpolation[#,2,cdf],2] & /@ Flatten[XY[[2]], 1] ;*)
		XY[[2]] = Max[Max[Select[#,#[[2]] <= cdf&][[All,1]] ],2] & /@ Flatten[XY[[2]], 1] ;
		(* XY[[2]] = Max[#[[All, 1]]] & /@ Flatten[XY[[2]], 1] ;(* We need a column vector *) *)
		XY[[2]] = {XY[[2]]} // Transpose; (* column vector *)
		XY[[3]] = Automatic;
	];
	
	If[XY[[3]]=!=Automatic,
		If[Or@@ (#==0. &/@ Flatten[XY[[3]]]),
			XY[[3]] = Automatic;
		];
	];
	
	Return[XY]
];

(*	XY[[1]] -> design matrix
	XY[[2]] -> observations (optional, if missing place {})
	Get a matrix X whose column i to represent the symbol i. Generates a matrix X output where the column j is the expression in the term j.
	It removes from X the lines that do not belong to the domain of the terms. It adjust the Y accordinlgy.
	Return XY in the new format and appended the mask for filtering the input X.
*)
FormatXY[XY_,symbols_,terms_] := Module[{X,Y,domains,mask,wholeMask,rules},
	Assert[Length[symbols] == Length[XY[[1]]//Transpose]];
	(* Get X and Y. *)
	X = XY[[1]];
	Y = XY[[2]];
	
	If[Length[terms]>0,
		rules = ToRules[ symbols == N[#] ] & /@ X;
		
		(* Cleanup function domains. *)
		domains = FunctionDomain[#, symbols] & /@ terms;
		wholeMask = (domains /. #) & /@ rules;
		mask = !MemberQ[#,False] & /@ wholeMask;
		X = Pick[X,mask];
		If[Length[Y]>0,
			If[AssociationQ[Y],(* Here we have an association such as for vinstrPercentage or vnumeric: {vl,dl}->Ydata *)
				Y = Pick[#,mask] & /@ Y;
				, 
				Y = Pick[Y,mask];
			];
		];
		
		(* compute the terms! *)
		rules = ToRules[ symbols == N[#] ] & /@ X;
		X = (terms /. #) & /@ rules;
		
		,
		(* compute the terms! *)
		X = {} & /@ X;
	];
	
	
	
	(* At this point if Y is estimated as a constant, X is empty. Fit in a constant. *)
	If[Length[X[[1]]] == 0,
		X = Transpose[{ConstantArray[1,Length[X]]}];
	];
	
	Return[{X,Y,mask}];
];

MyCachedSimplify[par0_,symbolsN_,validRange_,vars_] := (*MyCachedSimplify[par0,symbolsN,validRange,vars]=*)
	Simplify[
		TimedCachedReduce[
			ForAll[
				Evaluate[Symbol[#]& /@symbolsN[[par0[[1]] + 1]]  ]
				,And@@Flatten[{validRange[[par0[[1]] + 1]]}],par0[[2]] 
			]
		,vars,Reals] 
	,
	validRange]


TryReduce[predNames_,validRange_,pdTerm_,vars_,dom_] := (*TryReduce[predNames,validRange,pdTerm,vars,dom] =*)
Module[{expr,res,exprs,symbolsN,i},
	expr = ForAll[Evaluate[Symbol[#]&/@predNames],And@@Flatten[validRange],pdTerm>= 0 ];
	res = Quiet[
			Check[
				TimedCachedReduce[Evaluate[expr],vars,dom]
				,
				(* Reduce failed, reduce one parameter per time  *)
				symbolsN = Reap[For[i=1,i<=Length[validRange],i++,
								Sow[Select[predNames,MemberQ[And@@validRange[[i]],(Symbol[#])]& ] ] 
							]][[2,1]];
				Check[
					exprs = NestList[
								{	#[[1]] + 1 ,
									MyCachedSimplify[#,symbolsN,validRange,vars]
								 	, symbolsN[[#[[1]] + 1]] 
								}&, {0,pdTerm>= 0,{}},
								Length[predNames]
							];
					
					exprs[[Length[predNames]+1,2]]
				
					,
					symbolsN = Select[vars,MemberQ[pdTerm,#]&];
					And@@ (#>=0 & /@ symbolsN)
				]
			]
		];
	
	Return[res];
];

CachedReduce[a_,b_,c_] := CachedReduce[a,b,c] =
Quiet[
	Check[
		TimeConstrained[Reduce[a,b,c],3],$Aborted
	,{Reduce::inex,Reduce::useq}]
];

TimedCachedReduce[a_,b_,c_] :=
Module[{res,termList,tersm2anonymusTerms,rules,oldCoeff2newCoeff,oldParameter2newParameter,cEnumerated=0,
		aReformatted,reFormat,t},
	reFormat = Head[a] === ForAll;
	
	If[reFormat,
		(* Reformat and rename coefficients before caching 
		such to exploit the simmetries of arithmetic formulas *)
		termList = a[[3, 1]];
		If[Head[termList] === Plus,
			termList = List @@ termList;
			,
			termList= {termList};
		];
		
		rules = (# -> c0) & /@ b;
		
		oldParameter2newParameter = Association[a[[1,#]]->Symbol["p"<>ToString[#]]& /@ Range[Length[a[[1]]] ]];
		
		tersm2anonymusTerms = Association[# -> (#/.  rules) & /@ termList];
		tersm2anonymusTerms = Sort[tersm2anonymusTerms];
		
		oldCoeff2newCoeff = Association[#->Null & /@b];
		Do[
			If[!FreeQ[t,#] && oldCoeff2newCoeff[#]===Null,
				cEnumerated = cEnumerated+1;
				 oldCoeff2newCoeff[#] = Symbol[ "c"<>ToString[cEnumerated] ];
			]& /@ b;
		,{t,Keys[tersm2anonymusTerms]}];
		
		oldCoeff2newCoeff = Select[oldCoeff2newCoeff,#=!=Null&];
		
		rules = (Rule@@ #) & /@ Transpose[{
									Keys[Join[oldCoeff2newCoeff,oldParameter2newParameter]],
									Values[Join[oldCoeff2newCoeff,oldParameter2newParameter]]
								}];
		
		aReformatted = a /. rules;
		
		,
		aReformatted = a;
	];
	
	res = CachedReduce[aReformatted,b,c];
	
	If[reFormat,
		rules = (Rule@@ #) & /@ Transpose[{
									Values[Join[oldCoeff2newCoeff,oldParameter2newParameter]],
									Keys[Join[oldCoeff2newCoeff,oldParameter2newParameter]]
								}];
		res=res /. rules;
	];
	
	If[ res === $Aborted || Head[res] === Reduce,
		TimedCachedReduce::timeout = "Reduce has timed out or failed";
		Message[TimedCachedReduce::timeout];
	];	
	Return[res];
];

GenerateConstraint[eq_,predictorNames_,validRange_,coefficients_] :=
	GenerateConstraint[eq,predictorNames,validRange,coefficients] = 
Module[{tmp,derPolyTerms,derNPolyTerms,newConstraint,t},
	Quiet[
		Check[
			(*newConstraint = Evaluate[ForAll[Evaluate[Symbol[#]&/@predictorNames],And@@Flatten[validRange],eq>= 0 ]];*)
			
			newConstraint = TimedCachedReduce[Evaluate[ForAll[Evaluate[Symbol[#]&/@predictorNames],And@@Flatten[validRange],eq>= 0 ]]
				,coefficients,Reals]	
		,	
			(* Reduce dies when polynomial terms and non polynomial terms are mixed within a ForAll statement... *)
			(* Deal with those terms separately, intersect the regions such that each term is>=0, thus their sum must be >=0 *)
			tmp=Reap[
					Do[Sow[t,PolynomialQ[t,Symbol[#]&/@predictorNames]];
					,{t,Flatten[{If[Head[eq]===Plus,List@@(eq),eq]}]}];
				,{True,False}][[2]];
			derPolyTerms=tmp[[1]] /. List->Plus;
			derNPolyTerms=Flatten[tmp[[2]]];
							
			newConstraint = TimedCachedReduce[
								ForAll[Evaluate[Symbol[#]&/@predictorNames],And@@Flatten[validRange],derPolyTerms>= 0 ]
							, coefficients,Reals];
			newConstraint = Simplify[And@@ 
								Append[TryReduce[predictorNames,validRange,#, coefficients,Reals] &/@ derNPolyTerms,newConstraint]
								, And@@Flatten[validRange] ];
							
		]
	];
	Return[newConstraint];
];

GeneratePositivityConstraint[eq_,dEq_,predictorNames_,validRange_,coefficients_]:=
Module[{newConstraint},
	newConstraint =
	Return[newConstraint];
];

(* FIXME: 	The training function do not see the parameters but the terms themself.
			Training function check the monotonicity over the terms
			We here translate constraints to term coefficients.
			A better approach would be to check monotonicity after the model is trained without translation but that would mean
			a drastic change in the infrastructure.
			 *)
FormatConstraints[posNmonotonConstraints_,predictorTerms_,predictorNames_] := 
Module[{coefficients,coeffConstraints={True},varN,validRange,linearEquation,predictorConstraint,
	partialDerivative,
	monotonConstraints,definedPositive},
	varN = Length[predictorTerms];
	coefficients = Symbol[#] &/@ Map["coeff"<>ToString[#]&,Range[varN+1]];
	linearEquation = (Plus @@ Flatten[{coefficients[[varN+1]] , (coefficients[[#]] predictorTerms[[#]])&/@Range[varN]}]);
	
	monotonConstraints = Select[posNmonotonConstraints,ListQ[#]& ];
	definedPositive = Complement[posNmonotonConstraints,monotonConstraints];
	If[Length[definedPositive]=!={True},definedPositive=False,definedPositive=True];
	
	validRange = monotonConstraints[[All,3]];
	
	If[MemberQ[monotonConstraints[[All,2]],True ]||definedPositive,
		coeffConstraints = Reap[
			Do[(* For each predictor whose monotonicity constraints is enabled *)
				If[predictorConstraint[[2]],
					partialDerivative = D[linearEquation,Symbol[predictorConstraint[[1]] ]];
					
					Sow[GenerateConstraint[partialDerivative,predictorNames,validRange,coefficients]];
					,
					If[definedPositive,
						partialDerivative = D[linearEquation,Symbol[predictorConstraint[[1]] ]];
						If[Refine[
							And@@ Flatten[validRange]/.Symbol[predictorConstraint[[1]]]->Infinity,
								And@@ Complement[validRange,{predictorConstraint[[3]]} ]
							],
							partialDerivative = Limit[partialDerivative,(* If it does not fall indefinetly low it might be acceptable *)
													Symbol[predictorConstraint[[1]] ]->Infinity
												];
						];
						(* When the metric is defined positive,
							the negative derivative is accepted if and only if the metric itslef does
							not become negative within the validity range. *)
						Sow[ Simplify[
								GenerateConstraint[linearEquation,predictorNames,validRange,coefficients] ||
								GenerateConstraint[partialDerivative,predictorNames,validRange,coefficients]
							] 
						];
					];
				];
			,{predictorConstraint,monotonConstraints}];
		][[2,1]] ;
		,
		coeffConstraints = {True};
	];
	
	Return[{coefficients, And @@ Flatten[{coeffConstraints}]}];
];

CleanTermQ[term_,allPredictorNames_] := Module[{vars,availableVars},
	vars = Union@Cases[{term}, Except[__Symbol?(Context@# === "System`" &), _Symbol], {1, Infinity}, Heads -> True];
	availableVars = Symbol[#] & /@ allPredictorNames;
	Return[Complement[vars,availableVars]==={} ]
];

SetAttributes[ParseExtrapolationListRow,HoldAll]
ParseExtrapolationListRow[metricNameOut_,metricTypeOut_,memFunctionFlagOut_,modelTypeOut_,
	predictorTermsOut_, posNmonotonConstraintsOut_,fastOut_,weightingFunctionOut_,boundariesOut_,
	extrapolationListRowIn_,initialWeightingFunctionIn_,predictorNamesInOut_
	] := Module[{predictorNamesOut},
	
	metricNameOut = ToString[extrapolationListRowIn[[1]]];(* The definition of memX knots needs conversion *)
	metricTypeOut = extrapolationListRowIn[[2]];
	
	If[(!MemberQ[metricTypeOptions[metricNameOut], metricTypeOut]) && !(NumberQ[ToExpression[metricNameOut]]===True),
		ParseExtrapolationListRow::typeMismatch =
			"Error: the metric "<>metricNameOut<> " is not supposed to be of type: " <> metricTypeOut; 
		Message[ParseExtrapolationListRow::typeMismatch]; 
		If[!MemberQ[Keys[metricType2ModelTypes],metricTypeOut],
			ParseExtrapolationListRow::typeError =
				"Error: the metric type "<>metricNameOut<> " is unknown. Set to default value: numeric."; 
			Message[ParseExtrapolationListRow::typeError];
			metricNameOut = "numeric";
		];
	];
	
	
	modelTypeOut  = If[Characters[extrapolationListRowIn[[3]]][[1]] == "{",
					ToExpression[extrapolationListRowIn[[3]]],
					extrapolationListRowIn[[3]]];
	memFunctionFlagOut = metricTypeOut=="memFunction";
	
	predictorTermsOut = extrapolationListRowIn[[4]];
	If[predictorTermsOut =!= ".",
		(* If terms strings, then they are parameter names to be used in model selection. *)
		Quiet[Check[
			If[Length[ToExpression[predictorTermsOut]]>0,
				If[StringQ[ ToExpression[predictorTermsOut][[1]] ],
					predictorNamesOut = ToExpression[predictorTermsOut];
					predictorTermsOut = ".";
					
					predictorNamesInOut = Intersection[predictorNamesInOut,predictorNamesOut];
				];
			];
			,
			(* This will be executed in the case the ToExpression fails,
			i.e. if extrapolationListRowIn[[4]] is not a string but a list of actual tersm *)
			predictorTermsOut = extrapolationListRowIn[[4]];
		]];
		
	];
				
	If[extrapolationListRowIn[[5]] == ".",(* TODO: editable from the GUI... *)
		posNmonotonConstraintsOut = {#,False,{True}} & /@ predictorNamesInOut;
		PrependTo[posNmonotonConstraintsOut,False];
		,
		(* FIXME: clean the constraints as doing for the terms? CleanTermQ -> CleanMonotonConstraintQ ? *)
		posNmonotonConstraintsOut = ToExpression[ extrapolationListRowIn[[5]] ];
		If[Intersection[posNmonotonConstraintsOut,{True,False}]==={},
			PrependTo[posNmonotonConstraintsOut,True];
		];
	];
	
	
	fastOut = ToExpression[extrapolationListRowIn[[6]]];
	If[fastOut,	
		weightingFunctionOut = WLRfunctions[[1]];
		,
		weightingFunctionOut = initialWeightingFunctionIn;
	];
	 

	boundariesOut = ToExpression[extrapolationListRowIn[[7]]]; 
];

Options[FitSingleThreadExtrapolationModel] = {"Verbose"->False,"ExtrapolationList"->defaultExtrapolationList,
												"SelModelInstrCount"->Null,"SelTermsNThreads"->{},
												"HeteroscedasticityCompensation"->Automatic,
												"MainComplexities"->mainComplexityTermsDefinitions};
(* Return a list of metricModels, one for each metric in the list extrapolationList.
	An metricModel is a tuple:
	{metricName, metricType, predictorNames, predictorTerms, modelType, model}
	
	If metricType == "memFunction", then model is a list of 2 models: 1st the memfunction model and secnd the model of maximum memry.
	Where modelType is either one in 'modelTypeDefinition'
	predictorNames are the names of dataKeyValue used within the model.
*)
FitSingleThreadExtrapolationModel[algAssociation_,OptionsPattern[]] :=
 Module[{algoModel,predictorNames, metricName,metricType, modelType, boundaries,
 			metricModel,maxMemModel,knotMemModels,maxMemXXYdata,memXXYdata,knots,knotValues,k,memXName,
 			(* this variables are for the memX pred. models *)
 			metricNameLocal,metricTypeLocal,memFunctionFlagLocal,modelTypeLocal,
			predictorTermsLocal, posNmonotonConstraintsLocal,fastLocal,weightingFunctionLocal,
			boundariesLocal,
			
 			X,XY,Y,m,allPredictorNames,predictorTerms, posNmonotonConstraints,
 			coefficientConstraintsLRFormat,
 			selModel,selModelInstrCount=Null,selTermsNThreads={},instrCountTerm=Null,
 			noMemposNmonotonConstraints,memFunctionFlag,mask,
 			t,
 			extrapolationList,fast,
 			availableMetrics,
 			weightingFunction = Automatic,initialWeightingFunction=Automatic,
 			mainComplexities,metricMainComplexities,
 			
 			dl,vl,vimixKeys},
 	(* Retrive options *)
 	extrapolationList = OptionValue["ExtrapolationList"];
 	selModelInstrCount = OptionValue["SelModelInstrCount"];
 	initialWeightingFunction = OptionValue["HeteroscedasticityCompensation"];
 	mainComplexities = OptionValue["MainComplexities"];
 	selTermsNThreads = OptionValue["SelTermsNThreads"];
 	If[selTermsNThreads==={},selTermsNThreads=Null;];
 	If[mainComplexities===Automatic,mainComplexities=mainComplexityTermsDefinitions];
 	
 	If[selModelInstrCount =!= Null,
		If[selModelInstrCount["predictorTerms"] =!= {}&&
			MemberQ[{"LinearRegression","WeightedLinearRegression"},selModelInstrCount["modelType"]],
 			
 			instrCountTerm = selModelInstrCount["model"] @ {selModelInstrCount["predictorTerms"]};
 			,
 			instrCountTerm = selModelInstrCount["model"] @ {1};
		];
	];
 			
 	algoModel = {};
 	predictorNames = GetXNames[algAssociation];
 	availableMetrics = algAssociation[ GetScalingConfigurations[algAssociation][[1]] ] [[1,1]] [[All,1]];
 	
 	vimixKeys = Flatten[Table[{vl, dl}, 
	 							{vl, GetVectorLengths[]}, 
	 							{dl, GetDataLengths[]}
	 					], 1];
 	
 	(* for all metrics to be extrapolated *)
 	algoModel = Reap[
 		For[ m=1, m<=Length[extrapolationList], m++,
 			(* Setup *)
 			ParseExtrapolationListRow[metricName,metricType,memFunctionFlag,modelType,predictorTerms,
				posNmonotonConstraints,fast,weightingFunction,boundaries,
				extrapolationList[[m]],initialWeightingFunction,predictorNames];
	 		
 			If[ NumberQ[ToExpression[ metricName ]], Continue[] ];(* Rows starting with numbers represents number of memory knots to use from the previous memFunction row. *)
	 		
	 		If[!MemberQ[availableMetrics,metricName],
	 			If[OptionValue["Verbose"],Print["Warning: the metric "<>metricName<> " not included in the profile"];];
	 			Continue[];
	 		];
	 		
	 		If[AssociationQ[mainComplexities],
	 			metricMainComplexities = mainComplexities[metricName];
	 			If[MissingQ[metricMainComplexities],
	 				metricMainComplexities = mainComplexities[Automatic];
	 				If[MissingQ[metricMainComplexities],
	 					metricMainComplexities = mainComplexityTermsDefinitions[Automatic];
	 				];
	 			];
	 			,
	 			metricMainComplexities = mainComplexities;(* backward compatability *)
	 		]; 
	 		
	 		(* If there is a memory function, first train the maxMemX model *)
	 		If[memFunctionFlag,
	 			(* set default memX model parameters*)
	 			posNmonotonConstraintsLocal = posNmonotonConstraints;
	 			weightingFunctionLocal = weightingFunction;
	 			fastLocal=False;
	 			predictorTermsLocal = ".";
	 			
	 			(* If specified in the Extrapolation list, update the parameters *)
	 			If[m < Length[extrapolationList],
	 				knots = ToExpression[ extrapolationList[[m+1]][[1]] ];
	 				If[NumberQ[knots ],
			 			ParseExtrapolationListRow[metricNameLocal,metricTypeLocal,memFunctionFlagLocal,
			 				modelTypeLocal,predictorTermsLocal,posNmonotonConstraintsLocal,fastLocal,
			 				weightingFunctionLocal,boundariesLocal,
							extrapolationList[[m+1]],initialWeightingFunction,predictorNames];
	 				];
	 			];
	 			
	 			(* Get XY and possible weights *)
	 			XY = 	GetXYmem[metricName, predictorNames, algAssociation, "CDF"->1];
	 			maxMemXXYdata = XY;
	 			noMemposNmonotonConstraints = Select[Select[posNmonotonConstraintsLocal,ListQ[#]&], !MemberQ[ GetMemXIndicesNames[knotMemModels], #[[1]] ] &];
	 			If[OptionValue["Verbose"],
	 				Print["Selecting model for maxMemX in "<>metricName];
	 			];
	 			If[initialWeightingFunction===Automatic && XY[[3]]=!=Automatic,
	 				weightingFunctionLocal = 1/XY[[3]];
	 			];
	 			
	 			(* Compute predictor terms *)
	 			If[predictorTermsLocal === "." ,
		 			selModel = ModelSelection[XY[[1]],predictorNames  ,XY[[2]],
		 										"HeteroscedasticityCompensation"->weightingFunctionLocal,
		 										"MonotoneConstraints"->noMemposNmonotonConstraints, 
		 										"ModelTypeOptions"->{"WeightedLinearRegression"},
		 										"Fast"->fastLocal,
		 										"MemFunctionFlag"->False,
		 										"MemXIndices"->{},
		 										"Verbose"->OptionValue["Verbose"],
					 							"NThreadsTerms"->selTermsNThreads,
					 							"MainComplexities"->metricMainComplexities];
					 							
					modelTypeLocal = selModel["modelType"];
					(*predictorTermsLocal not updated because if it is '.' it has to be recomputed every time.*) 
	 			];
	 			
	 			(* Format X *)
	 			XY = FormatXY	[XY,Symbol[#] & /@ predictorNames,
	 				If[predictorTermsLocal===".",
						selModel["predictorTerms"],
						predictorTermsLocal
					]
				];
	 			X = XY[[1]];
	 			Y = XY[[2]];
	 			
	 			
	 			(* fit the model *)
	 			maxMemModel = TrainingFunction[X, Flatten[Y],modelTypeLocal,{{},{True}}, False, 
	 								"WeightingFunction"->weightingFunctionLocal]; 
	 			maxMemModel = Association[ 	"metricName" -> "max"<>memXAxisName, "metricType" -> "numeric",
	 							"predictorNames" -> predictorNames,
	 							"predictorTerms" -> If[predictorTermsLocal===".",
														selModel["predictorTerms"],
														predictorTermsLocal
													],
	 							"modelType" -> modelTypeLocal, "model" -> maxMemModel,
	 							"posNmonotonConstraints" -> noMemposNmonotonConstraints ];
	 							
	 							
	 					
	 			(* maxMemModel predicts memX that gives CDF=1. memX predictions for others CDFs can be stored in knotMemModels *)
	 			knotMemModels = {};
	 			If[m < Length[extrapolationList],
	 				knots = ToExpression[ extrapolationList[[m+1]][[1]] ];
	 				If[NumberQ[knots ] && ((knots>0)===True),
	 					(* m is unchanged here because it is used later as well.
	 						extrapolationList rows that start with a number are skept from the for loop *)
	 					
	 					(* maxMemXXYdata was precomputed *)
	 					(* Compute knotValues and memXYdata *)
	 					knotValues = Array[N[#]&, knots+2,{0,1}] [[2;;-2]]; 
	 					
	 					memXXYdata = Reap[Do[
	 						Sow[GetXYmem[metricName, predictorNames, algAssociation, "CDF"->k] ];
	 					,{k,knotValues}];][[2,1]];
	 					
	 					(* drop duplicates of maxMemX *)
	 					mask = (#[[2]] =!= maxMemXXYdata[[2]] )& /@ memXXYdata;
	 					memXXYdata  = Pick[memXXYdata , mask];
	 					knotValues = Pick[knotValues, mask];
	 					
	 					(* drop memXYdata duplicates. Let knotValues be an association from the lowest value over the duplicates to all the duplicates. *)
	 					knotValues = Association[Reap[Do[
	 						mask = (# === Y )& /@ memXXYdata;
	 						Sow[Min[Pick[knotValues, mask]]->Pick[knotValues, mask]];
	 					,{Y,Union[memXXYdata]}];][[2,1]]];
	 					memXXYdata = Union[memXXYdata];
	 					
	 					(* Generate models. *)
	 					knotMemModels = Reap[Do[
	 						memXName = GetMemXModelName[Keys[knotValues][[k]]];
				 			If[OptionValue["Verbose"],
				 				Print["Selecting model for " <> memXName <>" in "<>metricName];
				 			];
				 			XY = memXXYdata[[k]];
				 			
				 			If[initialWeightingFunction===Automatic && XY[[3]]=!=Automatic,
				 				weightingFunctionLocal = 1/XY[[3]];
				 			];
				 			
				 			(* Compute predictor terms *)
				 			If[predictorTermsLocal === "." ,
					 			selModel = ModelSelection[XY[[1]],predictorNames  ,XY[[2]],
					 					"HeteroscedasticityCompensation"->weightingFunctionLocal,
					 					"MonotoneConstraints"->noMemposNmonotonConstraints, 
					 					"ModelTypeOptions"->{"WeightedLinearRegression"},
					 					"Fast"->fastLocal,
					 					"MemFunctionFlag"->False,
		 								"MemXIndices"->{},
					 					"Verbose"->OptionValue["Verbose"],
										"NThreadsTerms"->selTermsNThreads,
										"MainComplexities"->metricMainComplexities];
								 							
								modelTypeLocal = selModel["modelType"];
								(*predictorTermsLocal not updated because if it is '.' it has to be recomputed every time.*) 
				 			];
				 			
				 			
				 			(* Format X *)
				 			XY = FormatXY	[XY,Symbol[#] & /@ predictorNames,
				 				If[predictorTermsLocal===".",
									selModel["predictorTerms"],
									predictorTermsLocal
								]
							];
				 			X = XY[[1]];
				 			Y = XY[[2]];
	 						
	 						knotMemModels = TrainingFunction[X, Flatten[Y],
	 								modelTypeLocal,{{},{True}}, False, 
	 								"WeightingFunction"->weightingFunctionLocal];
	 						knotMemModels = Association[ 	"metricName" -> memXName,
	 							"metricType" -> "numeric", "predictorNames" -> predictorNames,
	 							"predictorTerms" -> If[predictorTermsLocal===".",
														selModel["predictorTerms"],
														predictorTermsLocal
													],
	 							"modelType" -> modelTypeLocal, "model" -> knotMemModels,
	 							"posNmonotonConstraints" -> noMemposNmonotonConstraints ];
	 							
	 						Do[(* These models should all be copies. *)
	 							knotMemModels["metricName"] = memXName;
	 							Sow[knotMemModels];(* knotMemModels is Reaped from here *)
	 						,{memXName,GetMemXModelName[#]& /@ knotValues[[k]]}];
	 					
	 					,{k,Range[Length[knotValues]]}];][[2,1]];
	 					
	 					knots = knots;
	 					,
	 					knots = 0;(* knots is not a number *)
	 					knotValues = {};
	 				];
	 				
	 				,
	 				knots = 0;(* knots is unefined. *)
	 				knotValues = {};
	 			];
	 		
	 			(* Setup posNmonotonConstraints accounting for memX variables *)
	 			posNmonotonConstraints =
	 					{#,True,
	 						Which[#==memXAxisName,
	 							ToExpression[#]>=1,
	 							
	 							#==normMemXAxisName,
	 							{ToExpression[#]>0,ToExpression[#]<1},
	 							
	 							#==lognormMemXAxisName,
	 							{ToExpression[#]>1,ToExpression[#]<2},
	 							
	 							StringMatchQ[#,RegularExpression[normMemXAxisName<>"[0-9]+"]],
	 							{ToExpression[#]>0},
	 							
	 							StringMatchQ[#,RegularExpression[lognormMemXAxisName<>"[0-9]+"]],
	 							{ToExpression[#]>1}
	 						]
	 					} & /@ GetMemXIndicesNames[knotMemModels];
	 					
	 			,
	 			maxMemModel = Null;
	 			knotMemModels = Null;
	 			knotValues = {};
	 		];
	 		
	 		(* Get the observations on X and Y *)
	 		allPredictorNames = predictorNames;
	 		Which[memFunctionFlag,
	 			allPredictorNames = Flatten[{allPredictorNames,GetMemXIndicesNames[knotMemModels]}];
	 			,
	 			
	 			(metricType == "instrPercentage"||metricType == "vinstrPercentage") &&
	 				MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType],
	 			AppendTo[allPredictorNames , instrCountName];
	 		];
			XY = GetXY[metricName, metricType, predictorNames, algAssociation,modelType,knotValues];
	 		If[initialWeightingFunction===Automatic && XY[[3]]=!=Automatic,
	 			weightingFunction = 1/XY[[3]];
	 		];
	 		
	 		(* clean predictor terms if needed! *)
			If[predictorTerms =!= ".",
				If[StringQ[predictorTerms],predictorTerms = ToExpression[predictorTerms]]; (*& /@ StringSplit[predictorTerms,","]*);
				mask = CleanTermQ[#,allPredictorNames] & /@ predictorTerms;
				predictorTerms = Pick[predictorTerms,mask];
			];
	 		
	 		(* Initialize the terms, run model selection if "." *)
	 		Which[Intersection[Flatten[{modelType}],Union[perBinModelTypeDefinition,inversePerBinModelTypeDefinition]]==={},
		 		If[predictorTerms === "." ,
			 		Which[metricType == "instrPercentage",	
			 		(* If possible, reuse results from main instruction count, otherwise throw a warning and generate the model *)
			 			If[selModelInstrCount === Null || (selModelInstrCount[["modelType"]] != Flatten[{modelType}][[1]]),
			 				If[OptionValue["Verbose"],
			 					Print["Warning, selecting model for "<>metricName <> " before setting the model for the instruction count."];
			 				];
				 			selModel = ModelSelection[XY[[1]],allPredictorNames ,XY[[2]],
	 										"HeteroscedasticityCompensation"->weightingFunction,
				 							"MonotoneConstraints"->posNmonotonConstraints,
				 				 "ModelTypeOptions"->Flatten[{modelType}],"Fast"->fast,
				 				 "MemFunctionFlag"->False,"MemXIndices"->{},
				 				 "Verbose"->OptionValue["Verbose"],
				 				 "NThreadsTerms"->selTermsNThreads,
				 				 "MainComplexities"->metricMainComplexities];
				 				 
				 			modelType = selModel["modelType"];
				 			predictorTerms = selModel["predictorTerms"];
				 			,
			 				If[OptionValue["Verbose"],
			 					Print["Forwarding terms selected for "<> instrCountName <>", to the "<>metricName <> " model."];
			 				];
			 				(*(* TODO: this part would adjust the LSys model rather than copying it.
			 					But I am not sure if it is better. *)
			 				selModel = ModelSelection[XY[[1]],allPredictorNames ,XY[[2]] ,
				 				"MonotoneConstraints"->posNmonotonConstraints,
				 				"ModelTypeOptions"->Flatten[{selModelInstrCount["modelType"]}],"Fast"->fast,
				 				"MemFunctionFlag"->False,"Verbose"->OptionValue["Verbose"],
				 				"InitialTerms"->{}(*{Symbol[instrCountName]}*)(*selModelInstrCount["predictorTerms"]*),
				 				"ParentCounterTerms"->
				 							Append[selModelInstrCount["predictorTerms"],Symbol[instrCountName]],
				 				"NThreadsTerms"->selTermsNThreads
				 			];
				 			modelType = selModel["modelType"];
				 			predictorTerms = selModel["predictorTerms"];*)
				 			modelType = selModelInstrCount["modelType"];
				 			predictorTerms = selModelInstrCount["predictorTerms"];
			 			];
			 			
			 			,(* vinstrPercentage *)
			 			metricType == "vinstrPercentage",(* For vinstrPercentage we do not support model selection.
			 												Only term forwarding from overall instrCountModel, or
			 												all scaling parameters as terms. *)
			 			If[selModelInstrCount ===Null || (selModelInstrCount[["modelType"]] != Flatten[{modelType}][[1]]),
			 				Print["Warning, training model for "<>metricName <> " before setting the model for the instruction count. Predictor terms not set!!!"];
			 				
				 			modelType = Flatten[{modelType}][[1]];
				 			predictorTerms = Symbol[#] &/@ predictorNames;
			 				,
			 				
			 				If[OptionValue["Verbose"],
			 					Print["Forwarding terms selected for "<> instrCountName <>", to the "<>metricName <> " model."];
			 				];
				 			modelType = selModelInstrCount["modelType"];
				 			predictorTerms = selModelInstrCount["predictorTerms"];
			 			];
			 			
			 			,(* vnumeric *)
			 			metricType == "vnumeric",(* For vnumeric we did not implement a model selection,
			 										because it would require either:
			 										1) predictorTerms to be an association of the form: {vl,dl}->terms.
			 											This would require significant changes in the FormatXY and all
			 											functions that evaluate the model.
			 											Or
			 										2) Major changes in the ModelSelection function such to handle
			 											a single predictorTerms set while mantaining several models
			 											in the form: {vl,dl}->model.
			 											
			 										Thus, no model selection and terms are all predictor terms.
			 										*)
		 				Print["Warning, model selection not implemented for vnumeric. Using all available predictors to generate the model for: "<>metricName];
		 				
			 			modelType = Flatten[{modelType}][[1]];
			 			predictorTerms = Symbol[#] &/@ predictorNames;
			 			
			 			,
			 			True,
			 			If[OptionValue["Verbose"],
		 					Print["Selecting model for "<>metricName];
			 			];
			 			selModel = 
			 				ModelSelection[XY[[1]],allPredictorNames ,XY[[2]] ,
		 						"HeteroscedasticityCompensation"->weightingFunction,
				 				"MonotoneConstraints"->posNmonotonConstraints,
				 				"ModelTypeOptions"->Flatten[{modelType}],"Fast"->fast,
				 				"MemFunctionFlag"->memFunctionFlag,
			 					"MemXIndices"->GetMemXIndicesNames[knotMemModels],
				 				"Verbose"->OptionValue["Verbose"],
					 			"NThreadsTerms"->selTermsNThreads,
					 			"MainComplexities"->metricMainComplexities];
			 			modelType = selModel["modelType"];
			 			predictorTerms = selModel["predictorTerms"];
		 			];
		 		];
		 		
		 		,(* Per Bin model types do not support model selection and expect the memX (the bin) as last predictor. *)
		 		Intersection[Flatten[{modelType}],perBinModelTypeDefinition]=!={},
		 		predictorTerms = ToExpression[#] & /@ Flatten[{predictorNames,memXAxisName}];
		 		
		 		,(* Inverse Per Bin model exploits model selection of the knotMemX. It simply aggregate those. *)
		 		Intersection[Flatten[{modelType}],inversePerBinModelTypeDefinition]=!={},
		 		predictorTerms = ToExpression[#] & /@ Flatten[{predictorNames,GetMemXIndicesNames[knotMemModels,"Unique"->False]}];
 			];
	 		
	 		
	 		(* Format X and Y accordingly to the terms. Format the monotonicity constraints as well... *)
	 		XY = FormatXY	[XY,Symbol[#] & /@ allPredictorNames,predictorTerms];
	 		X = XY[[1]]; 
	 		Y = XY[[2]];
	 		(* These are valid only for the linear model *)
	 		coefficientConstraintsLRFormat = FormatConstraints[posNmonotonConstraints,predictorTerms,allPredictorNames];
	 		
	 		(* fit the model *)
	 		If[memFunctionFlag && Length[predictorTerms]>=1,
	 			mask = Reap[Do[
	 					Sow[Or @@ (!FreeQ[t,Symbol[#]] & /@ GetMemXIndicesNames[knotMemModels])]
	 				,{t,predictorTerms} ] ] [[2,1]]; 
	 			,
	 			mask = False & /@ predictorTerms;
	 		];
	 		
	 		If[metricType == "vinstrPercentage" || metricType == "vnumeric",
	 			(* vinstrPercentage and vnumeric needs special care because is the training of several models. *)
		 		If[modelType == "WeightedLinearRegression",
		 			If[weightingFunction === Automatic,
			 			weightingFunction = Association[
			 				(# -> SelectWeightingFunctionCrossvalidationBased[ X, Flatten[Y[#]]])& /@ vimixKeys
			 			];
		 			];
		 			If[!AssociationQ[weightingFunction],
		 				weightingFunction = Association[
			 				(# -> weightingFunction)& /@ vimixKeys
		 				];
		 			];
		 		];
	 			metricModel = Association[
	 							(#->TrainingFunction[X,Flatten[Y[#]],modelType, coefficientConstraintsLRFormat,mask,
	 							"WeightingFunction"->weightingFunction[#]]
	 							)& /@ vimixKeys
	 						];(*, PerformanceGoal -> "Quality"];*)
	 			,
	 			
	 			(* not an vinstrPercentage nor vnumeric *)
		 		If[modelType == "WeightedLinearRegression" && weightingFunction === Automatic,
		 			weightingFunction = SelectWeightingFunctionCrossvalidationBased[ X, Flatten[Y]];
		 		];
	 			metricModel = TrainingFunction[X,Flatten[Y],modelType, coefficientConstraintsLRFormat,mask,
	 							"WeightingFunction"->weightingFunction];(*, PerformanceGoal -> "Quality"];*)
	 		];
	 		
	 		Sow[
	 			Association["metricName" -> metricName, "metricType" -> metricType,
	 						"heteroscedasticityCompensation"->
	 							If[modelType =="WeightedLinearRegression",weightingFunction,Null],
	 						"predictorNames" -> predictorNames, "predictorTerms" -> predictorTerms,
	 						"modelType" -> modelType, "model" -> metricModel,
	 						"posNmonotonConstraints" -> posNmonotonConstraints , "maxMemModel" -> maxMemModel,
	 						"knotMemModels" -> knotMemModels,
	 						"boundaries"->boundaries] 
	 		];
	 		
	 		(* The selection of NThreads is carriedout within the multi-threaded context. Not here. *)					
			(*If[MultiThreadExtrapolation`Private`threadCountName == metricName,
				If[predictorTerms=!={} &&
				MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType],
					
					selTermsNThreads = {
						(*(metricModel @ {predictorTerms}),*)
						1/(metricModel @ {predictorTerms})
					};
					,
					selTermsNThreads = {};
				];
			];*)
			
			If[instrCountName == metricName &&
				MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType],
				
				selModelInstrCount = Association["metricName" -> metricName, "metricType" -> metricType,
	 						"heteroscedasticityCompensation"->
	 							If[modelType =="WeightedLinearRegression",weightingFunction,Null],
	 						"predictorNames" -> predictorNames, "predictorTerms" -> predictorTerms,
	 						"modelType" -> modelType, "model" -> metricModel,
	 						"posNmonotonConstraints" -> posNmonotonConstraints , "maxMemModel" -> maxMemModel,
	 						"knotMemModels" -> knotMemModels,
	 						"boundaries"->boundaries];
				
				If[predictorTerms =!= {},
 					instrCountTerm = metricModel @ {predictorTerms};
 					,
 					instrCountTerm = metricModel @ {1};
				];
			];
	 	];
 	][[2,1]];
 	
 	(* For instrPercentage type, replace the term LSys with the model of LSys *)
 	If[instrCountTerm=!=Null,
	 	For[m=1,m<=Length[algoModel],m++,
	 		If[algoModel[[m,"metricType"]] == "instrPercentage",
	 			algoModel[[m,"predictorTerms"]] = algoModel[[m,"predictorTerms"]]
	 												/. Symbol[instrCountName]-> instrCountTerm;
	 		];
	 	];
 	];
 	
 	Return [algoModel]
 ];
 
 
ExtrapolateSingleThreadAlgorithm[algoModel_, scalingParametersAssociation_] :=
ExtrapolateSingleThreadAlgorithm[algoModel, scalingParametersAssociation] =
Module[{crtModel,predictorNames,nPredictors,predictorTerms,allThere,boundaries,
 			predictorVector,formattedPredictorVector,predInstrCount,predMemInstrCount,extrapolationResult,m,
 			predValue,
 			memX,newX,Y,
 			knotMemModels,memXIndicesNames,memXIndicesValues,
 			keys,vl,dl,dlData},
 			
 	extrapolationResult = {};
 	(* Find the instruction count metricModel such to enable normalization of instrPercentage *)
 	crtModel = Select[algoModel, #["metricName"] == instrCountName &, 1] [[1]];
 	
 	(* Even if I store the predictor names for each metricModel, I here assume that these are constants (the terms might change still!) *)
 	predictorNames = crtModel["predictorNames"]; 
 	nPredictors = Length[predictorNames];
 	predictorTerms = crtModel["predictorTerms"];
 	
 	(* Check if all predictor variables are in the list *)
 	allThere = KeyExistsQ[scalingParametersAssociation,#] & /@ predictorNames;
 	If[MemberQ[allThere,False] ,
 		Print["Mandatory parameters for extrapolation are: "<> StringJoin[Riffle[predictorNames,","]]];
 		Return[{}];
 	];
 	
 	(* Retrive the predictor vector *) 
 	predictorVector = scalingParametersAssociation[ predictorNames[[#]] ] & /@ Range[nPredictors];
 	
 	
 	(* predict the instruction counter *)
 	formattedPredictorVector = FormatXY[{{predictorVector},{}},Symbol[#] & /@ predictorNames,predictorTerms ][[1]];
 	predInstrCount = crtModel["model"] @@ { formattedPredictorVector };
 	
 	(* Predict all data and organize result *)
 	For[ m = 1 , m<=Length[algoModel] , m++,
 		crtModel = algoModel[[m]];
 		If[KeyExistsQ[crtModel,"boundaries"],
 			boundaries = crtModel["boundaries"];
 			,
 			boundaries = {-Infinity,Infinity};
 		];
 		
 		If[(* Simple prediction *)
 			crtModel["metricType"] == "numeric", 
 			
 			(* Predictor vector is assumed constant but we shall reformat the terms *)
 			predictorTerms = crtModel["predictorTerms"];
 			formattedPredictorVector = FormatXY[{{predictorVector},{}},Symbol[#] & /@ predictorNames,predictorTerms ][[1]];
 			predValue = crtModel["model"] @@ {formattedPredictorVector};
 			predValue = Max[Flatten[{predValue,boundaries[[1]]}]];
 			predValue = Min[Flatten[{predValue,boundaries[[2]]}]];
 		];
 			
 		If[(* Predict, then normalize to return percentage *)
 			crtModel["metricType"] == "instrPercentage",  
 			
 			(* Predictor vector is assumed constant but we shall reformat the terms *)
 			predictorTerms = crtModel["predictorTerms"];
 			formattedPredictorVector = FormatXY[{{predictorVector},{}},Symbol[#] & /@ predictorNames,predictorTerms ][[1]];
 			predValue = (crtModel["model"] @@ {formattedPredictorVector});
 			predValue = Max[Flatten[{predValue,boundaries[[1]]}]];
 			predValue = Min[Flatten[{predValue,boundaries[[2]]}]];
 			predValue = predValue / predInstrCount;
 		];
 		
 		If[(* Predict for each vectorLength and dataLength *)
 			crtModel["metricType"] == "vinstrPercentage" || crtModel["metricType"] == "vnumeric",  
 			
 			(* Predictor vector is assumed constant but we shall reformat the terms *)
 			predictorTerms = crtModel["predictorTerms"];
 			formattedPredictorVector = FormatXY[{{predictorVector},{}},Symbol[#] & /@ predictorNames,predictorTerms ][[1]];
 			
 			keys = Keys[crtModel["model"]];
 			predValue = Reap[Do[
 				dlData = Reap[Do[
 				
		 			predValue = (crtModel["model"][{vl,dl}] @@ {formattedPredictorVector});
		 			predValue = Max[Flatten[{predValue,boundaries[[1]]}]];
		 			predValue = Min[Flatten[{predValue,boundaries[[2]]}]];
		 			If[crtModel["metricType"] == "vinstrPercentage",(* normalize to return percentage *)
		 				predValue = predValue / predInstrCount;
		 			];
		 			
		 			Sow[{dl,predValue}];
 					
 				,{dl,Select[keys,#[[1]] == vl &][[All,2]]}]][[2,1]]; 
 				
 				Sow[{vl,dlData}]
 			,{vl,Union[keys[[All,1]]] }]][[2,1]];
 			
 			VMixCanonize[predValue];
 		];
 			
 		If[(* Introduce the memory axis, then predict *)
 			crtModel["metricType"] == "memFunction",
 			
 			(* Predictor vector is constant but we have to append the memXIndices *)
 			predictorTerms = crtModel["predictorTerms"];
 			
 			
 			(* Retrieve the the memXIndices *)
 			knotMemModels = crtModel["knotMemModels"];
 			If[MissingQ[knotMemModels],knotMemModels={}];
 			memXIndicesNames = GetMemXIndicesNames[knotMemModels,"Unique"->False];
 			memXIndicesValues = GetMemXIndicesPredValues[predictorVector,predictorNames,crtModel["maxMemModel"],knotMemModels];
 			memX = (memXIndicesValues//Transpose)[[1]];
 			
 			(* Append the memXIndices to the predictorVector *)
			newX = ConstantArray[predictorVector, Length[memXIndicesValues]]; (* Keep the data features related to the run *)
			newX = Join[ newX//Transpose , memXIndicesValues//Transpose] // Transpose; (* Add as last columns the memory axis *)
 			
 			(* Predict the memFunction *)
 			formattedPredictorVector = FormatXY[{newX,{}},Symbol[#] & /@	Flatten[{predictorNames,memXIndicesNames}]  , predictorTerms ][[1]];
 			Y =  Flatten[crtModel["model"] @@ {{#}} & /@ formattedPredictorVector];
 			Y = Max[Flatten[{#,boundaries[[1]]}]]&/@Y;
 			Y = Min[Flatten[{#,boundaries[[2]]}]]&/@Y;
 			predValue = Transpose[{memX , Y}];
 		];
 		
 		
 		extrapolationResult = Append[extrapolationResult, {crtModel["metricName"], predValue}];
 	];
 	
 	FixInstrMix[extrapolationResult];
 	Return[Association[{{0,0}(* The first thread *)->extrapolationResult}]];
]; 
  
DefaultExtrapolationList[] :=
Module[{l=defaultExtrapolationList},
	Return[l];
];
  
(******************************** Model selection ********************************)

(* The complexity terms in consideration *)
(*
	They have to be declared in order starting for the most complex one.
	Possible to use different complexities for different metrics (use "metricName"->complexityTerms).
	Automatic is the default (for mterics not specified.
*)
mainComplexityTermsDefinitions = Association[
	Automatic -> ({ (*N[10^#],*) Exp[N[#]], (*N[2^#],*) N[#]^3, N[#]^2, N[#] Log[N[#]], N[#], Log[N[#]]^2, Log[N[#]]} &)
];
DefaultComplexities[] := mainComplexityTermsDefinitions;
(*mainComplexityTermsDefinitions = {  N[#]^3, Log[N[#]]^2 } &;
interactionComplexityTermsDefinitions = {  N[#]^3,  Log[N[#]]^2 } &;*)

ExtrapolationAlgorithmErrorAggregation[eV_] := Mean[eV];
(*ExtrapolationAlgorithmErrorAggregation[eV_] := Module[{squares}, squares = #^2 & /@ eV; Return[Plus @@ squares];];*)


Options[TenFoldCrossvalidation] = {"WeightingFunction"->Automatic};
(* Return the LOO prediction error (MSE) *)
TenFoldCrossvalidation[X_,predictorNames_ ,Y_,terms_, modelType_,
						posNmonotonConstraints : Except[_Rule] : {},memMask : Except[_Rule] :  {} , OptionsPattern[]] :=
Module[{XY,Xloo,Yloo,Xo,Yo,maxY,minY,
	looModel,SE,SSE = 0, i,
	constraints,
	weightDomain,rules,mask,
	crossvalSet,folds=10,weightingFunction,looWeightingFunction},
	weightingFunction = OptionValue["WeightingFunction"];
	
	
	XY = FormatXY	[{X,Y},Symbol[#] & /@ predictorNames,terms];
	constraints = FormatConstraints[posNmonotonConstraints,terms,predictorNames];
	
	If[modelType == "WeightedLinearRegression",
		If[weightingFunction === Automatic,
			weightingFunction = SelectWeightingFunctionCrossvalidationBased[ XY[[1]], Flatten[XY[[2]]] ];
		];
		If[ListQ[weightingFunction],
			weightDomain = True;(* This is to support actual weights rather than weighting functions.*)
			,
			weightDomain = FunctionDomain[weightingFunction[y],y];
		];
	
		rules = ToRules[ y == N[#] ] & /@ Flatten[XY[[2]]];
		mask = (weightDomain /. #) & /@ rules;
		XY[[1]] = Pick[XY[[1]],mask];
		XY[[2]] = Pick[XY[[2]],mask];
		,
		weightingFunction = 1&;
	];
	
	
	looWeightingFunction = weightingFunction; 
	folds = Min[folds,Length[ XY[[1]] ]];
	maxY = Max[Flatten[XY[[2,All]]]];
	minY = Min[Flatten[XY[[2,All]]]];
	If[folds > 0,
		SE = Reap[For[i=1 , i<=folds , i++,
				crossvalSet = Select[Range[ Length[XY[[1]]] ], Mod[#,folds] == (i-1)& ];
			
				Xo = XY[[1,crossvalSet]];
				Yo = XY[[2,crossvalSet]];
				
				Xloo = Delete[XY[[1]],({#}&/@crossvalSet)];
				Yloo = Delete[XY[[2]],({#}&/@crossvalSet)];
				If[ListQ[weightingFunction],
					looWeightingFunction = Delete[weightingFunction,({#}&/@crossvalSet)];
				];
			 		
			 	(* fit the model *)
			 	
			 	looModel = TrainingFunction[Xloo, Flatten[Yloo],modelType ,constraints,memMask,
			 					"WeightingFunction"->looWeightingFunction];(*, PerformanceGoal -> "Quality"];*)
				
				(* Compute prediction error *)
				(* Sum of squares (weighted by the weighting function) *)
				(*Sow[ ((looModel[{Xo[[#]]}] - Yo[[#]])^2) [[1]] * weightingFun[Yo[[#]]] ] & /@ Range[Length[Xo]];*)
				(* Sum of squares (not weighted by the weighting function), comparable between different models *)
				(*Sow[ ((looModel[{Xo[[#]]}] - Yo[[#]])^2) [[1]] ] & /@ Range[Length[Xo]];*)
				(* Option: RelativeError (relative to the excursion) *)
				If[ Yo[[#,1]]!=0 ,
					Sow[ 
						If[maxY!=minY,
							Abs[ ((looModel[{Xo[[#]]}] - Yo[[#]]))/(maxY-minY) ]
							,
							0
						]  
					];
					(*Sow[ Abs[(((looModel[{Xo[[#]]}] - Yo[[#]]))/(Yo[[#]]) [[1]]]  ];*)
				] & /@ Range[Length[Xo]];
			]][[2]];
		If[Length[SE] >= 1,
			SE = SE[[1]];
			(*SSE = Plus @@ SE;*)
			SSE = ExtrapolationAlgorithmErrorAggregation[SE];
			,
			SSE=0;
		];
		,
		SSE = 0;
	];
	Return[SSE];
];

Options[R2Adjusted] = Options[TenFoldCrossvalidation];
R2Adjusted[X_,predictorNames_ ,Y_,terms_, modelType_,
			posNmonotonConstraints : Except[_Rule] : {},memMask : Except[_Rule] :  {}, OptionsPattern[]] :=
Module[{model,XY,R2,R2adj,n,p,constraints,weightingFunction},
	weightingFunction = OptionValue["WeightingFunction"];
	
	XY = FormatXY	[{X,Y},Symbol[#] & /@ predictorNames,terms];
	constraints = FormatConstraints[posNmonotonConstraints,terms,predictorNames];
	If[constraints[[2]] === False, Return[0.]];
	
	
	p = Length[terms];
	n = Length[XY[[2]]];
	
		 		
	(* fit the model *)
	model = TrainingFunction[XY[[1]], Flatten[XY[[2]]],modelType ,constraints,memMask,
							"WeightingFunction"->weightingFunction];
	(*pred = model[#] & /@ XY[[1]];
	
	
	SStot = Sum[( N[Mean[XY[[2]]]] - N[XY[[2,i]]])^2,	{i,n}][[1]];
	SSres = Sum[( N[Flatten[pred][[i]]] - N[XY[[2,i]]])^2,	{i,n}][[1]];
	
	If[SStot == 0. , R2=1;, R2 = (SStot-SSres) / SStot;];	*)
	R2 = model["RSquared"];
	
	
	R2adj = R2 - (1-R2) (p)/(n-p-1);
	Return[R2adj];
];

Options[AIC] = Options[TenFoldCrossvalidation];
AIC[X_,predictorNames_ ,Y_,terms_, modelType_,
		posNmonotonConstraints : Except[_Rule] : {},memMask : Except[_Rule] :  {}, OptionsPattern[]] :=
Module[{model,XY,aic,constraints,weightingFunction},
	weightingFunction = OptionValue["WeightingFunction"];
	
	XY = FormatXY	[{X,Y},Symbol[#] & /@ predictorNames,terms];
	constraints = FormatConstraints[posNmonotonConstraints,terms,predictorNames];
		 		
		 	(* fit the model *)
	model = TrainingFunction[XY[[1]], Flatten[XY[[2]]],modelType,constraints,memMask,
							"WeightingFunction"->weightingFunction];
	
	aic = model["AIC"];
	Return[aic];
];

Options[BIC] = Options[TenFoldCrossvalidation];
BIC[X_,predictorNames_ ,Y_,terms_, modelType_,
	posNmonotonConstraints : Except[_Rule] : {},memMask : Except[_Rule] :  {}, OptionsPattern[]] :=
Module[{model,XY,bic,constraints,weightingFunction},
	weightingFunction = OptionValue["WeightingFunction"];
	
	XY = FormatXY	[{X,Y},Symbol[#] & /@ predictorNames,terms];
	constraints = FormatConstraints[posNmonotonConstraints,terms,predictorNames];
		 		
		 	(* fit the model *)
	model = TrainingFunction[XY[[1]], Flatten[XY[[2]]],modelType,constraints,memMask,
							"WeightingFunction"->weightingFunction];
	
	bic = model["BIC"];
	Return[bic];
];

Options[SignificanceTest] = Options[TenFoldCrossvalidation];
SignificanceTest[X_,predictorNames_ ,Y_,terms_, modelType_,posNmonotonConstraints : Except[_Rule] : {},OptionsPattern[]] :=
Module[{model,XY,pvalues,constraints,confidence=0.05,weightingFunction},
	weightingFunction = OptionValue["WeightingFunction"];
	If[!MemberQ[{"LinearRegression","WeightedLinearRegression"},modelType] ,Return[True];];
	XY = FormatXY	[{X,Y},Symbol[#] & /@ predictorNames,terms];
	constraints = FormatConstraints[posNmonotonConstraints,terms,predictorNames];
		 		
		 	(* fit the model *)
	model = TrainingFunction[XY[[1]], Flatten[XY[[2]]],modelType,constraints,False,
						"WeightingFunction"->weightingFunction];
	
	pvalues = Quiet[model["ParameterPValues"],{General::unfl}];
	
	Return[pvalues[[ Length[XY[[1,1	]] ] + 1 ]] < confidence ];
];

(*
	Generates an association from each term to the complexities to be accounted for during model selection.
	mainComplexities here have been already selected for a specific metric.
	I.e. they are a list of complexity functions and not an association from metric name to complexity functions.
*)
FormatMainComplexities[predictorNames_,mainComplexities_,parentCounterTerms_,nThreadsTerms_] :=
Module[{res,term,mask,complexities,parallelizedMask,parallelizedComplexities={}},
	res = Association[Reap[Do[
		parallelizedComplexities = {};
		
		If[term === Symbol[instrCountName],
			(* This is LSys itself that could be used as term for instruction mix metrics. *)
			Sow[term -> {term}];
			Continue[];
		]; 
		
		complexities = Join[ mainComplexities[term] ];
		complexities = Select[complexities, ! FreeQ[#, term] &];(* This enables to include in the complexities specialized parameters, e.g. {2^scale,N[#]^2,...}& *)
		If[nThreadsTerms=!={} ,
			parallelizedComplexities = Join @@ (complexities*# & /@ nThreadsTerms) ;
		];
		
		
		If[parentCounterTerms =!=Null ,
			mask = MemberQ[parentCounterTerms,#]& /@ complexities;
			mask = (Or @@ mask[[1;;#]] & /@ Range[Length[mask]]);
			complexities = Pick[complexities,mask];
			
			If[nThreadsTerms=!={} ,
				parallelizedMask = MemberQ[parentCounterTerms,#]
														& /@ parallelizedComplexities;
				parallelizedMask = (Or @@ parallelizedMask[[1;;#]] & /@ Range[Length[parallelizedMask]]);
				parallelizedMask = Or @@ # & /@ ({mask , parallelizedMask} // Transpose);
				
				parallelizedComplexities = Pick[parallelizedComplexities,parallelizedMask];
			];
		];
		
		parallelizedComplexities = If[ parallelizedComplexities === Null, {}, parallelizedComplexities];
		Sow[term -> Join[complexities,parallelizedComplexities]];
	,{term,(Symbol[#]& /@predictorNames)}] ] [[2,1]] ];
	
	Return[res];
];

Options[ModelSelection] = {"Verbose"->False,"MonotoneConstraints"->{},
							"ModelTypeOptions"->{"LinearRegression"},
							"MemFunctionFlag"->False,
							"MemXIndices" -> {},
							"Fast"->True,
							"MainComplexities"->mainComplexityTermsDefinitions[Automatic],(* metric specific *)
							"InitialTerms"->{},
							"ParentCounterTerms" -> Null,
							"NThreadsTerms" -> {},
							"HeteroscedasticityCompensation" -> Automatic
							};
(* Returns the best Terms, and modelType combination. (metricType shall be selected somewhereelse) *)
(*
	Model selection based on LOO crossvalidation error.
	Assuming a Central Composite DOE, Circumscribed (including a full 2 level factorial).
	Each predictor takes 5 different levels (-1 during LOO, ?-1 for the constant?) -> up to 3 terms per predictor.
	Theoretically, interactions effects including up to all predictors might be evaluated thanks to the full factorial.
	For practical reasons, we include only interactions with up to 3 predictors.
	Each predictor combination can appear only once (perhaps twice but further researches shall be done on this subject).
	Up to n-2 terms in total can be found
		(1 degree of freedom lost for the constant coefficient and one less degree of freedom during crossvalidation).
		
	Mathemathica global optimization heuristics are not nice and the problem is pretty complex.
	Use greedy optimization!
*)
ModelSelection[X_ ,predictorNames_ ,Y_,OptionsPattern[]] :=
Module[{bestScore = -Infinity, bestModelScore = -Infinity, Score, initialScore (*for debugging only*),
		forcedBackward=False,initialTerms = {}, parentCounterTerms = Null, nThreadsTerms = {},
		bestTerms = {}, terms = {}, 
		bestModelType="LinearRegression", modelType="LinearRegression",
		newTerm, (*predictorsInNewTerm,*)
		freedomDegrees, freedomDegreesPerPredictor, 
		mainEffectOptions,mainEffectGlobalOptions,interactionEffectOptions,allOptions,
		t,term2pred,term2term
		,scoreFunction,memMask,
		posNmonotonConstraints,modelTypeOptions,memFunctionFlag,memXIndices,
		mainComplexities,formattedMainComplexities,fast,
		changedBackward=False,changedForward=False,
		weightingFunction},
	(* Retrive options *)
	posNmonotonConstraints = OptionValue["MonotoneConstraints"];
	modelTypeOptions = OptionValue["ModelTypeOptions"];
	memFunctionFlag = OptionValue["MemFunctionFlag"];
	memXIndices = OptionValue["MemXIndices"];
	mainComplexities = OptionValue["MainComplexities"];
	fast = OptionValue["Fast"];
	initialTerms = OptionValue["InitialTerms"];If[initialTerms =!= {},forcedBackward=True];
	nThreadsTerms = OptionValue["NThreadsTerms"];
	parentCounterTerms = OptionValue["ParentCounterTerms"];
 	weightingFunction = OptionValue["HeteroscedasticityCompensation"];
	
	If[fast && weightingFunction === Automatic,
		weightingFunction = WLRfunctions[[1]];
	];
	
	
	formattedMainComplexities = FormatMainComplexities[predictorNames,mainComplexities
														,parentCounterTerms,nThreadsTerms];
	
	(* Setup *)
	modelType=modelTypeOptions[[1]];
	bestModelType = modelType; 
	freedomDegrees = Length[X] - 2; (* 1 degree lost for the constant and one during LOO *)
	freedomDegreesPerPredictor = Association[ 
			predictorNames[[#]] -> Length[Union[Flatten[X[[All,#]]]]] - 1 &/@ Range[Length[predictorNames]] 
		];
	Assert[Length[X//Transpose] == Length[predictorNames] , "ModelSelection, number of predictors does not match." ];
	memMask = {};
	
	scoreFunction[Xp_,predictorNamesp_ ,Yp_,termsp_, modelTypep_,posNmonotonConstraintsp_] :=
	Module[{s},
		If[memFunctionFlag && Length[termsp]>0,
			memMask = Reap[Do[
					Sow[Or @@ (!FreeQ[t,Symbol[#]] & /@ memXIndices)]
				,{t,termsp} ] ] [[2,1]]; 
		];
		Which[
			modelTypeOptions === {"LinearRegression"} && fast,
			(* This implements Calotoiu2013 *)
			s= R2Adjusted[Xp,predictorNamesp ,Yp,termsp, modelTypep,
					posNmonotonConstraintsp,memMask,"WeightingFunction"->weightingFunction];
			
			,
			modelTypeOptions === {"WeightedLinearRegression"} && fast,
			(* This implements Mariani2015 CF *)
			s= -BIC[Xp,predictorNamesp ,Yp,termsp, modelTypep,posNmonotonConstraintsp,memMask
					,"WeightingFunction"->weightingFunction];
			
			
			,
			modelTypeOptions === {"WeightedLinearRegression"} && Not[fast],
			(* This implements Mariani2016 IJPP *)
			s= -TenFoldCrossvalidation[Xp,predictorNamesp ,Yp,termsp, modelTypep,
					posNmonotonConstraintsp,memMask,"WeightingFunction"->weightingFunction];
			
			,
			True,
			s= -TenFoldCrossvalidation[Xp,predictorNamesp ,Yp,termsp, modelTypep,
					posNmonotonConstraintsp,memMask,"WeightingFunction"->weightingFunction];
		];
		If[Head[s] === List,
			s[[1]],s
		]
	];
	
	(* Setup parallelization *)
	terms = initialTerms;
	SetSharedVariable[terms];
	
	(* Initialize, constant model *)
	bestTerms = terms;
	bestScore = scoreFunction[X,predictorNames,Y,bestTerms,modelType,posNmonotonConstraints];
	initialScore = bestScore;
	
	
	
	Do[
		bestModelScore = scoreFunction[X,predictorNames ,Y,initialTerms, modelType,posNmonotonConstraints];
		
		If[modelType=="LinearRegression" || modelType == "WeightedLinearRegression",
			(* LR model selection *)
			
			(* Initialize with no terms *)
			terms = initialTerms;
			
			(* 	Loop between two phases:
				first:  increase the model and
				second: reduce the model (some main terms could have been added because of their interactions)
				The 2 phases are executed until the model stops changing.
				The second phase is currently disabled (commented).
			*)
			While[True,
				(* Loop: find the best term, add it, save model if better, or stop. *)
				changedForward = False;
				mainEffectGlobalOptions = Association[
					predictorNames[[#]]->formattedMainComplexities[ Symbol[predictorNames[[#]]] ]
					& /@ Range[Length[predictorNames]]
				];
				While[True,
					(* Generate main effect terms *)
					mainEffectOptions = Association[#->Complement[mainEffectGlobalOptions[[#]],terms] & /@ predictorNames];
					mainEffectOptions = Union[Flatten[Reap[
							If[(Length[mainEffectGlobalOptions[[#]] ] - Length[mainEffectOptions[[#]] ])
								< freedomDegreesPerPredictor[[#]],(* This predictor has additional degrees of freedom *)
								Sow[ mainEffectOptions[[#]] ]; 
								,
								Sow[Nothing];
							] & /@ predictorNames;
						][[2,1]]
					]];
					
					(* Generate interaction effect terms, only among main effect options *)
					If[Length[terms]<1,
						interactionEffectOptions = {};
						,
						term2pred = Association@@ Reap[Do[Sow[t->(Symbol[#] &/@Select[predictorNames,!FreeQ[t,Symbol[#]]&]) ],{t,terms}]][[2,1]];
						
						term2term = Association@@ Reap[Do[
							interactionEffectOptions = Select[terms,FreeQ[#,Alternatives@@ term2pred[t]]&];
							
							
							interactionEffectOptions = Select[mainEffectOptions,FreeQ[#,Alternatives@@ term2pred[t]]&];
							
							Sow[t->interactionEffectOptions];
						,{t,terms}]][[2,1]]; 
						
						
						interactionEffectOptions = Flatten[Outer[Times, {#}, term2term[#] ] ] &/@ terms;
						interactionEffectOptions = Complement[Union[Flatten[interactionEffectOptions]],terms];
					];
					
					(* Evaluate options *)
					allOptions = Union[Flatten[mainEffectOptions],interactionEffectOptions] ;
					allOptions = Complement[allOptions,terms];
					
					(*DistributeDefinitions[terms]; (* FIXME: here ParallelMap shall be used but it throws unknown errors... *)*)
					Score =  Map[scoreFunction[X,predictorNames ,Y,Append[terms,#], modelType,posNmonotonConstraints] & , allOptions] ;
					
					
					(* Verify improvement wrt bestModelScore. Break search if no term to add. (current model, add the new term) *)
					If[Max[Score] > bestModelScore && (Abs[bestModelScore] > 0.000001 || (Max[Score]-bestModelScore > 0.000001 )) ,
						bestModelScore = Max[Score];
						newTerm = Select[{allOptions,Flatten[Score]}//Transpose,#[[2]] == bestModelScore &] [[1,1]];
						
						If[True &&
							!SignificanceTest[X,predictorNames ,Y,Append[terms,newTerm],
							modelType,posNmonotonConstraints,"WeightingFunction"->weightingFunction],
							
							Break[];
						];
						
						(*predictorsInNewTerm = Select[predictorNames, !FreeQ[newTerm,Symbol[#]]&];*)
						
						terms = Append[terms,newTerm];
						changedForward = True;
						
						,
						Break[];
					];
					
					(* Verify that an increased model might be generated. Break if no more degrees of freedom *)
					If[Length[terms]>=freedomDegrees, Break[];	];
					
				];
				
				(* Exit condition *)
				If[fast || (!changedForward &&  !forcedBackward),
					Break[];
				];
				
				(* reducing loop *)
				changedBackward = False;
				(* Decommenting the following part will allow the model construction to remove previously inserted terms. *)
				(*forcedBackward = False;
				If[terms =!= {},
					While[True,
						(* Generate options *)
						allOptions = terms;
						
						(* Evaluate options *)	
						DistributeDefinitions[terms]; (* FIXME: here ParallelMap shall be used but it throws unknown errors... *)
						Score =  Map[scoreFunction[X,predictorNames ,Y,Complement[terms,{#}], modelType,posNmonotonConstraints] & , allOptions] ;
						
						
						(* Verify improvement wrt bestModelScore. Break search if no term to add. (current model, add the new term) *)
						If[Max[Score] >= bestModelScore,
							bestModelScore = Max[Score];
							newTerm = Select[{allOptions,Flatten[Score]}//Transpose,#[[2]] == bestModelScore &] [[1,1]];
							
							newTerm = SelectFirst[Range[Length[terms]],terms[[#]]===newTerm&];
							
							terms = Delete[terms,newTerm];
							changedBackward = True;
							
							,
							Break[];
						];
					];
				];*)
				
				
				(* Exit condition *)
				If[!changedBackward,
					Break[];
				];
			];
			,
			
			(* Non LR model selection *)
			(*
				The interactions are assumed to be accounted within the model itself rather than
				explicitally as in the LR models.
				Additionally, there is no need to remove terms since they are never introduced if not needed
				(i.e. it does not happen that an interaction terms should be added but first its main terms
				have to appear).
			*)
			While[True,
				(* Generate main effect terms *)
				mainEffectOptions = Symbol[#] & /@ predictorNames;
				If[nThreadsTerms =!= {},
					AppendTo[mainEffectOptions,#] & /@ nThreadsTerms;
				];
				allOptions=Complement[Union[Flatten[mainEffectOptions]],terms];
				
				DistributeDefinitions[terms]; (* FIXME: here ParallelMap shall be used but it throws unknown errors... *)
				Score =  Map[scoreFunction[X,predictorNames ,Y,Append[terms,#], modelType,posNmonotonConstraints] & , allOptions] ;
				
				
				(* Verify improvement wrt bestModelScore. Break search if no term to add. (current model, add the new term) *)
				If[Max[Score] > bestModelScore,
					bestModelScore = Max[Score];
					newTerm = Select[{allOptions,Flatten[Score]}//Transpose,#[[2]] == bestModelScore &] [[1,1]];
					(*predictorsInNewTerm = Select[predictorNames, !FreeQ[newTerm,Symbol[#]]&];*)
					
					terms = Append[terms,newTerm];
					
					
					,
					Break[];
				];
				
				(* Verify that an increased model might be generated. Break if no more degrees of freedom *)
				If[Length[terms]>=freedomDegrees, Break[];	];
				
			];
		];
		
		(* Verify improvement wrt bestScore (all models, save the whole model) *)
		If[bestModelScore > bestScore,
			bestScore = bestModelScore;
			bestTerms = terms;
			bestModelType = modelType;
		];
		
	,{modelType,modelTypeOptions}];
	
	If[OptionValue["Verbose"],
		Print["Selected model"];
		Print["Terms: ",bestTerms];
		Print["bestModelScore: ",bestScore];
	];
				
	Return[Association[	"predictorTerms"->bestTerms, (* "model"-> bestModel *)
						"modelType"->bestModelType, "Score"->bestScore]];
];
  
  
(******************************** GUI ********************************)

(* Setup formatting *)
summaryHeadingFormat = {Bold,Larger};


ExtrapolationInvestigationPlot[metricModel_, instrCountModel_, trainAlgAssociation_,
				testAlgAssociation_, varXs_, maxXs_, parameterList_,showAbsolute_,vLength_,dLength_] :=
Module[{model, memFunctionFlag,  metricTypeForPlotting,
				subSetTrain,subSetTest,algInfo, c, maxMemXVarIndex, flag,
				trainPoints,testPoints,symbols,noMemSymbols,
				parameterVector, parName,x1,x2,
				noMemTerms, formattedPredictorVectorMemX,memXIndices,
				maxMemXPred, maxMemXTruth, maxMemXTruthFunFlag, maxMemXTruthResolved, maxMemXPredResolved,
				knotMemXPred,kModel,memXReminderVector,memXPred,
				memX, trainKeys,testKeys
				,localFunction,investigation2Dplot,investigation3Dplot},
				
	trainKeys = GetScalingConfigurations[trainAlgAssociation];
	testKeys = GetScalingConfigurations[testAlgAssociation];
	
				
				
	
	(* Get the model of interest *)
	memFunctionFlag = False;
	symbols = Symbol[#] & /@ metricModel["predictorNames"];
	noMemSymbols = symbols;
	model[v_] := Module[{s=symbols,t=metricModel["predictorTerms"],fpv},
		fpv = FormatXY[{{v},{}},s,t ][[1]];
		Return[metricModel["model"][fpv] ];
	];
	Which[metricModel["metricType"] == "memFunction",
		model[v1_] := Module[{s=symbols,t=metricModel["predictorTerms"],fpv,res},
			If[MemberQ[v1,Indeterminate],
				res = 0;
				,
				
				fpv = FormatXY[{{ReleaseHold[v1]},{}},s,t ][[1]];
				res = metricModel["model"][fpv];
			];
			Return[res ];
		];
		memFunctionFlag = True;
		memXIndices = GetMemXIndicesNames[metricModel["knotMemModels"]];
		symbols = Join[symbols, (Symbol[#] & /@ memXIndices) ];
		
		,
		metricModel["metricType"] == "vinstrPercentage" || metricModel["metricType"] == "vnumeric",(* collect data from all models. *)
			model[v2_] := Module[{formattedVectorInstr,formattedParameterVector,predValue,dl,vl,dlData,iCount},
				formattedParameterVector = FormatXY[{{v2},{}},symbols,metricModel["predictorTerms"] ][[1]];
				formattedVectorInstr = FormatXY[{{v2},{}},symbols,instrCountModel["predictorTerms"] ][[1]];
			 	
			 	If[metricModel["metricType"] == "vinstrPercentage",
			 		iCount = instrCountModel["model"][formattedVectorInstr];
			 	];
				
	 			predValue = Reap[Do[
	 				dlData = Reap[Do[
			 			predValue = metricModel["model"][{vl,dl}][formattedParameterVector];
			 			
					If[!showAbsolute && metricModel["metricType"] == "vinstrPercentage",
			 			Sow[{dl,predValue/iCount}];
			 			,
			 			Sow[{dl,predValue}];
					];
	 					
	 				,{dl,InstructionMix`Private`dataLengths }]][[2,1]]; 
	 				
	 				Sow[{vl,dlData}]
	 			,{vl,InstructionMix`Private`vectorLengths }]][[2,1]];
				
				Return[VMixGetData[predValue, "VectorLength"->vLength, "DataLength"->dLength] ];
			];
		
		,
		metricModel["metricType"] == "instrPercentage",(* Show the actual metric value, i.e. the percentage. *)
		If[!showAbsolute,
			model[v2_] := Module[{formattedVectorInstr,formattedParameterVector},
				formattedParameterVector = FormatXY[{{v2},{}},symbols,metricModel["predictorTerms"] ][[1]];
				formattedVectorInstr = FormatXY[{{v2},{}},symbols,instrCountModel["predictorTerms"] ][[1]];
				Return[metricModel["model"][formattedParameterVector]/instrCountModel["model"][formattedVectorInstr] ];
			];
		];
		
	];			
				
	(* Filter away the trainAlgAssociation and testAlgAssociation to keep only the keys fitting the parameterList. *)
	localFunction[keys_] := Module[{localFlag,localParVal,localParListElement},
		Flatten[Reap[
			Do[
				localFlag = True;
				Do[
					If[Not[( memFunctionFlag && (localParListElement[[1]] == memXAxisName) )],
						localParVal = algInfo[ localParListElement[[1]]];
						localFlag = localFlag && localParVal == localParListElement[[2]];
					]; 
				,{localParListElement,parameterList}];
				
				If[localFlag,
					Sow[algInfo]
				];
			,{algInfo,keys}];
		][[2]],1]
	];
	
	subSetTrain = localFunction[trainKeys];
	subSetTest = localFunction[testKeys];
	
	
	(* Get training and test point lists, 3D if needed *)
	If[memFunctionFlag && (Intersection[memXIndices,varXs] =!= List[]) ,
		(* If the memX axis is a free parameter. *)
		localFunction[algAssociation_,subSet_] := Module[{retPoints,n,a,localParName,allThreadsKeyValueLists,dummyVector},
			If[Length[subSet]>0,
				(* Get the data to be visualized: from all threads of all scalingConfiguration in subSet *)
				allThreadsKeyValueLists = Values[algAssociation[#][[1]]] & /@ subSet; 
				
				
				(* Get the memX and the metric value *)
				retPoints = Flatten[
								GetKeyValue[#,metricModel["metricName"]] & /@ Flatten[allThreadsKeyValueLists,1]
							,1];
							
				(* Include the additional parameter if 3D *)
				localParName = Complement[varXs,GetMemXIndicesNames[{}] ];
				If[localParName =!= List[],
					localParName = localParName[[1]];
					n = If[localParName==varXs[[1]],1,2];
					dummyVector = 	Flatten[Reap[Do[Sow[
											ConstantArray[subSet[[a,localParName]],
												Total[
													Length[GetKeyValue[#,metricModel["metricName"]]]
														&/@ allThreadsKeyValueLists[[a]]
												]
											]
										],
									{a,Range[Length[subSet]]}] ][[2,1]]];
					
					retPoints = Insert[retPoints//Transpose, dummyVector	,n]//Transpose;
				];
				
				,
				retPoints ={};
			];
			retPoints
		];
		
		trainPoints = localFunction[trainAlgAssociation,subSetTrain];
		testPoints = localFunction[testAlgAssociation,subSetTest];
		,
		
		(* Get the metric ("numeric"), no denormalization/normalization ("instrPercentage"). So the plot will display the actual metric. *)
		(* Here, we cannot have the memX axis as free parameter but we may still have a memFunction *)
		metricTypeForPlotting = If[ (metricModel["metricType"]=="instrPercentage") && !showAbsolute,"numeric",metricModel["metricType"]];
		
		localFunction[algAssociation_,subSet_] := Module[{retPoints,localParVal,XY,n},
			If[Length[subSet]>0,
				XY = GetXY[metricModel["metricName"], metricTypeForPlotting , varXs, KeyTake[algAssociation,subSet],Null,{},"ShowAbsolute"->showAbsolute];
				If[metricModel["metricType"] == "vinstrPercentage" || metricModel["metricType"] == "vnumeric",
					XY[[2]] = VMixGetData[VMixAssociation2KeyValueList[ XY[[2]] ],"VectorLength"->vLength,"DataLength"->dLength];
				];
				retPoints = Insert[ XY[[1]]//Transpose , Flatten[XY[[2]]], 1+Length[XY[[1,1]]] ] // Transpose;
				,
				retPoints = {};
			];
			
			(* Sort out the memX axis if needed. *)
			If[MemberQ[parameterList[[All,1]], memXAxisName ],
				localParVal = Select[parameterList,#[[1]]==memXAxisName &][[1,2]];
				n = Length[varXs]+1;
				
				If[Length[retPoints]>0,
					retPoints = Select[retPoints, #[[n]]==localParVal &];
					If[Length[retPoints]>0,
						If[n==2,
							retPoints = Transpose[ {retPoints[[All,1]], retPoints[[All,Length[retPoints[[1]]]]]} ];
							,
							retPoints = Transpose[ {retPoints[[All,1]], retPoints[[All,2]], retPoints[[All,Length[retPoints[[1]]]]]} ];
						];
						,
						retPoints = {};
					];
					,
					retPoints = {};
				];
			];
			
			retPoints
		];
		
		trainPoints = localFunction[trainAlgAssociation,subSetTrain];
		testPoints = localFunction[testAlgAssociation,subSetTest];
	];
	
	(* Remove values that are not machine size *)
	localFunction[points_] := Module[{localMask},
		If[Length[varXs]==1,
			localMask =  -$MaxMachineNumber<#[[1]] && -$MaxMachineNumber<#[[2]] && 
				$MaxMachineNumber>#[[1]] && $MaxMachineNumber>#[[2]] & /@ points;
			,
			localMask =  -$MaxMachineNumber<#[[1]] && -$MaxMachineNumber<#[[2]] && -$MaxMachineNumber<#[[3]] && 
				$MaxMachineNumber>#[[1]] && $MaxMachineNumber>#[[2]] && $MaxMachineNumber>#[[3]] & /@ points;
		];
		If[MemberQ[localMask,False],
			Print["Some test point values in the investigation plot are not machine numbers and are not visualized (possible overfitting)."];
		];
		
		Pick[points,localMask]
	];
	
	trainPoints = localFunction[trainPoints];
	testPoints = localFunction[testPoints];
	
	(* Organize the parameter vector where to evaluate the model. *)
	If[Length[parameterList]>= 1,
		parameterVector = Reap[Do[
					If[MemberQ[parameterList[[All,1]], parName],
						Sow[ Select[parameterList,#[[1]] == parName &][[1,2]] ];
						,
						Sow[If[parName==varXs[[1]],x1,x2]];
					];
				,{parName,metricModel["predictorNames"]}];
			][[2,1]];
									
		,
		If[Length[varXs] == 1,
			parameterVector = {x1};
			,
			parameterVector = {x1,x2};
		];
	];
	
	If[memFunctionFlag,
	 	noMemTerms = metricModel["maxMemModel"]["predictorTerms"];
 		formattedPredictorVectorMemX = FormatXY[{{parameterVector},{}},noMemSymbols,noMemTerms ][[1]];
 		maxMemXPred = Max[metricModel["maxMemModel"]["model"] @@ {formattedPredictorVectorMemX},1];
 		maxMemXTruthFunFlag = False;
 		
 		Which[(* For 2D plots we have a single scaling configuration and a single true value of maxMemXTruth *)
 			Length[testPoints]>0 && Length[varXs]==1,
 			maxMemXTruth = Max[testPoints[[All,1]]];
 			,
 			
 			Length[trainPoints]>0 && Length[varXs]==1,
 			maxMemXTruth = Max[trainPoints[[All,1]]];
 			,
 			
 			True,
 			maxMemXTruthFunFlag = True;
 			maxMemXTruth = maxMemXPred;
 		];
 		
 		
 		If[Intersection[varXs,GetMemXIndicesNames[{}]]=!=List[],
 			(* GetMemXIndicesNames[{}]: we allow only normalization for maxMemX for axis visualization. *)
 			If[MemberQ[GetMemXIndicesNames[{}],varXs[[1]]],
 				c = 1;
 				memX = x1;
 				,
 				c = 2;
 				memX = x2;
 			];
 			
 			If[maxMemXTruthFunFlag,
 				maxMemXVarIndex = If[c == 1,2,1];
 				maxMemXTruthResolved[x_] := maxMemXTruth /. If[maxMemXVarIndex==1,x1,x2] -> x;
 				maxMemXPredResolved[x_] := maxMemXPred /. If[maxMemXVarIndex==1,x1,x2] -> x;
 				,
 				maxMemXTruthResolved[x_] := maxMemXTruth;
 				maxMemXPredResolved[x_] := maxMemXPred;
 			];
 		];
 		
 		(* setup predictions of memXknots *)
 		If[Length[metricModel["knotMemModels"]] > 0,
	 		knotMemXPred = Reap[Do[
	 			noMemTerms = kModel["predictorTerms"];
	 			formattedPredictorVectorMemX = FormatXY[{{parameterVector},{}},noMemSymbols,noMemTerms ][[1]];
	 			memXPred = Max[kModel["model"] @@ {formattedPredictorVectorMemX},1];
	 			Sow[memXPred];
	 		,{kModel,metricModel["knotMemModels"]}]][[2,1]];
	 		
	 		,
	 		knotMemXPred = {};
 		];
 		
 		(* compute memX and maxMemX derivatives *)
		Which[MemberQ[varXs,memXAxisName],(* Only one of the memX indeces is the actual variable *)
			parameterVector = Append[ parameterVector, memX];
			parameterVector = Append[ parameterVector, memX/maxMemXPred ];(* normMemX *)
			parameterVector = Append[ parameterVector, 1+Log[memX]/Log[maxMemXPred] ];(* lognormMemX *)
			
			,
			
			MemberQ[varXs,normMemXAxisName],
			memX = memX * maxMemXTruth;
			parameterVector = Append[ parameterVector, memX];
			parameterVector = Append[ parameterVector, memX/maxMemXPred ];(* normMemX *)
			parameterVector = Append[ parameterVector, 1+Log[memX]/Log[maxMemXPred] ];(* lognormMemX *)
			
			If[Length[testPoints]>0,
				testPoints[[All,c]] = (#[[c]]/
				If[maxMemXTruthFunFlag,maxMemXTruthResolved[#[[If[c==1,2,1]]]], maxMemXTruth]) & /@ testPoints[[All]]
			];
			If[Length[trainPoints]>0,
				trainPoints[[All,c]] = (#[[c]]/
				If[maxMemXTruthFunFlag,maxMemXTruthResolved[#[[If[c==1,2,1]]]], maxMemXTruth]) & /@ trainPoints[[All]]
			];
			,
			
			MemberQ[varXs,lognormMemXAxisName],
			memX = Exp[(memX - 1)*Log[maxMemXTruth]];
			parameterVector = Append[ parameterVector, memX];
			parameterVector = Append[ parameterVector, memX/maxMemXPred ];(* normMemX *)
			parameterVector = Append[ parameterVector, 1+Log[memX]/Log[maxMemXPred] ];(* lognormMemX *)
			
			If[Length[testPoints]>0,testPoints[[All,c]] = 1+Log[#[[c]]]/Log[
				If[maxMemXTruthFunFlag,maxMemXTruthResolved[#[[If[c==1,2,1]]]], maxMemXTruth]]
				& /@ testPoints[[All]] ];
			If[Length[trainPoints]>0,trainPoints[[All,c]] = 1+Log[#[[c]]]/Log[
				If[maxMemXTruthFunFlag,maxMemXTruthResolved[#[[If[c==1,2,1]]]], maxMemXTruth]]
				& /@ trainPoints[[All]] ];
			,
			
			True,
			memX =Select[parameterList,#[[1]] == memXAxisName &][[1,2]];
			parameterVector = Append[ parameterVector, memX];
			parameterVector = Append[ parameterVector,memX/maxMemXPred ];
			parameterVector = Append[ parameterVector, 1+Log[memX]/Log[maxMemXPred] ];(* lognormMemX *)
		];
		
 		
 		(* compute knotMemXModels surrogates *)
 		If[Length[knotMemXPred]>0,
 			(* memX is the vector setted up previously! Sow the reminder of the predictorVector. *)
 			memXReminderVector = Reap[
 				Do[
 					Sow[memX/memXPred];(* normMemX *)
 					Sow[1+Log[memX]/Log[memXPred]];(* lognormMemX *)
 				,{memXPred,knotMemXPred}];
 			][[2,1]];
 			
 			parameterVector = Join[parameterVector,memXReminderVector];(* Appends memXReminderVector *)
 		];
 		
	];
	
	(* 2D plotting functionalities *)
	investigation2Dplot[] := Module[{rangeXFit,rangeXPlot,listPlotTrain,listPlotTest,allYValues,
									domains,plotRange,fitPlot},
		(* Get axis ranges, this was working poorly for 1 varX, not implemented for 2 varXs *)
		domains = FunctionDomain[#,Symbol[varXs[[1]]]] & /@ metricModel["predictorTerms"];
		
		If[memFunctionFlag,
			Which[
				(varXs[[1]] == memXAxisName),
				rangeXFit = {1,Max[{maxMemXPred,maxMemXTruth}]};
				rangeXPlot = rangeXFit;
				,
				(varXs[[1]] == normMemXAxisName),
				rangeXFit = {1/maxMemXTruth,maxMemXPred/maxMemXTruth};
				rangeXPlot = {0,Max[{1,rangeXFit}]};
				,
				(varXs[[1]] == lognormMemXAxisName),
				rangeXFit = {1,1+Log[maxMemXPred]/Log[maxMemXTruth]};
				rangeXPlot = {1,Max[{2,rangeXFit}]};
				,
				True,
				rangeXFit = Union[#[varXs[[1]]] & /@ Union[trainKeys,testKeys]];
				rangeXPlot = rangeXFit;
			];
			,
			
			rangeXFit = Union[#[varXs[[1]]] & /@ Union[trainKeys,testKeys]];
			rangeXPlot = rangeXFit;
		];
		flag = domains /. Symbol[varXs[[1]]] -> # & /@ rangeXFit;
		flag = !MemberQ[#,False] & /@ flag; 
		rangeXFit = Pick[rangeXFit,flag];
		rangeXFit = {Min[rangeXFit],	If[NumberQ[ maxXs[[1]] ], rangeXPlot[[2]] = maxXs[[1]] ,Max[rangeXFit] ]};
		rangeXPlot = {Min[rangeXPlot],	If[NumberQ[ maxXs[[1]] ], rangeXPlot[[2]] = maxXs[[1]] ,Max[rangeXPlot] ]};
		
		rangeXPlot = {rangeXPlot[[1]],	rangeXPlot[[2]], 0.05 (rangeXPlot[[2]] - rangeXPlot[[1]])};
		
		(* Organize the graphics *)
		If[Length[trainPoints]>0,
			listPlotTrain = ListPlot[trainPoints,PlotStyle->{Black,PointSize[0.02]}];
		];
		If[Length[testPoints]>0,
			listPlotTest = ListPlot[testPoints,PlotStyle->{Red,PointSize[0.02]}];
		];
		
		(* TODO: fix to fit in the domain of knotmemModels. *)
		allYValues = Join[trainPoints[[All,2]], testPoints[[All,2]],
						{	model [ ReleaseHold[(parameterVector/. x1->rangeXFit[[1]])]],
							model [ ReleaseHold[(parameterVector/. x1->rangeXFit[[2]])] ] }
					];
		plotRange = {{rangeXPlot[[1]] - rangeXPlot[[3]] ,rangeXPlot[[2]] + rangeXPlot[[3]]},
						{  Min[allYValues] - 0.05 (Max[allYValues]-Min[allYValues]) , 
						Max[allYValues] + 0.05 (Max[allYValues]-Min[allYValues]) }
					};
		If[ plotRange[[2,1]]< -$MaxMachineNumber || $MaxMachineNumber <plotRange[[2,2]],
			plotRange[[2]] = {Automatic,Automatic};
			Print["Investigation plot range includes non-machine numbers and is converted to Automatic (possible overfitting)."];
		];
					
		fitPlot = Plot[model [parameterVector], {x1,rangeXFit[[1]] ,rangeXFit[[2]] } ,
							AxesLabel -> {varXs[[1]],metricModel["metricName"]},
							AxesOrigin -> {Max[0,rangeXPlot[[1]]-rangeXPlot[[3]]],Automatic}, PlotRange -> plotRange
					];
		(* Show the graphics *)
		Which[
			(Length[trainPoints]>=1) && (Length[testPoints]>=1),
			Show[fitPlot,listPlotTrain ,listPlotTest, PlotRange -> plotRange, ImageSize -> 500],
			
			(Length[trainPoints]>=1)  ,
			Show[fitPlot,listPlotTrain, PlotRange -> plotRange, ImageSize -> 500],
			
			(Length[testPoints]>=1)  ,
			Show[fitPlot,listPlotTest, PlotRange -> plotRange, ImageSize -> 500],
			
			True,
			Show[fitPlot, PlotRange -> plotRange, ImageSize -> 500]
		]
	];
	
	
	
	(* 3D plotting functionalities *)
	investigation3Dplot[] := Module[{rangeX={{},{}},allYValues,rangeY,n,
									colourDomain={380.,750.},colorFunction,contours=10,
									trainDensityPlot,trainContourPlot,predDensityPlot,predContourPlot,
									domains,domainFlags},
		(* Get axis ranges, this was working poorly for 1 varX, not implemented for 2 varXs *)
		domains = FunctionDomain[#,{Symbol[varXs[[1]]],Symbol[varXs[[2]]]} ] & /@ metricModel["predictorTerms"];
		
		If[memFunctionFlag && (Intersection[varXs,GetMemXIndicesNames[{}]]=!=List[]),
 			(* GetMemXIndicesNames[{}]: we allow only normalization for maxMemX for axis visualization. *)
			n = If[MemberQ[GetMemXIndicesNames[{}],varXs[[1]]], 2, 1 ];
			rangeX[[n]] = Join[#[varXs[[n]]] & /@ Join[trainKeys,testKeys]];
			c = Length[rangeX[[n]]];
			rangeX[[n]] = Join[rangeX[[n]],rangeX[[n]]];
			
			n = If[n==1,2,1];
			Which[
				(varXs[[n]] == memXAxisName),
				If[!maxMemXTruthFunFlag,
					rangeX[[n]] = Flatten[{ConstantArray[1,c],ConstantArray[Max[{maxMemXPred,maxMemXTruth}],c]},1];
					,
					rangeX[[n]] = 	Flatten[{ConstantArray[1,c],
											(Max[maxMemXPredResolved[#],maxMemXTruthResolved[#]] &/@ rangeX[[If[n==1,2,1], 1;;c]] )}
									,1];
				];
				,
				(varXs[[n]] == normMemXAxisName),
				rangeX[[n]] = Flatten[{ConstantArray[1/maxMemXTruth,c],ConstantArray[Max[{1/maxMemXTruth,maxMemXPred/maxMemXTruth}],c]},1];
				,
				(varXs[[n]] == lognormMemXAxisName),
				rangeX[[n]] = Flatten[{ConstantArray[1,c],ConstantArray[Max[{2,1+Log[maxMemXPred]/Log[maxMemXTruth]}],c]},1];
			];
			,
			
			rangeX[[1]] = Join[#[varXs[[1]]] & /@ Join[trainKeys,testKeys]];
			rangeX[[2]] = Join[#[varXs[[2]]] & /@ Join[trainKeys,testKeys]];
		];
		domainFlags = domains /. {Symbol[varXs[[1]]] -> #[[1]],Symbol[varXs[[2]]] -> #[[2]]} & /@ (rangeX // Transpose);
		domainFlags = !MemberQ[#,False] & /@ domainFlags; 
		rangeX[[1]] = Pick[rangeX[[1]],domainFlags];
		rangeX[[2]] = Pick[rangeX[[2]],domainFlags];
		rangeX[[1]] = {Min[rangeX[[1]]], If[NumberQ[maxXs[[1]]], maxXs[[1]], Max[rangeX[[1]]]] };
		rangeX[[2]] = {Min[rangeX[[2]]], If[NumberQ[maxXs[[2]]], maxXs[[2]], Max[rangeX[[2]]]] };
		
		(* Organize the graphics *)
		allYValues = Join[trainPoints[[All,3]], testPoints[[All,3]],
						{	
							model [ ReleaseHold[(parameterVector/. {x1->rangeX[[1,1]] , x2->rangeX[[2,1]]})]],
							model [ ReleaseHold[(parameterVector/. {x1->rangeX[[1,1]] , x2->rangeX[[2,2]]})]],
							model [ ReleaseHold[(parameterVector/. {x1->rangeX[[1,2]] , x2->rangeX[[2,1]]})]],
							model [ ReleaseHold[(parameterVector/. {x1->rangeX[[1,2]] , x2->rangeX[[2,2]]})]]
						}
					];
		rangeY = {Min[Select[allYValues,#>-$MaxMachineNumber&]],Max[Select[allYValues,#<$MaxMachineNumber&]]}; 
		
		
		colorFunction =(
							ColorData["VisibleSpectrum"][(# - rangeY[[1]])/(If[rangeY[[2]]!=rangeY[[1]],rangeY[[2]] - rangeY[[1]],1])
							* (colourDomain[[2]]-colourDomain[[1]]) + colourDomain[[1]] ]
						&);
						
		(*trainDensityPlot,trainContourPlot,predDensityPlot,predContourPlot*)
		(* Density train and pred. plots *)
		If[Length[trainPoints]>1,
			trainDensityPlot = ListDensityPlot[	trainPoints, BoundaryStyle -> {Thick, Black}, PlotRange -> {Full, Full, Full}, 
   												ColorFunction -> colorFunction, ColorFunctionScaling -> False, InterpolationOrder -> 2];
		];
   											
		predDensityPlot = DensityPlot[	model[parameterVector],
										{x1,rangeX[[1,1]] ,rangeX[[1,2]]}, {x2,rangeX[[2,1]] ,rangeX[[2,2]]},
										BoundaryStyle -> {Thick, Red}, PlotRange -> {Full, Full, Full},
										ColorFunction -> colorFunction, ColorFunctionScaling -> False,
										PlotLegends -> BarLegend[Automatic, LegendLabel -> metricModel["metricName"], 
		     								LabelStyle -> {Italic, FontFamily -> "Helvetica"}]
		     				];
		     				
		(* Contour train and pred. plots *)
		If[Length[trainPoints]>1,
			trainContourPlot = ListContourPlot[trainPoints, PlotRange -> Full, ContourShading -> None, ContourStyle -> Black, 
  												InterpolationOrder -> 2, Contours -> contours];
		];
   								
		predContourPlot = ContourPlot[	model[parameterVector],
										{x1,rangeX[[1,1]] ,rangeX[[1,2]]}, {x2,rangeX[[2,1]] ,rangeX[[2,2]]},
										PlotRange -> Full, ContourShading -> None, ContourStyle -> Red, 
  										Contours -> contours
  							];
		     				
		(* Show the graphics *)
		If[Length[trainPoints]>1,
			Show[	predDensityPlot, trainDensityPlot, trainContourPlot, predContourPlot, Frame -> True, 
  					FrameLabel -> {Style[varXs[[1]], Black, 14], Style[varXs[[2]], Black, 14]},
  					ImageSize -> 500]
     		,
			Show[	predDensityPlot, predContourPlot, Frame -> True, 
  					FrameLabel -> {Style[varXs[[1]], Black, 14], Style[varXs[[2]], Black, 14]},
  					ImageSize -> 500]
		]		
	];
	
	If[Length[varXs]==1,
		investigation2Dplot[]
		,
		investigation3Dplot[]
	]
	
]

(* Given a metricModel, and the chose axis, set the additional parameters (if any) and plot the model. *)
ExtrapolationInvestigationPlotViwer[metricModel_ , instrCountModel_
									, trainAlgAssociation_, testAlgAssociation_, varX1_, varX2_, vLength_,dLength_] :=
DynamicModule[{parameterList, allPredictorNames, domains, rangeM, flag,
				extrapolationPlot, memFunctionFlag ,m, controlGridVector, showAbsolute = False, trainKeys,testKeys,k, varXs,
				fastControlGridVector,fastControlGridOptions, maxXs={Automatic, Automatic}},
	
	varXs = If[varX1!=varX2 && varX1!="--" && varX2!="--" , {varX1,varX2}, {varX1}];
	
	trainKeys = GetScalingConfigurations[trainAlgAssociation];
	testKeys = GetScalingConfigurations[testAlgAssociation];
				
	allPredictorNames = If[metricModel["metricType"]=="memFunction",
		Append[metricModel["predictorNames"],memXAxisName ],metricModel["predictorNames"] ];
	
	
	(* Get the model of interest *)
	If[metricModel["metricType"] == "memFunction",
		memFunctionFlag = True;
		,
		memFunctionFlag = False;
	];
	
		
	(* Setup parameter lists and default values *)
	parameterList = Complement[allPredictorNames, varXs ];
	If[Intersection[varXs,GetMemXIndicesNames[{}]]=!=List[],(* memX axis visualization only is reference to maXmemX *)
		parameterList = Complement[parameterList,{memXAxisName}];
	];
	parameterList = {parameterList, Reap[Sow[Nothing];Do[
										domains = FunctionDomain[#,Symbol[m]] & /@ metricModel["predictorTerms"];
										If[ memFunctionFlag && (m == memXAxisName),
											rangeM = Union[Flatten[ GetKeyValue[#, metricModel["metricName"]][[All, 1]]
														& /@ Union[trainAlgAssociation[#][[1,1]] & /@ trainKeys,
																	testAlgAssociation[#][[1,1]] & /@ testKeys]
																]];
											rangeM = Complement[rangeM,{0}];
											,
											rangeM = Union[#[m]& /@ trainKeys, #[m]& /@ testKeys];
										];
										flag = domains /. Symbol[m] -> # & /@ rangeM;
										flag = !MemberQ[#,False] & /@ flag;
										rangeM = Pick[rangeM,flag];
										If[ memFunctionFlag && (m == memXAxisName),
											Sow[{1,rangeM}];(*Sow[{Min[rangeM],rangeM}];*)
											,
											Sow[{Max[rangeM],rangeM}];
										];
										
										,{m,parameterList}]
									][[2,1]]
								};
	parameterList = Insert[parameterList,parameterList[[2,All,2]], 3];
	parameterList[[2]] = parameterList[[2,All,1]];
	parameterList = Transpose[parameterList];
	
	(* Initialize default plot *)
	extrapolationPlot = Dynamic[
							ExtrapolationInvestigationPlot[metricModel, instrCountModel,
															trainAlgAssociation, testAlgAssociation, varXs , maxXs,
															parameterList,showAbsolute, vLength, dLength]
							, TrackedSymbols :> {maxXs,parameterList,showAbsolute} , SynchronousUpdating -> False];
	
	(* Setup parameter control grid *)
	controlGridVector =  Join[{parameterList[[#,1]] ,PopupMenu[Dynamic[ parameterList[[#,2]] ] , parameterList[[#,3]]] , "" }
										& /@ Range[Length[parameterList]] ];
	If[metricModel["metricType"] == "instrPercentage" || metricModel["metricType"] == "vinstrPercentage",
		controlGridVector =  Join[controlGridVector,{{"Show absolute count",Checkbox[Dynamic[showAbsolute]],""}}];
	];
	controlGridVector = Grid[controlGridVector,Alignment->{{Right,Left,Center}}];
	
	(* Setup parameter fast control grid *)
	fastControlGridOptions = Reap[Sow[Nothing];
		Do[
			fastControlGridOptions = parameterList;
			
			fastControlGridOptions = fastControlGridOptions /. ({#, _ , myPattern_} -> {#, k[[#]], myPattern} & /@ Keys[k]);
			
			Sow[fastControlGridOptions -> k];
		,{k,Union[KeyTake[Union[trainKeys,testKeys] , parameterList[[All,1]] ]]}];
	][[2,1]];
	fastControlGridVector = PopupMenu[Dynamic[parameterList], fastControlGridOptions];
	
	(* Show the graphics *)
	Grid[{
		{controlGridVector,fastControlGridVector},
		{
			("Maximum "<>varX1<> ": ")InputField[Dynamic[maxXs[[1]]],Number,FieldSize->Small],
			If[varX2 != "--",
				("Maximum "<>varX2<> ": ")InputField[Dynamic[maxXs[[2]]],Number,FieldSize->Small],
				Nothing
			]
		},
		{extrapolationPlot,SpanFromLeft}
	}]
];

ExtrapolationScatterPlot[metricModel_, instrCountModel_,
	trainAlgAssociation_, testAlgAssociation_, logScale_, showAbsolute_,vLength_,dLength_] :=
Module[{trainPoints,testPoints,allPoints,trainPred,testPred,XY,Yact,Ypred,yTrainPred,yTestPred,
		lPlot,plot,listPlotTrain,listPlotTest,bisectionPlot,plotRange,gap,a,memXOK,trainKeys,testKeys,mask},
	
	trainKeys = GetScalingConfigurations[trainAlgAssociation];
	testKeys = GetScalingConfigurations[testAlgAssociation];
	(* Gather predictions for the train and the test sets *)
	If[metricModel["metricName"] == instrCountName,
		trainPred = ExtrapolateSingleThreadAlgorithm[{metricModel}, #][[1]] & /@trainKeys;
		testPred  = ExtrapolateSingleThreadAlgorithm[{metricModel}, #][[1]] & /@testKeys;
		,
		trainPred = ExtrapolateSingleThreadAlgorithm[{metricModel,instrCountModel}, #]
							[[1]] & /@trainKeys;
		testPred  = ExtrapolateSingleThreadAlgorithm[{metricModel,instrCountModel}, #]
							[[1]] & /@testKeys;
	];
	
	yTrainPred = GetKeyValue[#,metricModel["metricName"]] & /@ trainPred;
	yTestPred  = GetKeyValue[#,metricModel["metricName"]] & /@ testPred;
	
	If[(metricModel["metricType"]=="instrPercentage") && showAbsolute,
		yTrainPred = yTrainPred * (GetKeyValue[#, instrCountName ]& /@ trainPred);
		yTestPred = yTestPred *   (GetKeyValue[#, instrCountName ]& /@ testPred);
	];
	
	If[(metricModel["metricType"]=="vinstrPercentage") && showAbsolute,
		yTrainPred = VMixGetAbsoluteCount[ yTrainPred[[#]], GetKeyValue[trainPred[[#]], instrCountName]] & /@ Range[Length[trainPred]];
		yTestPred =  VMixGetAbsoluteCount[ yTestPred[[#]], GetKeyValue[testPred[[#]], instrCountName]] & /@ Range[Length[testPred]];
	];
	
	(* Get training and test point lists *)
	If[metricModel["metricType"] == "memFunction",
		(* Iterate on each algorithm and push out all the predicted points. *)
		(* TODO: takecare here! *)
		
		If[Length[trainKeys]>0,
			trainPoints = Union @@
				Reap[For[a=1,a<=Length[trainKeys],a++,
					Ypred = yTrainPred[[a]];
					Yact = GetKeyValue[trainAlgAssociation[trainKeys[[a]]][[1,1]],metricModel["metricName"]];
					memXOK = Intersection[Ypred[[All,1]],Yact[[All,1]]];
					Ypred = Select[Ypred,MemberQ[memXOK,#[[1]]] &];
					Yact = Select[Yact,MemberQ[memXOK,#[[1]]] &];
					Sow[{Yact[[All,2]],Ypred[[All,2]]} // Transpose ];
				]][[2,1]];
			trainPoints = Flatten[#] & /@ trainPoints; 
		];
		
		If[Length[testKeys]>0,
			testPoints = Union @@
				Reap[For[a=1,a<=Length[testKeys],a++,
					Ypred = yTestPred[[a]];
					Yact = GetKeyValue[testAlgAssociation[testKeys[[a]]][[1,1]],metricModel["metricName"]];
					memXOK = Intersection[Ypred[[All,1]],Yact[[All,1]]];
					Ypred = Select[Ypred,MemberQ[memXOK,#[[1]]] &];
					Yact = Select[Yact,MemberQ[memXOK,#[[1]]] &];
					Sow[{Yact[[All,2]],Ypred[[All,2]]} // Transpose ];
				]][[2,1]];
			testPoints = Flatten[#] & /@ testPoints;
			
			,
			testPoints={};
		];
		
		,
		
		(* Get the metric ("numeric"), no denormalization/normalization ("instrPercentage"). So the plot will display the actual metric. *)
		If[Length[trainKeys]>0,
			XY = GetXY[metricModel["metricName"], metricModel["metricType"] , metricModel["predictorNames"], trainAlgAssociation,Null,{},"ShowAbsolute"->showAbsolute];
			Yact = XY[[2]];
			If[metricModel["metricType"] == "vinstrPercentage" || metricModel["metricType"] == "vnumeric",
				Yact = VMixGetData[VMixAssociation2KeyValueList[Yact],"VectorLength" -> vLength,"DataLength" -> dLength];
				trainPoints = {Flatten[Yact],Flatten[VMixGetData[#,"VectorLength" -> vLength,"DataLength" -> dLength] & /@ yTrainPred]} // Transpose;
				,
				trainPoints = {Flatten[Yact],Flatten[yTrainPred]} // Transpose;	
			];
			,
			trainPoints = {};
		];
		If[Length[testKeys]>0,
			XY = GetXY[metricModel["metricName"], metricModel["metricType"] , metricModel["predictorNames"], testAlgAssociation,Null,{},"ShowAbsolute"->showAbsolute];
			Yact = XY[[2]];
			If[metricModel["metricType"] == "vinstrPercentage" ||  metricModel["metricType"] == "vnumeric",
				Yact = VMixGetData[VMixAssociation2KeyValueList[Yact],"VectorLength" -> vLength,"DataLength" -> dLength];
				testPoints = {Flatten[Yact],Flatten[VMixGetData[#,"VectorLength" -> vLength,"DataLength" -> dLength] & /@ yTestPred]} // Transpose;
				,
				testPoints = {Flatten[Yact],Flatten[yTestPred]} // Transpose;
			];
			,
			testPoints = {};
		];
	];
	
	(* Remove values that are not machine size *)
	mask =  -$MaxMachineNumber<#[[1]] && -$MaxMachineNumber<#[[2]] && 
			$MaxMachineNumber>#[[1]] && $MaxMachineNumber>#[[2]] & /@ trainPoints;
	If[MemberQ[mask,False],
		Print["Some training point values in the scatter plot are not machine numbers and are not visualized (possible overfitting)."];
		trainPoints = Pick[trainPoints,mask];
	];
	mask =  -$MaxMachineNumber<#[[1]] && -$MaxMachineNumber<#[[2]] && 
			$MaxMachineNumber>#[[1]] && $MaxMachineNumber>#[[2]] & /@ testPoints;
	If[MemberQ[mask,False],
		Print["Some test point values in the scatter plot are not machine numbers and are not visualized (possible overfitting)."];
		testPoints = Pick[testPoints,mask];
	];
	
	(* Setup plotting interface *)
	If[logScale,
		lPlot = ListLogLogPlot;
		plot = LogLogPlot;
		,
		lPlot = ListPlot;
		plot = Plot;
	];
	allPoints = Union[trainPoints,testPoints];
	If[logScale,
		allPoints = Select[allPoints,(#[[1]] > 0) && (#[[2]] > 0) &];
		plotRange = {{Min[Flatten[allPoints]]*0.95,Max[Flatten[allPoints]]*1.05 },
				 	{Min[Flatten[allPoints]]*0.95,Max[Flatten[allPoints]]*1.05 }};
		,
		gap = (Max[Flatten[allPoints]] - Min[Flatten[allPoints]]) * 0.05;
		If[gap == 0, gap = Max[Flatten[allPoints]] * 0.05];
		plotRange = {{Min[Flatten[allPoints]]-gap,Max[Flatten[allPoints]]+gap },
				 	{Min[Flatten[allPoints]]-gap,Max[Flatten[allPoints]]+gap }};
	];
	
	(* Organize the graphics *)
	If[Length[trainPoints]>0,
		listPlotTrain = lPlot[trainPoints,PlotStyle->{Black,PointSize[0.02]}];
	];
	If[Length[testPoints]>0,
		listPlotTest = lPlot[testPoints,PlotStyle->{Red,PointSize[0.02]}];
	];
	
	If[plotRange[[1,1]] != plotRange[[2,2]],
		bisectionPlot = plot[x, {x,Min[plotRange[[All,1]]],Max[plotRange[[All,2]]] } ,
							AxesLabel -> {"Observed "<>metricModel["metricName"],"Predicted "<>metricModel["metricName"]}
							(*,AxesOrigin -> {Max[0,rangeX[[1]]-rangeX[[3]]],Automatic}*), PlotRange -> plotRange,
							PlotStyle->{Blue,Dashed}
					];
		,
		bisectionPlot = Null;
	];
				
	(* Show the graphics *)
	Which[
		bisectionPlot === Null,
		Style["Scatter plot not generated because the metric is constant.",Gray],
		
		(Length[trainPoints]>=1) && (Length[testPoints]>=1),
		Show[bisectionPlot,listPlotTrain ,listPlotTest, ImageSize -> 500],
		
		(Length[trainPoints]>=1)  ,
		Show[bisectionPlot,listPlotTrain, ImageSize -> 500],
		
		(Length[testPoints]>=1)  ,
		Show[bisectionPlot,listPlotTest, ImageSize -> 500]
	]
	
	
];

ExtrapolationScatterPlotViewer[metricModel_ , instrCountModel_, trainAlgAssociation_, testAlgAssociation_,
								vLength_,dLength_] :=
Module[{logScale,showAbsolute=False},
	If[metricModel["metricType"] == "instrPercentage" || metricModel["metricType"] == "vinstrPercentage",
		Manipulate[
			ExtrapolationScatterPlot[metricModel, instrCountModel, trainAlgAssociation, testAlgAssociation,
										logScale,showAbsolute,vLength,dLength],
			{{logScale,False,"Logarithmic scale"},{False,True}},
			{{showAbsolute,False,"Show absolute instruction count"},{False,True}}
			(*,TrackedSymbols :> {metricModel,instrCountModel,logScale,showAbsolute}*)
			,SynchronousUpdating -> False, Paneled -> False, AppearanceElements -> None
		]
		,
		Manipulate[
			ExtrapolationScatterPlot[metricModel, instrCountModel, trainAlgAssociation, testAlgAssociation,
										logScale,showAbsolute,vLength,dLength],
			{{logScale,False,"Logarithmic scale"},{False,True}}
			,SynchronousUpdating -> False, Paneled -> False, AppearanceElements -> None
		]
	]
	
];

(* Given a metricModel, select the axis and visualize the ExtrapolationInvestigationPlotViwer *)
SetAttributes[ExtrapolationPlotViwerSelection, HoldFirst]; (* This let the first parameters behave as a reference. *)
ExtrapolationPlotViwerSelection[algoModel_ , trainAlgAssociation_, testAlgAssociation_] :=
Module[{	metricName,metricType,varX1,allPredictorNames,m,instrM,
					row1,investigationPlotPrefix,investigationPlot,scatterPlot,
					varX2Flag=False,varX2="--",vLength="Accumulate",dLength="Accumulate"},
	(* Keep the formatting in the tabulator: row1 <- select metricName; row2 <- select varX1 ; row3 <- select parameter list & plot *)
	
	(**** Variable initialization ****)
	instrM = Position[algoModel[[All,"metricName"]], instrCountName][[1,1]];
	m = 1;
	metricName = algoModel[[ m,"metricName" ]];
	metricType = algoModel[[ m,"metricType" ]]; 
	allPredictorNames = If[metricType=="memFunction",
							Flatten[{algoModel[[m,"predictorNames"]],GetMemXIndicesNames[{}]}](* memX axis visualization only is reference to maXmemX *)
							, algoModel[[m,"predictorNames"]] ];
	
	(**** Metric selection and dynamically update variable status ****)
	row1 = 	Grid[{{Style["Metric to be analyzed:",summaryHeadingFormat],
				PopupMenu[
					Dynamic[metricName,
						metricName = #;
						m = Position[algoModel[[All,"metricName"]], metricName][[1,1]];
						metricType = algoModel[[ m,"metricType" ]] ; 
						allPredictorNames = If[metricType=="memFunction",
												Flatten[{algoModel[[m,"predictorNames"]],GetMemXIndicesNames[{}]}]
												, algoModel[[m,"predictorNames"]] ];
						varX1 = allPredictorNames[[1]];
						If[!varX2Flag,varX2="--"];
						&,
						TrackedSymbols :> {varX1,varX2Flag}
					]
				,algoModel[[All,"metricName"]] ]
			}}];
	
	(**** Select free parameter varX1. On change, update the list of parameters that must be set ups. ****)
	investigationPlotPrefix = Grid[{
		{Style["Model Investigation",summaryHeadingFormat],SpanFromLeft},
		{
			"x1 axis:"
			,Dynamic[PopupMenu[ Dynamic[varX1, varX1=#;If[!varX2Flag,varX2="--"];& ] ,allPredictorNames]]
			,"\tx2 axis"
			,Dynamic[Checkbox[ Dynamic[varX2Flag,varX2Flag=#;If[!varX2Flag,varX2="--"];&] ,{False,True}]],":"
			,Dynamic[PopupMenu[ Dynamic[varX2] , Complement[allPredictorNames,Union[GetMemXIndicesNames[{}],{varX1}] ],
					Enabled -> If[varX2Flag,True,False],
					BaseStyle->If[varX2Flag,Automatic,Tiny],
					Background->If[varX2Flag,Automatic,LightGray],
					MenuStyle->If[varX2Flag,Automatic,Gray]],
					TrackedSymbols :> {varX1,varX2Flag,allPredictorNames}]
		}
	}];
	(**** Setup interface for other parameter list (if needed), and plot result ****)
	investigationPlot = Dynamic[
			ExtrapolationInvestigationPlotViwer[algoModel[[ m ]] , algoModel[[ instrM ]],
												trainAlgAssociation, testAlgAssociation,varX1,varX2,vLength,dLength]
				(*,TrackedSymbols :> {varX1,varX2,m,algoModel}*), SynchronousUpdating -> False];
	(**** Setup the scatter plot ****)
	scatterPlot = Dynamic[ExtrapolationScatterPlotViewer[algoModel[[ m ]] , algoModel[[ instrM ]],
													 trainAlgAssociation, testAlgAssociation,vLength,dLength]
					(*,TrackedSymbols :> {m,algoModel}*), SynchronousUpdating -> False];
	
	Panel[ Grid[{ {row1,SpanFromLeft},
				{ Style["Scatter Plot",summaryHeadingFormat],	investigationPlotPrefix},
				{ scatterPlot, 									investigationPlot} },
				Frame -> All]
		]
];



Options[FitSingleThreadModelGUI] = {"Verbose"->False,"ClusterId"->0};
(* Visualize models and statistics while fitting the algoModel *)
SetAttributes[FitSingleThreadModelGUI, HoldAll];(* This let the all parameters behave as a reference. *)
(* algoModel, extrapolationList, and mainComplexities could be changed interactiverly. *)
FitSingleThreadModelGUI[algoModel_,ExtrAxTrainAlgInfo_,ExtrAxTestAlgInfo_,targetApplicationName_,
						extrapolationList_, mainComplexities_, selTermsNThreads_,OptionsPattern[]] :=
Module[{m,controlPanel,investigationPanel,verbose, clusterId,
		trainAlgAssociation, testAlgAssociation},
	verbose = OptionValue["Verbose"];
	clusterId = OptionValue["ClusterId"];
	trainAlgAssociation = ExtrAxTrainAlgInfo[[targetApplicationName]];
	testAlgAssociation = ExtrAxTestAlgInfo[[targetApplicationName]];
	If[MissingQ[testAlgAssociation],testAlgAssociation=Association[]];
				
	(* First train the algorithm extrapolation model if currently uninitialized *)
	If[!ValueQ[algoModel],
		algoModel = FitSingleThreadExtrapolationModel[trainAlgAssociation,
						"ExtrapolationList"->extrapolationList,"Verbose"->verbose,
						"MainComplexities"->mainComplexities];
	];
	
	controlPanel = ExtrapolationListControlGUI[extrapolationList, mainComplexities, ExtrAxTrainAlgInfo,
												targetApplicationName, ExtrAxTestAlgInfo, algoModel,
												Evaluate[clusterId], True, selTermsNThreads, "Verbose"->verbose];
	
	
	
	(**** Fit visualization section ****)
	investigationPanel = ExtrapolationPlotViwerSelection[
		If[MultiThreadModelQ[algoModel],algoModel[[clusterId]],algoModel],trainAlgAssociation,testAlgAssociation];
	
	(* show the whole interface *)
	Grid[{{controlPanel},{investigationPanel}}]
	(*Grid[{{controlPanel}}]*)
];

Options[ExtrapolationListControlRow] = {	"Verbose"->False
									};
(*
	All optional parameters are mandatory if the control row is for inspection and retraining,
	useless if the row is for overall initialization.
*)
SetAttributes[ExtrapolationListControlRow, HoldAll];
ExtrapolationListControlRow[extrapolationList_,algInfo_,id_, testAlgInfo : Except[_Rule] :  Association[],
							model : Except[_Rule] :  {}, accuracies : Except[_Rule] :  {}, reTrain : Except[_Rule] :  False,
							mainComplexities : Except[_Rule] :  Automatic,
							selTermsNThreads : Except[_Rule] :  {},OptionsPattern[]] := Module[
	{
		metricName, configurations, availableMetrics, suggestedMetrics, metricOptions, metricColor,
		metricTypeOptions, modelTypeOptions, addAfter, addBefore, delete, retrain,
		res,
		defaultNewRow, instrMIndex, localExtrapolationList, verbose
	},
	verbose = OptionValue["Verbose"];
	
	(*
		The result length depends on wether or not the wether or not accuracy statistics are to be shown.
		Statistics are shown only when an extrapolation model is to be inspected (and possibly retrained).
	*)
	If[reTrain,
		res = Range[9];
		
		,
		res = Range[8];
	];
	
	
	(* Initialize the available metrics *)
	If[!MissingQ[algInfo] && Length[algInfo]>1,
		configurations 		= GetScalingConfigurations[algInfo];
		availableMetrics 	= Flatten[(Values[algInfo[#]] [[1, All, All, 1]] & /@ configurations),1]; 
		availableMetrics 	= Intersection@@ availableMetrics;
		AppendTo[availableMetrics, "NThreads"];
		,
		availableMetrics 	= {};
	];
	suggestedMetrics	= Keys[GetExtrapolationMetricTypeOptions[]];
	
	metricColor[mName_] :=
		If[!reTrain,
			If[MemberQ[availableMetrics, mName],
				If[Count[extrapolationList[[All,1]], mName ]>1,
					(* Metric listed twice *)
					Red
					,
					If[MemberQ[suggestedMetrics, mName],
						(* Ok *)
						If[GetExtrapolationMetricTypeOptions[][[mName]] === {"notSupported"},
							Red,
							Black
						],
						(* Not recommended but possible *)
						Darker[Orange]
					]
				]
				,
				(* Metric not available in some PISA profiles *)
				Red
			]
			,
			If[mName === model[[id,"metricName"]],
				Black,
				Red
			]
		];
	
	metricOptions = (# -> Style[ #,metricColor[#] ]) & /@ Union[suggestedMetrics, availableMetrics];
	
	
	
	(* metric name selection *)
	metricName = extrapolationList[[id, 1]];
	res[[1]] = PopupMenu[ Dynamic[extrapolationList[[id, 1]]], metricOptions ,Enabled->!reTrain];
	
	(* metric type selection *)
	metricTypeOptions = If[!MissingQ[GetExtrapolationMetricTypeOptions[][[metricName]]],
		GetExtrapolationMetricTypeOptions[][[metricName]],
		Complement[Union[Flatten[Values[GetExtrapolationMetricTypeOptions[]],1]],{"notSupported"}]
	];
	res[[2]] = PopupMenu[ Dynamic[extrapolationList[[id, 2]]], metricTypeOptions ];
	
	(* model type selection *)
	modelTypeOptions = If[!MissingQ[
								GetExtrapolationMetricType2ModelTypes[][[extrapolationList[[id, 2]]]]
							],
		GetExtrapolationMetricType2ModelTypes[][[extrapolationList[[id, 2]]]],
		Union[Flatten[Values[GetExtrapolationMetricType2ModelTypes[]],1]]
	];
	res[[3]] = PopupMenu[ Dynamic[extrapolationList[[id, 3]]], modelTypeOptions ];
	
	(* model terms selection and result visualization *)
	res[[4]] = If[!reTrain,
					InputField[Dynamic[extrapolationList[[id, 4]]] , String, FieldSize->Small],
					Grid[{
						{InputField[Dynamic[extrapolationList[[id, 4]]] , String, FieldSize->Small,Enabled->True]},
						{
							Button["Show model construction result",
								MessageDialog[
									
									Dynamic["{" <>
										StringJoin[Riffle[ToString[StandardForm[ScientificForm[#,4]]] & /@ 
											model[[id,"predictorTerms"]],", "] ] <> "}"
										,TrackedSymbols :> {model}]
									,
									{"Paste in extrapolation list and close.":>  
										(extrapolationList[[id, 4]] = "{" <>
										StringJoin[Riffle[ToString[#,InputForm] & /@ 
											model[[id,"predictorTerms"]],", "] ] <> "}")
									,"Close.":>Null}
									,
									WindowTitle -> ("Terms found for "<>extrapolationList[[id, 1]]), WindowSize->{1100, 500}
								];
							]
						}
					}]
	];
		
	(* model constraints selection *)
	res[[5]] = InputField[Dynamic[extrapolationList[[id, 5]]], String, FieldSize->Small];
	
	(* Fast or Accurate? *)
	res[[6]] = PopupMenu[Dynamic[extrapolationList[[id, 6]]], {"False"->"Accurate", "True"->"Fast"}];
	
	(* metric boundaries *)
	res[[7]] = InputField[Dynamic[extrapolationList[[id, 7]]], String, FieldSize->Small];
	
	If[reTrain,
		(* Accuracy measure *)
		res[[8]] = Grid[{If[NumberQ[#],NumberForm[#,{4,3}],#] &/@ accuracies[[id]]}, ItemSize->5.9];
		
		(* Retrain button *)
		retrain = Button["Train",
			localExtrapolationList = {extrapolationList[[id,All]]};
			
			(* instrMIndex will be needed when asking for retraining *)
			instrMIndex = Position[extrapolationList[[All,1]], instrCountName ][[1,1]];
		
			model[[id]] = FitSingleThreadExtrapolationModel[algInfo,
											"MainComplexities"->mainComplexities,
											"ExtrapolationList"->localExtrapolationList,
											"Verbose"->verbose,
											"SelModelInstrCount" -> If[instrMIndex==0,
																		Null,
																		model[[instrMIndex]]
																	],
											"SelTermsNThreads" ->selTermsNThreads
										][[1]];
		,Method->"Queued"];
		res[[9]] = retrain;
		
		,
		(* Add and remove buttons *)
		defaultNewRow := {"", "numeric", "WeightedLinearRegression", ".", ".", "False", "{0,Infinity}" };
		
		addAfter = Button["Add after",
			extrapolationList = Insert[extrapolationList,defaultNewRow,id+1];
		];
		addBefore = Button["Add before",
			 extrapolationList = Insert[extrapolationList,defaultNewRow,id];
		];
		delete = Button["Delete",
			 extrapolationList = Drop[extrapolationList,{id}];
		];
		res[[8]] = Grid[{{addAfter, addBefore, delete} }];
	];
	
	Return[res];
];

(* Format accuracies to list them in the ExtrapolationListControlGUI when retraining is enabled *)
FormatAccuracies[trainAlgInfo_,testAlgInfo_,testFlag_,eList_,extrapolationModel_] := Module[
	{RETrain, RETest, r2, metricNames, tmpRes, m, tmpAlgInfo
		,tmpExtrapolationModel},
	metricNames = eList[[All,1]];
	
	
	RETrain = ExtrapolationDiagnosticRE[extrapolationModel,trainAlgInfo,metricNames];
	
	RETrain = Reap[Sow[Nothing];Do[
		tmpRes = RETrain[[m]];
		If[eList[[m,2]] == "memFunction",
			Sow[ ErrNaNMean[Abs[Flatten[tmpRes[[All,2,All,2]] ] ] ] ];
			,
			Sow[ ErrNaNMean[Abs[Flatten[tmpRes[[All,2]] ] ] ] ];
		];
		
	,{m,Range[Length[metricNames]]}]][[2,1]];
	
	r2 = ExtrapolationDiagnosticR2[extrapolationModel,trainAlgInfo,metricNames];
	r2 = r2[[All,2]];
	
	If[testFlag,
		RETest = ExtrapolationDiagnosticRE[extrapolationModel,testAlgInfo,
											metricNames];
											
		RETest = Reap[Sow[Nothing];Do[
			tmpRes = RETest[[m]];
			If[eList[[m,2]] == "memFunction",
				Sow[ ErrNaNMean[Abs[Flatten[tmpRes[[All,2,All,2]] ] ] ] ];
				,
				Sow[ ErrNaNMean[Abs[Flatten[tmpRes[[All,2]] ] ] ] ];
			];
			
		,{m,Range[Length[metricNames]]}]][[2,1]];
		
		,
		RETest = "-" & /@ metricNames;
	];
	
	
	Return[N[{r2, RETrain, RETest}//Transpose]];
];

Options[ExtrapolationListControlGUI] = {	"Verbose"->False
									};
(* Edit the extrapolation list by adding/removing metrics or by enabling refitting *)
SetAttributes[ExtrapolationListControlGUI, HoldAll];
ExtrapolationListControlGUI[extrapolationList_,mainComplexities_,ExtrAxTrainAlgInfo_,
	targetApplicationName_, ExtrAxTestAlgInfo : Except[_Rule] :  Association[], algoModel : Except[_Rule] :  Association[],
	clusterId : Except[_Rule] :  0,	reTrain : Except[_Rule] :  False, selTermsNThreads : Except[_Rule] :  {},OptionsPattern[]] :=
Module[{
	predictorNames, predictorNameMessage, controlHeading, controlGrid, complexityControl, id,
	accuracies, testFlag, verbose, ExtrAxTrainAlgInfoCpy, ExtrAxTestAlgInfoCpy,
	tmpExtrapolationList, tmpModel, m, mId0, mId
	},
	(* Initialize *)
	verbose = OptionValue["Verbose"];
	
	(* Initialize copies are local not to allow external changes *)
	ExtrAxTrainAlgInfoCpy = ExtrAxTrainAlgInfo;
	ExtrAxTestAlgInfoCpy = ExtrAxTestAlgInfo;
	
	(* Initialize test flag and setup accuracy computation *)
	testFlag = reTrain && (!MissingQ[ExtrAxTestAlgInfo[targetApplicationName]]);
	accuracies = {"","",""} & /@ Range[Length[extrapolationList]];
	
	
	(* Compute predictor names *)
 	predictorNames = Dynamic[
 		If[ MissingQ[ExtrAxTrainAlgInfo[[targetApplicationName]] ],
 			{}
 			,
	 		ToString[
	 			GetXNames[ExtrAxTrainAlgInfo[[targetApplicationName]]]
	 		]
 		]
 	,TrackedSymbols :> {ExtrAxTrainAlgInfo,targetApplicationName}
 	,SynchronousUpdating->False];
 	predictorNameMessage := Grid[{
			{ExtrAxBodyStyle["Scaling parameters for " <>
						"any extrapolation model:"],
						ExtrAxBodyStyle[predictorNames]},
			{ExtrAxBodyStyle["Additional reuse distance parameters " <>
						"for \"D0dreuse\" and \"D0ireuse\":"],
						ExtrAxBodyStyle[ToString[
									{
										memXAxisName,
										normMemXAxisName,
										lognormMemXAxisName
									}
								]]
				}
		},Alignment->{{Right,Center},{Left,Center}},Frame->All];
			
	(* Control grid setup *)
	controlHeading =  If[!reTrain,
		{
			{
				ExtrAxBoldStyle["Metric name"] , ExtrAxBoldStyle["Extrapolation options"], 
				SpanFromLeft, SpanFromLeft, SpanFromLeft, SpanFromLeft, SpanFromLeft,
				ExtrAxBoldStyle["Add and remove"]
			} ,
			{
				SpanFromAbove,
				ExtrAxBoldStyle["Metric type"],ExtrAxBoldStyle["Model type"],
				ExtrAxBoldStyle["Terms"],ExtrAxBoldStyle["Constraints"],
				ExtrAxBoldStyle["Accurate"],ExtrAxBoldStyle["Boundaries"],
				SpanFromAbove
			}
		},
		{
			{
				ExtrAxBoldStyle["Metric name"] , ExtrAxBoldStyle["Extrapolation options"], 
				SpanFromLeft, SpanFromLeft, SpanFromLeft, SpanFromLeft, SpanFromLeft,
				Dynamic[(* Visualize "Accuracy" label and make sure that accuracies are updated *)
					accuracies = 
					FormatAccuracies[
						ExtrAxTrainAlgInfoCpy[[targetApplicationName]],
						If[testFlag,ExtrAxTestAlgInfoCpy[[targetApplicationName]],Association[]],testFlag,
						extrapolationList, If[MultiThreadModelQ[algoModel],algoModel[[clusterId]],algoModel]
					];
					ExtrAxBoldStyle["Accuracy"]
				,TrackedSymbols:>{algoModel}
				,SynchronousUpdating->False]
				, ExtrAxBoldStyle["Train"]
			} ,
			{
				SpanFromAbove,
				ExtrAxBoldStyle["Metric type"],ExtrAxBoldStyle["Model type"],
				ExtrAxBoldStyle["Terms"],ExtrAxBoldStyle["Constraints"],
				ExtrAxBoldStyle["Accurate"],ExtrAxBoldStyle["Boundaries"],
				Grid[{{
					ExtrAxBoldStyle["R2\ntrain"],ExtrAxBoldStyle["MRE\ntrain"],ExtrAxBoldStyle["MRE\ntest"]
				}}, ItemSize->5.9, Alignment->{Center,Center}],SpanFromAbove
			}
		}
	];
	
	controlGrid = Dynamic[
		Grid[
			Join[
				controlHeading,
				Reap[ Sow[Nothing];
					Do[
						If[MultiThreadModelQ[algoModel],
							Sow[ExtrapolationListControlRow[
								extrapolationList,
								ExtrAxTrainAlgInfo[[targetApplicationName]],
								Evaluate[id],
								Evaluate[If[testFlag,ExtrAxTestAlgInfo[[targetApplicationName]],Association[]]],
								algoModel[[ clusterId ]],
								accuracies,
								reTrain,
								mainComplexities, selTermsNThreads,
								"Verbose"->verbose
							]];
							,
							Sow[ExtrapolationListControlRow[
								extrapolationList,
								ExtrAxTrainAlgInfo[[targetApplicationName]],
								Evaluate[id],
								Evaluate[If[testFlag,ExtrAxTestAlgInfo[[targetApplicationName]],Association[]]],
								algoModel,
								accuracies,
								reTrain,
								mainComplexities, selTermsNThreads,
								"Verbose"->verbose
							]];
						];
					,{id,Range[Length[extrapolationList]]}];
				][[2,1]]
			]
		, Frame->All, Alignment->{Center,Center}
		, Background->
			{None,{
				{
					{LightGreen, LightBrown}
				}, 
				{
					1 -> LightYellow, 2 -> LightYellow
				}
			}}
		]
	,TrackedSymbols :> {extrapolationList, ExtrAxTrainAlgInfo, targetApplicationName,accuracies}
	,SynchronousUpdating->False];
	
	(* Control the complexities *)
	complexityControl = Grid[ 
				{{ExtrAxBoldStyle["Order of complexity definitions:"]},
				{InputField[Dynamic[mainComplexities], FieldSize->Large]}} 
			,Frame->True];
			
	Return[
		Grid[{
				{ complexityControl, SpanFromLeft },
				{ Dynamic[predictorNameMessage,TrackedSymbols :> {predictorNameMessage}], SpanFromLeft },
				{ controlGrid,SpanFromLeft, SpanFromLeft },
				If[!reTrain,
					{
						Button["RestoreDefault",
							extrapolationList = DefaultExtrapolationList[];
						]
					,SpanFromLeft}
					,
					
					{
						Button["Retrain all",
							(* Retrain all, indiscriminately *)
							If[MultiThreadModelQ[algoModel],
								algoModel[[clusterId]] = FitSingleThreadExtrapolationModel[
									ExtrAxTrainAlgInfo[[targetApplicationName]],
									"Verbose" -> verbose, 
									"ExtrapolationList" -> extrapolationList, 
									"MainComplexities" -> mainComplexities,
									"SelTermsNThreads" ->selTermsNThreads
								];
								,
								algoModel = FitSingleThreadExtrapolationModel[
									ExtrAxTrainAlgInfo[[targetApplicationName]],
									"Verbose" -> verbose, 
									"ExtrapolationList" -> extrapolationList, 
									"MainComplexities" -> mainComplexities,
									"SelTermsNThreads" ->selTermsNThreads
								];
							];
						],
						Button["Retrain all metrics that depends on \""<>instrCountName<>"\"",
							(* Find all LSys, instrPercentage, vinstrPercentage and train their models *)
							tmpExtrapolationList = Select[Range[Length[extrapolationList]],
															(extrapolationList[[#,1]] == instrCountName ||
															extrapolationList[[#,2]] == "instrPercentage" ||
															extrapolationList[[#,2]] == "vinstrPercentage") ||
															If[MultiThreadModelQ[algoModel],
																algoModel[[clusterId,#,"metricType"]] == "instrPercentage" ||
																algoModel[[clusterId,#,"metricType"]] == "vinstrPercentage"
																,
																algoModel[[#,"metricType"]] == "instrPercentage" ||
																algoModel[[#,"metricType"]] == "vinstrPercentage" 
															] &
													];
							tmpExtrapolationList = extrapolationList[[tmpExtrapolationList]];
													
							tmpModel = FitSingleThreadExtrapolationModel[
									ExtrAxTrainAlgInfo[[targetApplicationName]],
									"Verbose" -> verbose, 
									"ExtrapolationList" -> tmpExtrapolationList, 
									"MainComplexities" -> mainComplexities,
									"SelTermsNThreads" -> selTermsNThreads
								];
							(* Replace the algoModel with the new models *)
							Do[
								m = tmpExtrapolationList[[mId0,1]];
								mId = If[MultiThreadModelQ[algoModel],
									FirstPosition[
										algoModel[[clusterId, All, "metricName"]],m
									],
									FirstPosition[
										algoModel[[All, "metricName"]],m
									]
								];
								If[MultiThreadModelQ[algoModel],
									algoModel[[clusterId,mId]] = tmpModel[[mId0]];
									,
									algoModel[[mId]] = tmpModel[[mId0]];
								];
							,{mId0,Range[Length[tmpExtrapolationList]]}];
						]
					}
				]	
		},Alignment->{Center,Center}]
	];
];


(******************************** GUI ********************************)

  
  
End[] (* End Private Context *)
  
EndPackage[]
