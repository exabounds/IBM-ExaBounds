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

BeginPackage["VectorSizeConvert`"]

Needs["ExaBoundsGeneric`"];
Needs["ModelWarnings`"];
Needs["InstructionMix`"];
(* Exported symbols added here with SymbolName::usage *)
	
GetAlgorithmVectorProperties::usage =
	"GetAlgorithmVectorProperties[archProperties_, algProperties_]"

GetConvertedInstructionFractions::usage =
	"GetConvertedInstructionFractions[algProperties_, maxVector_]" 
  
GetTotalVMixFraction::usage =
	"GetTotalVMixFraction[VMixStructure_, OptionsPattern[]]"
	
GetTotalOperationCount::usage = 
	"GetTotalOperationCount[algProperties_, type_, OptionsPattern[]]"
	
GetMaxVectorSizeBits::usage =
	 "GetMaxVectorSizeBits[algProperties_]"

Begin["`Private`"] (* Begin Private Context *)

(* Wrapper function for VMixGetData to keep support for old scalar-type data *)
Options[GetTotalVMixFraction] = 	{
							"VectorLength" -> "Accumulate",
							"DataLength" -> "Accumulate"
						};
GetTotalVMixFraction[VMixStructure_, OptionsPattern[]] :=
Block[{vectorLength, dataLength},
	vectorLength = OptionValue["VectorLength"];
	dataLength = OptionValue["DataLength"];
	
	If[Head[VMixStructure]=!=List, Return[VMixStructure]];
	
	Return[
		Switch[vectorLength,
			"Scalar", VMixGetData[VMixStructure, VectorLength->1, DataLength->dataLength],
			"Vector", VMixGetData[VMixStructure, VectorLength->"Accumulate", DataLength->dataLength] - VMixGetData[VMixStructure, VectorLength->1, DataLength->dataLength],
			_, VMixGetData[VMixStructure, VectorLength->vectorLength, DataLength->dataLength]
		]
	];
];

(* Wrapper function for VMixGetData to keep support for old scalar-type data *)
Options[GetTotalOperationCount] = 	{
							"VectorLength" -> "Accumulate",
							"DataLength" -> "Accumulate"
						};
GetTotalOperationCount[algProperties_, key_, OptionsPattern[]] :=
Block[{VMixStructure, LSys, lengths, vectorLength, dataLength},
	vectorLength = OptionValue["VectorLength"];
	dataLength = OptionValue["DataLength"];
	
	lengths = GetVectorLengths[];
	
	LSys = GetKeyValue[algProperties, "LSys"];
	VMixStructure = GetKeyValue[algProperties, key];
	
	If[Head[VMixStructure]=!=List, Return[VMixStructure * LSys]];
	
	Return[
		Switch[vectorLength,
			"Scalar", VMixGetData[VMixStructure, VectorLength->1, DataLength->dataLength] * LSys,
			"Vector", Total[((VMixGetData[VMixStructure, VectorLength->#, DataLength->dataLength] - VMixGetData[VMixStructure, VectorLength->1, DataLength->dataLength]) * # * LSys) & /@ lengths],
			_, Total[(VMixGetData[VMixStructure, VectorLength->#, DataLength->dataLength] * # * LSys) & /@ lengths]
		]
	];
];

GetAlgorithmVectorProperties[archProperties_, algProperties_] :=
Block[{vectorProperties, vectorBits},
	vectorBits = GetArchKeyValueCheck[archProperties, "n0vectorbits"];
	
	vectorProperties = ConvertAlgVectorSize[algProperties, vectorBits];
	
	Return[vectorProperties];
];

(* This function changes an algProperties keyvalue list to one with a maximum vector size of maxVectorBits and returns a new keyvalue list with updated values *)
ConvertAlgVectorSize[algProperties_, maxVectorBits_] :=
Block[{vectorProperties},
	vectorProperties = GetConvertedInstructionFractions[algProperties, maxVectorBits];
	
	Return[MergeKeyValueList[algProperties, vectorProperties]];
];

D0dreuseCDFToCount[algProperties_] :=
Block[{F0mem, LSys, D0dreuse},
	F0mem = VMixGetData[GetKeyValue[algProperties, "F0mem"]];
	LSys = GetKeyValue[algProperties, "LSys"];
	D0dreuse = GetKeyValue[algProperties, "D0dreuse"];
	
	Return[Table[{i[[1]], i[[2]]*F0mem*LSys}, {i, D0dreuse}]];
];

D0dreuseCountToCDF[VMixList_, LSys_, D0dreuseCount_] :=
Block[{F0mem},
	F0mem = VMixGetData[GetKeyValue[VMixList, "F0mem"]];
	
	Return[Table[{i[[1]], i[[2]]/(F0mem*LSys)}, {i, D0dreuseCount}]];
];

SetAttributes[IncreaseCumulativeReuseCount, HoldFirst];
IncreaseCumulativeReuseCount[D0dreuseCount_, L_] :=
Block[{},
	(D0dreuseCount[[#, 2]]+=L) & /@ Range[Length[D0dreuseCount]];
];

propertyList = {"F0load", "F0store", "F0mem",
					"F0addr", "F0int",  "F0intOnly", "F0intmul",
					"F0intdiv", "F0fp", "F0fpmul", "F0fpdiv",
					"F0control", "F0vector", "F0other"}; 
totalsPropertyList = {"F0mem", "F0addr", "F0int", "F0fp", "F0control", "F0vector", "F0other"};

(* Generate a new subKeyValuelist with the instruction fractions, new LSys and updated ILP and ILP per type *)
GetConvertedInstructionFractions[algProperties_, maxVectorBits_] :=
(* UNCOMMENT FOR CACHING GetConvertedInstructionFractions[algProperties, maxVector] =*)
BlockCheck[{index, AccumulateLSys, L, F, VMix, VMixEmpty, VMixCount,
	LSys, L0int, L0mem, L0control, L0fp,
	ILP0, ILP0int, ILP0mem, ILP0control, ILP0fp,
	Span0, Span0int, Span0mem, Span0control, Span0fp,
	D0dreuse, D0dreuseCount, n0Bits, Ltempint, indexInt, intOpsQ, memOpsQ, controlOpsQ, fpOpsQ},
	n0Bits = 64; (* TODO: set to archproperty *)
	
	(* Get all the instruction couns *)
	LSys = GetAlgKeyValueCheck[algProperties, "LSys"];
	L0int = (VMixGetData[GetAlgKeyValueCheck[algProperties, "F0int"]] + VMixGetData[GetAlgKeyValueCheck[algProperties, "F0addr"]]) * LSys;
	L0mem = VMixGetData[GetAlgKeyValueCheck[algProperties, "F0mem"]] * LSys;
	L0control = VMixGetData[GetAlgKeyValueCheck[algProperties, "F0control"]] * LSys;
	L0fp = VMixGetData[GetAlgKeyValueCheck[algProperties, "F0fp"]] * LSys;
	(* Get old ILP and calculate span *)
	ILP0 = GetAlgKeyValueCheck[algProperties, "ILP0"];
	ILP0int = GetAlgKeyValueCheck[algProperties, "ILP0int"];
	ILP0mem = GetAlgKeyValueCheck[algProperties, "ILP0mem"];
	ILP0control = GetAlgKeyValueCheck[algProperties, "ILP0control"];
	ILP0fp = GetAlgKeyValueCheck[algProperties, "ILP0fp"];
	(* Set the xOpsQ to True or False such that we know if we have ops in the first place. *)
	intOpsQ = !(L0int == 0 || ILP0int == 0);
	memOpsQ = !(L0mem == 0 || ILP0mem == 0);
	controlOpsQ = !(L0control == 0 || ILP0control == 0);
	fpOpsQ = !(L0fp == 0 || ILP0fp == 0);
	(* Calculate the span *)
	Span0 = LSys / ILP0;
	Span0int = If[intOpsQ, L0int / ILP0int, 0];
	Span0mem = If[memOpsQ, L0mem / ILP0mem, 0];
	Span0control = If[controlOpsQ, L0control / ILP0control, 0];
	Span0fp = If[fpOpsQ, L0fp / ILP0fp, 0]; (* If fpOpsQ is False, we set the span to zero as well. *)
	
	(* Get the reuse distance in instruction counts *)
	D0dreuseCount = D0dreuseCDFToCount[algProperties];
		
	(* Create an empty set of VMixCounts *)
	VMixEmpty = {};
	VMixCanonize[VMixEmpty];
	VMixCount = Table[{key, VMixEmpty}, {key, propertyList}];
	
	(* We first convert all fractions to instruction counts *)
	Do[
		Do[
			Do[
				VMix = GetKeyValue[algProperties, key];
				L = VMixGetData[VMix, VectorLength->vec, DataLength->bits] * LSys;
				index = FirstPosition[VMixCount[[;;,1]], key][[1]];
				VMixSetData[VMixCount[[index, 2]], vec, bits, L];
				, {bits, GetDataLengths[]}
			], {vec, GetVectorLengths[]}
		], {key, propertyList}
	];
	
	(* Now we add all larger vectors to the largest vector size allowed and correct the instruction count.
	We only change the instruction count if the key is in totalsPropertyList, otherwise we account instructions double
	We operate on a reverted vectorlength list (largest first) *)
	
	indexInt = FirstPosition[VMixCount[[;;,1]], "F0int"][[1]];
	AccumulateLSys = LSys;
	Do[
		index = FirstPosition[VMixCount[[;;,1]], key][[1]];
		Do[
			Do[
				If[(vec * bits > maxVectorBits && vec > 1) || (bits == 0 && vec * n0Bits > maxVectorBits && vec > 1),
					(* The current vector size is larger than the max. vector length request, add to half-size vector *)
					L = VMixGetData[VMixCount[[index, 2]], VectorLength->vec, DataLength->bits];
					
					Which[(key == "F0vector" && vec/2 == 1) || (key == "F0other" && vec/2 == 1),
						(* Special case for F0vector which do not appear in scalar *)
						AccumulateLSys -= L;
						(* Erase old count *)
						VMixSetData[VMixCount[[index, 2]], vec, bits, 0];
						,
						key == "F0vector",
						(* Special case for F0Vector that stays constant for decreased vector sizes *)
						L = L + VMixGetData[VMixCount[[index, 2]], VectorLength->vec/2, DataLength->bits];
						(* Store new count *)
						VMixSetData[VMixCount[[index, 2]], vec/2, bits, L];
						(* Erase old count *)
						VMixSetData[VMixCount[[index, 2]], vec, bits, 0],
						
						True ,
						(* Correct LSys for the new amount *)
						If [MemberQ[totalsPropertyList, key],
							(*Add to total *)
							AccumulateLSys += L;
							(*And add to the right ILP per type L *)
							(* For mem, we add extra to reuse size 0 , and not ILP per type mem*)
							Switch[key,
								"F0mem",  Span0mem += L; L0mem += L; IncreaseCumulativeReuseCount[D0dreuseCount, L]; Ltempint = L + VMixGetData[VMixCount[[indexInt, 2]], VectorLength->1, DataLength->n0Bits]; VMixSetData[VMixCount[[indexInt, 2]], 1, n0Bits, Ltempint]; AccumulateLSys += L; Span0int += L; L0int += L,
								(*"F0mem", D0dreuseCount[[1,2]] += L,*)
								"F0addr", L0int += L;,
								"F0int", L0int += L,
								"F0fp", L0fp += L,
								"F0control", L0control += L
							];
						];
						(* Multiply by two to get the number of instructions at size half. *)
						L *= 2;
						(* Load an add the old value *)
						L = L + VMixGetData[VMixCount[[index, 2]], VectorLength->vec/2, DataLength->bits];
						(* Store new count *)
						VMixSetData[VMixCount[[index, 2]], vec/2, bits, L];
						(* Erase old count *)
						VMixSetData[VMixCount[[index, 2]], vec, bits, 0];
			 
					(* Otherwise, do nothing *)
					];
				];
				, {bits, GetDataLengths[]}
			], {vec, Sort[GetVectorLengths[], Greater]}
		], {key, propertyList}
	];
	
	(* Recalculate all instruction fractions *)
	Do[
		index = FirstPosition[VMixCount[[;;,1]], key][[1]];
		Do[
			Do[
				F = VMixGetData[VMixCount[[index, 2]], VectorLength->vec, DataLength->bits] / AccumulateLSys;
				VMixSetData[VMixCount[[index, 2]], vec, bits, F];
				, {bits, GetDataLengths[]}
			], {vec, GetVectorLengths[]}
		], {key, propertyList}
	];
	
	(* Recalculate the ILP *)
	ILP0 = AccumulateLSys / Span0;
	ILP0int = If[intOpsQ && Span0int != 0, L0int / Span0int, 0];
	ILP0mem = If[memOpsQ && Span0mem != 0, L0mem / Span0mem, 0];
	ILP0control = If[controlOpsQ && Span0control != 0, L0control / Span0control, 0];
	ILP0fp = If[fpOpsQ && Span0fp != 0, L0fp / Span0fp, 0]; (* Check if span is 0, if so, set ILP to 0 *)
	
	(* Recalculate reuse distance *)
	D0dreuse = D0dreuseCountToCDF[VMixCount, AccumulateLSys, D0dreuseCount];
	(*D0dreuse = D0dreuseCountToCDF[algProperties, AccumulateLSys, D0dreuseCount];*)
	Return[Join[{{"LSys", AccumulateLSys}, {"D0dreuse", D0dreuse}, {"ILP0", ILP0}, {"ILP0int", ILP0int}, {"ILP0mem", ILP0mem},  {"ILP0control", ILP0control}, {"ILP0fp", ILP0fp} }, VMixCount]];
	Return[Join[{{"LSys", AccumulateLSys} ,{"D0dreuse", D0dreuse}(*, {"ILP0", ILP0}, {"ILP0int", ILP0int},  {"ILP0mem", ILP0mem},  {"ILP0control", ILP0control}, {"ILP0fp", ILP0fp} *) }, VMixCount]];
	
];

(* Get the maximum size of the vectors in bits. Returns zero on no vectors *)
GetMaxVectorSizeBits[algProperties_] :=
Block[{groups, data, ret},
	(* Get all groups *)
	groups = SelectFirst[GetInstrMixGroups[], #[[1]] == 1. && StringMatchQ[#[[2]], "Equal"] &][[3]];
	(* We accumulate all data such that we only have to check for the largest values once. *)
	data = Table[GetAlgKeyValueCheck[algProperties, key], {key, groups}];
	data = VMixAccumulate[data];
	ret = 0;
	Do[
		Do[
			If[vec >= 2, (* Only for vectors *)
				If[vec * bit > ret, (* We only check if we can actually set ret to a larger value *)
					If[VMixGetData[data, VectorLength -> vec, DataLength -> bit] != 0, (* Check if there is a fraction of instructions for this specific vector and data length *)
						ret = vec * bit (* If yes, update ret with the new vector size *)
					]
				]
			];
			, {vec, GetVectorLengths[]}
		]
		, {bit, GetDataLengths[]}
	];
	Return[ret];
];

End[] (* End Private Context *)

EndPackage[]