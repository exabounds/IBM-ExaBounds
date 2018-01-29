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

BeginPackage["InstructionMix`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"];

(* 
	Fill missing data in a vectorized instruction type. This function is HoldFirst.
	WARNING, this function erase PISA data for vector lengths not included in vectorLengths.
	For PISA data lengths not in dataLengths, their value is accumulated towards the upper closest value in dataLengths.
	If there is no value in dataLengths higher than the PISA reported one, the data is not accumulated (lost).
*)
VMixCanonize::usage = 
		"VMixCanonize[VMixKeyValueList]"
		
(*
	Given a VMixKeyValueList (of a specific instruction type), get the data about a target vector-length and data-length.
	Options:	"VectorLength" -> "Accumulate" (* default "Accumulate" means that we return cumulative data for all vector lengths *)
				"DataLength" -> "Accumulate" (* default "Accumulate" means that we return cumulative data for all data lengths (measured in bits) *)	
	
	VMixKeyValueList must be in canonical form, that is it should include data for all and only vector lenghts
		in vectorLengths and data length in dataLengths. Additionally, the ordering of data should follow the
		one in the vectors: vectorLengths and dataLengths. 
*)
VMixGetData::usage = 
		"VMixGetData[VMixKeyValueList]"	
		
(*
	This function sets a specific value in a VMixKeyValueList
*)
VMixSetData::usage = 
	"VMixSetData[VMixKeyValueList_, vec_, bits_, val_]"

(*
	Returns a VMixKeyValueList structure containg absolut instruction count information rather than relative data.
*)
VMixGetAbsoluteCount::usage = 
	"VMixGetAbsoluteCount[VMixKeyValueList_,totalIcount_]"

(*
	This function accumulates all VMixKeyValueLists in the list
*)
VMixAccumulate::usage = 
		"VMixAccumulate[VMixKeyValueListList]"
		
(*
	VMultiply the instruction mix times a constant (useful for managing relative--absolute instr count conversion)
*)
VMixMul::usage = 
		"VMixMul[VMixKeyValueListList,scalarVal]"
		
(*
	Transorm a VMixKeyValueList into a VMixAssociation.
*)
	VMixKeyValueList2Association::usage = 
		"VMixKeyValueList2Association[VMixKeyValueList]"
			
(*
	Transorm a VMixAssociation into a VMixKeyValueList.
*)
	VMixAssociation2KeyValueList::usage = 
		"VMixAssociation2KeyValueList[VMixAssociation]"
	
(*
	Remove vector length and data length information and return the list of values in the mix. 
*)
VMixFlatten::usage = 
		"VMixFlatten[VMixFlatten]"
		



(* Test that the instruction mix sum up to 1 and that instructions belonging to a group sum up to the total group count *)
TestInstrMix::usage = 
		"TestInstrMix[keyValueList]"
		
(* Adjust mix using proportions such to let the TestInstrMix pass *)
FixInstrMix::usage =
		"FixInstrMix[keyValueList]"

GetVectorLengths::usage =
	"GetVectorLengths[]"

GetDataLengths::usage =
	"GetDataLengths[]"
	
GetInstrMixGroups::usage
	"GetInstrMixGroups[]"

Begin["`Private`"] (* Begin Private Context *) 

(* 
	For vector instruction-types we reqire information about the following vector length.
	In the functions to access mix information, vector length of "Accumulate" refers to the overall sum
	for all vector length.
	Other vector lengths are not supported and 0. will be returned.
*)
vectorLengths = {1, 2, 4, 8, 16};
GetVectorLengths[] := vectorLengths; (* Export for other notebooks *)

(* 
	For vector instruction-types we reqire information about the following data lengths (in bits).
	Data length of 0 is for unknown data types
	In the functions to access mix information, data length of 0 refers to the overall sum for all data-length.
	Other data length will be searched for and, if not found, 0. will be returned.
	If PISA includes information for data length not power of 2, these will be ceiled to the closest power of two.  
*)(* Keep them sorted! *)
dataLengths = {0, 4, 8, 16, 32, 64, 128};
GetDataLengths[] := dataLengths; (* Export for other notebooks *)

(*
	A metric in an instruction mix can be either a scalar (for instruction types without vector/data-type information)
	or a structure as follows:
	VMixKeyValueList = {
		{(*vector lengt*)1, { {(*data-length in bits*)0,(*percentage*) 0.1}, {4,0.01}, {8,0.04}, {16,0.21}, {32,0.04}, {64,0.04}, {128,0.04} }},
		{2, { {0, 0.1}, {4, 0.1}, {8,0.04}, {16,0.21}, {32,0.04}, {64,0.04}, {128,0.04} } },
		{4, { {0, 0.1}, {4, 0.1}, {8,0.04}, {16,0.21}, {32,0.04}, {64,0.04}, {128,0.04} } },
		{8, { {0, 0.1}, {4, 0.1}, {8,0.04}, {16,0.21}, {32,0.04}, {64,0.04}, {128,0.04} } },
		{16, { {0, 0.1}, {4, 0.1}, {8,0.04}, {16,0.21}, {32,0.04}, {64,0.04}, {128,0.04} } }
	}
	
	Few functions use VMixAssociation as data structure:
	VMixAssocaition = <|
		{(*vector lengt*)1,(*data-length in bits*)0} -> (*percentage*) 0.1, {1,4}->0.01, {1,8->0.04, {1,16}->0.21, {1,32}->0.04, {1,64->0.04, {1,128}->0.04,
		{2,0} -> 0.1, {2,4}->0.01, {2,8}->0.04, {2,16}->0.21, {2,32}->0.04, {2,64->0.04, {2,128}->0.04,
		{4,0} -> 0.1, {4,4}->0.01, {4,8}->0.04, {4,16}->0.21, {4,32}->0.04, {4,64->0.04, {4,128}->0.04,
		{8,0} -> 0.1, {8,4}->0.01, {8,8}->0.04, {8,16}->0.21, {8,32}->0.04, {8,64->0.04, {8,128}->0.04,
		{16,0} -> 0.1, {16,4}->0.01, {16,8}->0.04, {16,16}->0.21, {16,32}->0.04, {16,64->0.04, {16,128}->0.04
	|>
*)

VMixKeyValueList2Association[VMixKeyValueList_] := Module[{res,vData,dData},
	res = Association[Reap[
		Do[
			Do[
				Sow[ {vData[[1]],dData[[1]]}->dData[[2]] ];
			,{dData,vData[[2]]}];
		,{vData,VMixKeyValueList}];
	][[2,1]] ];
	
	Return[res];
];
			
VMixAssociation2KeyValueList[VMixAssociation_] := Module[{res,vl,dl,vData,p,vLengths,dLengths,keys},
	keys = Keys[ VMixAssociation ];
	vLengths = Union[keys[[All,1]]];
	dLengths = Union[keys[[All,2]]];
	
	res = Reap[
		Do[
			vData = Reap[Do[
				p = VMixAssociation[{vl,dl}];
				If[!MissingQ[p],
					Sow[{dl,p}];
				];
			,{dl,dLengths}]][[2,1]];
			
			Sow[{vl,vData}];
		,{vl,vLengths}];
	][[2,1]];
	
	Return[res];
];


SetAttributes[VMixCanonize, HoldFirst]; (* This let the first parameters behave as a reference. *)
VMixCanonize[VMixKeyValueList_] := Module[{vlAvailable,vl,dlData,VMixCanonizeDL},
	(* Canonize values for data lengths *)
	VMixCanonizeDL[dlLocalData_] := Module[{d,nextD,localRes,v},
		localRes = ({#,0.} & /@ dataLengths);
		
		Do[(* Here d is: {dataLength, value} *)
			nextD = SelectFirst[dataLengths,# >= d[[1]]&];
			If[!MissingQ[nextD],(* Loosing data for data-length larger than Max[dataLengths]. *)
				v = GetKeyValue[localRes,nextD] + d[[2]];
				
				localRes = localRes /. {nextD, _} -> {nextD, v};
			];
			
		,{d,dlLocalData}];
		
		localRes
	];
	
	(* Get preliminary vector length list *)
	If[!ListQ[VMixKeyValueList] || VMixKeyValueList==={},
		vlAvailable = {};
		, 
		vlAvailable = VMixKeyValueList[[All,1]];
	];
	
	(* Canonize values for vector lengths *)
	VMixKeyValueList = Reap[
		Do[
			If[MemberQ[vlAvailable,vl],
				dlData = GetKeyValue[VMixKeyValueList,vl];
				,
				dlData = {};
			];
			Sow[{vl,VMixCanonizeDL[dlData]} ];
		,{vl,vectorLengths}];
	][[2,1]];
];

Options[VMixGetData] = 	{
							"VectorLength" -> "Accumulate",
							"DataLength" -> "Accumulate"
						};
VMixGetData[VMixKeyValueList_,OptionsPattern[]] :=
Module[{res,vectorLength,dataLength, vData, tmp},
	If[AtomQ[VMixKeyValueList],(* This grants most of backward compatability support *)
		Return[VMixKeyValueList];
	];
	
	vectorLength = OptionValue["VectorLength"];
	dataLength = OptionValue["DataLength"];
	
	If[vectorLength == "Accumulate",(* Accumulate over vector lengths *)(* This works only after canonization. *)
		tmp = #[[2]] & /@ VMixKeyValueList;
		tmp = Total[tmp];
		vData = {#[[1]]/Length[vectorLengths],#[[2]]} & /@ tmp; 
		
		,(* vector length specified *)
		If[MemberQ[vectorLengths,vectorLength],
			vData = GetKeyValue[ VMixKeyValueList, vectorLength ];
			,
			
			(* Vector length not available *)
			Return[0.];
		];
	];
	
	If[dataLength == "Accumulate",(* Accumulate over data lengths *)
		res = Total[ vData[[All,2]] ];
		
		,(* data length specified *)
		If[MemberQ[dataLengths,dataLength],
			res = GetKeyValue[ vData, dataLength ]; 
			
			
			,(* Vector length not available *)
			Return[0.];
		];
	];
	
	Return[res];
];

VMixFlatten[VMixKeyValueList_] := Module[{},
	Return[Flatten[VMixKeyValueList[[All,2,All,2]]]];
];

SetAttributes[VMixSetData, HoldFirst];
VMixSetData[VMixKeyValueList_, vec_, bits_, val_] :=
Block[{vecIndex, bitsIndex},
	vecIndex = FirstPosition[VMixKeyValueList[[;;,1]], vec, 0, 1][[1]];
	bitsIndex = FirstPosition[VMixKeyValueList[[vecIndex,2]][[;;,1]], bits, 0, 1][[1]];
	
	VMixKeyValueList[[vecIndex,2,bitsIndex,2]] = val;
]

VMixGetAbsoluteCount[VMixKeyValueList_,totalIcount_] := Module[{res},
	res= VMixKeyValueList;
	res[[All,2,All,2]] = res[[All,2,All,2]] * totalIcount ;
	Return[res];
];

VMixAccumulate[VMixKeyValueListList_] := Module[{res,vDataList,vRes,listLength},
	listLength = Length[VMixKeyValueListList];
	res = VMixKeyValueListList // Transpose;
	
	res = Reap[Do[
		vRes = Total[vDataList[[All, 2]]];
		vRes = {#[[1]]/listLength,#[[2]]} & /@ vRes; 
		
		Sow[ {vDataList[[1, 1]], vRes } ];
	, {vDataList, res}]][[2, 1]];
	
	Return [res];
];

SetAttributes[VMixMul,HoldFirst];
VMixMul[VMixKeyValueListList_,scalarVal_] := Module[{},
	VMixKeyValueListList[[All, All, All, 2, All, 2]] =
						#[[All, All, 2, All, 2]] * scalarVal & /@ VMixKeyValueListList;
];


(*
	A list of of groups: {target, boundType, metrics }, where target is a bound for the metrics sum.
	boundType can be "Equal", "Greater", "Lower":
			--> target == Total[metrics] , target >= Total[metrics] , target <= Total[metrics] 
	Target can be either a metric name or an absolute number.
	If target is unavailable in PISA, then do not consider the group.
	If a metric in the group is not available, then confider it to be 0.
*)
instrMixGroups = {
	{"F0mem",	"Equal",	{"F0load", "F0store"}},
	{"F0int",	"Greater",	{"F0intmul", "F0intdiv"}},
	{"F0fp",	"Greater",	{"F0fpmul", "F0fpdiv"}},
	{1.,		"Equal",	{"F0mem", "F0addr", "F0int", "F0fp", "F0control", "F0vector", "F0other"}}
};
GetInstrMixGroups[] := instrMixGroups;

TestInstrMix::nm = "Instruction mix not valid because the rule: `1` -> {`2`,`3`,`4`} ";
TestInstrMix[keyValueList_] := Block[{res,rule, target, test, metrics,m, metricValues, flags},
	res = True;
	
	Do[
		target = rule[[1]];
		test = rule[[2]];
		metrics = rule[[3]];
		
		If[StringQ[target],
			flags = MemberQ[keyValueList[[All,1]],# ] & /@ Append[metrics,target];
			,
			flags = MemberQ[keyValueList[[All,1]],# ] & /@ metrics;
		];
		
		If[(And @@ flags),(* All metrics in the rule are valid *)
			target = If[StringQ[target], SelectFirst[keyValueList,#[[1]] == target&] , target];
			metricValues = Reap[Do[ Sow[ SelectFirst[keyValueList,#[[1]] == m&] ] ,{m,metrics}]][[2,1]];
			
			flags = MissingQ[#] & /@ Flatten[{metricValues,target}];
			If[!NumberQ[target], target = VMixGetData[target[[2]]] ];
			metricValues = VMixGetData[#] & /@ metricValues[[All,2]];
			
			Which[
				test == "Equal",
				If[!(target == Total[metricValues]),
					Message[TestInstrMix::nm,ToString[rule],target, test, metricValues ];
					res = False;
				],
				
				test == "Greater",
				If[!(target >= Total[metricValues]),
					Message[TestInstrMix::nm,ToString[rule] ];
					res = False;
				],
				
				test == "Lower",
				If[!(target <= Total[metricValues]),
					Message[TestInstrMix::nm,ToString[rule] ];
					res = False;
				]
			];
			
		];
		
	,{rule,instrMixGroups}];
	
	Return[res];
];

FixInstrMix::nm = "Some metrics of instruction mix are missing: `1`";
SetAttributes[FixInstrMix, HoldFirst]; (* This let the first parameters behave as a reference. *)
FixInstrMix[keyValueList_] := Block[{rule, target, targetC, metrics, vmixKeys, k, vl,dl, total,
								test, m, metricValues, flags, ratio},
	
	vmixKeys = Flatten[Table[{vl, dl}, 
 							{vl, InstructionMix`Private`vectorLengths}, 
 							{dl, InstructionMix`Private`dataLengths}
 					], 1];
	Do[
		target = rule[[1]];
		test = rule[[2]];
		metrics = rule[[3]];
		
		If[StringQ[target],
			flags = MemberQ[keyValueList[[All,1]],# ] & /@ Append[metrics,target];
			,
			flags = MemberQ[keyValueList[[All,1]],# ] & /@ metrics;
		];
		
		
		If[(And @@ flags),(* All metrics in the rule are valid *)
		
			metricValues = Reap[Do[ Sow[ SelectFirst[keyValueList,#[[1]] == m&] ] ,{m,metrics}]][[2,1]];
			
			target = If[StringQ[target],
				(* Get the target vectorized instruction mix from the metric itself *)
				targetC = Null;
				SelectFirst[keyValueList,#[[1]] == target&] [[2]]
				,
				
				(* Reconstruct the target vectorized instruction mix from the underlying metrics *)
				targetC = target;(* save the constant *)
				target = VMixAccumulate[metricValues[[All,2]]]
			];
			target = Association[(* Make target an association *)
				# -> (VMixGetData[target,"VectorLength"->#[[1]],"DataLength"->#[[2]] ] * targetC)
				& /@ vmixKeys 
			];
			(* Adjust target mix to fit in the constraint *)
			If[targetC=!=Null,
				ratio = targetC / Total[target];
				
				target = target * ratio;
			];
		
			
			metricValues = metricValues[[All,2]];
			flags = False;(* Needs modifications? *)
			Do[
				vl = k[[1]];
				dl = k[[2]];
				total = Total[
							VMixGetData[#,"VectorLength"->vl,"DataLength"->dl]&/@metricValues
							];
				Which[
					test == "Equal",
					If[!(target[k] == total),
						ratio = target[k]/total;
						VMixSetData[metricValues[[#]],vl,dl, VMixGetData[metricValues[[#]],"VectorLength"->vl,"DataLength"->dl] * ratio]
									&/@ Range[Length[metricValues]];
						
						flags = True;
					],
					
					test == "Greater",
					If[!(target[k] >= total),
						ratio = target[k]/total;
						VMixSetData[metricValues[[#]],vl,dl, VMixGetData[metricValues[[#]],"VectorLength"->vl,"DataLength"->dl] * ratio]
									&/@ Range[Length[metricValues]];
						
						flags = True;
					],
					
					test == "Lower",
					If[!(target[k] <= total),
						ratio = target[k]/total;
						VMixSetData[metricValues[[#]],vl,dl, VMixGetData[metricValues[[#]],"VectorLength"->vl,"DataLength"->dl] * ratio]
									&/@ Range[Length[metricValues]];
						
						flags = True;
					]
				];
				
				If[flags,(* Adjust *)
		   				keyValueList = keyValueList /. 
		   					((
		   						{metrics[[#]], _} -> {metrics[[#]], metricValues[[#]]}
		   					)& /@ Range[Length[metrics]] );
				];
			,{k,vmixKeys}];
			
			(*
			,(* Some metrics are missing *)
			Message[FixInstrMix::nm,ToString[Pick[metrics,flags]] ];
			This happens in the GUI when I generate predictions for a single metric at a time.
			*)
		];
		
	,{rule,instrMixGroups}];
];


End[] (* End Private Context *)

EndPackage[]